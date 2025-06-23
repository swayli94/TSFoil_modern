'''
This is a python interface for TSFOIL.

The wind tunnel option is not implemented.
The 'CLSET' option is not implemented.

Note
------
Data security:
- CRITICAL: All PyTSFoil instances in the same Python process share the same 
  underlying Fortran module data (tsf.common_data, tsf.solver_data, etc.)
- Creating multiple PyTSFoil objects (e.g., pytsfoil1 = PyTSFoil(), pytsfoil2 = PyTSFoil()) 
  will result in shared state - changes made by one instance affect all others
- This can lead to data corruption, incorrect results, and unpredictable behavior

Safe usage patterns:
1. Single instance per process: Use only one PyTSFoil instance at a time in each Python process
2. Sequential analysis: Complete one analysis before starting another
3. Multiprocessing: For parallel analyses, use multiprocessing.Pool where each process 
   gets its own isolated copy of the Fortran data
4. Process isolation: Each subprocess will have independent Fortran module variables

Example of UNSAFE usage (same process):
    pytsfoil1 = PyTSFoil()  # ⚠️  These share the same
    pytsfoil2 = PyTSFoil()  # ⚠️  Fortran data!

Example of SAFE usage (multiprocessing):
    import multiprocessing as mp
    def run_analysis(params):
        pytsfoil = PyTSFoil()  # ✅ Each process gets its own data
        # ... run analysis
    with mp.Pool() as pool:
        pool.map(run_analysis, case_list)

'''

import sys
import os
from pathlib import Path
import numpy as np
from scipy.interpolate import CubicSpline
from scipy import integrate

try:
    import tsfoil_fortran as tsf
except ImportError as e:
    print("ERROR: Could not import tsfoil_fortran module!")
    print(f"Import error: {e}")
    print()
    print("Make sure you have compiled the Fortran modules with f2py:")
    print("  python3 pyTSFoil/compile_f2py.py")
    sys.exit(1)


class PyTSFoil(object):
    '''
    Python interface for TSFOIL Fortran module.
    '''
    def __init__(self,
            airfoil_file: str,
            work_dir:str|None = None):
        '''
        Initialize the TSFoil object.
        '''
        self.config = {}
        self.airfoil = {'file': airfoil_file}
        self.mesh = {}
        self.data_summary = {}
        self.data_cpxs = {}
        self.data_field = {}

        # Change to parent directory where input/output files are located
        if work_dir is None:
            script_dir = Path(__file__).parent
            parent_dir = script_dir.parent
            os.chdir(parent_dir)
        else:
            os.chdir(work_dir)
            
        print(f"pyTSFoil working directory: {os.getcwd()}")
        print()
        
        self._default_config()
    
    def set_config(self, **kwargs):
        '''
        Set the configuration parameters.
        '''
        for key, value in kwargs.items():
            if key in self.config:
                self.config[key] = value
            else:
                raise ValueError(f"Invalid configuration parameter: {key}")
    
    def run(self):
        '''
        Run the TSFoil_modern main program flow (matching main.f90 exactly).
        '''
        self.initialize_data()
        
        self.set_airfoil()
        
        self.set_mesh()
        
        self.compute_mesh_indices()

        # Scale variables to similarity form
        tsf.solver_functions.scale()

        # Set far field boundary conditions
        tsf.solver_functions.farfld()
        
        self.compute_geometry_derivatives()
        
        # Compute finite difference coefficients
        tsf.solver_base.difcoe()
        
        # Set boundary conditions
        tsf.solver_functions.setbc(0)
        
        # Solve transonic flow equations
        tsf.main_iteration.solve()
        
        # Print final results
        tsf.io_module.print()
    
    def _default_config(self):
        '''
        Set the default configuration parameters.
        '''
        # Default configuration (user-input parameters, namelist /INP/ in io_module.f90)
        # NU, NL, DELTA are set in set_airfoil()
        # IMAXI, JMAXI are set in set_mesh()
        self.config = {
            'AK': 0.0,              # Free stream similarity parameter
            'ALPHA': 0.0,           # Angle of attack
            'BCTYPE': 1,            # Boundary condition identifiers (1 = free air, 2 = tunnel)
            'CVERGE': 0.00001,      # Error criterion for convergence
            'DVERGE': 10.0,         # Error criterion for divergence
            'EMACH': 0.75,          # Mach number
            'EPS': 0.2,             # Convergence tolerance
            'FCR': 1,               # Whether difference equations are fully conservative (True)
            'IPRTER': 100,          # Print interval for convergence history
            'KUTTA': 1,             # Whether Kutta condition is enforced (True)
            'MAXIT': 1000,          # Maximum number of iterations
            'PHYS': 1,              # Physical (True) vs similarity (False)
            'POR': 0.0,             # Porosity
            'RIGF': 0.0,            # Rigidity factor for transonic effects
            'SIMDEF': 3,            # Similarity scaling (1 = Cole, 2 = Spreiter, 3 = Krupp)
            'WCIRC': 1.0,           # Weight for circulation jump at trailing edge (0.0-1.0)
            'WE': [1.8, 1.9, 1.95], # SOR relaxation factors
            'NWDGE': 0,             # Viscous wedge parameters (0 = no wedge, 1 = Murman wedge, 2 = Yoshihara wedge)
            'REYNLD': 4.0E6,        # Reynolds number
            'WCONST': 4.0,          # Wall constant for Murman wedge
            'IFLAP': 0,             # Flap flag
            'DELFLP': 0.0,          # Flap deflection angle
            'FLPLOC': 0.77,         # Flap location
            'n_point_x': 81,        # Number of points in the x-direction (IMAXI)
            'n_point_y': 60,        # Number of points in the y-direction (JMAXI)
            'n_point_airfoil': 51,  # Number of points on the airfoil
        }
        
        # Default parameters
        self.skiprows : int = 1
        self.x_scale : float = 5.0
        self.y_scale : float = 4.0

    def initialize_data(self) -> None:
        '''
        Initialize the data in Fortran module and Python module.
        '''
        # Initialize common data
        tsf.common_data.initialize_common()
        
        # Initialize spline module (required for airfoil geometry calculations)
        tsf.spline_module.initialize_spline(2000)

        # Check parameters
        if self.config['EMACH'] < 0.5 or self.config['EMACH'] > 2.0:
            raise ValueError("EMACH must be between 0.5 and 2.0")
        if self.config['ALPHA'] < -9.0 or self.config['ALPHA'] > 9.0:
            raise ValueError("ALPHA must be between -9.0 and 9.0")
        if self.config['NWDGE'] > 0 and self.config['EMACH'] > 1.0:
            raise ValueError("NWDGE must be 0 if EMACH <= 1.0")
        
        # Set AK=0 for physical coordinates
        if self.config['PHYS'] == 1:
            self.config['AK'] = 0.0
            
        # Constants
        self.n_mesh_points = tsf.common_data.n_mesh_points
        self.nmp_plus1 = tsf.common_data.nmp_plus1
        self.nmp_plus2 = tsf.common_data.nmp_plus2
        
        # Open output files
        tsf.io_module.open_output_files()
    
        # Apply self.config to common data
        for key, value in self.config.items():
            setattr(tsf.common_data, key.lower(), value)

    def set_airfoil(self) -> None:
        '''
        Read the airfoil geometry from a file, and set the airfoil geometry.
        
        Attributes
        ----------
        airfoil_file : str
            The file containing the airfoil geometry.
            The data starts from the airfoil's trailing edge in the upper surface,
            and then goes counter-clockwise around the airfoil.
            
        skiprows : int
            The number of rows to skip in the airfoil file.
        '''
        x, y = np.loadtxt(self.airfoil['file'], skiprows=self.skiprows).T
        le_pos = x.argmin()

        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]
        
        # Interpolate the airfoil and get the maximum thickness (DELTA)
        x_interp = np.linspace(np.min(x), np.max(x), num=501)
        yu_interp = np.interp(x_interp, xu, yu)
        yl_interp = np.interp(x_interp, xl, yl)
        t_max = np.max(yu_interp - yl_interp)
        
        self.airfoil['t_max'] = t_max
        self.airfoil['x'] = x
        self.airfoil['y'] = y
        self.airfoil['xu'] = xu
        self.airfoil['yu'] = yu
        self.airfoil['xl'] = xl
        self.airfoil['yl'] = yl
        
        tsf.common_data.nu = xu.shape[0]
        tsf.common_data.nl = xl.shape[0]
        tsf.common_data.delta = np.float32(t_max)
        
        tsf.common_data.xu[:len(xu)] = xu.astype(np.float32)
        tsf.common_data.yu[:len(yu)] = yu.astype(np.float32)
        tsf.common_data.xl[:len(xl)] = xl.astype(np.float32)
        tsf.common_data.yl[:len(yl)] = yl.astype(np.float32)

    def set_mesh(self) -> None:
        '''
        Set the mesh coordinates.
        
        The user-defined mesh is provided to TSFOIL with 1-d arrays XIN, YIN.
        XIN and YIN are the x-coordinates and y-coordinates of the mesh points, respectively.
        The mesh is a 2-d nonuniform Cartesian grid.
        The airfoil has a unit chord length, the leading edge is at (0,0), and the trailing edge is at (1,0).
        The XIN distributes more points near x=0 and x=1, and the YIN distributes more points near y=0.
        
        Attributes
        ----------
        n_point_x: int
            The number of points in the x-direction.
            
        n_point_y: int
            The number of points in the y-direction.
            
        n_point_airfoil: int
            The number of points on the airfoil.
            
        x_scale: float
            The range of the x-coordinate of the mesh, x in [-x_scale, x_scale].
            
        y_scale: float
            The range of the y-coordinate of the mesh, y in [-y_scale, y_scale].
        '''

        #* Generate x-coordinates with clustering near x=0 and x=1 (interior points)
        # Split domain into three segments: [-x_scale, 0], [0, 1], [1, x_scale]
        # Distribute remaining points between left and right segments
        n_remaining = int((self.config['n_point_x']-self.config['n_point_airfoil'])*0.5) + 1
        
        # Segment 1: [x_min, 0] with clustering near x=0 (right end)
        x_left_norm = self.clustcos(n_remaining, a0=1.00, a1=0.999, beta=1.0)
        x_left = - x_left_norm[::-1] * self.x_scale
        
        # Segment 2: [0, 1] with clustering at both ends
        x_center = self.clustcos(self.config['n_point_airfoil'], a0=0.01, a1=0.96, beta=1.0)
        
        # Segment 3: [1, x_max] with clustering near x=1 (left end)
        x_right_norm = self.clustcos(n_remaining, a0=0.001, a1=0.1, beta=1.0)
        x_right = 1 + x_right_norm * (self.x_scale - 1)
        
        # Combine segments, removing duplicate boundary points
        xx = np.concatenate([
            x_left[:-1],    # Exclude right boundary (x=0)
            x_center,
            x_right[1:]     # Exclude left boundary (x=1)
        ])
        
        #* Generate symmetric distribution of y-coordinates with clustering near y=0
        half_points = self.config['n_point_y'] // 2 + 1  # Include y=0
        y_half = self.clustcos(half_points, a0=1.0, a1=0.999, beta=2.0)
        
        # Create symmetric distribution: negative half + positive half
        # y_half goes from 0 to 1, we want symmetric distribution about 0
        yy_normalized = np.concatenate([
            -y_half[1:][::-1],  # Negative side: -1 to 0 (excluding 0)
            y_half[1:]          # Positive side: 0 to 1 (excluding 0)
        ])
        
        # Scale to [-y_scale, y_scale]
        yy = yy_normalized * self.y_scale
        
        # Store mesh parameters
        self.config['n_point_x'] = xx.shape[0]
        self.config['n_point_y'] = yy.shape[0]

        self.mesh['x_min'] = -self.x_scale
        self.mesh['x_max'] = self.x_scale
        self.mesh['y_min'] = -self.y_scale
        self.mesh['y_max'] = self.y_scale
        self.mesh['xx'] = xx
        self.mesh['yy'] = yy
        self.mesh['xx_airfoil'] = x_center

        # imax, jmax equals to imaxi, jmaxi,
        # because the mesh point is already checked to be odd
        # for all sections (which was done in CKMESH in mesh_module.f90)
        tsf.common_data.imaxi = self.config['n_point_x']
        tsf.common_data.jmaxi = self.config['n_point_y']
        tsf.common_data.imax = self.config['n_point_x']
        tsf.common_data.jmax = self.config['n_point_y']
        
        # The final mesh array x, y is the same as xin, yin
        tsf.common_data.xin[:len(xx)] = xx.astype(np.float32)
        tsf.common_data.yin[:len(yy)] = yy.astype(np.float32)
        tsf.common_data.x[:len(xx)] = xx.astype(np.float32)
        tsf.common_data.y[:len(yy)] = yy.astype(np.float32)
    
    def compute_mesh_indices(self):
        '''
        Compute mesh indices, including:
        1. ILE and ITE (leading and trailing edge)
        2. JLOW and JUP (lower and upper surface)
        '''
        # Find first point where X >= 0.0 (leading edge)
        ile = np.where(self.mesh['xx'] >= 0.0)[0][0] # 0-based index
        self.mesh['ile'] = ile # 0-based index
        tsf.common_data.ile = ile + 1 # 1-based index
        
        # Find first point where X > 1.0 (trailing edge)
        ite = np.where(self.mesh['xx'] > 1.0)[0][0] # 0-based index
        self.mesh['ite'] = ite - 1 # 0-based index
        tsf.common_data.ite = ite # 1-based index

        # Find first point where Y >= 0.0 (upper surface)
        j = np.where(self.mesh['yy'] >= 0.0)[0][0] # 0-based index

        self.mesh['jlow'] = j - 1 # 0-based index
        self.mesh['jup'] = j # 0-based index
        tsf.common_data.jlow = j
        tsf.common_data.jup = j + 1
        
        # Number of points on airfoil
        self.mesh['nfoil'] = self.mesh['ite'] - self.mesh['ile'] + 1
        tsf.common_data.nfoil = self.mesh['nfoil']

    @staticmethod
    def clustcos(n_points: int, a0=0.0079, a1=0.96, beta=1.0, index_point: int|None=None) -> np.ndarray:
        '''
        Point distribution on x-axis [0, 1]. (More points at both ends)

        Parameters
        ----------
        n_points: int
            total amount of points
            
        a0: float
            Parameter for distributing points near x=0.
            Smaller a0, more points near x=0.
            
        a1: float
            Parameter for distributing points near x=1.
            Larger a1, more points near x=1.
            
        beta: float
            Parameter for distribution points.
            
        index_point: int|None
            The index of the point to return.
            If None, return all points.
            
        Returns
        -------
        xx: np.ndarray|float
            The x-coordinates of the points.
            If index_point is not None, return the x-coordinate of the point at the given index.
        
        Examples
        ---------
        >>> xx = clustcos(n, a0, a1, beta)
        >>> xx = clustcos(n, a0, a1, beta, index_point=i)

        '''
        aa = np.power((1-np.cos(a0*np.pi))/2.0, beta)
        dd = np.power((1-np.cos(a1*np.pi))/2.0, beta) - aa
        
        if isinstance(index_point, int):
            yt = index_point/(n_points-1.0)
        else:
            yt = np.linspace(0.0, 1.0, num=n_points)
        
        a  = np.pi*(a0*(1-yt)+a1*yt)
        xx = (np.power((1-np.cos(a))/2.0,beta)-aa)/dd

        return xx

    def compute_geometry_derivatives(self):
        '''
        Compute airfoil geometry's derivatives (equivalent to BODY)
        
        This function translates the Fortran BODY subroutine to Python using scipy.
        It performs cubic spline interpolation on airfoil surfaces, computes volume,
        handles flap deflection, and computes camber and thickness distributions.
        '''
        # Get data from common_data (Fortran module variables)
        delta = self.airfoil['t_max']
        rigf = self.config['RIGF']
        
        # Airfoil geometry coordinates 
        xu = self.airfoil['xu']
        yu = self.airfoil['yu'] 
        xl = self.airfoil['xl']
        yl = self.airfoil['yl']
        nu = xu.shape[0]
        nl = xl.shape[0]
        
        # Mesh coordinates
        xfoil = self.mesh['xx_airfoil']
        nfoil = self.mesh['nfoil']
        
        # Flap parameters
        iflap = self.config['IFLAP']
        delflp = self.config['DELFLP']  
        flploc = self.config['FLPLOC']
        
        # Scaling factor
        delinv = 1.0
        if self.config['PHYS'] == 1:
            delinv = 1.0 / delta

        # Upper surface cubic spline interpolation
        # Calculate derivatives at endpoints for boundary conditions
        dy1_u = (yu[1] - yu[0]) / (xu[1] - xu[0])
        dy2_u = (yu[nu-1] - yu[nu-2]) / (xu[nu-1] - xu[nu-2])
        
        # Create cubic spline with derivative boundary conditions
        cs_upper = CubicSpline(xu, yu, bc_type=((1, dy1_u), (1, dy2_u)))
        
        # Interpolate upper surface at mesh x-coordinates
        fu = cs_upper(xfoil) * delinv
        fxu = cs_upper(xfoil, 1) * delinv
        
        # Lower surface cubic spline interpolation  
        dy1_l = (yl[1] - yl[0]) / (xl[1] - xl[0])
        dy2_l = (yl[nl-1] - yl[nl-2]) / (xl[nl-1] - xl[nl-2])
        
        cs_lower = CubicSpline(xl, yl, bc_type=((1, dy1_l), (1, dy2_l)))
        
        # Interpolate lower surface at mesh x-coordinates
        fl = cs_lower(xfoil) * delinv
        fxl = cs_lower(xfoil, 1) * delinv
        
        # Compute volume by Simpson's rule
        vol = integrate.simpson(y=fu-fl, x=xfoil)
        
        # Add flap deflection if any
        if iflap != 0:
            dflap = delflp / 57.29578  # Convert degrees to radians
            sdflap = np.sin(dflap)
            
            # Find flap hinge point
            ifp = 0
            for i in range(nfoil):
                if xfoil[i] >= flploc:
                    ifp = i
                    break
            
            # Apply flap deflection
            for i in range(ifp, nfoil):
                dely = (xfoil[i] - flploc) * sdflap * delinv
                fu[i] = fu[i] - dely
                fl[i] = fl[i] - dely
                fxu[i] = fxu[i] - dflap * delinv
                fxl[i] = fxl[i] - dflap * delinv
        
        # Compute camber and thickness
        camber = 0.5 * (fu + fl)
        thick = 0.5 * (fu - fl)
        
        # Apply rigidity factor correction to surface slopes
        fxu = fxu / np.sqrt(1.0 + rigf * (delta * fxu)**2)
        fxl = fxl / np.sqrt(1.0 + rigf * (delta * fxl)**2)
        
        # Store results in common_data arrays
        tsf.common_data.vol = vol
        
        # Pad arrays to expected size
        tsf.common_data.fu[:nfoil] = fu.astype(np.float32)
        tsf.common_data.fl[:nfoil] = fl.astype(np.float32)
        tsf.common_data.fxu[:nfoil] = fxu.astype(np.float32)
        tsf.common_data.fxl[:nfoil] = fxl.astype(np.float32)
        tsf.common_data.xfoil[:nfoil] = xfoil.astype(np.float32)
        tsf.common_data.camber[:nfoil] = camber.astype(np.float32)
        tsf.common_data.thick[:nfoil] = thick.astype(np.float32)
        
        # Print or log geometry (equivalent to PRBODY call)
        print(f"Airfoil geometry computed successfully:")
        print(f"  Number of points: {nfoil}")
        print(f"  Volume: {vol:.6f}")
        print(f"  Max thickness: {delta:.6f}")
        if iflap != 0:
            print(f"  Flap deflection: {delflp:.2f} degrees at x={flploc:.3f}")
    

if __name__ == "__main__":
    
    
    pytsfoil = PyTSFoil(airfoil_file="rae2822.dat")
    
    pytsfoil.set_config(
        ALPHA=0.5,
        EMACH=0.75,
        MAXIT=9999,
        NWDGE=0,
        n_point_x=200,
        n_point_y=80,
        n_point_airfoil=100,
        EPS=0.2,
        CVERGE=1e-6
    )
    
    pytsfoil.run()


