import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
if path not in sys.path:
    sys.path.append(path)

import numpy as np
import pandas as pd
import os
from io import StringIO
from scipy.optimize import fsolve

import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import imageio


NUM_LEVELS = 100
CBAR_TIKCS = 0.1

# PROGRAM_NAME = "tsfoil"   # for old version
PROGRAM_NAME = "tsfoil_modern"


class Plotter(object):

    def gen_cmap(self):
        c = ["blue","cyan","lightgreen", "yellow","red"]
        v = np.linspace(0, 1, len(c))
        cmap=LinearSegmentedColormap.from_list('bgr',list(zip(v,c)), N=128)
        return cmap

    def gen_png_filename(self, row):
        values = row[["airfoil", "mach", "alpha"]].to_list()
        return "_".join(map(str, values)) +".png"

    def gen_vertical_label(self, row):
        # Get values, using fallbacks if columns don't exist
        mach_val = self.config.get("EMACH", 0.75) if "mach" not in row.index else row["mach"]
        alpha_val = self.config.get("alpha", 0.0) if "alpha" not in row.index else row["alpha"]
        cl_val = row.get("cl", 0.0)
        cm_val = row.get("cm", 0.0) 
        cdwave_val = row.get("total cdwave", 0.0)
        
        values = [mach_val, alpha_val, cl_val, cm_val, cdwave_val]
        names  = [x.ljust(8) for x in ["Mach", "Alpha", "CL", "CM", "CDwave"]]
        airfoil= row["airfoil"].upper().center(17)
        details= "\n".join(["{}={:8.5f}".format(k,v) for k,v in zip(names, values)])
        return "{}\n{}".format(airfoil, details)

    def gen_horizontal_header(self):
        return "".join([x.center(8) for x in ["Mach", "Alpha", "CL", "CM", "CDwave"]])

    def gen_horizontal_label(self, row):
        values = row[["mach", "alpha", "cl", "cm", "total cdwave"]].to_list()
        return "".join(["{:8.5f}".format(v) for v in values])

    def gen_cpx_mach_ax(self, mach=False):
        fig, ax = plt.subplots(figsize=(8, 6))
        plt.rcParams['font.family'] = 'monospace'

        ax.set_title("Mach-x" if mach else "Cp-x")
        ax.set_ylabel("Mach" if mach else "Cp")
        ax.set_xlabel("x/c")

        ax.set_xlim(-0.1, 1.1)
        if not mach:
            ax.invert_yaxis()
        return ax
        
    def legend_magic(self, ax):
        leg = ax.legend(frameon=False, loc="upper right")
        for item in leg.legend_handles:
            item.set_visible(False)
            
    def plot_cpx(self, buffer_row=-1, save_to_folder=None):
        row = self.buffer.iloc[buffer_row]
        ax = self.gen_cpx_mach_ax(mach=False)
        ax.axhline(y=row["cp*"], color='r', linestyle='--', lw=1)
        ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
                np.concatenate([row['cp_up'], row['cp_low'][::-1]]), 
                lw=0.8, label=self.gen_vertical_label(row))

        self.legend_magic(ax)
        
        if save_to_folder:
            filename = os.path.join(save_to_folder, f"cp_x_mach_{row['mach']:.3f}.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")
        
    def plot_machx(self, buffer_row=-1, save_to_folder=None):
        row = self.buffer.iloc[buffer_row]
        ax = self.gen_cpx_mach_ax(mach=True)
        ax.axhline(y=1, color='r', linestyle='--', lw=1)
        ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
                np.concatenate([row['m_up'], row['m_low'][::-1]]), 
                lw=0.8, label=self.gen_vertical_label(row))

        self.legend_magic(ax)
        
        if save_to_folder:
            filename = os.path.join(save_to_folder, f"mach_x_mach_{row['mach']:.3f}.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")

    def pseq_machx(self, df=None, save_to_folder=None):
        df = df if isinstance(df, pd.core.frame.DataFrame) else self.buffer
        ax = self.gen_cpx_mach_ax(mach=True)
        ax.axhline(y=1, color='r', linestyle='--', lw=1, label=self.gen_horizontal_header())

        for index, row in df.iterrows():
            ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
                    np.concatenate([row['m_up'], row['m_low'][::-1]]), 
                    lw=0.8, label=self.gen_horizontal_label(row))

        self.legend_magic(ax)
        
        if save_to_folder:
            filename = os.path.join(save_to_folder, "mach_sequence.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")

    def gen_contour_fig(self, mach=False):
        fig, ax = plt.subplots(figsize=(8, 6))
        plt.rcParams['font.family'] = 'monospace'

        ax.set_title('Contours of Mach' if mach else 'Contours of Cp')
        ax.set_ylabel("y/c")
        ax.set_xlabel("x/c")
        ax.set_xlim(-0.5, 1.5)
        ax.set_ylim(-0.75, 0.75)

        return fig, ax
        
    def plot_contour(self, row, mach=False, savepng=False, custom_filename=None):
        fig, ax  = self.gen_contour_fig(mach)

        # Check if mesh data is available and valid
        if len(row["mesh_x"]) == 0 or len(row["mesh_y"]) == 0:
            ax.text(0.5, 0.5, "No mesh data available for contour plot", 
                   horizontalalignment='center', verticalalignment='center',
                   transform=ax.transAxes, fontsize=12)
            ax.set_title('Contour Plot - No Data')
            if savepng:
                if custom_filename:
                    filename = custom_filename
                else:
                    filename = self.gen_png_filename(row)
                plt.savefig(filename, dpi=300, bbox_inches='tight')
                plt.close()
                return filename
            return

        try:
            X, Y = np.meshgrid(row["mesh_x"], row["mesh_y"])
            Z = row["mach_map"] if mach else row["cp_map"]

            # Check if Z data is available and valid
            if Z.size == 0:
                raise ValueError("Contour data is empty")
            
            # Check dimensions compatibility
            if Z.shape != X.shape:
                print(f"Warning: Dimension mismatch - X,Y shape: {X.shape}, Z shape: {Z.shape}")
                # Try to fix by transposing Z if needed
                if Z.shape == X.shape[::-1]:
                    Z = Z.T
                    print("   Fixed by transposing Z")
                else:
                    raise ValueError(f"Cannot reconcile shapes: X,Y {X.shape} vs Z {Z.shape}")

            # Check for valid data range
            if np.isnan(Z).any() or np.isinf(Z).any():
                raise ValueError("Contour data contains NaN or infinite values")
            
            if Z.max() == Z.min():
                raise ValueError("Contour data has no variation (constant values)")

            increment = (Z.max() - Z.min())/NUM_LEVELS
            ax.text(-0.4, -0.7, "Increment :{:1.3f}".format(increment), fontsize=8, color='black')

            foil=[(0, 1), (0, 0), 'black']
            ax.plot(*foil, label=self.gen_vertical_label(row))
            
            cs = ax.contour(X, Y, Z, 
                    levels=np.linspace(Z.min(), Z.max(), NUM_LEVELS),
                    linewidths=0.5,
                    cmap=self.gen_cmap())

            sm = plt.cm.ScalarMappable(cmap = cs.cmap)
            sm.set_array(Z)  # Set the array for the ScalarMappable
            fig.colorbar(sm, ax=ax, ticks=np.arange(Z.min(), Z.max(), CBAR_TIKCS))

            self.legend_magic(ax)

        except Exception as e:
            print(f"Error in contour plotting: {e}")
            ax.text(0.5, 0.5, f"Error in contour plotting:\n{str(e)}", 
                   horizontalalignment='center', verticalalignment='center',
                   transform=ax.transAxes, fontsize=10)
            ax.set_title('Contour Plot - Error')

        if savepng:
            if custom_filename:
                filename = custom_filename
            else:
                filename = self.gen_png_filename(row)
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            plt.close()
            return filename

    def plot_isomach(self, buffer_row=-1, savepng=False, save_to_folder=None):
        if save_to_folder and not savepng:
            # If save_to_folder is specified, automatically save
            row = self.buffer.iloc[buffer_row]
            filename = os.path.join(save_to_folder, f"isomach_mach_{row['mach']:.3f}.png")
            self.plot_contour(row, True, savepng=True, custom_filename=filename)
            print(f"      Saved: {filename}")
        else:
            self.plot_contour(self.buffer.iloc[buffer_row], True, savepng)

    def plot_isocp(self, buffer_row=-1, savepng=False, save_to_folder=None):
        if save_to_folder and not savepng:
            # If save_to_folder is specified, automatically save
            row = self.buffer.iloc[buffer_row]
            filename = os.path.join(save_to_folder, f"isocp_mach_{row['mach']:.3f}.png")
            self.plot_contour(row, False, savepng=True, custom_filename=filename)
            print(f"      Saved: {filename}")
        else:
            self.plot_contour(self.buffer.iloc[buffer_row], False, savepng)

    def plot_grid(self, save_to_folder=None):
        row = self.buffer.iloc[-1]
        fig, ax = plt.subplots(figsize=(8, 6))

        # Check if mesh data is available
        if len(row["mesh_x"]) == 0 or len(row["mesh_y"]) == 0:
            ax.text(0.5, 0.5, "No mesh data available", 
                   horizontalalignment='center', verticalalignment='center',
                   transform=ax.transAxes, fontsize=12)
            ax.set_title('Grid Specification - No Data')
            if save_to_folder:
                filename = os.path.join(save_to_folder, "grid_layout.png")
                plt.savefig(filename, dpi=300, bbox_inches='tight')
                print(f"      Saved: {filename}")
            return

        ax.text(-0.4, -0.7, "{} x {}".format(len(row["mesh_x"]), len(row["mesh_y"])), fontsize=8)

        X, Y = np.meshgrid(row["mesh_x"], row["mesh_y"])
        ax.vlines(X[0], *Y[[0,-1],0], lw=0.5)
        ax.hlines(Y[:,0], *X[0, [0,-1]], lw=0.5)

        foil=[(0, 1), (0, 0), 'black']
        ax.plot(*foil)

        ax.set_title('Grid Specification')
        ax.set_ylabel("y/c")
        ax.set_xlabel("x/c")
        ax.set_xlim(-0.5, 1.5)
        ax.set_ylim(-0.75, 0.75)

        if save_to_folder:
            filename = os.path.join(save_to_folder, "grid_layout.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")

    def plot_mesh_tecplot(self, filename="mesh.dat", save_to_folder=None, show_every_nth=1):
        """Plot the full mesh from Tecplot format file with all grid lines"""
        try:
            # Read the full mesh data
            with open(filename, 'r') as f:
                lines = f.readlines()
            
            # Parse header to get grid dimensions
            zone_line = lines[1].strip()
            parts = zone_line.split()
            ni = int(parts[2])  # I= value  
            nj = int(parts[4])  # J= value
            
            # Read all data points
            data = []
            for i in range(2, len(lines)):
                line = lines[i].strip()
                if line:
                    x_val, y_val = map(float, line.split())
                    data.append([x_val, y_val])
            
            data = np.array(data)
            x_points = data[:, 0].reshape(nj, ni)
            y_points = data[:, 1].reshape(nj, ni)
            
            # Create plot
            fig, ax = plt.subplots(figsize=(12, 8))
            
            # Plot grid lines in both directions
            # Plot every nth line to avoid overcrowding
            for i in range(0, nj, show_every_nth):
                ax.plot(x_points[i, :], y_points[i, :], 'b-', lw=0.3, alpha=0.7)
            
            for j in range(0, ni, show_every_nth):
                ax.plot(x_points[:, j], y_points[:, j], 'b-', lw=0.3, alpha=0.7)
            
            # Highlight airfoil (assume it's around y=0, x=0 to 1)
            airfoil_mask = (np.abs(y_points) < 0.01) & (x_points >= 0) & (x_points <= 1)
            if np.any(airfoil_mask):
                airfoil_x = x_points[airfoil_mask]
                airfoil_y = y_points[airfoil_mask]
                ax.plot(airfoil_x, airfoil_y, 'r-', lw=2, label='Airfoil')
            
            ax.set_title(f'Computational Grid ({ni} x {nj} points, showing every {show_every_nth})')
            ax.set_xlabel('x/c')
            ax.set_ylabel('y/c')
            ax.set_aspect('equal')
            ax.grid(True, alpha=0.3)
            ax.legend()
            
            # Add info text
            ax.text(0.02, 0.98, f"Grid: {ni} x {nj} = {ni*nj} points", 
                   transform=ax.transAxes, fontsize=10, 
                   verticalalignment='top', bbox=dict(boxstyle="round,pad=0.3", facecolor="wheat"))
            
            if save_to_folder:
                filename_out = os.path.join(save_to_folder, "mesh.png")
                plt.savefig(filename_out, dpi=300, bbox_inches='tight')
                print(f"      Saved: {filename_out}")
            
        except FileNotFoundError:
            print(f"Error: {filename} not found")
        except Exception as e:
            print(f"Error reading mesh file: {e}")

    def plot_field_tecplot(self, filename="field.dat", save_to_folder=None, show_every_nth=1):
        """Plot the mesh from field.dat with contour field data"""
        try:
            x_unique, y_unique, mach_2d, cp_2d = self.extract_field_tecplot(filename)
            
            if len(x_unique) == 0 or len(y_unique) == 0:
                print(f"No valid field data found in {filename}")
                return
            
            # Create subplot for both mesh and field visualization
            fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(16, 12))
            
            # 1. Plot mesh with grid lines
            X, Y = np.meshgrid(x_unique, y_unique)
            
            # Plot every nth grid line to avoid overcrowding
            for i in range(0, len(y_unique), show_every_nth):
                ax1.plot(X[i, :], Y[i, :], 'b-', lw=0.3, alpha=0.7)
            for j in range(0, len(x_unique), show_every_nth):
                ax1.plot(X[:, j], Y[:, j], 'b-', lw=0.3, alpha=0.7)
            
            # Highlight airfoil 
            airfoil_mask = (np.abs(Y) < 0.01) & (X >= 0) & (X <= 1)
            if np.any(airfoil_mask):
                ax1.plot(X[airfoil_mask], Y[airfoil_mask], 'r-', lw=2, label='Airfoil')
            
            ax1.set_title(f'Computational Grid from {filename}')
            ax1.set_xlabel('x/c')
            ax1.set_ylabel('y/c')
            ax1.set_aspect('equal')
            ax1.grid(True, alpha=0.3)
            ax1.legend()
            
            # 2. Plot Mach contours if available
            if mach_2d.size > 0:
                levels = np.linspace(mach_2d.min(), mach_2d.max(), NUM_LEVELS)
                cs1 = ax2.contour(X, Y, mach_2d, levels=levels, linewidths=0.5, cmap=self.gen_cmap())
                ax2.plot([0, 1], [0, 0], 'k-', lw=2, label='Airfoil')
                ax2.set_title('Mach Number Contours')
                ax2.set_xlabel('x/c')
                ax2.set_ylabel('y/c')
                ax2.set_aspect('equal')
                fig.colorbar(cs1, ax=ax2)
            else:
                ax2.text(0.5, 0.5, 'No Mach data available', ha='center', va='center', transform=ax2.transAxes)
                ax2.set_title('Mach Number Contours - No Data')
            
            # 3. Plot Cp contours if available  
            if cp_2d.size > 0:
                levels = np.linspace(cp_2d.min(), cp_2d.max(), NUM_LEVELS)
                cs2 = ax3.contour(X, Y, cp_2d, levels=levels, linewidths=0.5, cmap=self.gen_cmap())
                ax3.plot([0, 1], [0, 0], 'k-', lw=2, label='Airfoil')
                ax3.set_title('Pressure Coefficient Contours')
                ax3.set_xlabel('x/c')
                ax3.set_ylabel('y/c')
                ax3.set_aspect('equal')
                fig.colorbar(cs2, ax=ax3)
            else:
                ax3.text(0.5, 0.5, 'No Cp data available', ha='center', va='center', transform=ax3.transAxes)
                ax3.set_title('Pressure Coefficient Contours - No Data')
            
            # 4. Show field statistics
            ax4.axis('off')
            stats_text = f"Field Data Statistics from {filename}\n\n"
            stats_text += f"Grid dimensions: {len(x_unique)} x {len(y_unique)}\n"
            stats_text += f"Total points: {len(x_unique) * len(y_unique)}\n\n"
            
            if mach_2d.size > 0:
                stats_text += f"Mach Number:\n"
                stats_text += f"  Min: {mach_2d.min():.4f}\n"
                stats_text += f"  Max: {mach_2d.max():.4f}\n"
                stats_text += f"  Mean: {mach_2d.mean():.4f}\n\n"
            
            if cp_2d.size > 0:
                stats_text += f"Pressure Coefficient:\n"
                stats_text += f"  Min: {cp_2d.min():.4f}\n"
                stats_text += f"  Max: {cp_2d.max():.4f}\n"
                stats_text += f"  Mean: {cp_2d.mean():.4f}\n"
            
            ax4.text(0.1, 0.9, stats_text, transform=ax4.transAxes, fontsize=10,
                    verticalalignment='top', fontfamily='monospace',
                    bbox=dict(boxstyle="round,pad=0.5", facecolor="lightgray"))
            
            plt.tight_layout()
            
            if save_to_folder:
                filename_out = os.path.join(save_to_folder, "field_overview.png")
                plt.savefig(filename_out, dpi=300, bbox_inches='tight')
                print(f"      Saved: {filename_out}")
            
        except Exception as e:
            print(f"Error plotting field data from {filename}: {e}")

    def gplot(self, x, y, row=None, color='red', save_to_folder=None):
        fig, ax = plt.subplots(figsize=(8, 6))
        ax.set_title("{} vs {}".format(x.upper(), y.upper()))
        ax.set_xlabel("{}".format(x.upper()))
        ax.set_ylabel("{}".format(y.upper()))
        if row:
            ax.plot(self.buffer.iloc[row][x], self.buffer.iloc[row][y], lw=0.8, color=color)
        else:
            ax.plot(self.buffer[x], self.buffer[y], lw=0.8, color=color)
            
        if save_to_folder:
            safe_x = x.replace(" ", "_").replace("/", "_")
            safe_y = y.replace(" ", "_").replace("/", "_")
            filename = os.path.join(save_to_folder, f"{safe_x}_vs_{safe_y}.png")
            plt.savefig(filename, dpi=300, bbox_inches='tight')
            print(f"      Saved: {filename}")

    def animate(self, mach=True):
        images = []
        for index, row in self.buffer.iterrows():
            images.append(imageio.imread(self.plot_contour(row, mach=mach, savepng=True)))
        imageio.mimsave("Animation.gif", images, fps=5)


class TSFoil(Plotter):
    
    def __init__(self, tsfoil_exe_path=None):
        self.airfoil      = {}
        self.config       = {"MAXIT":2500}
        self.current_foil = ""
        self.buffer       = pd.DataFrame()
        self.LEN_HEADER   = 26
        self.basename_foil= ""
        
        # Set the path to the executable
        if tsfoil_exe_path is None:
            # Default to executable in the same directory as this script
            script_dir = os.path.dirname(os.path.abspath(__file__))
            # Detect Windows and use .exe extension
            if os.name == 'nt' or sys.platform.startswith('win'):
                exe_name = PROGRAM_NAME + ".exe"
            else:
                exe_name = PROGRAM_NAME
            self.tsfoil_exe_path = os.path.join(script_dir, exe_name)
        else:
            # Use user-defined path
            if os.path.isfile(tsfoil_exe_path):
                self.tsfoil_exe_path = tsfoil_exe_path
            elif os.path.isdir(tsfoil_exe_path):
                # If directory provided, append executable name
                if os.name == 'nt' or sys.platform.startswith('win'):
                    exe_name = PROGRAM_NAME + ".exe"
                else:
                    exe_name = PROGRAM_NAME
                self.tsfoil_exe_path = os.path.join(tsfoil_exe_path, exe_name)
            else:
                # Assume it's a full path even if file doesn't exist yet
                self.tsfoil_exe_path = tsfoil_exe_path
                
        # Ensure executable permissions on Linux/Unix systems
        self._ensure_executable_permissions()

    def _ensure_executable_permissions(self):
        """Ensure the executable file has proper permissions on Linux/Unix systems"""
        import stat
        if os.path.exists(self.tsfoil_exe_path):
            try:
                # Get current permissions
                current_permissions = os.stat(self.tsfoil_exe_path).st_mode
                # Add execute permission for owner, group, and others
                new_permissions = current_permissions | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
                os.chmod(self.tsfoil_exe_path, new_permissions)
                print(f"Set executable permissions for: {self.tsfoil_exe_path}")
            except PermissionError:
                print(f"Warning: Could not set executable permissions for {self.tsfoil_exe_path}")
        else:
            print(f"Note: Executable file {self.tsfoil_exe_path} not found yet")

    def load(self, datfile):
        x, y = np.loadtxt(datfile, skiprows=1).T
        le_pos = x.argmin()

        xu = x[:le_pos+1][::-1]
        yu = y[:le_pos+1][::-1]
        xl = x[le_pos:]
        yl = y[le_pos:]

        fmt={'float_kind':'{:9.7f}'.format}

        self.airfoil["XU"] = np.array2string(xu, formatter=fmt, separator=", ")[1:-1]
        self.airfoil["YU"] = np.array2string(yu, formatter=fmt, separator=", ")[1:-1]
        self.airfoil["XL"] = np.array2string(xl, formatter=fmt, separator=", ")[1:-1]
        self.airfoil["YL"] = np.array2string(yl, formatter=fmt, separator=", ")[1:-1]

        self.current_foil  = datfile
        self.basename_foil = datfile.replace(".dat", "")
        self.inp_file      = "{}.inp".format(self.basename_foil)

    def set(self, config_dict):
        self.config.update(config_dict)

    def gen_inp(self, filename, title="Input File for TSFOIL"):
        settings    = "\n".join(["{} = {}".format(k,v) for k,v in self.config.items()])
        cordinates  = "\n".join(["{} = {}".format(k,v) for k,v in self.airfoil.items()])
        with open(filename, 'w') as f:
            f.write("{}\n$INP\n{}\n{}\n$END\n".format(title,settings,cordinates))    
    
    def extract_smry(self, filename="smry.out"):
        # Check if file exists and is not empty
        if not os.path.exists(filename):
            raise FileNotFoundError(f"Summary file {filename} not found")
        
        if os.path.getsize(filename) == 0:
            raise ValueError(f"Summary file {filename} is empty - {PROGRAM_NAME} likely failed to execute properly")
            
        df = pd.read_csv(filename, skiprows=9, sep="=", header=None).set_index(0).T
        df.columns = [c.strip().lower() for c in df.columns.str.replace('0',"")]
        return df

    def extract_mesh_tecplot_compatible(self, filename="mesh.dat", contour_shape=None):
        """Extract mesh data from Tecplot format file, adjusted to match contour data dimensions"""
        if not os.path.exists(filename):
            raise FileNotFoundError(f"Mesh file {filename} not found")
        
        with open(filename, 'r') as f:
            lines = f.readlines()
        
        # Parse header to get grid dimensions
        zone_line = lines[1].strip()  # ZONE I= {ni} J= {nj} F= POINT
        parts = zone_line.split()
        ni = int(parts[2])  # I= value
        nj = int(parts[4])  # J= value
        
        # Read data points starting from line 2 (0-indexed)
        data = []
        for i in range(2, len(lines)):
            line = lines[i].strip()
            if line:  # Skip empty lines
                x_val, y_val = map(float, line.split())
                data.append([x_val, y_val])
        
        data = np.array(data)
        x_points = data[:, 0]
        y_points = data[:, 1]
        
        # Reshape data to 2D grid format (ni x nj)
        x_2d = x_points.reshape(nj, ni)
        y_2d = y_points.reshape(nj, ni)
        
        if contour_shape is not None:
            # Adjust mesh to match contour data dimensions
            target_nj, target_ni = contour_shape
            print(f"   Adjusting mesh from ({ni}x{nj}) to match contour ({target_ni}x{target_nj})")
            
            # Extract coordinates to match contour dimensions
            # Typically contour data is on cell centers, so we need (ni-1) x (nj-1)
            if target_ni == ni - 1 and target_nj == nj - 1:
                # Cell-centered data: take midpoints
                x_unique = 0.5 * (x_2d[0, :-1] + x_2d[0, 1:])  # Midpoints in x-direction
                y_unique = 0.5 * (y_2d[:-1, 0] + y_2d[1:, 0])  # Midpoints in y-direction
                print("   Using cell-centered mesh coordinates")
            elif target_ni == ni and target_nj == nj:
                # Same dimensions - use node-centered data
                x_unique = x_2d[0, :]
                y_unique = y_2d[:, 0]
                print("   Using node-centered mesh coordinates")
            else:
                # Try to interpolate or subsample to match
                x_indices = np.linspace(0, ni-1, target_ni, dtype=int)
                y_indices = np.linspace(0, nj-1, target_nj, dtype=int)
                x_unique = x_2d[0, x_indices]
                y_unique = y_2d[y_indices, 0]
                print(f"   Interpolated mesh to match contour dimensions")
        else:
            # No contour data available, use original approach
            x_unique = x_2d[0, :]
            y_unique = y_2d[:, 0]
            print("   No contour data for reference, using full mesh")
        
        # Reverse y coordinates to match the original mesh.out format orientation
        # This ensures the flow field has the correct orientation (not upside down)
        y_unique = y_unique[::-1]
        print("   Reversed y coordinates to match original orientation")
        
        print(f"   Final mesh arrays: x({len(x_unique)}), y({len(y_unique)})")
        
        return x_unique, y_unique

    def extract_field_tecplot(self, filename="field.dat"):
        """Extract field data from Tecplot format file (X, Y, Mach, Cp)"""
        if not os.path.exists(filename):
            print(f"Warning: {filename} not found")
            return np.array([]), np.array([]), np.array([]), np.array([])
        
        try:
            with open(filename, 'r') as f:
                lines = f.readlines()
            
            # Parse header to get variables and grid dimensions
            variables_line = lines[0].strip()  # VARIABLES = "X", "Y", "Mach", "Cp"
            zone_line = lines[1].strip()       # ZONE I= {ni} J= {nj} F= POINT
            
            # Extract grid dimensions
            parts = zone_line.split()
            ni = int(parts[2])  # I= value
            nj = int(parts[4])  # J= value
            
            print(f"   Reading field data from {filename}: {ni} x {nj} points")
            
            # Read data points starting from line 2 (0-indexed)
            data = []
            for i in range(2, len(lines)):
                line = lines[i].strip()
                if line:  # Skip empty lines
                    values = list(map(float, line.split()))
                    if len(values) >= 4:  # X, Y, Mach, Cp
                        data.append(values[:4])
            
            if len(data) == 0:
                print(f"   Warning: No valid data found in {filename}")
                return np.array([]), np.array([]), np.array([]), np.array([])
            
            data = np.array(data)
            x_coords = data[:, 0]
            y_coords = data[:, 1]
            mach_data = data[:, 2]
            cp_data = data[:, 3]
            
            # Reshape data to 2D grids
            x_2d = x_coords.reshape(nj, ni)
            y_2d = y_coords.reshape(nj, ni)
            mach_2d = mach_data.reshape(nj, ni)
            cp_2d = cp_data.reshape(nj, ni)
            
            # Extract unique coordinates for mesh arrays
            x_unique = x_2d[0, :]  # First row for x-coordinates
            y_unique = y_2d[:, 0]  # First column for y-coordinates
            
            # Reverse y coordinates to match original orientation
            y_unique = y_unique[::-1]
            mach_2d = mach_2d[::-1, :]  # Flip vertically to match mesh orientation
            cp_2d = cp_2d[::-1, :]      # Flip vertically to match mesh orientation
            
            print(f"   Loaded field data: mesh({len(x_unique)} x {len(y_unique)}), mach{mach_2d.shape}, cp{cp_2d.shape}")
            
            return x_unique, y_unique, mach_2d, cp_2d
            
        except Exception as e:
            print(f"Warning: Error reading {filename}: {e}")
            return np.array([]), np.array([]), np.array([]), np.array([])

    def extract_cpxs(self, filename="cpxs.dat"):
        return np.genfromtxt(filename, skip_header=5).T

    def extract_mmap_fallback(self, filename="mmap.out"):
        """Extract Mach contour data from mmap.out (fallback method)"""
        if not os.path.exists(filename):
            print(f"Warning: {filename} not found")
            return np.array([])
        try:
            data = np.loadtxt(filename)
            print(f"   Loaded mach map from {filename}: shape {data.shape}")
            return data
        except Exception as e:
            print(f"Warning: Error reading {filename}: {e}")
            return np.array([])

    def extract_cpmp_fallback(self, filename="cpmp.out"):
        """Extract Cp contour data from cpmp.out (fallback method)"""
        if not os.path.exists(filename):
            print(f"Warning: {filename} not found")
            return np.array([])
        try:
            data = np.loadtxt(filename)
            print(f"   Loaded cp map from {filename}: shape {data.shape}")
            return data
        except Exception as e:
            print(f"Warning: Error reading {filename}: {e}")
            return np.array([])

    def extract_mmap(self, filename="field.dat"):
        """Extract Mach contour data from field.dat (Tecplot format)"""
        x_unique, y_unique, mach_2d, cp_2d = self.extract_field_tecplot(filename)
        if mach_2d.size > 0:
            print(f"   Loaded mach map from field.dat: shape {mach_2d.shape}")
            return mach_2d
        else:
            # Fallback to old format if field.dat not available
            return self.extract_mmap_fallback()

    def extract_cpmp(self, filename="field.dat"):
        """Extract Cp contour data from field.dat (Tecplot format)"""
        x_unique, y_unique, mach_2d, cp_2d = self.extract_field_tecplot(filename)
        if cp_2d.size > 0:
            print(f"   Loaded cp map from field.dat: shape {cp_2d.shape}")
            return cp_2d
        else:
            # Fallback to old format if field.dat not available
            return self.extract_cpmp_fallback()

    def extract_cnvg(self, filename="smry.out"):
        return "SOLUTION CONVERGED" in open(filename).read()

    def extract_iter(self, filename="cnvg.out"):
        iteration, error = np.loadtxt("cnvg.out", skiprows=2, usecols=(0,5)).T
        return iteration, error

    def extract_res(self):
        df = self.extract_smry()
        df["converged"] = self.extract_cnvg()
        df.insert(0,'airfoil', self.current_foil)
        
        # Add input parameters to the DataFrame
        df["mach"] = self.config.get("EMACH", 0.0)
        df["alpha"] = self.config.get("ALPHA", 0.0)
        
        # Try to extract field data from field.dat first (unified approach)
        field_x, field_y, mach_map, cp_map = self.extract_field_tecplot("field.dat")
        
        # If field.dat is not available, fall back to separate files
        if mach_map.size == 0:
            print("   Field.dat not available, falling back to separate files")
            mach_map = self.extract_mmap_fallback()
            cp_map = self.extract_cpmp_fallback()
            
            # Determine the required mesh dimensions from contour data
            if mach_map.size > 0:
                contour_shape = mach_map.shape
                print(f"   Contour data shape: {contour_shape}")
            elif cp_map.size > 0:
                contour_shape = cp_map.shape
                print(f"   Contour data shape: {contour_shape}")
            else:
                contour_shape = None
                print("   No contour data available")
            
            # Read mesh from mesh.dat when field.dat is not available
            try:
                x, y = self.extract_mesh_tecplot_compatible("mesh.dat", contour_shape)
                print("   Reading mesh from mesh.dat (Tecplot format)")
            except FileNotFoundError:
                print("   Warning: No mesh file found")
                x, y = [], []
        else:
            # Use mesh coordinates from field.dat
            x, y = field_x, field_y
            print("   Using mesh coordinates from field.dat")
        
        df["mesh_x"] = [x]
        df["mesh_y"] = [y]

        x_c, cp_up, m_up, cp_low, m_low = self.extract_cpxs()
        df["x_c"]   = [x_c]
        df["cp_up"] = [cp_up]
        df["m_up"]  = [m_up]
        df["cp_low"]= [cp_low]
        df["m_low"] = [m_low]

        try:
            iteration, error = self.extract_iter()
            df["iter"]  = [iteration]
            df["error"] = [error]
        except Exception as e:
            print(f"   Warning: Could not extract iteration data: {e}")
            df["iter"]  = [np.array([])]
            df["error"] = [np.array([])]
        
        # Use the previously extracted contour maps
        df["mach_map"] = [mach_map]
        df["cp_map"]   = [cp_map]

        self.buffer = pd.concat([self.buffer, df], ignore_index=True)    
    
    def exec(self, key=None, value=None, absorb=True):
        if key:
            self.config[key.upper()] = value

        self.gen_inp(self.inp_file)
        
        # Check if executable exists
        if not os.path.isfile(self.tsfoil_exe_path):
            raise FileNotFoundError(f"{PROGRAM_NAME} not found at: {self.tsfoil_exe_path}")
        
        # Use relative path for input file to avoid path length issues
        inp_file_relative = os.path.basename(self.inp_file)
        
        # Print debug information
        print(f"Executing: {self.tsfoil_exe_path}")
        print(f"Input file (relative): {inp_file_relative}")
        print(f"Working directory: {os.getcwd()}")
        
        # Use subprocess instead of os.system for better error handling
        import subprocess
        try:
            # Use relative path for input file to avoid path length limitations
            cmd = [self.tsfoil_exe_path, inp_file_relative]
            print(f"Command: {cmd}")
            
            result = subprocess.run(cmd, 
                                  capture_output=True, text=True, cwd=os.getcwd(),
                                  shell=False)
            
            print(f"Exit code: {result.returncode}")
            if result.stdout:
                print(f"stdout: {result.stdout}")
            if result.stderr:
                print(f"stderr: {result.stderr}")
                
            if result.returncode != 0:
                print(f"Warning: {PROGRAM_NAME} returned exit code {result.returncode}")
                
        except FileNotFoundError as e:
            print(f"Failed to execute {PROGRAM_NAME}: {e}")
            raise

        if absorb:
            self.extract_res()

    def fixed_cl(self, cl, alpha_0=1):
        def exec_wrapper(alpha):
            self.exec("ALPHA", value=alpha.item(), absorb=False)
            return self.extract_smry()["cl"].item() - cl
        res = fsolve(exec_wrapper, x0=alpha_0, xtol=0.005)
        if res:
            self.extract_res()


if __name__ == "__main__":
    
    # TSFoil Example Usage with Plotting
    print("=" * 30)
    print("TSFoil Python Interface - Example Usage")
    print("=" * 30)
    
    # Create plots directory
    plots_dir = "plots"
    if not os.path.exists(plots_dir):
        os.makedirs(plots_dir)
        print(f"Created plots directory: {plots_dir}")
    else:
        print(f"Using existing plots directory: {plots_dir}")
    
    # Initialize TSFoil (you can specify tsfoil.exe path if needed)
    tsfoil = TSFoil()
    
    # Load airfoil data
    print("\n1. Loading airfoil data...")
    tsfoil.load(os.path.join(path, "rae2822.dat"))
    tsfoil.set({"ALPHA": 0.5})
    
    # Single case execution
    print("\n2. Running single case (Mach 0.75)...")
    tsfoil.exec("EMACH", 0.75)
    
    # Display buffer information
    print(f"\nBuffer contains {len(tsfoil.buffer)} case(s)")
    print("Available data columns:", list(tsfoil.buffer.columns))
    
    # Plot single case results
    print("\n3. Plotting single case results...")
    
    print("   - Grid layout (basic)")
    tsfoil.plot_grid(save_to_folder=plots_dir)
    
    print("   - Full mesh from Tecplot format (detailed)")
    tsfoil.plot_mesh_tecplot(save_to_folder=plots_dir, show_every_nth=2)
    
    print("   - Field data overview from field.dat (if available)")
    tsfoil.plot_field_tecplot(save_to_folder=plots_dir, show_every_nth=3)
    
    print("   - Cp-x distribution")
    tsfoil.plot_cpx(save_to_folder=plots_dir)
    
    print("   - Mach-x distribution") 
    tsfoil.plot_machx(save_to_folder=plots_dir)
    
    print("   - Mach contours")
    tsfoil.plot_isomach(save_to_folder=plots_dir)
    
    print("   - Cp contours")
    tsfoil.plot_isocp(save_to_folder=plots_dir)
    