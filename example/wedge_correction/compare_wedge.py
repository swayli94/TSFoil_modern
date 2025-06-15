'''
Compare the results of TSFoil with different wedge correction methods.

Wedge correction is used to correct the pressure distribution on the airfoil surface,
where the shock wave is located.

NWEDGE = 0: no wedge correction
NWEDGE = 1: Murman wedge correction
NWEDGE = 2: Yoshiara wedge correction

'''

import os
import sys
import matplotlib.pyplot as plt

# Add parent directory to path to import tsfoil
path_parent = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.append(path_parent)

from tsfoil import TSFoil


if __name__ == "__main__":
    
    wedge_series = {
        '0': {'NWDGE': 0},
        '1': {'NWDGE': 1},
        '2': {'NWDGE': 2},
    }
    
    wedge_colors = {
        '0': 'k',
        '1': 'b',
        '2': 'g',
    }
    
    config_series = {
        '1': {'ALPHA': 1.0, 'EMACH': 0.50, 'MAXIT': 9999},
        '2': {'ALPHA': 1.5, 'EMACH': 0.65, 'MAXIT': 9999},
        '3': {'ALPHA': 0.5, 'EMACH': 0.75, 'MAXIT': 9999},
        '4': {'ALPHA': 1.0, 'EMACH': 0.75, 'MAXIT': 9999},
    }
    
    # Create output directory if it doesn't exist
    output_dir = os.path.join(path_parent, 'example', 'wedge_correction', 'plot')
    os.makedirs(output_dir, exist_ok=True)
    
    for config_name, config_dict in config_series.items():

        all_data = {}

        for wedge_name, wedge_config in wedge_series.items():
            
            fname = os.path.join(output_dir, f'{config_name}_{wedge_name}.png')
            
            tsfoil = TSFoil()
            tsfoil.set_airfoil(os.path.join(path_parent, 'rae2822.dat'))
            tsfoil.set_mesh(
                n_point_x=200,
                n_point_y=80,
                n_point_airfoil=100
            )
            
            config_dict.update(wedge_config)
            tsfoil.set_config(config_dict)
            tsfoil.exec()
            tsfoil.load_smry()
            tsfoil.plot_all_results(fname)   
            tsfoil.move_result_files(output_dir, f'{config_name}_WEDGE{wedge_name}-')
            
            all_data[wedge_name] = {
                'x': tsfoil.data_cpxs['x'],
                'm_up': tsfoil.data_cpxs['m_up'],
                'm_low': tsfoil.data_cpxs['m_low'],
                'CL': tsfoil.data_summary['CL'],
                'CD': tsfoil.data_summary['CD'],
                'CM': tsfoil.data_summary['CM'],
                'CD_wave': tsfoil.data_summary['CD_wave'],
            }
            
        # Create a figure to compare cpxs of each config
        fig, ax = plt.subplots(figsize=(10, 5))
        
        for wedge_name, wedge_data in all_data.items():
            ax.plot(wedge_data['x'], wedge_data['m_up'], label=wedge_name, color=wedge_colors[wedge_name])
            ax.plot(wedge_data['x'], wedge_data['m_low'], label=None, color=wedge_colors[wedge_name])
            
        ax.legend()
        ax.set_xlabel('X/c')
        ax.set_ylabel('Mach Number')
        ax.set_title(f'Mach Number Distribution on Y=0 for {config_name}')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_xlim([-0.2, 1.2])
        
        plt.savefig(os.path.join(output_dir, f'{config_name}_mach_distribution.png'), dpi=300)
        plt.close()
        
        # Create a figure to compare CL of each config
        fig, ax = plt.subplots(figsize=(10, 5))
        
        y_cl_values = []
        y_cd_values = []
        y_cd_wave_values = []
        for wedge_name, wedge_data in all_data.items():
            y_cl_values.append(wedge_data['CL'])
            y_cd_values.append(wedge_data['CD']*100)
            y_cd_wave_values.append(wedge_data['CD_wave']*100)
            
        ax.plot(wedge_series.keys(), y_cl_values, 'ko-', label='CL')
        ax.plot(wedge_series.keys(), y_cd_values, 'ro-', label='CD*100')
        ax.plot(wedge_series.keys(), y_cd_wave_values, 'bo-', label='CD_wave*100')

        ax.set_xlabel('Wedge Correction Method')
        ax.set_ylabel('CL, CD, CD_wave')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        plt.savefig(os.path.join(output_dir, f'{config_name}_Coefs.png'), dpi=300)
        plt.close()        

