'''
Mesh convergence study of TSFoil with RAE2822 airfoil.
'''

import os
import sys
import matplotlib.pyplot as plt

# Add parent directory to path to import tsfoil
path_parent = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.append(path_parent)

from tsfoil import TSFoil


if __name__ == "__main__":
    
    mesh_series = {
        'coarse': {'n_point_x':  50, 'n_point_y':  20, 'n_point_airfoil':  25},
        'medium': {'n_point_x': 100, 'n_point_y':  40, 'n_point_airfoil':  50},
        'fine': {'n_point_x': 200, 'n_point_y': 80, 'n_point_airfoil': 100},
        'very_fine': {'n_point_x': 400, 'n_point_y': 160, 'n_point_airfoil': 200},
    }
    
    mesh_colors = {
        'coarse': 'k',
        'medium': 'b',
        'fine': 'g',
        'very_fine': 'r',
    }
    
    config_series = {
        '1': {'ALPHA': 1.0, 'EMACH': 0.50, 'MAXIT': 9999},
        '2': {'ALPHA': 1.5, 'EMACH': 0.65, 'MAXIT': 9999},
        '3': {'ALPHA': 0.5, 'EMACH': 0.75, 'MAXIT': 9999},
        '4': {'ALPHA': 1.0, 'EMACH': 0.75, 'MAXIT': 9999},
    }
    
    # Create output directory if it doesn't exist
    output_dir = os.path.join(path_parent, 'example', 'mesh_convergence', 'plot')
    os.makedirs(output_dir, exist_ok=True)
    
    for config_name, config_dict in config_series.items():

        all_data = {}

        for mesh_name, mesh_config in mesh_series.items():
            
            fname = os.path.join(output_dir, f'{config_name}_{mesh_name}.png')
            
            tsfoil = TSFoil()
            tsfoil.set_airfoil(os.path.join(path_parent, 'rae2822.dat'))
            tsfoil.set_mesh(**mesh_config)
            tsfoil.set_config(config_dict)
            tsfoil.exec()
            tsfoil.load_smry()
            tsfoil.plot_all_results(fname)   
            tsfoil.move_result_files(output_dir, f'{config_name}_{mesh_name}-')
            
            all_data[mesh_name] = {
                'x': tsfoil.data_cpxs['x'],
                'm_up': tsfoil.data_cpxs['m_up'],
                'm_low': tsfoil.data_cpxs['m_low'],
                'CL': tsfoil.data_summary['CL'],
                'CD': tsfoil.data_summary['CD'],
                'CM': tsfoil.data_summary['CM'],
                'CD_wave': tsfoil.data_summary['CD_wave'],
                'n_point_x': mesh_config['n_point_x'],
                'n_point_y': mesh_config['n_point_y'],
            }
            
        # Create a figure to compare cpxs of each config
        fig, ax = plt.subplots(figsize=(10, 5))
        
        for mesh_name, mesh_data in all_data.items():
            ax.plot(mesh_data['x'], mesh_data['m_up'], label=mesh_name, color=mesh_colors[mesh_name])
            ax.plot(mesh_data['x'], mesh_data['m_low'], label=None, color=mesh_colors[mesh_name])
            
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
        
        x_mesh_size = []
        y_cl_values = []
        y_cd_values = []
        
        for mesh_name, mesh_data in all_data.items():
            x_mesh_size.append(mesh_data['n_point_x']*mesh_data['n_point_y'])
            y_cl_values.append(mesh_data['CL'])
            y_cd_values.append(mesh_data['CD']*10)
            
        ax.plot(x_mesh_size, y_cl_values, 'ko-', label='CL')
        ax.plot(x_mesh_size, y_cd_values, 'ro-', label='CD*10')
            
        ax.set_xlabel('Mesh')
        ax.set_ylabel('CL, CD')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        plt.savefig(os.path.join(output_dir, f'{config_name}_Coefs.png'), dpi=300)
        plt.close()        

