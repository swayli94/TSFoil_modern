#!/usr/bin/env python3
"""
Test file for TSFoil mesh generation functionality.
This script tests the set_mesh method and visualizes the generated mesh.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt

# Add parent directory to path to import tsfoil
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from tsfoil import TSFoil


def test_mesh_generation():
    '''
    Plot mesh density analysis, and 2D Cartesian mesh.
    '''
    foil = TSFoil()

    foil.set_mesh()
    
    # Extract mesh data
    xx = foil.mesh['xx']
    yy = foil.mesh['yy']
    xx_airfoil = foil.mesh['xx_airfoil']
    
    print(f"\nGenerated mesh:")
    print(f"  Actual n_point_x: {len(xx)}")
    print(f"  Actual n_point_y: {len(yy)}")
    print(f"  Airfoil points: {len(xx_airfoil)}")
    print(f"  X range: [{xx.min():.3f}, {xx.max():.3f}]")
    print(f"  Y range: [{yy.min():.3f}, {yy.max():.3f}]")

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    
    #* X/Y-direction mesh density analysis.
    dx = np.diff(xx)
    x_mid = (xx[1:] + xx[:-1]) / 2
    dy = np.diff(yy)
    y_mid = (yy[1:] + yy[:-1]) / 2
    ax1.plot(x_mid, dx, 'bo-', markersize=2, linewidth=1, label='X-direction')
    ax1.plot(y_mid, dy, 'g*-', markersize=2, linewidth=1, label='Y-direction')
    ax1.set_xlabel('x (y)')
    ax1.set_ylabel('Δ (mesh spacing)')
    ax1.set_title('Mesh Spacing')
    ax1.grid(True, alpha=0.3)
    ax1.legend()
    ax1.set_yscale('log')   
    
    #* Plot 2D Cartesian mesh.
    X, Y = np.meshgrid(xx, yy)
    
    # Plot vertical grid lines (constant x)
    for i in range(len(xx)):
        ax2.plot([xx[i], xx[i]], [yy[0], yy[-1]], 'b-', alpha=0.6, linewidth=0.1)
    
    # Plot horizontal grid lines (constant y)
    for j in range(len(yy)):
        ax2.plot([xx[0], xx[-1]], [yy[j], yy[j]], 'b-', alpha=0.6, linewidth=0.1)
    
    ax2.set_xlabel('x')
    ax2.set_ylabel('y')
    ax2.set_title('2D Cartesian Mesh Grid')
    ax2.set_aspect('equal', adjustable='box')
    
    plt.tight_layout()
    plt.savefig('test/mesh_analysis.png', dpi=300, bbox_inches='tight')
    plt.close()


if __name__ == "__main__":

    test_mesh_generation()
    