'''
Empty gym environment for reinforcement learning.
'''

import gym
from gym import spaces
import numpy as np
from pyTSFoil.pytsfoil import PyTSFoil


class TSFoilEnv_Empty(gym.Env):
    '''
    Empty gym environment for reinforcement learning.

    The action is left empty.

    The observation is the lift to drag ratio.
    '''
    def __init__(self, airfoil_file: str = "rae2822.dat") -> None:
        
        super(TSFoilEnv_Empty, self).__init__()

        self.action_space = spaces.Box(low=-1.0, high=1.0, shape=(1,))
        self.observation_space = spaces.Box(low=0.0, high=1.0, shape=(1,))

        self.observation = 0.0
        self.reward = 0.0
        self.done = False
        self.info = {}

        self.i_step = 0
        self.i_max_step = 100
        
        self.pytsfoil = PyTSFoil(airfoil_file=airfoil_file, )

    def step(self, action) -> tuple[float, float, bool, dict]:
        return self.observation, self.reward, self.done, self.info

    def reset(self) -> float:
        return self.observation

    def render(self, mode='human') -> None:
        '''
        Render the environment.
        '''
        pass

    def close(self) -> None:
        pass



