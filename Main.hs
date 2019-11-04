module Main where

import Controller
import Model
import View
import System.Random

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (1920, 1080) (0, 0)) -- Or FullScreen
              black            -- Background color
              300              -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function