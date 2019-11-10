module Main where

import TypeClasses
import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

import System.Random

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

initialState :: GameState
initialState = GameState {Objects {initOb (Point 200 0) (Point 0 0) newPlayer, Enemies {[], []}}, Nothing, 0, False, False}
