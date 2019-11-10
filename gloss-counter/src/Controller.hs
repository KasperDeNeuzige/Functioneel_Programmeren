-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = do randomNumber <- randomIO
                                                            let newNumber = abs randomNumber `mod` 10
                                                            return (gstate{elapsedTime = elapsedTime gstate + secs})
  | otherwise                                          = return (gstate {elapsedTime = elapsedTime gstate + secs})

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate | c == 'w'  = gstate {action = Move (Point 0 1)} 
                                          | c == 'a'  = gstate {action = Move (Point -1 0)}
                                          | c == 's'  = gstate {action = Move (Point 0 -1)}
                                          | c == 'd'  = gstate {action = Move (Point 1 0)}
                                          | c == 'v'  = gstate {action = Shoot}
                                          | c == 'p'  = gstate {action = Pause}
                                          | otherwise = gstate {action = Nothing}
inputKey (EventKey (MouseButton m) _ _ _) gstate = gstate {action = Nothing}
inputKey _ gstate = gstate {action = Nothing}
