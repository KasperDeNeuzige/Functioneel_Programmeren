-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate
{-| GAMEOVER?-}  
  | otherwise = pictures [pictures (map draw (meteorites enemies objects gstate)),
                          pictures (map draw (bulletsEN spaceShips enemies objects gstate))
                          pictures (map draw (bulletsPL player objects gstate)),
                          pictures (map draw (spaceShips enemies objects gstate)),
                          draw spaceShip player objects gstate]
                            



{-= case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  -}
