{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate

getPlayerPosFromState :: GameState ->  (Float, Float)
-- getPlayerPosFromState = ship . posPlayer
getPlayerPosFromState x = posPlayer $ ship x 

setPlayerPosFromState :: GameState -> (Float, Float) -> GameState
setPlayerPosFromState x@GameState{ship=s} newPos= x{ship=newShip}
  where newShip = s{posPlayer = addPos (posPlayer s) newPos}

addPos :: (Float, Float) -> (Float, Float) -> (Float, Float)
addPos (p1, p2) (p3, p4) = (p1 + p3, p2 + p4)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyUp) _ _ _) gstate = return $ setPlayerPosFromState gstate (0, 0.01)
input (EventKey (SpecialKey KeyDown) _ _ _) gstate = return $ setPlayerPosFromState gstate (0, -0.01)
input (EventKey (SpecialKey KeyLeft) _ _ _) gstate = return $ setPlayerPosFromState gstate (-0.01, 0)
input (EventKey (SpecialKey KeyRight) _ _ _) gstate = return $ setPlayerPosFromState gstate (0.01, 0)
input _ gstate = return $ gstate