{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Set as S


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ gstate
          | (isPaused gstate) == Pause = return $ gstate 
          | S.member (SpecialKey KeyUp) (keys gstate) = return $ setPlayerPosFromState gstate (0, 25)
          | S.member (SpecialKey KeyDown) (keys gstate) = return $ setPlayerPosFromState gstate (0, -25)
          | S.member (SpecialKey KeyLeft) (keys gstate) = return $ setPlayerPosFromState gstate (-25, 0)
          | S.member (SpecialKey KeyRight) (keys gstate) = return $ setPlayerPosFromState gstate (25, 0)
          | otherwise = return $ gstate

getPosX :: Player -> Float
getPosX Player{ posPlayer = (p1, p2) } = p1 

getPosY :: Player -> Float
getPosY Player{ posPlayer = (p1, p2) } = p2

setPlayerPosFromState :: GameState -> (Float, Float) -> GameState
setPlayerPosFromState x@GameState{ship=s} newPos@(p1, p2) | p1 == 0 && p2 > 0 && getPosY s < 540  = x{ship=newShip}
                                                          | p1 == 0 && p2 < 0 && getPosY s > -500 = x{ship=newShip}
                                                          | p2 == 0 && p1 > 0 && getPosX s < 940  = x{ship=newShip}
                                                          | p2 == 0 && p1 < 0 && getPosX s > -940 = x{ship=newShip}
                                                          | otherwise                             = x
  where newShip = s{posPlayer = addPos (posPlayer s) newPos}

addPos :: (Float, Float) -> (Float, Float) -> (Float, Float)
addPos (p1, p2) (p3, p4) = (p1 + p3, p2 + p4)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input (EventKey (Char 'p') Down _ _) gstate | (isPaused gstate) == Play = return $ gstate{isPaused = Pause}
                                            | otherwise                 = return $ gstate{isPaused = Play}
input (EventKey (Char 'f') Down _ _) gstate = return $ gstate{bullets = (Bullet{ posBullet = (posPlayer $ ship gstate), direction = R } : (bullets gstate))}
input (EventKey k Down _ _) gstate = return $ gstate {keys = S.insert k (keys gstate)}
input (EventKey k Up _ _) gstate = return $ gstate {keys = S.delete k (keys gstate)}
input _ gstate = return $ gstate


