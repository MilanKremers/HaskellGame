-- | This module contains the data types
--   which represent the state of the game
module Model where
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.01

data GameState = GameState {
                   enemies  :: [Enemy]
                  ,bullets  :: [Bullet]
                  ,ship     :: Player
                  ,isPaused :: Status
                  ,keys     :: S.Set Key
                 }

data Direction = L | R
  deriving (Eq)

data Status = Play | Pause
  deriving(Eq)

data Player = Player {posPX :: Float, posPY :: Float, livesPlayer :: Int}
  
data Bullet = Bullet {posBX :: Float, posBY :: Float, direction :: Direction}

data Enemy = Enemy {posEX :: Float, posEY :: Float}

initialState :: GameState
initialState = GameState{enemies = [Enemy{posEX = 900, posEY = -400}, Enemy{posEX = 900, posEY = -200}, 
                                    Enemy{posEX = 900, posEY = 0}, Enemy{posEX = 900, posEY = 200}, 
                                    Enemy{posEX = 900, posEY = 400}], 
                         bullets = [], 
                         ship = Player{posPX = 0, posPY = 0, livesPlayer = 1}, 
                         isPaused = Play, 
                         keys = S.empty}