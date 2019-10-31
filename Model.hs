-- | This module contains the data types
--   which represent the state of the game
module Model where
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.05

data GameState = GameState {
                   enemies  :: [Enemy]
                  ,bullets  :: [Bullet]
                  ,ship     :: Player
                  ,isPaused :: Status
                  ,keys     :: S.Set Key
                 }

data Direction = L | R

data Player = Player {posPlayer :: (Float, Float), livesPlayer :: Int}
  
data Bullet = Bullet {posBullet :: (Float, Float), direction :: Direction}

data Enemy = Enemy {posEnemy :: (Float, Float), livesEnemy :: Int}

data Status = Play | Pause

class Moveable a where 
  move :: a -> (Float, Float) -> a -> GameState

class Killable a where
  isDead :: a -> Int -> Bool

class Renderable a where
  render :: a -> GameState

initialState :: GameState
initialState = GameState{enemies = [], bullets = [], ship = Player{posPlayer = (0, 0), livesPlayer = 3}, isPaused = Play, keys = S.empty}