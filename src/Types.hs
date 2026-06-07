{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( Damage(..)
  , Enemy(..)
  , EnemyId(..)
  , GameMap(..)
  , GameState(..)
  , GameStatus(..)
  , Gold(..)
  , HitPoints(..)
  , InputMode(..)
  , PlayerHealth(..)
  , Position(..)
  , Range(..)
  , Score(..)
  , Seconds(..)
  , Tower(..)
  , UpgradeLevel(..)
  , Vec2(..)
  , WaveState(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)

newtype Seconds = Seconds { unSeconds :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

newtype HitPoints = HitPoints { unHitPoints :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

newtype PlayerHealth = PlayerHealth { unPlayerHealth :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

newtype Gold = Gold { unGold :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

newtype Score = Score { unScore :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

newtype Damage = Damage { unDamage :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

newtype Range = Range { unRange :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

newtype EnemyId = EnemyId { unEnemyId :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num)

newtype UpgradeLevel = UpgradeLevel { unUpgradeLevel :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, Real, Integral)

data Position = Position
  { posX :: !Int
  , posY :: !Int
  }
  deriving stock (Eq, Ord, Show)

data Vec2 = Vec2
  { vecX :: !Double
  , vecY :: !Double
  }
  deriving stock (Eq, Ord, Show)

data Enemy = Enemy
  { enemyId :: !EnemyId
  , enemyPosition :: !Vec2
  , enemyPathIndex :: !Int
  , enemyProgress :: !Double
  , enemySpeed :: !Double
  , enemyHp :: !HitPoints
  , enemyMaxHp :: !HitPoints
  , enemyReward :: !Gold
  , enemyScoreValue :: !Score
  }
  deriving stock (Eq, Show)

data Tower = Tower
  { towerPosition :: !Position
  , towerDamage :: !Damage
  , towerRange :: !Range
  , towerAttackPeriod :: !Seconds
  , towerCooldown :: !Seconds
  , towerLevel :: !UpgradeLevel
  }
  deriving stock (Eq, Show)

data GameMap = GameMap
  { mapWidth :: !Int
  , mapHeight :: !Int
  , mapPath :: !(NonEmpty Position)
  , mapBlocked :: !(Set Position)
  }
  deriving stock (Eq, Show)

data WaveState = WaveState
  { waveNumber :: !Int
  , waveTotal :: !Int
  , waveRemainingToSpawn :: !Int
  , waveSpawnTimer :: !Seconds
  , waveSpawnInterval :: !Seconds
  , waveNextEnemyId :: !EnemyId
  }
  deriving stock (Eq, Show)

data GameStatus
  = Running
  | Paused
  | Won
  | GameOver
  deriving stock (Eq, Show)

data InputMode
  = NormalMode
  | PlaceMode
  | UpgradeMode
  deriving stock (Eq, Show)

data GameState = GameState
  { gsEnemies :: !(Map EnemyId Enemy)
  , gsTowers :: !(Map Position Tower)
  , gsPlayerHealth :: !PlayerHealth
  , gsGold :: !Gold
  , gsScore :: !Score
  , gsMap :: !GameMap
  , gsWave :: !WaveState
  , gsStatus :: !GameStatus
  , gsCursor :: !Position
  , gsInputMode :: !InputMode
  }
  deriving stock (Eq, Show)
