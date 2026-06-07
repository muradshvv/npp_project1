module GameLogic
  ( PlacementResult(..)
  , buildTower
  , defaultMap
  , initialState
  , inBounds
  , isBuildable
  , isPathTile
  , nextStatus
  , pathEnd
  , pathStart
  , positionCenter
  , positionDistance
  , tick
  , togglePause
  , towerCost
  , tryPlaceTower
  , tryUpgradeTower
  , upgradeCost
  ) where

import Data.List (foldl', maximumBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Types


defaultMap :: GameMap
defaultMap =
  GameMap
    { mapWidth = 40
    , mapHeight = 20
    , mapPath = path
    , mapBlocked = Set.fromList decorativeBlocks
    }
  where
    path =
      ( Position 0 3
        :| [ Position x 3 | x <- [1 .. 9] ]
      )
        <> NE.fromList [ Position 9 y | y <- [4 .. 14] ]
        <> NE.fromList [ Position x 14 | x <- [10 .. 18] ]
        <> NE.fromList [ Position 18 y | y <- [13,12 .. 6] ]
        <> NE.fromList [ Position x 6 | x <- [19 .. 30] ]
        <> NE.fromList [ Position 30 y | y <- [7 .. 17] ]
        <> NE.fromList [ Position x 17 | x <- [31 .. 38] ]
        <> NE.fromList [ Position 38 y | y <- [16,15 .. 11] ]
        <> NE.fromList [ Position 39 11 ]
    decorativeBlocks =
      [ Position 3 7
      , Position 4 7
      , Position 5 7
      , Position 13 4
      , Position 14 4
      , Position 14 5
      , Position 23 10
      , Position 24 10
      , Position 25 10
      , Position 26 10
      , Position 33 4
      , Position 34 4
      , Position 35 4
      , Position 4 16
      , Position 5 16
      , Position 6 16
      , Position 21 17
      , Position 22 17
      ]

inBounds :: GameMap -> Position -> Bool
inBounds gameMap (Position x y) =
  x >= 0 && y >= 0 && x < mapWidth gameMap && y < mapHeight gameMap

isPathTile :: GameMap -> Position -> Bool
isPathTile gameMap position =
  position `elem` NE.toList (mapPath gameMap)

isBuildable :: GameMap -> Position -> Bool
isBuildable gameMap position =
  inBounds gameMap position
    && not (isPathTile gameMap position)
    && not (Set.member position (mapBlocked gameMap))

pathStart :: GameMap -> Position
pathStart = NE.head . mapPath

pathEnd :: GameMap -> Position
pathEnd = NE.last . mapPath

positionCenter :: Position -> Vec2
positionCenter (Position x y) =
  Vec2 (fromIntegral x + 0.5) (fromIntegral y + 0.5)

positionDistance :: Vec2 -> Vec2 -> Double
positionDistance (Vec2 ax ay) (Vec2 bx by) =
  sqrt ((ax - bx) * (ax - bx) + (ay - by) * (ay - by))

data PlacementResult
  = PlacementApplied GameState
  | PlacementRejected
  deriving (Eq, Show)

initialState :: GameState
initialState =
  GameState
    { gsEnemies = Map.empty
    , gsTowers = Map.empty
    , gsPlayerHealth = PlayerHealth 25
    , gsGold = Gold 200
    , gsScore = Score 0
    , gsMap = defaultMap
    , gsWave = startWave 1 12
    , gsStatus = Running
    , gsCursor = Position 2 1
    , gsInputMode = NormalMode
    }

tick :: Seconds -> GameState -> GameState
tick dt state
  | gsStatus state /= Running = state
  | otherwise =
      nextStatus
        . advanceWave
        . resolveDestroyed
        . attackWithTowers dt
        . moveEnemies dt
        . spawnEnemies dt
        $ state

togglePause :: GameState -> GameState
togglePause state =
  case gsStatus state of
    Running -> state { gsStatus = Paused }
    Paused -> state { gsStatus = Running }
    Won -> state
    GameOver -> state

tryPlaceTower :: Position -> GameState -> PlacementResult
tryPlaceTower position state
  | gsStatus state /= Running && gsStatus state /= Paused = PlacementRejected
  | not (isBuildable (gsMap state) position) = PlacementRejected
  | Map.member position (gsTowers state) = PlacementRejected
  | gsGold state < towerCost = PlacementRejected
  | otherwise =
      PlacementApplied
        state
          { gsTowers = Map.insert position (buildTower position) (gsTowers state)
          , gsGold = gsGold state - towerCost
          }

tryUpgradeTower :: Position -> GameState -> PlacementResult
tryUpgradeTower position state =
  case Map.lookup position (gsTowers state) of
    Nothing -> PlacementRejected
    Just tower
      | gsGold state < upgradeCost tower -> PlacementRejected
      | otherwise ->
          PlacementApplied
            state
              { gsTowers = Map.insert position (upgradeTower tower) (gsTowers state)
              , gsGold = gsGold state - upgradeCost tower
              }

buildTower :: Position -> Tower
buildTower position =
  Tower
    { towerPosition = position
    , towerDamage = Damage 14
    , towerRange = Range 4.4
    , towerAttackPeriod = Seconds 0.52
    , towerCooldown = Seconds 0
    , towerLevel = UpgradeLevel 1
    }

nextStatus :: GameState -> GameState
nextStatus state
  | gsPlayerHealth state <= PlayerHealth 0 = state { gsStatus = GameOver }
  | waveNumber (gsWave state) > waveTotal (gsWave state)
      && Map.null (gsEnemies state) = state { gsStatus = Won }
  | otherwise = state

spawnEnemies :: Seconds -> GameState -> GameState
spawnEnemies dt state =
  state { gsEnemies = spawnedEnemies, gsWave = spawnedWave }
  where
    wave = gsWave state
    timer = waveSpawnTimer wave - dt
    canSpawn = waveRemainingToSpawn wave > 0 && timer <= Seconds 0
    (spawnedEnemies, spawnedWave)
      | canSpawn =
          ( Map.insert (waveNextEnemyId wave) (makeEnemy state) (gsEnemies state)
          , wave
              { waveRemainingToSpawn = waveRemainingToSpawn wave - 1
              , waveSpawnTimer = waveSpawnInterval wave
              , waveNextEnemyId = succ (waveNextEnemyId wave)
              }
          )
      | otherwise = (gsEnemies state, wave { waveSpawnTimer = max 0 timer })

moveEnemies :: Seconds -> GameState -> GameState
moveEnemies dt state =
  state
    { gsEnemies = survivors
    , gsPlayerHealth = gsPlayerHealth state - fromIntegral escaped
    }
  where
    moved = fmap (advanceEnemy (gsMap state) dt) (gsEnemies state)
    survivors = Map.mapMaybe (keepActive (gsMap state)) moved
    escaped = Map.size moved - Map.size survivors

attackWithTowers :: Seconds -> GameState -> GameState
attackWithTowers dt state =
  state { gsTowers = towers', gsEnemies = damagedEnemies }
  where
    (damageByEnemy, towers') =
      Map.mapAccum
        (\damageMap tower ->
           let (tower', target) = resolveTowerAttack dt state tower
            in (maybe damageMap (\targetId -> addDamage (towerDamage tower) targetId damageMap) target, tower'))
        Map.empty
        (gsTowers state)
    damagedEnemies = Map.mapWithKey (applyDamage damageByEnemy) (gsEnemies state)

resolveDestroyed :: GameState -> GameState
resolveDestroyed state =
  state
    { gsEnemies = alive
    , gsGold = gsGold state + earnedGold
    , gsScore = gsScore state + earnedScore
    }
  where
    (destroyed, alive) = Map.partition ((<= HitPoints 0) . enemyHp) (gsEnemies state)
    earnedGold = foldl' (\gold enemy -> gold + enemyReward enemy) 0 destroyed
    earnedScore = foldl' (\score enemy -> score + enemyScoreValue enemy) 0 destroyed

advanceWave :: GameState -> GameState
advanceWave state
  | waveNumber wave > waveTotal wave = state
  | waveRemainingToSpawn wave == 0 && Map.null (gsEnemies state) =
      state { gsWave = startWave (waveNumber wave + 1) (waveTotal wave) }
  | otherwise = state
  where
    wave = gsWave state

makeEnemy :: GameState -> Enemy
makeEnemy state =
  Enemy
    { enemyId = waveNextEnemyId wave
    , enemyPosition = positionCenter (pathStart (gsMap state))
    , enemyPathIndex = 0
    , enemyProgress = 0
    , enemySpeed = 1.5 + fromIntegral (waveNumber wave) * 0.14 + fromIntegral (waveNumber wave * waveNumber wave) * 0.012
    , enemyHp = hp
    , enemyMaxHp = hp
    , enemyReward = enemyGoldReward (waveNumber wave)
    , enemyScoreValue = Score (50 + waveNumber wave * 12)
    }
  where
    wave = gsWave state
    hp = enemyHitPoints (waveNumber wave)

advanceEnemy :: GameMap -> Seconds -> Enemy -> Enemy
advanceEnemy gameMap (Seconds dt) enemy =
  moveByDistance path (enemySpeed enemy * dt) enemy
  where
    path = NE.toList (mapPath gameMap)

moveByDistance :: [Position] -> Double -> Enemy -> Enemy
moveByDistance path distance enemy
  | enemyPathIndex enemy >= length path - 1 = enemy { enemyProgress = 1 }
  | distance <= remainingInSegment =
      enemy
        { enemyProgress = nextProgress
        , enemyPosition = interpolate currentPosition nextPosition nextProgress
        }
  | otherwise =
      moveByDistance
        path
        (distance - remainingInSegment)
        enemy
          { enemyPathIndex = enemyPathIndex enemy + 1
          , enemyProgress = 0
          , enemyPosition = nextPosition
          }
  where
    currentTile = path !! enemyPathIndex enemy
    nextTile = path !! (enemyPathIndex enemy + 1)
    currentPosition = positionCenter currentTile
    nextPosition = positionCenter nextTile
    remainingInSegment = max 0 (1 - enemyProgress enemy)
    nextProgress = enemyProgress enemy + distance

keepActive :: GameMap -> Enemy -> Maybe Enemy
keepActive gameMap enemy
  | enemyPathIndex enemy >= pathLength - 1 && enemyProgress enemy >= 1 = Nothing
  | otherwise = Just enemy
  where
    pathLength = length (NE.toList (mapPath gameMap))

resolveTowerAttack :: Seconds -> GameState -> Tower -> (Tower, Maybe EnemyId)
resolveTowerAttack dt state tower
  | newCooldown > Seconds 0 = (tower { towerCooldown = newCooldown }, Nothing)
  | otherwise =
      case selectTarget state tower of
        Nothing -> (tower { towerCooldown = Seconds 0 }, Nothing)
        Just targetId -> (tower { towerCooldown = towerAttackPeriod tower }, Just targetId)
  where
    newCooldown = max 0 (towerCooldown tower - dt)

selectTarget :: GameState -> Tower -> Maybe EnemyId
selectTarget state tower =
  fmap enemyId $
    highestProgress $
      filter (withinRange tower) $
        Map.elems (gsEnemies state)



highestProgress :: [Enemy] -> Maybe Enemy
highestProgress [] = Nothing
highestProgress enemies =
  Just (maximumBy (comparing enemyTrackDistance) enemies)

enemyTrackDistance :: Enemy -> Double
enemyTrackDistance enemy =
  fromIntegral (enemyPathIndex enemy) + enemyProgress enemy



withinRange :: Tower -> Enemy -> Bool
withinRange tower enemy =
  positionDistance (positionCenter (towerPosition tower)) (enemyPosition enemy)
    <= unRange (towerRange tower)

addDamage :: Damage -> EnemyId -> Map.Map EnemyId Damage -> Map.Map EnemyId Damage
addDamage damage enemyId' =
  Map.insertWith (+) enemyId' damage

applyDamage :: Map.Map EnemyId Damage -> EnemyId -> Enemy -> Enemy
applyDamage damageByEnemy enemyId' enemy =
  case Map.lookup enemyId' damageByEnemy of
    Nothing -> enemy
    Just damage -> enemy { enemyHp = enemyHp enemy - fromIntegral damage }

interpolate :: Vec2 -> Vec2 -> Double -> Vec2
interpolate (Vec2 ax ay) (Vec2 bx by) progress =
  Vec2 (ax + (bx - ax) * progress) (ay + (by - ay) * progress)



startWave :: Int -> Int -> WaveState
startWave current total =
  WaveState
    { waveNumber = current
    , waveTotal = total
    , waveRemainingToSpawn = if current > total then 0 else 8 + current * 3
    , waveSpawnTimer = Seconds 0
    , waveSpawnInterval = Seconds (max 0.28 (1.05 - fromIntegral current * 0.055))
    , waveNextEnemyId = EnemyId 1
    }


towerCost :: Gold
towerCost = Gold 60

enemyHitPoints :: Int -> HitPoints
enemyHitPoints wave =
  HitPoints (70 + wave * 34 + wave * wave * 15 + wave * wave * wave * 2)

enemyGoldReward :: Int -> Gold
enemyGoldReward wave =
  Gold (10 + wave * 4 + wave * wave)


upgradeCost :: Tower -> Gold
upgradeCost tower =
  Gold (45 * unUpgradeLevel (towerLevel tower) + 15)



upgradeTower :: Tower -> Tower
upgradeTower tower =
  tower
    { towerDamage = towerDamage tower + Damage (10 + unUpgradeLevel (towerLevel tower) * 2)
    , towerRange = towerRange tower + Range 0.45
    , towerAttackPeriod = max (Seconds 0.28) (towerAttackPeriod tower - Seconds 0.055)
    , towerLevel = towerLevel tower + UpgradeLevel 1
    }



