{-# LANGUAGE DerivingStrategies #-}

module UI
  ( runGame
  ) where

import Brick
  ( App(..)
  , AttrName
  , AttrMap
  , BrickEvent(..)
  , EventM
  , Widget
  , attrMap
  , attrName
  , cached
  , fill
  , hBox
  , hLimit
  , neverShowCursor
  , str
  , vBox
  , vLimit
  , withAttr
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Main (customMain, halt)
import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.State.Strict (get, put)
import Data.List (find, maximumBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ord (comparing)
import GameLogic
import Graphics.Vty
  ( Event(..)
  , Key(..)
  , defAttr
  , withBackColor
  , withForeColor
  )
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty as Vty
import Types

data Name
  = StaticTile Position
  deriving stock (Eq, Ord, Show)

data AppEvent
  = GameTick
  deriving stock (Eq, Show)

runGame :: IO ()
runGame = do
  eventChannel <- newBChan 16
  initialVty <- buildVty
  bracket
    (startTicker eventChannel)
    killThread
    (\_ -> void (customMain initialVty buildVty (Just eventChannel) app initialState))
  where
    buildVty = mkVty Vty.defaultConfig

startTicker :: BChan AppEvent -> IO ThreadId
startTicker eventChannel =
  forkIO $
    forever $ do
      threadDelay tickMicros
      writeBChan eventChannel GameTick

app :: App GameState AppEvent Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const attributeMap
    }

drawUI :: GameState -> [Widget Name]
drawUI state =
  [ vBox
      [ drawHeader state
      , asciiBorder
      , drawGrid state
      , asciiBorder
      , drawControlPanel state
      ]
  ]

handleEvent :: BrickEvent Name AppEvent -> EventM Name GameState ()
handleEvent event =
  case event of
    AppEvent GameTick -> do
      state <- get
      put (tick tickSeconds state)
    VtyEvent vtyEvent ->
      case commandFromEvent vtyEvent of
        Just QuitCommand -> halt
        Just command -> do
          state <- get
          put (applyCommand command state)
        Nothing -> pure ()
    MouseDown{} -> pure ()
    MouseUp{} -> pure ()

data Command
  = MoveCursor Int Int
  | PlaceCommand
  | UpgradeCommand
  | TogglePauseCommand
  | QuitCommand
  deriving stock (Eq, Show)

commandFromEvent :: Event -> Maybe Command
commandFromEvent event =
  case event of
    EvKey KUp [] -> Just (MoveCursor 0 (-1))
    EvKey KDown [] -> Just (MoveCursor 0 1)
    EvKey KLeft [] -> Just (MoveCursor (-1) 0)
    EvKey KRight [] -> Just (MoveCursor 1 0)
    EvKey (KChar 'w') [] -> Just (MoveCursor 0 (-1))
    EvKey (KChar 'W') [] -> Just (MoveCursor 0 (-1))
    EvKey (KChar 's') [] -> Just (MoveCursor 0 1)
    EvKey (KChar 'S') [] -> Just (MoveCursor 0 1)
    EvKey (KChar 'a') [] -> Just (MoveCursor (-1) 0)
    EvKey (KChar 'A') [] -> Just (MoveCursor (-1) 0)
    EvKey (KChar 'd') [] -> Just (MoveCursor 1 0)
    EvKey (KChar 'D') [] -> Just (MoveCursor 1 0)
    EvKey (KChar 'p') [] -> Just PlaceCommand
    EvKey (KChar 'P') [] -> Just PlaceCommand
    EvKey (KChar 'u') [] -> Just UpgradeCommand
    EvKey (KChar 'U') [] -> Just UpgradeCommand
    EvKey (KChar ' ') [] -> Just TogglePauseCommand
    EvKey (KChar 'q') [] -> Just QuitCommand
    EvKey (KChar 'Q') [] -> Just QuitCommand
    EvKey KEsc [] -> Just QuitCommand
    _ -> Nothing

applyCommand :: Command -> GameState -> GameState
applyCommand command state =
  case command of
    MoveCursor dx dy -> moveCursor dx dy state
    PlaceCommand ->
      case tryPlaceTower (gsCursor state) state of
        PlacementApplied state' -> state' { gsInputMode = PlaceMode }
        PlacementRejected -> state { gsInputMode = PlaceMode }
    UpgradeCommand ->
      case tryUpgradeTower (gsCursor state) state of
        PlacementApplied state' -> state' { gsInputMode = UpgradeMode }
        PlacementRejected -> state { gsInputMode = UpgradeMode }
    TogglePauseCommand -> togglePause state
    QuitCommand -> state

moveCursor :: Int -> Int -> GameState -> GameState
moveCursor dx dy state =
  state { gsCursor = Position clampedX clampedY }
  where
    Position x y = gsCursor state
    gameMap = gsMap state
    clampedX = clamp 0 (mapWidth gameMap - 1) (x + dx)
    clampedY = clamp 0 (mapHeight gameMap - 1) (y + dy)

drawHeader :: GameState -> Widget Name
drawHeader state =
  hLimit dashboardWidth $
    vLimit 1 $
      withAttr headerAttr $
        str $
          " HP: "
            <> show (unPlayerHealth (gsPlayerHealth state))
            <> " | Gold: "
            <> show (unGold (gsGold state))
            <> "g | Score: "
            <> show (unScore (gsScore state))
            <> " | Wave: "
            <> show (min (waveNumber wave) (waveTotal wave))
            <> "/"
            <> show (waveTotal wave)
            <> " | "
            <> statusText (gsStatus state)
            <> " | Base "
            <> healthBar maxPlayerHealth (unPlayerHealth (gsPlayerHealth state))
            <> padding
  where
    wave = gsWave state
    padding = replicate dashboardWidth ' '

drawGrid :: GameState -> Widget Name
drawGrid state =
  hLimit dashboardWidth $
    vLimit (mapHeight gameMap) $
      vBox [drawRow y | y <- [0 .. mapHeight gameMap - 1]]
  where
    gameMap = gsMap state
    drawRow y =
      hLimit dashboardWidth $
        hBox [drawCell state (Position x y) | x <- [0 .. mapWidth gameMap - 1]]

drawCell :: GameState -> Position -> Widget Name
drawCell state position
  | gsCursor state == position =
      withAttr cursorAttr (str "X")
  | Just enemy <- enemyAt state position =
      withAttr (enemyAttrFor enemy) (str "E")
  | Just tower <- Map.lookup position (gsTowers state) =
      withAttr (towerAttrFor tower) (str (towerSymbol tower))
  | pathEnd (gsMap state) == position =
      withAttr (baseAttrFor state) (str "B")
  | otherwise =
      cached (StaticTile position) (drawStaticCell state position)

drawStaticCell :: GameState -> Position -> Widget Name
drawStaticCell state position
  | isPathTile (gsMap state) position = withAttr pathAttr (str "=")
  | Set.member position (mapBlocked (gsMap state)) = withAttr wallAttr (str "#")
  | otherwise = withAttr groundAttr (str ".")

drawControlPanel :: GameState -> Widget Name
drawControlPanel state =
  hLimit dashboardWidth $
    vLimit 3 $
      vBox
        [ withAttr controlAttr $
            str "[WASD/Arrows] Move  [P] Tower  [U] Upgrade  [Space] Pause  [Q/Esc] Quit"
        , withAttr infoAttr $
            str $
              "Cursor "
                <> showPosition (gsCursor state)
                <> " | Mode "
                <> modeText (gsInputMode state)
                <> " | Enemies "
                <> show (Map.size (gsEnemies state))
                <> " active, "
                <> show (waveRemainingToSpawn (gsWave state))
                <> " queued"
                <> " | "
                <> enemySummary state
        , drawSelectionInfo state
        ]

drawSelectionInfo :: GameState -> Widget Name
drawSelectionInfo state =
  case Map.lookup (gsCursor state) (gsTowers state) of
    Nothing -> drawEmptySelection
    Just tower ->
      withAttr (towerAttrFor tower) $
        str $
          "Tower L"
            <> show (unUpgradeLevel (towerLevel tower))
            <> " | Damage "
            <> show (unDamage (towerDamage tower))
            <> " | Range "
            <> showOneDecimal (unRange (towerRange tower))
            <> " | Next upgrade "
            <> show (unGold (upgradeCost tower))
            <> "g"

drawEmptySelection :: Widget Name
drawEmptySelection =
      withAttr mutedAttr $
        str $
          "Tower: none | Place cost "
            <> show (unGold towerCost)
            <> "g"

asciiBorder :: Widget Name
asciiBorder =
  hLimit dashboardWidth $
    vLimit 1 $
      withAttr borderAttr (fill '-')

enemyAt :: GameState -> Position -> Maybe Enemy
enemyAt state position =
  find (enemyOnTile position) (Map.elems (gsEnemies state))

enemyOnTile :: Position -> Enemy -> Bool
enemyOnTile (Position x y) enemy =
  floor (vecX (enemyPosition enemy)) == x
    && floor (vecY (enemyPosition enemy)) == y

towerSymbol :: Tower -> String
towerSymbol tower
  | level <= 9 = show level
  | level <= 35 = [letterSymbols !! (level - 10)]
  | otherwise = "*"
  where
    level = unUpgradeLevel (towerLevel tower)
    letterSymbols = ['A' .. 'Z']

towerAttrFor :: Tower -> AttrName
towerAttrFor tower
  | level <= 1 = towerLevel1Attr
  | level == 2 = towerLevel2Attr
  | level == 3 = towerLevel3Attr
  | level == 4 = towerLevel4Attr
  | level == 5 = towerLevel5Attr
  | level <= 9 = towerLevel6Attr
  | level <= 14 = towerLevel10Attr
  | otherwise = towerLevel15Attr
  where
    level = unUpgradeLevel (towerLevel tower)

enemyAttrFor :: Enemy -> AttrName
enemyAttrFor enemy
  | ratio >= 0.75 = enemyHealthyAttr
  | ratio >= 0.45 = enemyBruisedAttr
  | ratio >= 0.2 = enemyWoundedAttr
  | otherwise = enemyCriticalAttr
  where
    ratio = hpRatio (enemyHp enemy) (enemyMaxHp enemy)

baseAttrFor :: GameState -> AttrName
baseAttrFor state
  | ratio >= 0.65 = baseHealthyAttr
  | ratio >= 0.3 = baseDamagedAttr
  | otherwise = baseCriticalAttr
  where
    ratio = fromIntegral (unPlayerHealth (gsPlayerHealth state)) / fromIntegral maxPlayerHealth
    ratio :: Double

enemySummary :: GameState -> String
enemySummary state =
  case selectedEnemy state <|> frontEnemy state of
    Nothing -> "Enemy HP: none"
    Just enemy ->
      "Enemy HP: "
        <> show (unHitPoints (enemyHp enemy))
        <> "/"
        <> show (unHitPoints (enemyMaxHp enemy))

selectedEnemy :: GameState -> Maybe Enemy
selectedEnemy state =
  enemyAt state (gsCursor state)

frontEnemy :: GameState -> Maybe Enemy
frontEnemy state =
  case Map.elems (gsEnemies state) of
    [] -> Nothing
    enemies -> Just (maximumBy (comparing enemyTrackDistance) enemies)

enemyTrackDistance :: Enemy -> Double
enemyTrackDistance enemy =
  fromIntegral (enemyPathIndex enemy) + enemyProgress enemy

hpRatio :: HitPoints -> HitPoints -> Double
hpRatio hp maxHp
  | maxHp <= 0 = 0
  | otherwise = max 0 (min 1 (fromIntegral hp / fromIntegral maxHp))

healthBar :: Int -> Int -> String
healthBar maxHp currentHp =
  "[" <> replicate filled '#' <> replicate (barWidth - filled) '-' <> "]"
  where
    filled = clamp 0 barWidth (round (filledRatio * fromIntegral barWidth))
    barWidth = 20
    filledRatio :: Double
    filledRatio = fromIntegral currentHp / fromIntegral maxHp

statusText :: GameStatus -> String
statusText status =
  case status of
    Running -> "Running"
    Paused -> "Paused"
    Won -> "Won"
    GameOver -> "Game Over"

modeText :: InputMode -> String
modeText mode =
  case mode of
    NormalMode -> "Ready"
    PlaceMode -> "Place"
    UpgradeMode -> "Upgrade"

showPosition :: Position -> String
showPosition (Position x y) =
  "(" <> show x <> "," <> show y <> ")"

showOneDecimal :: Double -> String
showOneDecimal value =
  show (fromIntegral (round (value * 10) :: Int) / 10 :: Double)

clamp :: Ord a => a -> a -> a -> a
clamp lower upper =
  min upper . max lower

attributeMap :: AttrMap
attributeMap =
  attrMap
    defAttr
    [ (headerAttr, withBackColor (withForeColor defAttr Vty.black) Vty.white)
    , (borderAttr, withForeColor defAttr Vty.brightBlack)
    , (groundAttr, withForeColor defAttr Vty.brightBlack)
    , (pathAttr, withForeColor defAttr Vty.brightBlue)
    , (wallAttr, withForeColor defAttr Vty.magenta)
    , (towerLevel1Attr, withForeColor defAttr Vty.cyan)
    , (towerLevel2Attr, withForeColor defAttr Vty.brightBlue)
    , (towerLevel3Attr, withForeColor defAttr Vty.yellow)
    , (towerLevel4Attr, withForeColor defAttr Vty.magenta)
    , (towerLevel5Attr, withBackColor (withForeColor defAttr Vty.brightWhite) Vty.blue)
    , (towerLevel6Attr, withBackColor (withForeColor defAttr Vty.black) Vty.brightGreen)
    , (towerLevel10Attr, withBackColor (withForeColor defAttr Vty.black) Vty.brightMagenta)
    , (towerLevel15Attr, withBackColor (withForeColor defAttr Vty.brightWhite) Vty.red)
    , (enemyHealthyAttr, withForeColor defAttr Vty.green)
    , (enemyBruisedAttr, withForeColor defAttr Vty.yellow)
    , (enemyWoundedAttr, withForeColor defAttr Vty.brightRed)
    , (enemyCriticalAttr, withBackColor (withForeColor defAttr Vty.white) Vty.red)
    , (baseHealthyAttr, withBackColor (withForeColor defAttr Vty.black) Vty.green)
    , (baseDamagedAttr, withBackColor (withForeColor defAttr Vty.black) Vty.yellow)
    , (baseCriticalAttr, withBackColor (withForeColor defAttr Vty.white) Vty.red)
    , (cursorAttr, withBackColor (withForeColor defAttr Vty.black) Vty.yellow)
    , (controlAttr, withForeColor defAttr Vty.brightWhite)
    , (infoAttr, withForeColor defAttr Vty.green)
    , (mutedAttr, withForeColor defAttr Vty.brightBlack)
    ]

headerAttr :: AttrName
headerAttr = attrName "header"

borderAttr :: AttrName
borderAttr = attrName "border"

groundAttr :: AttrName
groundAttr = attrName "ground"

pathAttr :: AttrName
pathAttr = attrName "path"

wallAttr :: AttrName
wallAttr = attrName "wall"

towerLevel1Attr :: AttrName
towerLevel1Attr = attrName "towerLevel1"

towerLevel2Attr :: AttrName
towerLevel2Attr = attrName "towerLevel2"

towerLevel3Attr :: AttrName
towerLevel3Attr = attrName "towerLevel3"

towerLevel4Attr :: AttrName
towerLevel4Attr = attrName "towerLevel4"

towerLevel5Attr :: AttrName
towerLevel5Attr = attrName "towerLevel5"

towerLevel6Attr :: AttrName
towerLevel6Attr = attrName "towerLevel6"

towerLevel10Attr :: AttrName
towerLevel10Attr = attrName "towerLevel10"

towerLevel15Attr :: AttrName
towerLevel15Attr = attrName "towerLevel15"

enemyHealthyAttr :: AttrName
enemyHealthyAttr = attrName "enemyHealthy"

enemyBruisedAttr :: AttrName
enemyBruisedAttr = attrName "enemyBruised"

enemyWoundedAttr :: AttrName
enemyWoundedAttr = attrName "enemyWounded"

enemyCriticalAttr :: AttrName
enemyCriticalAttr = attrName "enemyCritical"

baseHealthyAttr :: AttrName
baseHealthyAttr = attrName "baseHealthy"

baseDamagedAttr :: AttrName
baseDamagedAttr = attrName "baseDamaged"

baseCriticalAttr :: AttrName
baseCriticalAttr = attrName "baseCritical"

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

controlAttr :: AttrName
controlAttr = attrName "control"

infoAttr :: AttrName
infoAttr = attrName "info"

mutedAttr :: AttrName
mutedAttr = attrName "muted"

tickSeconds :: Seconds
tickSeconds = Seconds 0.05

tickMicros :: Int
tickMicros = 50000

dashboardWidth :: Int
dashboardWidth = 100

maxPlayerHealth :: Int
maxPlayerHealth = 25
