# Developer Guide

This project is a tower defense game in Haskell. It runs in the terminal.

## Project files

The project has four main Haskell files:

- `app/Main.hs`
- `src/Types.hs`
- `src/GameLogic.hs`
- `src/UI.hs`

## What each file does

### `app/Main.hs`

This file starts the game.

### `src/Types.hs`

This file has the main types of the game.

It has types for:

- game state
- enemies
- towers
- map positions
- waves
- gold
- score
- health

### `src/GameLogic.hs`

This file has the game rules and logic.

The main function is:

```haskell
tick :: Seconds -> GameState -> GameState
```

This function takes the old game state and gives back the new one.

It handles:

- enemy spawning
- enemy movement
- tower attacks
- enemy death
- gold rewards
- wave progress
- win and lose checks
- map data

### `src/UI.hs`

This file has the terminal screen and keyboard controls.

It uses Brick and Vty libraries.

The UI handles:

- drawing the map
- drawing towers and enemies
- keyboard input
- game tick events
- colors
- pause and quit

## Important idea

The game logic and the UI are separated.

The game logic does not read keys and does not draw the screen.

The UI reads the keys and draws the screen.

The game logic only changes the game state.

## How to build this project

```powershell
cabal build
```

## How to run this project

```powershell
cabal run tower-defense
```

If you see a folder called `dist-newstyle`, this is normal. Cabal makes it when it builds the project.

