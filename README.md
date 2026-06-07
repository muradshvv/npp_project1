# Tower Defense Game

This is a small tower defense game made in Haskell.

The game runs in the terminal. The player places towers on the map and tries to stop enemies before they reach the base.

## How To Run

Open a terminal in the project folder.

Build and run the game:

```powershell
cabal run tower-defense
```

The first run can take some time because Cabal downloads and builds the libraries.

After that, `cabal run` is faster.


## Controls

| Key | Action |
| --- | --- |
| `WASD` | Move the cursor |
| Arrow keys | Move the cursor |
| `P` | Place a tower |
| `U` | Upgrade a tower |
| `Space` | Pause or resume |
| `Q` | Quit the game |
| `Esc` | Quit the game |

## Game Symbols

| Symbol | Meaning |
| --- | --- |
| `.` | Empty ground |
| `=` | Enemy path |
| `#` | Blocked map tile |
| `1`, `2`, `3`, ... | Tower level 1 to 9 |
| `A`, `B`, `C`, ... | Tower level 10 and higher |
| `E` | Enemy |
| `X` | Player cursor |
| `B` | Base to defend |

## How To Play

The enemies enter from the left side of the map.

They follow the blue path to the base.

You place towers on empty ground. Towers cannot be placed on the path.

When a tower kills an enemy, you get gold and score.

Use gold to place more towers or upgrade old towers.

You win if you survive all waves.

You lose if too many enemies reach the base.

## Project Structure

There are only four Haskell source files:

```text
app/Main.hs
src/Types.hs
src/GameLogic.hs
src/UI.hs
```

## Developer Guide

### `app/Main.hs`

This is the entry point. It only starts the game UI.

### `src/Types.hs`

This file contains the main data types.

Examples:

- `GameState`
- `Enemy`
- `Tower`
- `Position`
- `WaveState`

The types help make the game safer because health, gold, score, damage, and time are not just plain numbers.

### `src/GameLogic.hs`

This file contains the pure game logic.

The most important function is:

```haskell
tick :: Seconds -> GameState -> GameState
```

It updates the game state each frame.

This file also contains:

- enemy spawning
- enemy movement
- tower attacks
- wave progress
- map data
- win and lose checks

### `src/UI.hs`

This file contains the terminal UI.

It uses `brick` and `vty`.

The UI has a separate tick thread. This means the game updates by custom events instead of sleeping inside the main UI loop.

The UI also uses only simple ASCII characters, so it works better on Windows terminals.
