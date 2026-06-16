# User Guide

This is a tower defense game. In the game you need to stop the enemies before they reach the base.

## How to run this project

First you open the project folder in the terminal.

Then you write:

```powershell
cabal run tower-defense
```

## How to play

- You can move the cursor with WASD or arrow keys.
- You can press P to place a tower.
- You can press U to upgrade a tower.
- You can press Space to pause or continue the game.
- You can press Q or Esc to quit.

## What the symbols mean

- `.` means empty ground.
- `=` means enemy path.
- `#` means blocked tile.
- `E` means enemy.
- `X` means your cursor.
- `B` means the base.
- `1` to `9` means tower level.
- `A`, `B`, `C` and other letters mean high tower levels.

## Main rules

- The enemies move on the path.
- You can place towers only on empty ground.
- The towers shoot enemies when they are close enough.
- When you kill an enemy, you get gold and score.
- You can use gold to buy towers and upgrades.
- You win when you survive all the waves.
- You lose when too many enemies reach the base.

