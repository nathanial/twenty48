/-
  Twenty48.Game.Logic
  Game logic and move processing
-/
import Twenty48.Core
import Twenty48.Game.State
import Twenty48.Game.Random

namespace Twenty48.Game

open Twenty48.Core

/-- Spawn a random tile on the grid -/
def spawnTile (state : GameState) : GameState :=
  let (rng1, pos) := randomEmptyCell state.grid state.rng
  match pos with
  | none => { state with rng := rng1 }
  | some p =>
    let (rng2, value) := randomTileValue rng1
    let newGrid := state.grid.set p.x p.y (some value)
    let newBestTile := max state.bestTile value
    { state with
      grid := newGrid
      rng := rng2
      bestTile := newBestTile
      anim := { state.anim with
        newTilePos := some p
        newTileTimer := 4
      }
    }

/-- Check if the grid has a winning tile -/
def hasWinningTile (grid : Grid) : Bool :=
  grid.maxTile >= winningExponent

/-- Check if the game is over (no moves possible) -/
def checkGameOver (state : GameState) : GameState :=
  if state.grid.canMove then
    state
  else
    { state with gameOver := true }

/-- Process a move in a direction -/
def move (state : GameState) (dir : Direction) : GameState := Id.run do
  -- Don't process moves if game over (and not continuing)
  if state.gameOver then return state

  -- Perform the slide
  let (newGrid, scoreGained, merges, changed) := state.grid.slide dir

  -- If nothing changed, return unchanged state
  if !changed then return state

  -- Save previous state for undo
  let prevGrid := some state.grid
  let prevScore := some state.score

  -- Calculate new score
  let newScore := state.score + scoreGained
  let newBestScore := max state.bestScore newScore

  -- Update best tile
  let newBestTile := max state.bestTile newGrid.maxTile

  -- Check for win
  let justWon := !state.won && !state.continuedAfterWin && hasWinningTile newGrid

  -- Create new state with animations
  let mergePositions := merges.map (·.pos)
  let newState : GameState :=
    { state with
      grid := newGrid
      score := newScore
      bestScore := newBestScore
      bestTile := newBestTile
      previousGrid := prevGrid
      previousScore := prevScore
      won := state.won || justWon
      anim := { state.anim with
        merging := mergePositions
        mergeTimer := 3
      }
    }

  -- Spawn a new tile
  let withNewTile := spawnTile newState

  -- Check for game over
  checkGameOver withNewTile

/-- Undo the last move -/
def undo (state : GameState) : GameState :=
  match state.previousGrid, state.previousScore with
  | some grid, some score =>
    { state with
      grid := grid
      score := score
      previousGrid := none
      previousScore := none
      gameOver := false
      anim := AnimState.default
    }
  | _, _ => state

/-- Continue playing after winning -/
def continueAfterWin (state : GameState) : GameState :=
  { state with
    won := false
    continuedAfterWin := true
  }

/-- Restart the game -/
def restart (state : GameState) : GameState :=
  let newState := GameState.new state.rng
  -- Keep the best score
  let withBest := { newState with bestScore := state.bestScore }
  -- Spawn two initial tiles
  let withFirst := spawnTile withBest
  spawnTile withFirst

/-- Initialize a new game with two starting tiles -/
def initGame (seed : UInt64) : GameState :=
  let state := GameState.new seed
  let withFirst := spawnTile state
  spawnTile withFirst

/-- Update animations (called each frame) -/
def updateAnimations (state : GameState) : GameState := Id.run do
  let mut anim := state.anim

  -- Decrement merge timer
  if anim.mergeTimer > 0 then
    anim := { anim with mergeTimer := anim.mergeTimer - 1 }
    if anim.mergeTimer == 0 then
      anim := { anim with merging := [] }

  -- Decrement new tile timer
  if anim.newTileTimer > 0 then
    anim := { anim with newTileTimer := anim.newTileTimer - 1 }
    if anim.newTileTimer == 0 then
      anim := { anim with newTilePos := none }

  { state with anim := anim }

end Twenty48.Game
