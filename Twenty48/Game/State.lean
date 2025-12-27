/-
  Twenty48.Game.State
  Game state structures
-/
import Twenty48.Core
import Terminus

namespace Twenty48.Game

open Twenty48.Core
open Terminus

/-- Animation state for visual effects -/
structure AnimState where
  -- Merge animation
  merging : List Point          -- Cells that just merged
  mergeTimer : Nat              -- Frames remaining

  -- New tile animation
  newTilePos : Option Point     -- Position of newly spawned tile
  newTileTimer : Nat            -- Frames remaining
  deriving Repr

def AnimState.default : AnimState :=
  { merging := []
  , mergeTimer := 0
  , newTilePos := none
  , newTileTimer := 0
  }

instance : Inhabited AnimState where
  default := AnimState.default

/-- Main game state -/
structure GameState where
  grid : Grid
  score : Nat
  bestScore : Nat
  bestTile : Nat                 -- Highest tile exponent achieved
  previousGrid : Option Grid     -- For undo
  previousScore : Option Nat
  won : Bool                     -- Created 2048 tile
  continuedAfterWin : Bool       -- Playing past 2048
  gameOver : Bool
  rng : UInt64
  anim : AnimState
  deriving Repr

/-- The winning tile exponent (2^11 = 2048) -/
def winningExponent : Nat := 11

/-- Create initial game state -/
def GameState.new (seed : UInt64) : GameState :=
  { grid := Grid.empty
  , score := 0
  , bestScore := 0
  , bestTile := 0
  , previousGrid := none
  , previousScore := none
  , won := false
  , continuedAfterWin := false
  , gameOver := false
  , rng := seed
  , anim := AnimState.default
  }

end Twenty48.Game
