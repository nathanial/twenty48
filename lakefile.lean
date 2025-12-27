import Lake
open Lake DSL

package twenty48 where
  precompileModules := true

-- Local workspace dependencies
require terminus from ".." / "terminus"
require crucible from ".." / "crucible"

@[default_target]
lean_lib Twenty48 where
  roots := #[`Twenty48]

lean_exe twenty48 where
  root := `Main

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe tests where
  root := `Tests.Main
