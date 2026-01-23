import Lake
open Lake DSL

package twenty48 where
  precompileModules := true

require terminus from git "https://github.com/nathanial/terminus" @ "v0.0.2"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.9"

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
