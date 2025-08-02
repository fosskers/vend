# vend

## Unreleased

#### Added

- `vend eval` for running arbitrary Lisp from the command line with all your dependencies available.
- More entries to the registry.

#### Fixed

- When dependencies within `vendored/` were symlinked to another local folder,
  `vend graph` would occasionally count certain dependencies more than once.

## 0.2.1 (2025-05-31)

#### Fixed

- Support `mgl-pax`'s new system, `autoload`.

## 0.2.0 (2025-05-13)

#### Added

- `vend init` for creating minimal project skeletons.
- More entries to the registry.

## 0.1.5 (2025-02-08)

#### Added

- Support for [`clisp`][clisp].
- More entries to the registry.

#### Fixed

- `vend get`: support for truly local dependencies that aren't registered with `vend`.
- `vend test`: Under `parachute`, be more sensitive to the name of the specified
  testing `defpackage`.
- `vend test`: Only run a specified test package once, even if multiple parent
  systems refer to it.

[clisp]: https://gitlab.com/gnu-clisp/clisp

## 0.1.4 (2025-01-25)

#### Added

- New command: `vend test`. Supports:
  - `parachute`
  - `clunit2`
  - `fiveam`

#### Fixed

- Updated `transducers` dependency to fix a `.asd` detection edge case.

## 0.1.3 (2025-01-19)

#### Added

- New command: `vend search`.
- Support for Allegro (`alisp`).
- Numerous additions to the registry.

#### Fixed

- Restored support for `postmodern`.

## 0.1.2 (2025-01-17)

#### Added

- `vend check` also warns about dependencies that couldn't be fetched.
- Improved dependency scanning accuracy and performance.

## 0.1.1 (2025-01-15)

#### Added

- Dependency support for more large projects.

#### Fixed

- An odd additional message when dependency resolution fails.
- Ignore `.qlot/` if present.

## 0.1.0 (2025-01-13)

Initial release.
