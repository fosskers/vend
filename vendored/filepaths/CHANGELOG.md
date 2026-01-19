# filepaths

## 1.1.0 (2026-01-06)

#### Changed

- `join`: Now only needs a single argument.

## 1.0.4 (2025-12-29)

Merry Christmas and Happy New Year!

#### Fixed

- `join`: Respect when the final path component indicates a directory.

## 1.0.3 (2025-08-17)

#### Added

- CMUCL support.

#### Fixed

- `ensure-directory` on paths that end in `*` or `**`.

## 1.0.2 (2025-06-26)

#### Added

- Windows support, by accounting for the `:device` field of `pathname`.

## 1.0.1 (2025-05-12)

#### Fixed

- Support `*` (`:wild`) in extension position.
- `ensure-directory` on a relative path with one component (e.g. `foo -> foo/`).

## 1.0.0 (2025-02-15)

Finalising `1.0.0` since it's been stable for over a year without issues.

#### Added

- `?`-suffixed aliases for the various predicate functions.

## 0.1.3 (2024-07-22)

#### Fixed

- A minor issue involving `**` when used with Allegro.

## 0.1.2 (2024-03-07)

#### Fixed

- Arguments to `join` can now be composite themselves, like: `(join "/foo/bar" "baz/test.json")`

## 0.1.1 (2024-01-27)

#### Fixed

- Handling of special path components like `..`.

## 0.1.0 (2024-01-24)

Initial release with confirmed support for SBCL, ECL, ABCL.

