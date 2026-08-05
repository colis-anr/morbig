# Changes

## 0.12.0 — 2026-08-04

This release brings Morbig up to date after the 0.11.0 release and strengthens
its development and portability infrastructure.

### Notable changes

- Rework assignment-word handling so assignment words are represented directly
  by command-prefix and command-suffix nodes instead of being embedded in the
  word CST.
- Recognize and strip tilde prefixes in assignment words more accurately.
- Support current Menhir releases, including the removal of the deprecated
  `MenhirLib.General` module.
- Support Yojson 3 while retaining compatibility with Yojson 2.
- Initialize the embedded OCaml runtime with native UTF-16 arguments on
  Windows, fixing compilation of the C API there.
- Raise the minimum supported OCaml version to 4.11.

### Tests and development

- Replace the legacy golden-test runner with an Alcotest-based harness.
- Add unit tests and QCheck properties.
- Add bytecode-only, lower-bound, macOS, Windows, Docker, and Nix CI coverage.
- Improve test diagnostics and organize golden tests by POSIX section.
- Add reproducible Nix development environments and formatting checks.

### Infrastructure

- Refresh GitHub Actions and restore GitHub Pages documentation deployment.
- Modernize apt-based installation checks and validate install, example build,
  and uninstall paths.
- Repair the source-distribution target so it derives the version from
  `dune-project` and archives the current release commit.
