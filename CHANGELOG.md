# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* "Containers" which are a way to draw an ASCII art frame around some textual content

## [0.10.0.0] - 2024-08-30

Was fixated on Docker.

### Fixed

* `--watch` will now build the gopherhole before any changes

## [0.9.0.0] - 2024-08-26

Project root release.

### Fixed

* Paths are a little more reliable when it comes to the project root

## [0.8.0.0] - 2024-08-25

### Fixed

* Error last release resulting in dependence on obsoleted config

## [0.7.0.0] - 2024-08-25

### Changed

* A bit more ridgid with project directory structure

## [0.6.0.0] - 2024-08-25

Good deal of under-the-hood config tweaking.

### Added

* `--config` file to specify the config to use
* `--watch` to rebuild when changes detected and also serve output

### Changed

* Now using TOML config format
* `spacecookie.json` now integrated into `gopherhole.ini` (`spacecookie.json` is no longer required)
* `gopherhole.ini` basically renamed to `burrow.toml`
* The CLI--just check it out

## [0.5.0.0] - 2024-08-14

### Added

* New entry for terminal-safe mode/asciiSafe in config

## [0.4.0.0] - 2024-08-13

### Added

* Font error and warning messages + automatic resolution

## [0.3.0.0] - 2024-08-10

### Added

* Menu links to the main phlog index and per tag atom feeds

### Changed

* Author name changed for generator (Atom/XML)

## [0.2.0.0] - 2024-08-05

Lots of stuff related to the project but not necessarily a part of the version software
update, like release management, docker file.

### Added

* Built-in Spacecookie server, now accessible through the burrow CLI

### Changed

* CLI commands (because of the introduction of the spacecoookie server)

## [0.1.15.0] - 2024-06-27: Production-oriented

The focus of this release was to prepare for making it easier to deploy and utilize in
production settings. This comes in the form of bug-fixing in terms of the actual software
that increments the release number, but there was also a big switch to using Stack and
`fpm` for making getting things deployed.

I also changed some of the example gopherhole.

Nix is no longer used.

### Fixed

* Fixed a bug where not all cases were being handled when converting gopher menu lines to
  text and would result in an error `this should be impossible!` (bad code practice)

## Oops!

I didn't account for many of the previous versions up to (inclusive) v0.1.14.0.

[unreleased]: https://github.com/someodd/burrow/compare/v0.10.0.0...HEAD
[0.10.0.0]: https://github.com/someodd/burrow/compare/v0.9.0.0...v0.10.0.0
[0.9.0.0]: https://github.com/someodd/burrow/compare/v0.8.0.0...v0.9.0.0
[0.8.0.0]: https://github.com/someodd/burrow/compare/v0.7.0.0...v0.8.0.0
[0.7.0.0]: https://github.com/someodd/burrow/compare/v0.6.0.0...v0.7.0.0
[0.6.0.0]: https://github.com/someodd/burrow/compare/v0.5.0.0...v0.6.0.0
[0.5.0.0]: https://github.com/someodd/burrow/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/someodd/burrow/compare/v0.3.0.0...v0.4.0.0
[0.3.0.0]: https://github.com/someodd/burrow/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/someodd/burrow/compare/v0.1.15.0...v0.2.0.0
[0.1.15.0]: https://github.com/someodd/burrow/release/v0.1.0.0
