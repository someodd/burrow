# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[unreleased]: https://github.com/someodd/burrow/compare/v0.2.0.0...HEAD
[0.2.0.0]: https://github.com/someodd/burrow/compare/v0.1.15.0...v0.2.0.0
[0.1.15.0]: https://github.com/someodd/burrow/release/v0.1.0.0
