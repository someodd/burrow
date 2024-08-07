# Contributing

## Releases

Releases are managed through GitHub Actions. Please see `.github/workflows/release.yml`.

It works like this:

1. Make sure the `CHANGELOG.md` is up-to-date (don't worry about setting the version or footnote)
1. Use `./reposcripts/prep-release.sh` which will tag with the next release, it'll suggest the release number, but you can review first.

  * double-check the `CHANGELOG.md`...

1. check the github action for release and then check the subsequent release cut and the packages attached