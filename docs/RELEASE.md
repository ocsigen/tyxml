# How to make a release

## Documentation

The documentation is built with odoc and themed with wodoc; see
[`doc/README.md`](../doc/README.md) for the sources, the build command and a
local preview. Publishing is automated:

- every push to `master` rebuilds and deploys the `dev` documentation;
- a release is published by running the **Documentation** workflow manually
  (GitHub, Actions, *Documentation*, "Run workflow") with the **version**
  input. That job freezes the current `dev` documentation as `/<version>/`,
  repoints the `latest` symlink and refreshes `versions.json`. It does not
  rebuild: the documentation of a release is exactly the `dev` documentation at
  that point.

The details, including how to do it by hand on a `gh-pages` checkout, are in
[`doc/README.md`](../doc/README.md).

## Release

- Update the [changelog](../CHANGES.md). Its first heading is the version
  number: `dune-release tag` reads it, and no `.opam` file or `dune-project`
  carries a version field.
- Check that the build, the tests and the documentation are clean:
  `dune build @check`, `dune runtest` and `dune build @doc`.
- Update the packages that depend on TyXML, so that a release never lands
  before its reverse dependencies can accept it.
- Use dune-release:

```
dune-release tag
dune-release distrib
export DUNE_RELEASE_GITHUB_TOKEN=$(gh auth token)   # or store it locally
export DUNE_RELEASE_DELEGATE=github-dune-release-delegate
dune-release publish -y
dune-release opam pkg
dune-release opam submit -y
```

`publish` takes no artefact argument since dune-release 2.0, and it
skips pushing the tag if it is already on the remote. It needs a GitHub
token, which it reads from `DUNE_RELEASE_GITHUB_TOKEN` or from
`~/.config/dune-release/github.token`.

`distrib` builds the archive, lints the four opam files and runs the test
suite from the archive. The JSX tests need `refmt`, so `reason` has to be
installed in the switch; without it, use `--skip-tests` and run
`dune runtest` on the extracted archive from a switch that has it.

Before submitting, it is worth checking that the published asset matches
the checksum of the generated opam file, since that is what opam will
verify:

```
curl -sL -o /tmp/a.tbz \
  https://github.com/ocsigen/tyxml/releases/download/<version>/tyxml-<version>.tbz
sha256sum /tmp/a.tbz
grep sha256 _build/tyxml.<version>/opam
```

- Publish the documentation of the new version with the **Documentation**
  workflow, as described above.
