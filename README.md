# cyclonedx-fpm

A [CycloneDX](https://cyclonedx.org/) SBOM generator for Fortran [fpm](https://fpm.fortran-lang.org/) projects.

Produces CycloneDX 1.6 JSON from an fpm project's `fpm.toml` manifest, recursively resolving all Git dependencies.

## Features

- Recursively resolves Git dependencies by cloning and parsing transitive `fpm.toml` manifests
- Generates [Package URL (PURL)](https://github.com/package-url/purl-spec) identifiers for each component
- Builds a full dependency graph with `dependencies[]` tracking
- Supports dev-dependencies (`--dev` flag)
- Extracts metadata (version, license, description, author, homepage) from each dependency's manifest
- Adds external references (VCS URLs, homepage) to components
- Outputs to stdout or a file

## Requirements

- A Fortran compiler (tested with `gfortran`)
- [fpm](https://fpm.fortran-lang.org/) (Fortran Package Manager)
- Git

## Build

```sh
fpm build
```

Run the tests:

```sh
fpm test
```

## Usage

```
cyclonedx-fpm [OPTIONS] [PROJECT_PATH]

ARGUMENTS:
  PROJECT_PATH  Path to fpm project (default: current directory)

OPTIONS:
  -o, --output FILE   Write SBOM to FILE (default: stdout)
  --dev               Include dev-dependencies
  -h, --help          Print help
  --version           Print version
```

### Examples

Generate an SBOM for the current directory and print to stdout:

```sh
fpm run
```

Generate an SBOM for a specific project and write to a file:

```sh
fpm run -- -o sbom.json /path/to/project
```

Include dev-dependencies:

```sh
fpm run -- --dev -o sbom.json
```
