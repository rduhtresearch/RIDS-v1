# RIDS-v1

## Overview

RIDS currently uses a lightweight manual deployment process.

- GitHub is the source of truth for code.
- The live app runs from versioned folders under `releases/`.
- Shared runtime state lives under `shared/`.
- Maintainers run local R checks, pull the latest code into the shared-drive clone, and publish a named release with one R script.

Users still launch the app by opening `deployment/Launch RIDS.bat`.

## Shared Drive Layout

After setup, the shared deployment folder looks like this:

```text
RIDS-v1/
├── app source files
├── deployment/
│   ├── Launch RIDS.bat
│   └── launch_app.R
├── releases/
│   ├── v0.5.0/
│   └── v0.5.1/
└── shared/
    ├── current_release.txt
    ├── deploy_log.tsv
    ├── deployment_config.R
    ├── data/
    ├── logs/
    ├── outputs/
    └── uploads/
```

Important points:

- `releases/` contains code only.
- `shared/` contains runtime state only.
- `current_release.txt` tells the launcher which version is live.
- `deploy_log.tsv` records publish and rollback activity.

## First-Time Setup

Run these steps once when creating or rebuilding the shared deployment location.

### What you need first

1. Put the RIDS repo in its shared-drive location.
2. Make sure `R`, `Rscript`, and `Git` are installed on the maintainer machine.
3. Make sure end-user laptops can access the shared drive.

### Setup steps

1. Open the project in `RStudio`, or open a terminal in the repo root.
2. Run [R/SETUP/new_setup.R](/Users/tategraham/Documents/NHS/RIDS-v1/R/SETUP/new_setup.R).

Example:

```r
source("R/SETUP/new_setup.R")
```

What setup does:

1. Installs missing R packages.
2. Creates `releases/`, `shared/`, and `deployment/`.
3. Creates `shared/deployment_config.R`.
4. Creates `deployment/Launch RIDS.bat`.
5. Creates the shared DuckDB database if needed.
6. Bootstraps the first live release automatically.

On a first-time setup, if `HEAD` already has a Git tag, setup uses that tag.
If not, setup creates a local bootstrap release from the current working tree so
the launcher works immediately.

## Development and Release Flow

The current workflow is:

1. Make a change on a branch.
2. Run local checks:

```bash
Rscript R/CI/run_ci_checks.R
```

3. Open and merge a pull request into `main`.
4. On the shared-drive clone, pull the latest `main`.
5. Publish the checked-out code as a named release.
6. Launch and verify the app.
7. Roll back if needed.

For the step-by-step operator runbook, see [DEPLOYMENT.md](/Users/tategraham/Documents/NHS/RIDS-v1/DEPLOYMENT.md:1).

## User Launch Steps

Normal users only need these steps:

1. Open `deployment/Launch RIDS.bat`.
2. Wait a few seconds.
3. Let the browser open automatically.
4. Sign in.

## Operator Reference Commands

### Run first-time setup

```r
source("R/SETUP/new_setup.R")
```

### Run local checks

```bash
Rscript R/CI/run_ci_checks.R
```

### Publish the current working tree

```bash
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0
```

### Rebuild an existing release folder

```bash
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0 --force
```

### Publish an exact Git tag

```bash
Rscript R/SETUP/release_publish.R publish --version v0.5.0
```

### Roll back to an earlier release

```bash
Rscript R/SETUP/release_publish.R rollback --version v0.4.1
```

## Notes

- `main` is the only permanent branch.
- Each live deployment can map either to a Git tag or to a manually named working-tree snapshot.
- Shared runtime folders should not be committed to Git.
- The current process is intentionally simple: local checks validate code, and one R script promotes or rolls back versions.
