# RIDS-v1

## Overview

RIDS now uses a lightweight CI/CD process designed for a small team:

- GitHub is the source of truth for code.
- GitHub Actions runs checks automatically on `main` and pull requests.
- The live app uses versioned folders under `releases/`.
- Shared runtime state lives under `shared/`.
- A maintainer publishes or rolls back a version with one R script.

Users still launch the app the same way: by opening `deployment/Launch RIDS.bat`.

## Shared Drive Layout

After setup, the shared app folder will look like this:

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
- `current_release.txt` says which version is live.
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
6. If the current `HEAD` commit already has a Git tag, bootstraps the first live release automatically.

If setup says no active release was created yet, publish a tagged version using the maintainer steps below.

## User Launch Steps

Normal users only need these steps:

1. Open `deployment/Launch RIDS.bat`.
2. Wait a few seconds.
3. Let the browser open automatically.
4. Sign in.

## Day-to-Day Development Flow

Use this flow for normal code changes.

1. Create a short-lived branch from `main`.
2. Make your changes.
3. Push the branch to GitHub.
4. Open a pull request into `main`.
5. Wait for GitHub Actions to run.
6. Look for the green tick.
7. If checks pass, merge into `main`.

You do not need to manually run GitHub Actions. GitHub runs them for you automatically.

## What GitHub Actions Does

There are two workflows in this repo:

- `CI`: runs on pull requests and pushes to `main`
- `Release Artifact`: runs when you push a tag like `v0.5.0`

The CI workflow:

1. Installs R.
2. Installs required R packages.
3. Runs the custom tests in `R/tests`.
4. Runs a lightweight bootstrap check to catch missing files or broken config loading.

The release workflow:

1. Re-runs the same checks for the tagged version.
2. Builds a `.zip` artifact from the exact Git tag.
3. Uploads that artifact to GitHub.

## Maintainer Release Process

This is the step-by-step process to put a new version live.

### 1. Make sure `main` contains the release

1. Confirm your PR is merged into `main`.
2. Pull the latest `main`.

Example:

```bash
git checkout main
git pull origin main
```

### 2. Create the release tag

Pick a new version name such as `v0.5.0`.

```bash
git tag v0.5.0
git push origin v0.5.0
```

This triggers the GitHub release workflow automatically.

### 3. Publish the tag to the shared drive

From the repo root on the maintainer machine that can access the shared drive, run:

```bash
Rscript R/SETUP/release_publish.R publish --version v0.5.0
```

What this does:

1. Verifies the tag exists.
2. Exports that exact tagged snapshot into `releases/v0.5.0/`.
3. Runs a lightweight smoke check.
4. Updates `shared/current_release.txt`.
5. Writes a line to `shared/deploy_log.tsv`.

When this succeeds, `v0.5.0` becomes the live version.

### 4. Confirm the release

1. Open `deployment/Launch RIDS.bat`.
2. Confirm the app loads.
3. Confirm the footer version matches the release tag.

## Rollback Process

If a release needs to be reversed, switch the live pointer back to an earlier release.

Example:

```bash
Rscript R/SETUP/release_publish.R rollback --version v0.4.1
```

What rollback does:

1. Verifies `releases/v0.4.1/` already exists.
2. Updates `shared/current_release.txt`.
3. Writes a line to `shared/deploy_log.tsv`.

Rollback does not copy files, rebuild the database, or touch uploads and outputs.

## Operator Reference Commands

### Run first-time setup

```r
source("R/SETUP/new_setup.R")
```

### Publish a release

```bash
Rscript R/SETUP/release_publish.R publish --version v0.5.0
```

### Rebuild an existing release folder

```bash
Rscript R/SETUP/release_publish.R publish --version v0.5.0 --force
```

### Roll back to an earlier release

```bash
Rscript R/SETUP/release_publish.R rollback --version v0.4.1
```

## Troubleshooting

### GitHub Actions failed

1. Open the failed workflow in GitHub.
2. Read the failing step.
3. Fix the branch locally.
4. Push again and wait for a new green tick.

Do not tag or publish a version while CI is red.

### Tag not found during publish

This means the tag does not exist in Git yet.

Check:

```bash
git tag
```

If needed, create and push the tag:

```bash
git tag v0.5.0
git push origin v0.5.0
```

### Shared drive unavailable

If the maintainer machine cannot see the shared drive:

1. Reconnect the drive.
2. Confirm the repo is opened from the shared-drive location.
3. Re-run the publish or rollback command.

### `current_release.txt` missing

This usually means no release has been published yet.

Fix:

1. Run setup if it has not been run.
2. Publish the first tagged version with `release_publish.R publish --version ...`

### Publish smoke check failed

The publish command will stop before changing the live version.

Common causes:

- the shared config file is missing
- the release folder is incomplete
- required app files are missing
- the tagged code no longer loads its deployment helpers cleanly

Recommended response:

1. Read the error printed by the publish script.
2. Fix the code or setup issue.
3. Re-run publish.

### Launcher opens but app does not start

Check:

1. `shared/current_release.txt` contains a version.
2. `releases/<that version>/app.R` exists.
3. `shared/deployment_config.R` exists.
4. `R` is installed on the laptop launching the app.

## Notes

- `main` is the only permanent branch.
- Each live deployment should map to a Git tag.
- Shared runtime folders should not be committed to Git.
- This process is intentionally simple: GitHub checks code, and one R script promotes or rolls back versions.
