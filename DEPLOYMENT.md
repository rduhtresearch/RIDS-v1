# Deployment Guide

## Overview

RIDS is currently deployed with a manual process.

The release path is:

1. Make and merge a code change into `main`.
2. Pull the latest `main` into the shared-drive clone.
3. Run local R checks.
4. Publish the checked-out code as a named release.
5. Launch the app and verify it.
6. Roll back to the previous release if needed.

This guide is for maintainers who manage the shared-drive deployment.

## Prerequisites

You need:

- a cloned repo on the shared drive
- `Git` installed
- `R` and `Rscript` installed
- permission to read and write the shared-drive repo

The shared-drive clone is the deployment repo. It should contain:

- `.git`
- `shared/`
- `releases/`
- `deployment/`

Example path:

```text
P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1
```

## First-Time Maintainer Setup

### 1. Open the shared-drive repo

In PowerShell:

```powershell
cd "P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1"
git status
```

If you see `fatal: not a git repository`, you are in the wrong folder. Move into the actual repo folder and try again.

### 2. Run setup if this machine has not been prepared yet

In `R` or `RStudio`:

```r
source("R/SETUP/new_setup.R")
```

### 3. Install repo dependencies if needed

If the machine is missing required R packages:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "source('R/dependencies.R')"
```

If `Rscript` is already on your `PATH`, you can use:

```powershell
Rscript -e "source('R/dependencies.R')"
```

## Standard Update Flow

### 1. Make and merge the code change

From your development machine:

```bash
git checkout -b codex/my-change
```

Make the change, then run local checks:

```bash
Rscript R/CI/run_ci_checks.R
```

Commit and push:

```bash
git add .
git commit -m "Describe the change"
git push -u origin codex/my-change
```

Open a pull request and merge it into `main`.

### 2. Update the shared-drive clone

On the maintainer machine, in the shared-drive repo:

```powershell
cd "P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1"
git checkout main
git pull origin main
```

### 3. Run local checks in the shared-drive clone

```powershell
Rscript R/CI/run_ci_checks.R
```

If `Rscript` is not recognized:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/CI/run_ci_checks.R"
```

Do not publish if this step fails.

### 4. Publish the new live version

Choose a version name, for example `v0.5.0`.

Then run:

```powershell
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0
```

Windows example with a full `Rscript.exe` path:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/SETUP/release_publish.R" publish-local --version v0.5.0
```

What this does:

1. Copies the current checked-out code into `releases/v0.5.0/`
2. Runs the release smoke check
3. Updates `shared/current_release.txt`
4. Writes a line to `shared/deploy_log.tsv`

### 5. Verify the release

Check the active release pointer:

```powershell
Get-Content "shared\current_release.txt"
```

Then launch:

```text
deployment\Launch RIDS.bat
```

Confirm:

- the app opens successfully
- the visible change is present
- the footer version pill matches the release version

## Rebuilding an Existing Release Folder

If a release folder already exists and you want to rebuild it, use `--force`:

```powershell
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0 --force
```

Windows example with a full `Rscript.exe` path:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/SETUP/release_publish.R" publish-local --version v0.5.0 --force
```

Use this when a previous attempt created a partial release folder.

## Rollback

If a release causes problems, switch the live pointer back to an earlier release.

Run:

```powershell
Rscript R/SETUP/release_publish.R rollback --version v0.4.1
```

Windows example with a full `Rscript.exe` path:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" "R/SETUP/release_publish.R" rollback --version v0.4.1
```

What rollback does:

1. Verifies `releases/v0.4.1/` exists
2. Updates `shared/current_release.txt`
3. Writes a line to `shared/deploy_log.tsv`

After rollback:

1. Open `deployment/Launch RIDS.bat`
2. Confirm the previous version is now live
3. Confirm the issue is no longer present

## Troubleshooting

### `fatal: not a git repository`

You are in the wrong folder.

Move into the actual shared-drive repo:

```powershell
cd "P:\RESEARCH SYSTEMS\RIDS_\RIDS-v1"
git status
```

### `there is no package called '...'`

Install repo dependencies on that machine:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "source('R/dependencies.R')"
```

Then retry the command.

### `Release folder already exists and is not empty`

Either:

- use a new version name, or
- rebuild that folder with `--force`

Example:

```powershell
Rscript R/SETUP/release_publish.R publish-local --version v0.5.0 --force
```

### Shared-drive path issues

Make sure:

- the drive is connected
- you are working inside the shared-drive repo clone
- the repo still contains `shared/`, `releases/`, and `deployment/`

### Release smoke check failed

The script stops before changing the live version if the release does not validate.

Common causes:

- the shared config file is missing
- required app files are missing
- dependencies are not installed on the machine
- the checked-out code does not load cleanly

Recommended response:

1. Read the error message carefully
2. Fix the code or machine setup issue
3. Re-run local checks
4. Re-run the publish command
