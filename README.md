# RIDS-v1

## What this setup does

This setup creates one shared RIDS system for everyone to use.

- the app stays in one shared folder
- the DuckDB database stays in one shared folder
- uploads and outputs stay in shared folders
- users open the app from one shared `.bat` file

For now, only one person should be using the system at a time.

## What you need before starting

1. Put this RIDS app folder in its shared drive location.
2. Make sure `R` is installed on each laptop that will open the app.
3. Make sure users can access the shared drive.
4. Decide where these shared folders should live:
   - database
   - uploads
   - outputs

## First-time setup

Do these steps once when setting up the shared system.

1. Open the RIDS project in `RStudio`.
2. Open [R/SETUP/new_setup.R](/Users/tategraham/Documents/NHS/RIDS-v1/R/SETUP/new_setup.R).
3. Check the folder paths near the top of the script.
4. Change them if needed.
5. Run the whole script.

The setup script will:

1. install missing R packages
2. create the shared folders if they do not already exist
3. create the shared deployment config
4. create the shared launcher file
5. create the central DuckDB database if it does not already exist

When setup finishes, it will tell you where the shared launcher file was created.

## How to launch the app

After setup is complete:

1. Open the shared `Launch RIDS.bat` file in the `deployment` folder.
2. Wait a few seconds.
3. Your web browser should open automatically.
4. If this is the first launch, create the first admin account on the login screen.
5. Sign in.

## Normal daily use

After the first-time setup, users only need to:

1. open the shared `Launch RIDS.bat`
2. wait for the browser to open
3. sign in

## If something goes wrong

1. If the browser does not open, wait 10 seconds and try the launcher again.
2. If you see a message saying `R` was not found, install `R` on that laptop.
3. If the launcher cannot find the app or config, re-run [R/SETUP/new_setup.R](/Users/tategraham/Documents/NHS/RIDS-v1/R/SETUP/new_setup.R) from the shared app folder.
4. If the shared drive is unavailable, reconnect to the shared drive and try again.

## Important limitation for now

- the database is central and shared
- the launcher is central and shared
- only one active user should use the system at a time while it is still using DuckDB
- SQL Server is planned later
