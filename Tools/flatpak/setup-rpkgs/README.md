# Helper functions to JASP work on flatpak

#### Building the flatpak locally.

From a terminal, run `Rscript R/flatpakGeneratePkgsList.R` to generate a tar.gz under archives that contains all packages.
Alternatively, open the project file in RStudio and source `flatpakGeneratePkgsList.R`.
To build JASP locally, clone https://github.com/flathub/org.jaspstats.JASP and then run `flatpak-builder --user --install --force-clean build org.jaspstats.JASP.json` where `build` is your build folder.

#### Debugging the flatpak build

First build the flatpak version locally.
Next, in your clone of flathub/org.jaspstats.JASP do
```
flatpak run -d --command=sh org.jaspstats.JASP
```
This starts shell instead of directly starting JASP.
The remainder of the commans should be run from that shell.
To start JASP with a debugger, do
```
gdb /app/bin/org.jaspstats.JASP
```
If you get a message about debugsymbols not found, you can hopefully locate these using:
```
find /app/ -type f -name "*.debug"
```
