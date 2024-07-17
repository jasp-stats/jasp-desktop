#!/usr/bin/env bash
echo "Cleaning up flatpak"
pwd
#Cant use the cellar post build.
rm -rf /app/lib64/cellar/
#cant delete read only renv cache stuff. very annoying
chmod -R +w _cache/
rm -rf _cache
