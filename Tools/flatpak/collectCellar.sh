#!/usr/bin/env bash
rm -rf ./_tmpCellar ./cellar.tar.gz 
mkdir -p _tmpCellar/cellar
find $1/_cache/renv-root/source/ | grep -E  "*.tar.gz" | xargs -i cp {} ./_tmpCellar/cellar
tar -C _tmpCellar -czf ./cellar.tar.gz cellar
rm -rf ./_tmpCellar
