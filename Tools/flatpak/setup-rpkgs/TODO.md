# TODO:

- [x] 1. make sure the installed packages are actually symlinks
- [x] 2. make a neat interface to loop over all modules (folder structure identical to JASP!)
- [x] 3. test it for 4-5 modules
- [ ] 4. look at how multiple versions are handled within a custom CRAN repo! test if renv::install(@version) works!
- [x] 5. bundle an renv tar.gz into this and make sure we can install it separately!

- [x] 6. look into running flatpak locally
  - [x] a. how to update jasp? can look at specific commit I guess

- [x] 7. can we do without the custom download script? No we can't
- [ ] 8. make this a github repo and push it (too much work went into it already!)
- [x] 9. add renv to the local CRAN repo and install it from there!

- [x] 10. Somehow put stuff in /app/lib64/
- [x] 11. Don't install all the r packages, only V8, renv, and jaspBase. + jaspGraphs

- [x] 12. Upload the big tar.gz to static.

- [ ] 13. There are messages about "package is not available" (devtools, roxygen2, etc.)
  - a. [ ] figure out if we can get these by also downloading Suggests!

- [ ] 14. The moduleEnvironments object is no longer necessary. It's probably useful for debugging to create it but it shouldn't be in the archive.

# BONUS

- [x] 1. Clean out the massive pile of shit that some people put in uncommon directories.
         At least flexplot/vignettes/* & flexplot/flexplot.jmo should be deleted.
- [ ] 2. delete ._v8?
- [x] 3. Don't install helpfiles and stuff
- [x] 4. Don't compress the archive locally so it can be updated. No bad idea.
- [ ] 5. the url for github should be case insensitive (since web urls are, but remotes specification is not)
  - a. [ ] Test the above.
