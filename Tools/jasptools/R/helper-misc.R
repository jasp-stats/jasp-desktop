# TODO: ask @timkdj whether these can be removed!
#
# initNamespace <- function(ns) {
#   if (ns %in% loadedNamespaces())
#     unloadNamespace(ns)
#   loadNamespace(ns)
# }
# 
# restoreOptions <- function(opts) {
#   options(opts) # overwrite changed options
#   addedOpts <- setdiff(names(options()), names(opts))
#   options(Map(function(x) NULL, addedOpts)) # remove added options
# }
