# legendToPlotRatio <- function(graph,
#                               device = grDevices::png,
#                               deviceOpts = list(filename = tempfile(), width = 420, height = 380),
#                               closeDevice = TRUE, removeTempFile = TRUE) {
# 
#     # NOTE: first argument of deviceOpts must be the file name for device.
#     # note that this is named and different devices have different names
#     # for the argment (i.e. png has filename and pdf has file).
# 
#     stopifnot(is.character(deviceOpts[[1]]))
# 
#     do.call(device, deviceOpts)
#     grob <- ggplot2::ggplotGrob(graph)
#     legendSize <- as.numeric(grid::convertWidth(grid::grobWidth(grob$grobs[[15]]), unitTo = "inches"))
#     plotSize <- as.numeric(grid::convertWidth(grid::grobWidth(grob$grobs[[7]]), unitTo = "inches"))
# 
#     if (closeDevice) {
#         grDevices::dev.off()
#         if (removeTempFile) {
#             file.remove(deviceOpts[[1]])
#         }
#     }
#     return (legendSize / plotSize)
# 
# }
