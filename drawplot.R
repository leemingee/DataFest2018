rgb888_error <- matrix(c(0.1030000, 0.1013333, 0.1036667,
                         0.1016667, 0.1013333, 0.1033333,
                         0.1170000, 0.1060000, 0.1073333,
                         0.1140000, 0.1043333, 0.1033333),4,byrow = T)
heatmap.2(rgb888_error, Rowv=NA, Colv=NA, col = "blue",
          scale="row", density.info="none", trace="none")