# Function to plot model input variables (eg SST)
# By Sam Woodman, Dec 2019

###############################################################################
# x <- read.csv("../whale-model-prep_data/Grid/Grid_all/WEAR_3km_2005-01-01.csv")
# x.sf <- st_as_sf(x, coords = c("lon", "lat"), agr = "constant")
# plot(x.sf[1], axes = TRUE)
# summary(x$sst.mean)


# ### Example
# library(dplyr)
# library(RColorBrewer)
# library(sf)
# gridplots_vars(
#   head(list.files("../whale-model-prep_data/Grid/Grid_all", full.names = TRUE)), 
#   "../whale-model-prep_data/Grid/Plot_test/", 
#   var.name = "sst.mean",
# )

### Function
gridplots_vars <- function(files.toplot, plot.folder, var.name, 
                           col.breaks = NULL, col.pal = NULL) {
  ### Inputs:
  # files.toplot: character; full files paths to grid csv files
  # plot.folder:  character; path (including trailing '/') to folder in 
  #   which to save plots
  # var.name:   character; variable name to plot
  # col.breaks: numeric; break values to use for color scheme for all plots
  # col.pal:    character; color palette to use for all plots
  
  
  stopifnot(
    inherits(files.toplot, "character"),
    inherits(plot.folder, "character") | is.null(plot.folder)
  )
  
  if (is.null(col.pal)) {
    n.b <- ifelse(is.null(col.breaks), 6, length(col.breaks))
    col.pal <- rev(brewer.pal(n.b - 1, "YlGnBu"))
  }
  
  for (i in files.toplot) {
    print(i)
    x <- read.csv(i)
    x.sel <- x[, c("lon", "lat", var.name)]
    x.sf <- st_as_sf(x.sel, coords = c("lon", "lat"), agr = "constant")
    
    if (is.null(col.breaks)) {
      col.breaks <- seq(
        from = floor(min(x.sel[, 3], na.rm = TRUE)), 
        to = ceiling(max(x.sel[, 3], na.rm = TRUE)), 
        length.out = 6
      )
      col.breaks <- c(floor(col.breaks[1:5]), ceiling(col.breaks[6]))
    }
    
    i.file <- tail(strsplit(i, "/")[[1]], 1)
    file.tosave <- gsub(".csv", paste0("_", var.name, ".png"), i.file)
    
    if (!is.null(plot.folder)) png(paste0(plot.folder, file.tosave), units = "in", width = 4, height = 6, res = 300)
    plot(x.sf, axes = TRUE, border = NA, pch = 15, cex = 0.5, 
         main = paste(i.file, var.name, sep = " - "), cex.main = 0.7, 
         xaxt = "n", yaxt = "n", xlim = c(-128, -115), ylim = c(32, 49),
         breaks = col.breaks, pal = col.pal, asp = 0, 
         key.length = 1, key.pos = 4, reset = FALSE)
    sf::.degAxis(1, at = c(-125, -120))
    sf::.degAxis(2)
    if (!is.null(plot.folder)) dev.off()
  }
}
