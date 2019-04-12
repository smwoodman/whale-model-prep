##################################### GridPlots.r ##############################################
#
#  This code was developed by Sam Woodman to facilitate plotting of cetacean density-model 
#  prediction grids.  
#
#   Written 03/2019  (Originally named "Plotting_for_Karin.r")
#
#   03/20/2019:  Renamed to GridPlots.r and edited by KAF for WEAR (Whale entanglement risk 
#   assessment) project; also added code to print individual plots to a specified PNG file
#   
#   04/12/2019: Edited by SMW. Removed 'st_wrap_dateline()' call, which was actually not helpful
#   because we want longitudes in [0, 360] range for predictions that span the dateline.
#   Added code for creating a [0. 360] base map for plotting Hawaii predictions
#   Added code for specifying break values to make color schemes consistent across plots
#
##################################################################################################
# Clear workspace

library(maps)
library(sf)
library(maptools)

# NOTE: function assumes coordinates are WGS 84 geographic coordinates

##################################################################################################
# Map for 0-180 coords
map.base <- st_as_sfc(maps::map('world', plot = FALSE, fill = TRUE))
# Map for 0-360 coordinates
map.base.360 <- st_as_sfc(maps::map('world2', plot = FALSE, fill = TRUE))


### Sam testing
# density.df <- read.csv("C:/Ensemble Tool/eSDM-data-local/eSDM_data_manuscript/Predictions_Becker_et_al_2016.csv")
# plot.dens.func(density.df, c(1,2,3), plot.title = "Becker et al", legend.pos = 4,
#                xlim = c(-125, -120), ylim = c(35, 40), cex = 1, pch = 18)

### Sam testing 2
x <- read.csv("Documents/R Directories/redateline/Brydes_sample_density_data.csv")
plot.dens.func(
  "Documents/R Directories/redateline/sample1a", x, c("mlon", "mlat", "Avg.Dens")
)
plot.dens.func(
  "Documents/R Directories/redateline/sample1b", x, c("mlon", "mlat", "Avg.Dens"), 
  map.base = map.base.360, 
  breaks.val = c(0, 0.0001, 0.0003, 0.0005, 0.0006, max(x$Avg.Dens, na.rm = TRUE))
)
plot.dens.func(
  "Documents/R Directories/redateline/sample1c", x, c("mlon", "mlat", "Avg.Dens"), 
  map.base = map.base.360, 
  breaks.val = c(0, 0.0001, 0.0003, 0.0005, 0.0006, max(x$Avg.Dens, na.rm = TRUE)), 
  col.pal = rev(RColorBrewer::brewer.pal(5, "Spectral"))
)


##################################################################################################
### Plotting function
plot.dens.func <- function(plot.file = NULL, density.df, col.names, map.base = NULL, 
                           breaks.val = NULL, col.pal = NULL, pch = 19, cex = 0.5, 
                           plot.title = "Plot", xlim = NULL, ylim = NULL, legend.pos = 4) {
  ### Inputs
  # plot.file: Name of file to which to save the plot (if desired). If not specified,
  #   map will be plotted within RStudio
  # density.df: Data frame with at least three columns (longitude, latitude, prediction value)
  # col.names: Vector of three strings (names of columns) or numbers (indices of columns).
  #   Order of names or indicies must be longitude, latitude, and density values
  # map.base: can pass in map.base object so function doesn't recreate it each time
  # breaks.val: Can specify the break points for plots to keep the colors/scale consistent
  #   across plots
  # col.pal: Can specify colors to be used for plot; must have one less element than 'breaks.val'
  # pch: Point type
  # plot.title: Plot title, as string
  # xlim: x axis limits, e.g. c(-135, -115). 
  #   Use the range ([-180, 180] or [0, 360]) of the density values
  # ylim: y axis limits, e.g. c(30, 50)
  # legend.pos: Legend position: 1 is bottom, 2 is left, 3 is top, and 4 is right of plot
  
  stopifnot(is.data.frame(density.df), length(col.names) == 3)
  
  # Convert points to sf object so it will be 'spatially' plotted
  density.sf <- st_as_sf(
    density.df[, col.names], coords = c(col.names[1:2]), 
    crs = 4326, agr = "constant"
  )
  
  # Generate map if necessary - should be generated outide and passed to function for efficiency
  if (is.null(map.base)) {
    if (st_bbox(density.sf)[3] > 180) {
      map.base <- st_as_sfc(maps::map('world2', plot = FALSE, fill = TRUE))
    } else {
      map.base <- st_as_sfc(maps::map('world', plot = FALSE, fill = TRUE))
    }
  }
  
  # Plot map, and save to file if 'plot.file' is specified
  if (!is.null(plot.file)) png(paste0(plot.file, ".png"), width = 8, height = 6, units = 'in', res = 450)
  
  if (is.null(breaks.val)) {
    plot(density.sf[1], main = plot.title, axes = TRUE, xlim = xlim, ylim = ylim, 
         pch = pch, cex = cex, 
         key.length = 1, key.pos = legend.pos, reset = FALSE)
  } else {
    if (is.null(col.pal)) col.pal <- sf.colors(n = length(breaks.val) - 1)
    stopifnot(length(col.pal) == (length(breaks.val) - 1))
    
    plot(density.sf[1], main = plot.title, axes = TRUE, xlim = xlim, ylim = ylim, 
         breaks = breaks.val, pal = col.pal, pch = pch, cex = cex, 
         key.length = 1, key.pos = legend.pos, reset = FALSE)
  }
  plot(map.base, add = TRUE, col = "tan", border = "black") 
  #^ set 'border = NA' to remove black border
  
  if (!is.null(plot.file)) dev.off()
}

##################################################################################################
