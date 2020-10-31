### Function for caculating evaluation metrics for whale predictions 
###   aggregated at a variety of time scales


###############################################################################
whalepreds_evaluate <- function(
  x, y, x.cols = NULL, x.col.idx, y.cols, grid.rad = 0.027/2, 
  csv.filename = NULL, plot.path = NULL, 
  plot.xlim = c(-127, -116.5), plot.ylim = c(31.5, 49), 
  col.breaks = c(0, 0.01, 0.02, 0.03, 0.05, 0.07, 0.09), col.pal = NULL, 
  plot.main = NULL)
{
  ### Inputs
  # x: data.frame; Aggregated whale predictions
  # y: data.frame; Validation data
  # x.cols: character or numeric; vector of length >=3 containing, 
  #   in order, column names or indices of lon, lat, and whale preds. 
  #   Defaults to 'c(3, 2, 6:ncol(x))' based on KAF model outputs
  # x.col.idx: numeric; indices of characters in column names of x that
  #   specify the date; must be either of length 8 or 10 
  # y.cols: character or numeric; vector with length of 4 containing, 
  #   in order, column names or indices of lon, lat, date, and validation data
  # grid.rad: numeric; radius (half of length of one side) of grid cells
  #   Default is for ~3km grid: 0.027 / 2
  # csv.filename: character; path and filename for csv file to which to save
  #   metrics info; if NULL, csv not saved
  
  # plot.path: character; path to folder in which to save plots of preds and 
  #   validation data; if NULL, plots not made
  # plot.xlim: numeric of length 2; x axis plot limits, use range [-180, 180]
  # plot.ylim: numeric of length 2; y axis plot limits, use range [-90, 90]
  # col.breaks: numeric; length one more than col.pal; break points for 
  #   color bins
  # col.pal: numeric; length one less than col.breaks; colors to be used 
  #   for color bins (from smallest to largest); 
  #   if NULL, palette will be generated using RColorBrewer 'YlGnBu'
  # plot.main: character; if not NULL, the plot title will be 
  #   paste(plot.main, date, sep = " - ")
  
  
  ### Output
  # Data frame with columns for date interval start, AUC vals, and TSS vals
  
  #----------------------------------------------------------------------------
  # Input checks and (some) processing
  
  stopifnot(
    require(dplyr), 
    require(eSDM), 
    require(lubridate), 
    require(purrr), 
    require(RColorBrewer), 
    require(readr), 
    require(sf)
  )
  
  ### Format and length checks
  if (is.null(x.cols)) x.cols <- c(3, 2, 6:ncol(x))
  if (is.null(col.pal)) 
    col.pal <- rev(RColorBrewer::brewer.pal(length(col.breaks) - 1, "YlGnBu"))
  
  stopifnot(
    inherits(x, "data.frame"), 
    inherits(y, "data.frame"), 
    inherits(x.cols, c("character", "integer", "numeric")), 
    length(x.cols) >= 3, 
    inherits(x.col.idx, c("integer", "numeric")),
    inherits(y.cols, c("character", "integer", "numeric")), 
    length(y.cols) == 4, 
    inherits(plot.xlim, "numeric"), 
    inherits(plot.ylim, "numeric"), 
    inherits(col.breaks, "numeric"), 
    inherits(col.pal, "character"), 
    length(col.breaks) == (length(col.pal) + 1)
  )
  
  names(plot.xlim) <- c("xmin", "xmax") #for st_bbox() calls below
  names(plot.ylim) <- c("ymin", "ymax") #for st_bbox() calls below
  
  
  #----------------------------------------------------------------------------
  # Process whale preds and get dates from column names
  
  ### Process preds and create geometry for x
  x.preds <- x %>%
    select(!!x.cols[-c(1:2)]) %>%
    set_names(substr(names(.), min(x.col.idx), max(x.col.idx)))
  
  x.geom <- x %>%
    select(!!x.cols[1:2]) %>%
    pts2poly_centroids(grid.rad, crs = 4326)
  
  
  ### Get dates from column names and create intervals
  # What are dates separated by?
  tmp <- if (all(grepl("[.]", names(x.preds)))) {
    "."
  } else if (all(grepl("_", names(x.preds)))) {
    "_"
  } else if (all(grepl("-", names(x.preds)))) {
    "-"
  } else {
    stop("The dates in the column names must all be separated by '.', '_', or '-'")
  }
  
  # Generate dates from column names (are dates 8 digit or 10 digit?)
  cols.dates <- switch(
    as.character(length(x.col.idx)), 
    "8" = as.Date(names(x.preds), format = paste0("%y", tmp, "%m", tmp, "%d")), 
    "10" = as.Date(names(x.preds), format = paste0("%Y", tmp, "%m", tmp, "%d")), 
    stop("x.col.idx must be of length 8 (e.g. \"05_01_01\" or \"05.01.01\") ", 
         "or 10 (e.g. \"2005_01_01\" or \"2005.01.01\")")
  ); rm(tmp)
  
  # Create intervals captured by each column
  d <- unique(diff(cols.dates))
  dates.diff <- ifelse(length(d == 1), d, NA)
  d.last <- ifelse(
    is.na(dates.diff),  tail(cols.dates, 1) + months(1), 
    tail(cols.dates, 1) + days(dates.diff)
  )
  cols.intervals.ends <- c(
    tail(cols.dates, -1), 
    if (is.na(dates.diff)) { #use if {} else {} b/c ifelse does some type coersion things
      tail(cols.dates, 1) + months(1) #if interval is monthly
    } else {
      tail(cols.dates, 1) + days(dates.diff)#if interval is # of days
    }
  )
  
  cols.intervals <- interval(cols.dates, cols.intervals.ends - days(1))
  rm(d, dates.diff, d.last, cols.intervals.ends)
  
  
  #----------------------------------------------------------------------------
  ### Process validation data
  stopifnot(
    inherits(y[[y.cols[1]]], c("integer", "numeric")), 
    inherits(y[[y.cols[2]]], c("integer", "numeric")), 
    inherits(y[[y.cols[3]]], c("Date", "POSIXct")), 
    inherits(y[[y.cols[4]]], c("integer", "numeric"))
  )
  
  y.data <- y %>% 
    select(lon = !!y.cols[1], lat = !!y.cols[2], 
           dt = !!y.cols[3], valid_data = !!y.cols[4]) %>% 
    mutate(dt = as.Date(dt)) #Since we only need dates
  
  
  #----------------------------------------------------------------------------
  ### Determine which validation data are in intervals, and which 
  ###   intervals contain validation data
  valid.which <- vapply(y.data$dt, function(i, j) {
    i.which <- which(i %within% j)
    if (length(i.which) > 1)
      stop("Error allocating validation data at date ", i, 
           " to a single prediction interval")
    if(length(i.which) == 0) NA else i.which
  }, 1, j = cols.intervals)
  
  cols.intervals.which <- sort(unique(unlist(valid.which)))
  
  
  #----------------------------------------------------------------------------
  ### Filter data, and create sf objects for preds and validation data
  x.sf <- x.preds %>% 
    select(all_of(cols.intervals.which)) %>% 
    st_sf(geometry = x.geom, agr = "constant")
  
  y.sf <- y.data %>% 
    mutate(valid_data_pa = ifelse(valid_data >= 1, 1, 0), 
           preds_col = names(x.preds)[valid.which]) %>% 
    filter(!is.na(preds_col)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
  
  
  #----------------------------------------------------------------------------
  # Calculate metrics for dates with both preds and validation data
  
  ### Dates with both preds and validation data
  dates.both <- sort(unique(y.sf$preds_col))
  if (length(dates.both) == 0) 
    stop("No dates have both predictions and validation data - \nhave ", 
         "predictions been aggregated correctly, and is 'x.col.idx' correct?")
  
  print(paste("Number of dates:", length(dates.both)))
  
  ### Prep for plotting, if necessary
  if (!is.null(plot.path)) {
    hw <- diff(plot.ylim) / diff(plot.xlim)
    check.sfc <- st_as_sfc(st_bbox(c(plot.xlim, plot.ylim), crs = 4326))
    crop.bbox  <- st_bbox(c(plot.xlim , plot.ylim) + c(-1, 1), crs = 4326)
    
    map.base <- st_as_sfc(maps::map('world', plot = FALSE, fill = TRUE))
    map.base <- suppressMessages(st_crop(map.base, crop.bbox))
    
    x.sf.crop <- suppressMessages(st_crop(x.sf, crop.bbox))
    
    tmp <- suppressMessages(st_intersects(y.sf, check.sfc))
    if (!(unique(vapply(tmp, length, 1)) == 1)) 
      warning("Some of the validation data points are outside of the ", 
              "provided plot limits", 
              call. = FALSE, immediate. = TRUE)
    rm(tmp, check.sfc, crop.bbox)
  }
  
  ### Calculate metrics and plot, if necessary
  metrics.list <- lapply(dates.both, function(i, j, j.plot, k) {
    print(i)
    k.curr <- k %>% filter(preds_col == i)
    
    # If desired, plot and save predictions with overlaid validation data
    if (!is.null(plot.path)) {
      png(paste0(plot.path, i, ".png"), width = 6, height = 6*hw, 
          units = 'in', res = 450)
      
      plot.main <- ifelse(
        is.null(plot.main), i, paste(plot.main, i, sep = " - ")
      )
      
      plot(j[i], axes = TRUE, border = NA, breaks = col.breaks, pal = col.pal, 
           main = plot.main, xaxt = "n", xlim = plot.xlim, ylim = plot.ylim,
           asp = 0, key.length = 1, key.pos = 4, reset = FALSE)
      sf::.degAxis(1, at = c(-125, -120))
      plot(st_geometry(filter(k.curr, valid_data_pa == 0)), add = TRUE, 
           pch = 19, cex = 0.2, col = "black", 
           key.pos = NULL, reset = FALSE)
      plot(st_geometry(filter(k.curr, valid_data_pa == 1)), add = TRUE, 
           pch = 19, cex = 0.3, col = "red", 
           key.pos = NULL, reset = FALSE)
      plot(map.base, add = TRUE, col = "tan", border = NA)
      graphics::box()
      legend("topright", title = "Validation data", legend = c("Pres", "Abs"), 
             col = c("red", "black"), pch = 19, pt.cex = 1.5)
      
      dev.off()
    }
    
    # Calculate metrics
    if (length(unique(k.curr$valid_data_pa)) == 2) {
      evaluation_metrics(j, i, k.curr, "valid_data_pa", count.flag = FALSE)
    } else {
      c(NA, NA, NA)
    }
  }, j = x.sf, j.plot = x.sf.crop, k = y.sf)
  
  metrics.df <- data.frame(
    preds_col = dates.both, do.call(rbind, metrics.list), 
    stringsAsFactors = FALSE
  ) %>% 
    select(preds_col, AUC = 2, TSS = 3)
  
  ### Write metrics to a csv file if desired
  if (!is.null(csv.filename)) write_csv(metrics.df, file = csv.filename)
  
  ### Return data frame
  metrics.df
}

###############################################################################
