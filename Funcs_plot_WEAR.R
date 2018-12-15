### Functions for plotting data outputted from 'CCE_DailyGridsCrop_Ind...R'
# Functions used in 'Exp_grid.R'
# By Sam Woodman, Dec 2018

###############################################################################

library(dplyr)
library(maps)
library(purrr)
library(sf)


###############################################################################
###############################################################################
# Functions for plotting

#------------------------------------------------------------------------------
### Convert centroids to polygons
pts_to_sfc_centroids <- function(x, y, crs.prov = NULL) {
  ### Inputs
  # x is data.frame with columns with longs and lats, respectively
  # y is half of length (or width) of pixel
  
  stopifnot(inherits(x, "data.frame"), ncol(x) >= 2, is.numeric(y))
  sfg.list <- unname(apply(x, 1, function(i, j) {
    st_polygon(list(matrix(c(i[1] + j, i[1] - j, i[1] - j, 
                             i[1] + j, i[1] + j, i[2] + j, i[2] + j, i[2] - j, 
                             i[2] - j, i[2] + j), ncol = 2)))
  }, j = y))
  if (is.null(crs.prov)) st_sfc(sfg.list) else st_sfc(sfg.list, crs = crs.prov)
}


#------------------------------------------------------------------------------
### Plot data
plot.dens.func <- function(dens.df, dens.sfc, data.names, 
                           map.base = NULL, map.base.col = "tan", 
                           plot.title = NULL, title.cex, xlim = NULL, ylim = NULL, 
                           breaks.vals, col.pal, sf.legend.pos = 4, 
                           lon.lab.which, lat.lab.which, lon.lab.at, lat.lab.at, 
                           axis.cex, 
                           pts.df = NULL, pts.which = NULL, pts.names, 
                           pts.cex, pts.col, pts.pch) {
  ### Inputs
  # dens.df: Data frame with at least three columns (longitude, latitude, prediction value)
  # dens.sfc: sfc object; output of pts_to_sfc_centroids()
  # data.names: Vector of string(s) (column name(s)) or numbers (column indice(s)).
  #   Columns with data to be plotted
  # plot.title: Plot title, as string
  # title.cex: Size of plot title
  # xlim: x axis limits, e.g. c(-135, -115). Use range [-180, 180]
  # ylim: y axis limits, e.g. c(30, 50). Use range [-90, 90]
  # break.vals: break values for color-coding. 
  #   First and last values must be <= and >= min and max plot values, respectively
  # col.pal: Color palette for plot values; must have one fewer element than 'break.vals'
  # sf.legend.pos: Legend position: 1 is bottom, 2 is left, 3 is top, and 4 is right of plot
  # lon.lab.which: Indices of data that get longitude labels.
  #   Corresponds to order of 'data.names'
  # lat.lab.which: Indices of data that get latitude labels.
  #   Corresponds to order of 'data.names'
  # lon.lab.at: Location of longitude tick marks and (maybe) tick labels
  # lat.lab.at: Location of latitude tick marks and (maybe) tick labels
  # axis.cex: Size of axis labels
  # pts.df: Data frame with at least two columns. 
  #   For adding points (e.g. sightings) to map
  # pts.names: Names of indices of columns with longitude and latitude data, respectively
  # pts.which: Indices of data that get these points plotted with them. 
  #   Corresponds to order of 'data.names'
  # pts.cex: cex (size) of additional points
  # pts.col: Color of additional points
  # pts.pch: pch (point type) of additional points
  
  #----------------------------------------------------------------------------
  stopifnot(
    is.data.frame(dens.df), 
    length(breaks.vals) == (length(col.pal) + 1)
  )
  
  
  #----------------------------------------------------------------------------
  # Plot data in each column specified using data.names
  for (j in seq_along(data.names)) {
    dens.sf <- dens.df[, data.names[j]] %>% 
      purrr::set_names("val.plot") %>% 
      st_sf(geometry = dens.sfc, agr = "constant") %>% 
      dplyr::mutate(val.plot = suppressWarnings(as.numeric(val.plot))) %>% 
      st_wrap_dateline() #Ensures lons are in [-180, 180]
    
    
    #----------------------------------------------------------------------------
    # Generate main plot
    plot(dens.sf[1], main = plot.title, cex.main = title.cex, 
         axes = TRUE, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", 
         breaks = breaks.vals, pal = col.pal, border = NA, 
         key.length = 1, key.pos = sf.legend.pos, reset = FALSE)
    if (is.null(map.base)) plot(map.base, add = TRUE, col = map.base.col)
    
    # Plot axis ticks and labels as necessary
    if (j %in% lon.lab.which) {
      axis(1, at = lon.lab.at, cex.axis = axis.cex, 
           labels = parse(text = degreeLabelsEW_sf(lon.lab.at)))
    } else {
      axis(1, at = lon.lab.at, labels = FALSE)
    }
    
    if (j %in% lat.lab.which) {
      axis(2, at = lat.lab.at, cex.axis = axis.cex, 
           labels = parse(text = degreeLabelsNS_sf(lat.lab.at)))
    } else {
      axis(2, at = lat.lab.at, labels = FALSE)
    }
    
    
    # Plot points, e.g. sightings points
    if (!is.null(pts.df) && j %in% pts.which) {
      pts.sfc <- st_as_sf(pts.df, coords = pts.names, crs = 4326) %>% 
        st_geometry() %>% 
        st_wrap_dateline()
      plot(pts.other.sf, add = TRUE, cex = pts.cex, col = pts.col, pch = pts.pch)
    }
  }
}


###############################################################################
###############################################################################
# Function for generating axis labels for plots of sf objects. From eSDM

### Adapted from https://github.com/r-spatial/sf/blob/master/R/graticule.R
degreeLabelsNS_sf = function(x) {
  pos = sign(x) + 2
  dir = c("~S", "", "~N")
  paste0(abs(x), "*degree", dir[pos])
}

degreeLabelsEW_sf = function(x) {
  x <- ifelse(x > 180, x - 360, x)
  pos = sign(x) + 2
  if (any(x == -180)) pos[x == -180] = 2
  if (any(x == 180)) pos[x == 180] = 2
  dir = c("~W", "", "~E")
  paste0(abs(x), "*degree", dir[pos])
}


###############################################################################
###############################################################################
# legend() function from grpahics package; added 'text.cex' argument
legend.smw <- function (x, y = NULL, legend, fill = NULL, col = par("col"), 
                        border = "black", lty, lwd, pch, angle = 45, density = NULL, 
                        bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"), 
                        box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, 
                        xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5), 
                        text.width = NULL, text.col = par("col"), text.font = NULL, 
                        merge = do.lines && has.pch, trace = FALSE, plot = TRUE, 
                        ncol = 1, horiz = FALSE, title = NULL, text.cex = NULL, inset = 0, xpd, title.col = text.col, 
                        title.adj = 0.5, seg.len = 2) {
  if (missing(legend) && !missing(y) && (is.character(y) || 
                                         is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (!missing(xpd)) {
    op <- par("xpd")
    on.exit(par(xpd = op))
    par(xpd = xpd)
  }
  title <- as.graphicsAnnot(title)
  if (length(title) > 1) 
    stop("invalid 'title'")
  legend <- as.graphicsAnnot(legend)
  n.leg <- if (is.call(legend)) 
    1
  else length(legend)
  if (n.leg == 0) 
    stop("'legend' is of length 0")
  auto <- if (is.character(x)) 
    match.arg(x, c("bottomright", "bottom", "bottomleft", 
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  if (is.na(auto)) {
    xy <- xy.coords(x, y, setLab = FALSE)
    x <- xy$x
    y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2) 
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  xlog <- par("xlog")
  ylog <- par("ylog")
  rect2 <- function(left, top, dx, dy, density = NULL, angle, 
                    ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    rect(left, top, r, b, angle = angle, density = density, 
         ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if (xlog) 
      x <- 10^x
    if (ylog) 
      y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog) 
      x <- 10^x
    if (ylog) 
      y <- 10^y
    text(x, y, ...)
  }
  if (trace) 
    catn <- function(...) do.call("cat", c(lapply(list(...), 
                                                  formatC), list("\n")))
  cin <- par("cin")
  Cex <- cex * par("cex")
  if (is.null(text.width)) 
    text.width <- max(abs(strwidth(legend, units = "user", 
                                   cex = cex, font = text.font)))
  else if (!is.numeric(text.width) || text.width < 0) 
    stop("'text.width' must be numeric, >= 0")
  xc <- Cex * xinch(cin[1L], warn.log = FALSE)
  yc <- Cex * yinch(cin[2L], warn.log = FALSE)
  if (xc < 0) 
    text.width <- -text.width
  xchar <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
  ychar <- yextra + ymax
  if (trace) 
    catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
                                                   ychar))
  if (mfill) {
    xbox <- xc * 0.8
    ybox <- yc * 0.5
    dx.fill <- xbox
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
                                                            0))) || !missing(lwd)
  n.legpercol <- if (horiz) {
    if (ncol != 1) 
      warning(gettextf("horizontal specification overrides: Number of columns := %d", 
                       n.leg), domain = NA)
    ncol <- n.leg
    1
  }
  else ceiling(n.leg/ncol)
  has.pch <- !missing(pch) && length(pch) > 0
  if (do.lines) {
    x.off <- if (merge) 
      -0.7
    else 0
  }
  else if (merge) 
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  if (has.pch) {
    if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L], 
                                                      type = "c") > 1) {
      if (length(pch) > 1) 
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type = "c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
    if (!is.character(pch)) 
      pch <- as.integer(pch)
  }
  if (is.na(auto)) {
    if (xlog) 
      x <- log10(x)
    if (ylog) 
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top <- y[2L]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust)) 
      xjust <- 0.5
    if (missing(yjust)) 
      yjust <- 0.5
  }
  else {
    h <- (n.legpercol + !is.null(title)) * ychar + yc
    w0 <- text.width + (x.intersp + 1) * xchar
    if (mfill) 
      w0 <- w0 + dx.fill
    if (do.lines) 
      w0 <- w0 + (seg.len + x.off) * xchar
    w <- ncol * w0 + 0.5 * xchar
    if (!is.null(title) && (abs(tw <- strwidth(title, units = "user", 
                                               cex = cex) + 0.5 * xchar)) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- par("usr")
      inset <- rep_len(inset, 2)
      insetx <- inset[1L] * (usr[2L] - usr[1L])
      left <- switch(auto, bottomright = , topright = , 
                     right = usr[2L] - w - insetx, bottomleft = , 
                     left = , topleft = usr[1L] + insetx, bottom = , 
                     top = , center = (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L] * (usr[4L] - usr[3L])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                      h + insety, topleft = , top = , topright = usr[4L] - 
                      insety, left = , right = , center = (usr[3L] + 
                                                             usr[4L] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace) 
      catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1), 
                                              rep.int(n.legpercol, ncol)))[1L:n.leg]
  yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol, 
                                             ncol)[1L:n.leg] - 1 + !is.null(title)) * ychar
  if (mfill) {
    if (plot) {
      if (!is.null(fill)) 
        fill <- rep_len(fill, n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
            col = fill, density = density, angle = angle, 
            border = border)
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines)) 
    col <- rep_len(col, n.leg)
  if (missing(lwd) || is.null(lwd)) 
    lwd <- par("lwd")
  if (do.lines) {
    if (missing(lty) || is.null(lty)) 
      lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) & 
      !is.na(lwd)
    if (trace) 
      catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
           yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
    if (plot) 
      segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
                  xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
                col = col[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep_len(pch, n.leg)
    pt.bg <- rep_len(pt.bg, n.leg)
    pt.cex <- rep_len(pt.cex, n.leg)
    pt.lwd <- rep_len(pt.lwd, n.leg)
    ok <- !is.na(pch)
    if (!is.character(pch)) {
      ok <- ok & (pch >= 0 | pch <= -32)
    }
    else {
      ok <- ok & nzchar(pch)
    }
    x1 <- (if (merge && do.lines) 
      xt - (seg.len/2) * xchar
      else xt)[ok]
    y1 <- yt[ok]
    if (trace) 
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
           ", ...)")
    if (plot) 
      points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
              bg = pt.bg[ok], lwd = pt.lwd[ok])
  }
  xt <- xt + x.intersp * xchar
  if (plot) {
    if (!is.null(title)) 
      text2(left + w * title.adj, top - ymax, labels = title, 
            adj = c(title.adj, 0), cex = text.cex, col = title.col)
    text2(xt, yt, labels = legend, adj = adj, cex = text.cex, 
          col = text.col, font = text.font)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top), 
                 text = list(x = xt, y = yt)))
}