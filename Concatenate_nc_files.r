# Concatenates of HYCOM .nc filefiles for E and W of dateline 
#
# 10/4/2018 --- Experimental - none of this is working yet!
#
#-------------------------------------------------------------------------------------

  rm(list=ls())
  library(ncdf4)
  setwd("C:/KAF/PROJECTS/SERDP-CCmodels/HICEAS2017/hycom/HYCOM_download testing")

  nc.path <- "C:/KAF/PROJECTS/SERDP-CCmodels/HICEAS2017/hycom/HYCOM_download testing/"
  nc.file1 <- paste0(nc.path,"expt_19.1_TEST_WEST.nc")
  nc.dataW <- nc_open(nc.file1)

  nc.path <- "C:/KAF/PROJECTS/SERDP-CCmodels/HICEAS2017/hycom/HYCOM_download testing/"
  nc.file2 <- paste0(nc.path,"expt_19.1_TEST_EAST.nc")
  nc.dataE <- nc_open(nc.file2)

#  str(nc.data)


  time.offset <- ncvar_get(nc.dataW,'time') *3600   # secs since 2000-01-01
  file.dateW <-substr(as.POSIXlt(time.offset, origin="2000-01-01",tz= "GMT"),1,10)
  file.dateW

  time.offset <- ncvar_get(nc.dataE,'time') *3600   # secs since 2000-01-01
  file.dateE <-substr(as.POSIXlt(time.offset, origin="2000-01-01",tz= "GMT"),1,10)
  file.dateE

  day.index <- 1
  pixel.size <- 0.08
  pixel.radius <- 1
  tgtlat <- 42.00
  tgtlon <- 180.12

  HYCOM.latE <- ncvar_get(nc.dataE,'lat')       # These are presumed to be l
  HYCOM.latW <- ncvar_get(nc.dataW,'lat')       # These are presumed to be lower

  HYCOM.lonE <- ncvar_get(nc.dataE,'lon')       #  left corner of each pixelower
  HYCOM.lonW <- ncvar_get(nc.dataW,'lon')       #  left corner of each pixel
  HYCOM.lonE2 <-  HYCOM.lonE + 360
  bound <- length (HYCOM.lonW)

  HYCOM.lon.mid <- c(HYCOM.lonW, HYCOM.lonE2) + pixel.size/2
  HYCOM.lat.mid <- HYCOM.latE + pixel.size/2

  c<-which(abs(HYCOM.lat.mid-tgtlat)==min(abs(HYCOM.lat.mid-tgtlat)))
  r<-which(abs(HYCOM.lon.mid-tgtlon)==min(abs(HYCOM.lon.mid-tgtlon)))

  row1    <- max(r-pixel.radius,1)
  numrows <- 2*pixel.radius+1       # number of pixels within pixel.radius  
  
  col1    <- max(c-pixel.radius,1)  # Note: assumes ROMS grid is larger than data grid
  numcols <- 2*pixel.radius+1
 
  newlon <- HYCOM.lon.mid[row1:(row1+numrows-1)]
  newlat <- HYCOM.lat.mid[col1:(col1+numcols-1)]

  newlat
  newlon

  bound
 
  if (r>bound+1) {    #  This means all pixels E of dateline
      ssh      <- matrix(ncvar_get(nc.dataE,'surf_el',start=c((row1-bound),col1,day.index),
                  count=c(numrows,numcols,1),verbose=FALSE)
                  , numrows, numcols, dimnames=list(newlon,newlat))
  } else {
    if (r<bound) { #  This means all pixels W of dateline
      ssh      <- matrix(ncvar_get(nc.dataW,'surf_el',start=c(row1,col1,day.index),
                  count=c(numrows,numcols,1),verbose=FALSE)
                  , numrows, numcols, dimnames=list(newlon,newlat))
    } else {        # This means they span the dateline

      numrowsW <- length(which(newlon<180))
      numrowsE <- length(which(newlon>180))

      sshE     <- matrix(ncvar_get(nc.dataE,'surf_el',start=c(1,col1,day.index),
                  count=c(numrowsE,numcols,1),verbose=FALSE)
                  , numrowsE, numcols, dimnames=list(newlon[newlon>180],newlat))
      sshW      <- matrix(ncvar_get(nc.dataW,'surf_el',start=c(row1,col1,day.index),
                  count=c(numrowsW,numcols,1),verbose=FALSE)
                  , numrowsW, numcols, dimnames=list(newlon[newlon<180],newlat))
      ssh <-   rbind(sshW,sshE)

    } # end if
  } # end if




