library(sf)

csvextract_to_shp <- function(df, xy.cols, file.out) {
  stopifnot(grepl(".shp", file.out))
  df.sf <- df %>% 
    st_sf(geometry = eSDM::pts_to_sfc_centroids(df[, xy.cols], 0.027 / 2, 4326), crs = 4326)

  st_write(df.sf, file.out)
}


csvextract_to_shp(
  read.csv("../whale-model-prep_data/Grid/Grid_CCSRA/WEAR_CCSRA_3km_2012-09-01.csv"), 
  c("lon", "lat"), 
  "../whale-model-prep_data/shapefiles/Exp_extractions/ccsra_2012-09-01.shp" 
)


csvextract_to_shp(
  read.csv("../whale-model-prep_data/Grid/Grid_Nonrectangle_3km_WEAR_bathy.csv"), 
  c("lon180", "lat"), 
  "../whale-model-prep_data/shapefiles/Exp_extractions/etopo.shp" 
)

csvextract_to_shp(
  read.csv("../whale-model-prep_data/Grid/Grid_murSST/WEAR_mursst_3km_2005-01-01.csv"), 
  c("lon", "lat"), 
  "../whale-model-prep_data/shapefiles/Exp_extractions/mursst_2005-01-01.shp" 
)
