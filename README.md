# whale-model-prep

This repository includes prep work (grid creation, data extraction) for modeling cetacean distribution off the U.S. West coast. It also includes files, which begin with 'Whalepreds_', and associated functions for processing the cetacean predictions

## Workflow for cetacean model prep work

For a new modeling project:

1. Use murSST_download_WEAR.R to download necessary murSST nc files. This script downloads and stores each day in a separate nc file
2. Extract predictor variable values for the user-specified segments
    + Use CCSRA_2D_Seg_WEAR.R and murSST_Seg_WEAR.R to extract data for segment midpoints
    + Use ETOPO_Grid+Seg_WEAR.R to extract bathymetry data for segment midpoints
    + Use Combine_seg.R to combine CCSRA, murSST, and ETOPO data into a single CSV file
3. Use gridmaker.R to create grid that will be predicted on (and thus also the grid for which to extract data)
4. Extract predictor variable values for the grid created by gridmaker, and times specified by the user
    + Use CCSRA_2D_Grid_WEAR.R and murSST_Grid_WEAR.R to extract data for the grid
    + Use ETOPO_Grid+Seg_WEAR.R to extract bathymetry data for grid centroids (output is one CSV file)
    + Use Combine_grid.R to combine CCSRA, murSST, and ETOPO data into a single CSV file for each day

## 'Whalepreds_' files

* Whalepreds_aggregate.R: Aggregate predictions at desired time interval (e.g. biweekly or monthly), or using user-defined data intervals
* Whalepreds_evaluate.R: Given predictions and validation data, use eSDM::evaluation_metrics to calculate AUC and TSS. Can also plot predictions and associated validation data

Associated '_test' files contain code that tests the functions, while '_demo' files provide a example code using the function

## Other files

* Concatenate_nc_files.r: Experimental code for concatenating nc files; not currently working
* Funcs_WEAR.R: Helper processing functions for modeling prep work; sourced within some prep files
* Funcs_plot_WEAR.R: Helper plotting functions for modeling prep work; sourced within some prep files
* GridPlots.R: Function for plotting spatial data (i.e. `sf` objects), specifically predictions
* GridPlots_vars.R: Functions for reading in and plotting input variables
* User_script_local.R: Script for determining whom is running the code (user info used to set appropriate file paths); sourced in prep files

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
