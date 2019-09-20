# RAIMBOW pre-processing function: whale predictions
# Aggregate raw (bidaily) predictions to desired level
#   Supported levels: biweekly, monthly, days (e.g. 7 days)
# If we anticipate any predictions coming in as shapefiles, 
#   we could change to S3 method

# By Sam Woodman, May 2019


###############################################################################
# Helper functions

### Based on esdm_weighted_var_amv
raimbow_se <- function(x) {
  ### Inputs:
  # x: numeric vector of values that were used to calculate mean
  
  x.mean <- mean(x, na.rm = TRUE)
  
  if (sum(!is.na(x)) == 0) {
    NA
  } else {
    sqrt(sum((x - x.mean)^2, na.rm = TRUE) / length(x))
  }
}

# ### For use within pmap() call
# raimbow_se_pmap <- function(...) {
#   ### Inputs:
#   # input from pmpa
# 
#   x <- unlist(list(...))
#   x.mean <- mean(x, na.rm = TRUE)
#   
#   if (sum(!is.na(x)) == 0) {
#     NA
#   } else {
#     sqrt(sum((x - x.mean)^2, na.rm = TRUE) / (length(x) - 1))
#   }
# }


###############################################################################
raimbow_pre_whalepreds_aggregate <- function(
  x, x.cols, x.col.idx, aggr.level = NULL, range.dates = NULL, se.calc = FALSE) 
{
  ### Inputs:
  # x: data frame with predictions
  # x.cols: column names or indices that contain prediction values
  #   Data column names of x must contain year/month/day as: 
  #   'yyyy.mm.dd' or 'yyyy_mm_dd'.
  #   Column not specified will be passed to output data frame
  # x.col.idx: indices of characters in column names that specfiy the date;
  #   must be either of length 8 or 10 
  # aggr.level: aggregation level; either "biweekly" , "monthly", or "#day" 
  #   (e.g. "5day"). Ignored if range.dates is not NULL
  # range.dates: object of class Date; custom date range into which to 
  #   aggregate predictions. Interval evaluation is [ , )
  #   If NULL, aggr.level must be specified
  # se.calc: logical indicating whether SE values should be calculated from
  #   aggregated values
  
  ### Output: data frame of aggregated/summarized whale predictions.
  
  #----------------------------------------------------------------------------
  ### Process and check inputs
  stopifnot(
    inherits(x, "data.frame"), 
    inherits(x.cols, c("character", "integer", "numeric")), 
    inherits(x.col.idx, c("integer", "numeric")), 
    inherits(se.calc, "logical")
  )
  
  if (!inherits(x.cols, "character")) x.cols <- names(x)[x.cols]
  stopifnot(all(x.cols %in% names(x)))
  
  #----------------------------------------------------------------------------
  # Prep
  
  ### Prep: extract data columns
  x.other <- x %>% select(-!!x.cols)
  x.data <- x %>% 
    select(!!x.cols) %>% 
    set_names(substr(names(.), min(x.col.idx), max(x.col.idx)))
  
  ### Prep: get dates from column names
  # Are dates separated by '.' or '_'?
  if (all(grepl("[.]", names(x.data)))) {
    tmp <- "."
  } else if (all(grepl("_", names(x.data)))) {
    tmp <- "_"
  } else {
    stop("The dates in the column names must all be separated by either '.' or '_'")
  }
  
  # Are dates 8 digit or 10 digit?
  if (length(x.col.idx) == 8) {
    cols.dates <- as.Date(names(x.data), format = paste0("%y", tmp, "%m", tmp, "%d"))
    
  } else if (length(x.col.idx) == 10) {
    cols.dates <- as.Date(names(x.data), format = paste0("%Y", tmp, "%m", tmp, "%d"))
    
  } else {
    stop("x.col.idx must be of length 8 (e.g. \"05_01_01\" or \"05.01.01\") ", 
         "or 10 (e.g. \"2005_01_01\" or \"2005.01.01\")")
  }
  rm(tmp)
  
  ### Prep: generate dates to define specfied intervals
  if (is.null(range.dates)) {
    # Date range NOT is manually specified
    stopifnot(
      aggr.level %in% c("biweekly", "monthly") | grepl("day", aggr.level)
    )
    
    if (aggr.level == "biweekly") {
      range.dates <- seq(from = min(cols.dates), to = max(cols.dates), by = "2 week")
      range.dates <- c(range.dates, max(range.dates) %m+% weeks(2))
      
    } else if (aggr.level == "monthly") {
      range.dates <- seq(from = min(cols.dates), to = max(cols.dates), by = "1 month")
      range.dates <- c(range.dates, max(range.dates) %m+% months(1))
      
    } else if (grepl("day", aggr.level)) {
      day.int <- as.numeric(strsplit(aggr.level, "day")[[1]][1])
      if (!inherits(day.int, "numeric")) {
        stop("Please ensure aggr.level is either \"monthly\", \"biweekly\", ", 
             "or in the form '#day', e.g. \"10day\"")
      }
      range.dates <- seq(from = min(cols.dates), to = max(cols.dates), by = paste(day.int, "day"))
      range.dates <- c(range.dates, max(range.dates) %m+% days(day.int))
      
    } else {
      stop("Error in aggr.level argument")
    }
    
  } else {
    # Date range is manually specified
    stopifnot(inherits(range.dates, "Date"))
    
    date.between <- between(cols.dates, min(range.dates), max(range.dates) - 1)
    aggr.level <- "user"
    
    if (!any(date.between)) {
      stop("None of the column dates fall within an interval defined by range.dates")
    } else if (!all(date.between)) {
      warning("Not all column dates fall within an interval defined by range.dates")
    }
  }
  
  #----------------------------------------------------------------------------
  ### Map columns to date intervals
  x.key.summ <- data.frame(x_cols = x.cols, stringsAsFactors = FALSE) %>% 
    mutate(x_data_cols = names(x.data),
           cols_interval = findInterval(
             cols.dates, range.dates, left.open = FALSE, rightmost.closed = FALSE
           )) %>% 
    group_by(cols_interval) %>% 
    summarise(x_data_cols_list = list(x_data_cols)) %>%
    filter(between(cols_interval, 1, length(range.dates) - 1)) %>% 
    mutate(range_beg = gsub("-", "_", range.dates[cols_interval])) %>% 
    select(cols_interval, range_beg, x_data_cols_list)
  
  
  if (nrow(x.key.summ) < (length(range.dates) - 1)) {
    d <- data.frame(begdate = head(range.dates, -1), 
                    enddate = tail(range.dates - 1, -1)) %>% 
      mutate(coldate_count = pmap_dbl(
        list(begdate, enddate), function(i, j, k) sum(between(k, i, j)), k = cols.dates)
      )
    
    warning("The intervals beginning with the following dates ", 
            "do not contain any column dates, ", 
            "and thus will not be included in the final output:\n", 
            paste(d$begdate[d$coldate_count == 0], collapse = ", "))
  }
  
  #----------------------------------------------------------------------------
  # Aggregate/average columns and return output
  
  ### Calculate mean prediction value
  data.summ <- data.frame(apply(x.key.summ, 1, function(i, j) {
    rowMeans(j[,  i[["x_data_cols_list"]]], na.rm = TRUE)
  }, j = x.data)) %>%
    set_names(paste("Avg", aggr.level, x.key.summ$range_beg, sep = "_"))
  
  ### Calculate SE of prediction values
  if (se.calc) {
    data.summ.se <- data.frame(apply(x.key.summ, 1, function(i, j) {
      apply(j[,  i[["x_data_cols_list"]]], 1, raimbow_se)
    }, j = x.data)) %>% 
      set_names(paste("SE", aggr.level, x.key.summ$range_beg, sep = "_"))
    
  } else {
    data.summ.se <- NULL
  }
  
  ### Return
  bind_cols(x.other, data.summ, data.summ.se)
}

###############################################################################
