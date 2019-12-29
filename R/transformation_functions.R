
# this file has feature transformations
#
# Example:
# getCategTransform returns list (column, function), where column is the name
# of the output column for the transformation and the function transforms
# a data.table to feature vector.

library(data.table)

#' get function that encodes away less common values in a feature
#' saves you from those errors on missing factor levels in test set
#' 
#' @param col: transformation function will transform this column
#' @param fit_dt: use data in fit_dt to calculate less common feature values
#' @param threshold: retain only those values that occur more than this many times in fit_dt
#' 
#' @return list(col, fun) where fun is the transformation function
#' @export
getCategTransform <- function(col, fit_dt, threshold=0, default_value="other") {

  # count how often each value in x occurs
  retain_values <- (
    fit_dt
    [, .(val=fit_dt[[col]])]
    [, .(n=.N), by=.(val)]
    [n > threshold,]
    $val
  )
  # return input_dt[[col]] but less common values set to default_value
  # 
  # @param input_dt: data.table with column <col>
  # 
  # @return vector that has transformed input feature
  categTransform <- function(input_dt) {
    x <- input_dt[[col]]
    x[!(x %in% retain_values)] <- default_value
    # TODO: handle unseen column values
    return(x)
  }
  rm(fit_dt)
  return(list(col=col, fun=categTransform))
}

#' get function that discretizes numerical feature to factor
#' 
#' @param col: transformation function will transform this column
#' @param fit_dt: use data in fit_dt to calculate discretization boundaries
#' @param n: calculate this many discretization boundary points
#' 
#' @return list(col, fun) where fun is the transformation function
#' @export
getDiscretizeTransform <- function(col, fit_dt, n=10) {

  # get discretization boundaries from fit_dt
  suppressWarnings(
  col_disc <- discretize(
    fit_dt[[col]],
    method = "frequency", #frequency implementation is broken
    breaks = min(n, countUniq(fit_dt[[col]])-1),
    right = FALSE,
    infinity=TRUE
  )
  )
  breaks <- attr(col_disc, "discretized:breaks")
  labels <- attr(col_disc, "levels")
  rm(col_disc)
  rm(fit_dt)
  discretizeTransform <- function(input_dt) {
    y <- cut(x, breaks=breaks, labels=labels, right=FALSE)
    # add additional "missing" that is not NA because models fail on NAs
    y <- factor(y, levels=c("Unknown", levels(y)))
    y[is.na(y)] <- "Unknown"
    y
  }

  return( list(col=col, fun=discretizeTransform) )
}

#' set NA values in column col to 0 and add new column that is 0/1 NA indicator
#' 
#' @param input_dt: data.table with column <col>
#' @param col: column name in input_dt, is of numeric type
#' 
#' @return TRUE, modifies input_dt by reference
#' @export
processNAColumn <- function(input_dt, col) {
  col_na <- paste0(col, "_na")
  # create new column
  input_dt[, (col_na) := 0 ]
  input_dt[is.na(input_dt[[col]]), (col_na) := 1 ]
  input_dt[is.na(input_dt[[col]]), (col) := 0 ]
  return(TRUE)
}

#' get function that calculates new feature: average of <avg_col> by group
#' 
#' @param group_cols: vector of column names to group by the average calculation
#' @param avg_col: column to average
#' @param output_col: column name to output the new feature
#' @param fit_dt: data.table with columns in group_cols and avg_col
#' 
#' @return list(col, fun) where fun is the transformation function
#' @export
getAveragingTransform <- function(
  group_cols, avg_col, out_col, fit_dt, n_threshold=500) {

  # calculate average for each group
  averaging_dt <- (
    fit_dt[,
      .(
        n=.N,
        avg = mean(.SD[[avg_col]])
      ),
      keyby=group_cols
    ]
    [n > n_threshold, ]
    [, n := NULL ]
  )
  rm(fit_dt)
  # merge each row of input_dt to correct average value
  averagingTransform <- function(input_dt) {
    averaging_dt[input_dt, on=group_cols]$avg
  }
  return(list(col=out_col, fun=averagingTransform))
}
