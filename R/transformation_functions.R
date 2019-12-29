
# this file has feature transformations
#
# Example:
# getCategTransform returns list (column, function), where column is the name
# of the output column for the transformation and the function transforms
# a data.table to feature vector.

library(data.table)

getCategTransform <- function(col, fit_dt, threshold=0, default_value="other") {
  # get function that encodes away less common values in a feature
  # saves you from those errors on missing factor levels in test set
  #
  # Args:
  #   col: transformation function will transform this column
  #   fit_dt: use data in fit_dt to calculate less common feature values
  #   threshold: retain only those values that occur more than this many times in fit_dt
  #
  # Returns:
  #   list(col, fun) where fun is the transformation function

  # count how often each value in x occurs
  retain_values <- (
    fit_dt
    [, .(val=fit_dt[[col]])]
    [, .(n=.N), by=.(val)]
    [n > threshold,]
    $val
  )
  categTransform <- function(input_dt) {
    # return input_dt[[col]] but less common values set to default_value
    #
    # Args:
    #   input_dt: data.table with column <col>
    #
    # Returns:
    #   vector that has transformed input feature
    x <- input_dt[[col]]
    x[!(x %in% retain_values)] <- default_value
    # TODO: handle unseen column values
    return(x)
  }
  rm(fit_dt)
  return(list(col=col, fun=categTransform))
}

getDiscretizeTransform <- function(col, fit_dt, n=10) {
  # get function that discretizes numerical feature to factor
  #
  # Args:
  #   col: transformation function will transform this column
  #   fit_dt: use data in fit_dt to calculate discretization boundaries
  #   n: calculate this many discretization boundary points
  #
  # Returns:
  #   list(col, fun) where fun is the transformation function

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
    x <- input_dt[[col]]
    y <- cut(x, breaks=breaks, labels=labels, right=FALSE)
    # add additional "missing" that is not NA because models fail on NAs
    y <- factor(y, levels=c("Unknown", levels(y)))
    y[is.na(y)] <- "Unknown"
    y
  }

  return( list(col=col, fun=discretizeTransform) )
}

processNAColumn <- function(input_dt, col) {
  # set NA values in column col to 0 and add new column that is 0/1 NA indicator
  #
  # Args:
  #   input_dt: data.table with column <col>
  #   col: column name in input_dt, is of numeric type
  #
  # Returns:
  #   TRUE, modifies input_dt by reference
  col_na <- paste0(col, "_na")
  # create new column
  input_dt[, (col_na) := 0 ]
  input_dt[is.na(input_dt[[col]]), (col_na) := 1 ]
  input_dt[is.na(input_dt[[col]]), (col) := 0 ]
  return(TRUE)
}

getAveragingTransform <- function(
  group_cols, avg_col, out_col, fit_dt, n_threshold=500) {
  # get function that calculates new feature: average of <avg_col> by group
  #
  # Args:
  #   group_cols: vector of column names to group by the average calculation
  #   avg_col: column to average
  #   output_col: column name to output the new feature
  #   fit_dt: data.table with columns in group_cols and avg_col
  #
  # Returns:
  #   list(col, fun) where fun is the transformation function

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
  averagingTransform <- function(input_dt) {
    # merge each row of input_dt to correct average value
    averaging_dt[input_dt, on=group_cols]$avg
  }
  return(list(col=out_col, fun=averagingTransform))
}
