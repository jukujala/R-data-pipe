# Simple functional R data pipeline

`simple.data.pipe` is a simple functional machine learning data pipeline for R.
It is both a paradigm how to structure your code and a package to support that.
Use `simple.data.pipe` to transform input data before calling 
R functions `train` and `predict`.

## Background

I got frustrated that R did not have good data pipelines similar to Python
[sklearn.Pipeline](https://scikit-learn.org/stable/modules/generated/sklearn.pipeline.Pipeline.html),
but luckily R function closures enable pipelines almost out of the box.
This repository uses package
[`data.table`](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html)
to represent and process data, but you can implement a similar approach in `dplyr`
or with plain `data.frame`.

## Steps to implement a pipeline

Write a function `createTransformFunction` that returns
another function `transformData` which transforms input `data.table` to
output `data.table`.
The function `createTransformFunction` can depend on train data.
R can persist the function `transformData` to a file.

## A short example

    #' create a function to transform input data for model
    #'
    #' @param fit_dt: train data to fit transformation function
    #'
    #' @return transformation function
    createTransformFunction <- function(fit_dt) {
      # fit some transformation
      fitted_transformation <- createFittedTransformation(fit_dt)
      # prevent serialisation of fit_dt
      rm(fit_dt)

      # define a function to transform input data
      transformData <- function(input_dt) {
        # define the label
        input_dt[, label := as.character(y) ]
        # validate binary label
        stopifnot(input_dt[, all(label %in% c("0", "1"))])
        # call a fitted transformation
        input_dt[, transformed_x := fitted_transformation(x) ]
        return( input_dt )
      }
      return(transformData)
    }

    transformData <- createTransformFunction(train_data)
    save(transformData, file="serialised_transformation.Rdata")
    transformed_train_data <- transformData(train_data)
    transformed_test_data <- transformData(test_data)

## Transformations supported by this R package

This R package implements some transformations:

  * `getCategTransform` processes a categorical input column:

    * unseen values map to a separate category
    * values with cardinality less than argument N are pooled together

  * `getDiscretizeTransform` discretizes a continuous input column.
    Boundaries set with k-means.
  * `processNAColumn` transforms a continuous input column to two columns:

    * input column with `NA` values mapped to zero.
    * new column `<input column name>_na` that has value one for `NA` values
      and zero otherwise.

  * `getAveragingTransform` calculates average of a continous input column
    over a given group.
    For example, add new column that has average sales by country.

## Long example of `createTransformFunction`

This example shows how to use the predefined transformations

    createTransformFunction <- function(fit_dt) {
      fit_dt <- copy(fit_dt)

      # create column recency as difference of input columns ts and ts_old
      fit_dt[, recency := as.numeric(difftime(ts, ts_old, units="mins")) ]
      # change data type
      fit_dt[, x := as.character(x) ]

      # define transformations fitted with fit_dt as a list of functions
      column_transforms <- list(
        # map values of x that occur more than 1000 times to separate values
        getCategTransform("x", fit_dt, threshold=1000),
        # discretise recency column to 10 categorical bins with k-means
        getDiscretizeTransform("recency", fit_dt, n=10),
        # add new column avg_sales that has the average sales of the country
        getAveragingTransform(
          group_cols="country",
          avg_col="sales",
          out_col="avg_sales",
          fit_dt
        )
      )
      # each element of column_transforms is a
      # list(col="<output column name>", fun=<transformation function>)

      # remove fit_dt from environment to prevent serialisation of fit_dt
      rm(fit_dt)

      transformData <- function(input_dt) {

        # add click-through-rate based on view and click columns
        input_dt[views > 20, ctr := clicks / views ]

        input_dt[, x := as.character(x) ]
        input_dt[, recency := as.numeric(difftime(ts, ts_old, units="mins")) ]
        # apply transformations defined in outer scope
        for(transform in column_transforms) {
          input_dt[, (transform$col) := transform$fun(.SD) ]
        }
        # handle missing numeric values
        processNAColumn(input_dt, "avg_sales")
        processNAColumn(input_dt, "ctr")
        # create a binary label
        input_dt[, label := ifelse(sales > 1000, "Y", "N")]
        # validate that there are no NA values in output
        stopifnot(all(!is.na(input_dt)))
        return( input_dt )
      }
      return(transformData)
    }

## Files

  * [transformation_functions.R](transformation_functions.R)
    has transformation functions defined in this package.

