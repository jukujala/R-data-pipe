
#library(testthat)
#source("../utils.r", chdir=TRUE)
#source("transformation_functions.R")

test_that("reading test data works", {
  dt <- fread("test_data.csv")
  expect_that(nrow(dt) > 10, is_true())
})
 
#getCategTransform <- function(col, fit_dt, threshold=0,) 

#test_that("JSONToDT works", {
#
#  x <- c(
#    "{\"x\": 1, \"y\": \"asdf\"}",
#    "{\"x\": 2, \"y\": \"xxx\"}",
#    "{\"x\": 3, \"y\": \"yyy\"}"
#  )
#  y_dt <- data.table(x=c(1,2,3), y=c("asdf", "xxx", "yyy"))
#
#  y <- JSONToDT(x)
#  expect_that(all.equal(y, y_dt), is_true())
#})

#test_that("trim works", {
#  x <- c(" foo", "  bar  ")
#  y <- trim(x)
#  expect_that(all.equal(y, c("foo", "bar")))
#})

#test_that("mapValue works", {
#  dt <- data.table(
#    country=c("Finland", "Sweden", "China")
#  )
#  dt_map <- data.table(
#    country=c("Finland", "China"),
#    area=c("Europe", "Asia")
#  )
#
#  dt[, area := mapValue(country, dt_map, area) ]
#
#  expect_that(all.equal(dt$area, c("Europe", NA, "Asia")))
#})

