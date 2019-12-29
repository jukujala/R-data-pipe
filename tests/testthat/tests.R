
test_that("reading test data works", {
  dt <- fread("test_data.csv")
  expect_that(nrow(dt) > 10, is_true())
})

#dt <- fread("../tests/testthat/test_data.csv")

test_that("reading test data works", {
  dt <- fread("test_data.csv")
  transformCountries <- getCategTransform("countries", dt)
  dt[, transformed_countries := transformCountries$fun(dt)]
  assert_this <- dt[, all(transformed_countries == countries)]
  expect_that(assert_this, is_true())
})

test_that("getCategTransform preserves train data", {
  dt <- fread("test_data.csv")
  transformCountries <- getCategTransform("countries", dt)
  test_dt = copy(dt)
  test_dt[, countries := "new country not in train"]
  test_dt[, transformed_countries := transformCountries$fun(test_dt)]
  assert_this <- test_dt[, all(transformed_countries == "other")]
  expect_that(assert_this, is_true())
})

test_that("getCategTransform threshold argument works", {
  dt <- fread("test_data.csv")
  transformCountries <- getCategTransform("countries", dt, threshold=1000)
  dt[, transformed_countries := transformCountries$fun(dt)]
  assert_this <- dt[, all(transformed_countries == "other")]
  expect_that(assert_this, is_true())
})

test_that("getDiscretizeTransform works", {
  dt <- fread("test_data.csv")
  discTransform <- getDiscretizeTransform("sales", dt, n=10)
  dt[, disc_sales := discTransform$fun(dt)]
  assert_this <- (length(unique(dt$disc_sales)) == 10)
  expect_that(assert_this, is_true())
})

test_that("processNAColumn works", {
  dt <- fread("test_data.csv")
  dt_na = copy(dt)
  dt_na[1, "sales"] = NA
  processNAColumn(dt_na, "sales")
  nrow(dt_na) == nrow(dt)
  expect_that(!is.na(dt_na[1, "sales"]), is_true())
  expect_that(dt_na[1, "sales"] == 0, is_true())
  expect_that(dt_na[1, "sales_na"] == 1, is_true())
  expect_that(dt_na[2, "sales_na"] == 0, is_true())
})

test_that("getAveragingTransform works", {
  dt <- fread("test_data.csv")
  countryStatsTransform <- getAveragingTransform(
    c("countries"), "sales", "country_sales", 
    dt, 
    n_threshold=1
  )
  dt[, country_sales := countryStatsTransform$fun(dt)]
  validation_dt <- dt[, 
    .(
      country_sales2=mean(sales), 
      country_sales=max(country_sales)
    ), 
    by=list(countries)
  ]
  assert_this <- validation_dt[, all(abs(country_sales -country_sales2)< 0.01)]
  expect_that(assert_this, is_true())
})


