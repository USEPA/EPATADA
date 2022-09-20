test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("TADAdataRetrieval ", {
  tada1 <- TADAdataRetrieval(siteid = c("USGS-054064785",
                                        "USGS-430305089260600"),
                             characteristicName = "Phosphorus")
  
})