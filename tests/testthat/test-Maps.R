# basic  map tests - checking to see if the functions return leaflet maps
test_that("TADA_ViewATTAINS return leaflet map", {
  testmap <- TADA_ViewATTAINS(Data_MT_AUMLRef)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})

test_that("TADA_OveriewMap return leaflet map", {
  testmap <- TADA_OverviewMap(Data_Nutrients_UT)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})

test_that("TADA_FlaggedSitesMap return leaflet map", {
  testmap <- TADA_FlaggedSitesMap(Data_Nutrients_UT)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})

test_that("TADA_NearbySitesMap return leaflet map", {
  testmap <- TADA_NearbySitesMap(Data_MT_MissoulaCounty)

  expect_true(all(c("leaflet", "htmlwidget") %in% class(testmap)))
})
