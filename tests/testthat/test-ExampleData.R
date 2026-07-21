test_that("package data files do not contain blank or NA CharacteristicName values", {
  data_objs <- data(package = "EPATADA")$results[, "Item"]

  for (obj in data_objs) {
    # Load the data object into the test environment
    utils::data(list = obj, package = "EPATADA", envir = environment())

    dat <- get(obj, envir = environment())

    if (is.data.frame(dat) && "CharacteristicName" %in% names(dat)) {
      bad <- is.na(dat$CharacteristicName) |
        trimws(dat$CharacteristicName) == ""

      expect_false(
        any(bad),
        info = paste0(
          obj,
          " contains NA/blank CharacteristicName values at rows: ",
          paste(which(bad), collapse = ", ")
        )
      )
    }
  }
})
