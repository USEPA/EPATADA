test_that(".TADA_UpdateRefFiles orchestrates all update steps in order", {
  calls <- character()

  fake_fun <- function(name) {
    force(name)
    function(...) {
      calls <<- c(calls, name)
      invisible(NULL)
    }
  }

  testthat::with_mocked_bindings(
    .TADA_UpdateATTAINSOrgIDsRef = fake_fun("orgids"),
    .TADA_UpdateATTAINSParamUseOrgRef = fake_fun("paramuse"),
    .TADA_UpdateWQXCharValRef = fake_fun("wqxcharval"),
    .TADA_UpdateMeasureUnitRef = fake_fun("measureunit"),
    .TADA_UpdateDetCondRef = fake_fun("detcond"),
    .TADA_UpdateDetLimitRef = fake_fun("detlimit"),
    .TADA_UpdateActivityTypeRef = fake_fun("activitytype"),
    .TADA_UpdateCharacteristicRef = fake_fun("characteristic"),
    .TADA_UpdateMeasureQualifierCodeRef = fake_fun("measurequal"),
    .TADA_UpdateMonLocTypeRef = fake_fun("monloctype"),
    .TADA_UpdateWQPOrganizationRef = fake_fun("wqporg"),
    .TADA_UpdateWQXCharAliasRef = fake_fun("wqxalias"),
    .TADA_CST_UpdateWorkbook = fake_fun("cst"),
    TADA_UpdateTribalLayers = fake_fun("tribal"),
    .TADA_UpdateTADACharAliasRef = fake_fun("tadacharalias"),
    .TADA_UpdateTADAUsesAliasRef = fake_fun("tadausesalias"),
    .package = "EPATADA",
    {
      expect_silent(.TADA_UpdateRefFiles())
    }
  )

  expect_identical(
    calls,
    c(
      "orgids",
      "paramuse",
      "wqxcharval",
      "measureunit",
      "detcond",
      "detlimit",
      "activitytype",
      "characteristic",
      "measurequal",
      "monloctype",
      "wqporg",
      "wqxalias",
      "cst",
      "tribal",
      "tadacharalias",
      "tadausesalias"
    )
  )
})

test_that(".TADA_UpdateRefFiles reports ATTAINS errors and stops that block", {
  testthat::with_mocked_bindings(
    .TADA_UpdateATTAINSOrgIDsRef = function(...) stop("org id failure"),
    .TADA_UpdateATTAINSParamUseOrgRef = function(...) {
      testthat::fail(
        "should not be called after failure in same tryCatch block"
      )
    },
    .TADA_UpdateWQXCharValRef = function(...) invisible(NULL),
    .TADA_UpdateMeasureUnitRef = function(...) invisible(NULL),
    .TADA_UpdateDetCondRef = function(...) invisible(NULL),
    .TADA_UpdateDetLimitRef = function(...) invisible(NULL),
    .TADA_UpdateActivityTypeRef = function(...) invisible(NULL),
    .TADA_UpdateCharacteristicRef = function(...) invisible(NULL),
    .TADA_UpdateMeasureQualifierCodeRef = function(...) invisible(NULL),
    .TADA_UpdateMonLocTypeRef = function(...) invisible(NULL),
    .TADA_UpdateWQPOrganizationRef = function(...) invisible(NULL),
    .TADA_UpdateWQXCharAliasRef = function(...) invisible(NULL),
    .TADA_CST_UpdateWorkbook = function(...) invisible(NULL),
    TADA_UpdateTribalLayers = function(...) invisible(NULL),
    .TADA_UpdateTADACharAliasRef = function(...) invisible(NULL),
    .TADA_UpdateTADAUsesAliasRef = function(...) invisible(NULL),
    .package = "EPATADA",
    {
      expect_message(
        .TADA_UpdateRefFiles(),
        "Error updating ATTAINS reference tables: org id failure"
      )
    }
  )
})

test_that(".TADA_UpdateRefFiles reports WQPWQX errors independently", {
  calls <- character()

  testthat::with_mocked_bindings(
    .TADA_UpdateATTAINSOrgIDsRef = function(...) {
      calls <<- c(calls, "orgids")
      invisible(NULL)
    },
    .TADA_UpdateATTAINSParamUseOrgRef = function(...) {
      calls <<- c(calls, "paramuse")
      invisible(NULL)
    },
    .TADA_UpdateWQXCharValRef = function(...) {
      calls <<- c(calls, "wqxcharval")
      stop("wqx failure")
    },
    .TADA_UpdateMeasureUnitRef = function(...) {
      testthat::fail(
        "should not be called after failure in same tryCatch block"
      )
    },
    .TADA_UpdateDetCondRef = function(...) invisible(NULL),
    .TADA_UpdateDetLimitRef = function(...) invisible(NULL),
    .TADA_UpdateActivityTypeRef = function(...) invisible(NULL),
    .TADA_UpdateCharacteristicRef = function(...) invisible(NULL),
    .TADA_UpdateMeasureQualifierCodeRef = function(...) invisible(NULL),
    .TADA_UpdateMonLocTypeRef = function(...) invisible(NULL),
    .TADA_UpdateWQPOrganizationRef = function(...) invisible(NULL),
    .TADA_UpdateWQXCharAliasRef = function(...) invisible(NULL),
    .TADA_CST_UpdateWorkbook = function(...) invisible(NULL),
    TADA_UpdateTribalLayers = function(...) invisible(NULL),
    .TADA_UpdateTADACharAliasRef = function(...) invisible(NULL),
    .TADA_UpdateTADAUsesAliasRef = function(...) invisible(NULL),
    .package = "EPATADA",
    {
      expect_message(
        .TADA_UpdateRefFiles(),
        "Error updating WQPWQX reference tables: wqx failure"
      )
    }
  )

  expect_identical(calls, c("orgids", "paramuse", "wqxcharval"))
})
