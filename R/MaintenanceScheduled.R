#' Update All TADA Reference Files (Internal)
#'
#' This internal function updates all TADA reference files by calling a series of update functions.
#' It is used to ensure that all reference data is current and accurate across various
#' datasets and geospatial layers.
#'
#' @details
#' The function sequentially calls several internal functions that update different sets
#' of reference data. Some updates may take longer than others, particularly the
#' `TADA_UpdateATTAINSParamUseOrgRef()` function.
#'
#' The specific reference files updated by this function include:
#' \itemize{
#'   \item ATTAINS organization IDs
#'   \item ATTAINS parameter use organization
#'   \item ATTAINS parameter WQP characteristics
#'   \item WQX characteristic values
#'   \item Measurement units
#'   \item Detection conditions
#'   \item Detection limits
#'   \item Activity types
#'   \item Characteristics
#'   \item Measurement qualifier codes
#'   \item Monitoring location types
#'   \item WQP organization providers
#'   \item EPA CST references
#'   \item Tribal geospatial layers
#' }
#'
#' @return
#' This function does not return any value. It performs updates as a side effect.
#'
#' @examples
#' \dontrun{
#' # Internal function to update all reference files
#' .TADA_UpdateRefFiles()
#' }
.TADA_UpdateRefFiles <- function() {
  # Update All TADA Reference Files

  # ATTAINSRefTables.R
  tryCatch(
    {
      TADA_UpdateATTAINSOrgIDsRef()
      TADA_UpdateATTAINSParamUseOrgRef() # takes a long time
      TADA_UpdateATTAINSParamToWQPCharRef()
    },
    error = function(e) {
      message("Error updating ATTAINS reference tables: ", e$message)
    }
  )

  # WQPWQXRefTables.R
  tryCatch(
    {
      TADA_UpdateWQXCharValRef()
      TADA_UpdateMeasureUnitRef()
      TADA_UpdateDetCondRef()
      TADA_UpdateDetLimitRef()
      TADA_UpdateActivityTypeRef()
      .TADA_UpdateCharacteristicRef()
      TADA_UpdateMeasureQualifierCodeRef()
      TADA_UpdateMonLocTypeRef()
      TADA_UpdateWQPOrgProviderRef()
    },
    error = function(e) {
      message("Error updating WQPWQX reference tables: ", e$message)
    }
  )

  # CriteriaRefTables.R
  tryCatch(
    {
      TADA_UpdateCriteriaSearchToolRef()
    },
    error = function(e) {
      message("Error updating EPA CST reference: ", e$message)
    }
  )
  # Legend for CriteriaRefTables.R
  tryCatch(
    {
      TADA_UpdateLegendCSTRef()
    },
    error = function(e) {
      message("Error updating Legend for EPA CST reference: ", e$message)
    }
  )
  # Sources for CriteriaRefTables.R
  tryCatch(
    {
      TADA_UpdateSourcesCSTRef()
    },
    error = function(e) {
      message("Error updating Sources for EPA CST reference: ", e$message)
    }
  )

  # TADAGeospatialRefLayers.R
  tryCatch(
    {
      TADA_UpdateTribalLayers()
    },
    error = function(e) {
      message("Error updating tribal geospatial layers: ", e$message)
    }
  )
}

#' Update Example Data for EPATADA Package (Internal)
#'
#' This internal function retrieves, processes, and saves various datasets for the EPATADA package.
#'
#' @details
#' The function fetches data from specified sources based on given parameters, processes it using
#' several functions from the EPATADA package, and saves the processed datasets as `.rda` files
#' for use within the package. The datasets include nutrient data for Utah, tribal data over five years,
#' harmonized datasets, and more.
#'
#' @return None. The function saves processed data files in the package's `data/` directory.
#'
#' @examples
#' \dontrun{
#' .TADA_UpdateExampleData()
#' }
.TADA_UpdateExampleData <- function() {
  tryCatch(
    {
      # =======================================
      # Generate Data_Nutrients_UT
      # =======================================
      Data_Nutrients_UT <- TADA_DataRetrieval(
        statecode = "UT",
        characteristicName = c("Ammonia", "Nitrate", "Nitrogen"),
        startDate = "2020-10-01",
        endDate = "2022-09-30",
        ask = FALSE
      )
      message("Data_Nutrients_UT")
      message(dim(Data_Nutrients_UT))
      usethis::use_data(
        Data_Nutrients_UT,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      rm(Data_Nutrients_UT)

      # =======================================
      # Generate Data_6Tribes_5y
      # =======================================
      Data_6Tribes_5y <- TADA_DataRetrieval(
        organization = c(
          "REDLAKE_WQX",
          "SFNOES_WQX",
          "PUEBLO_POJOAQUE",
          "FONDULAC_WQX",
          "PUEBLOOFTESUQUE",
          "CNENVSER"
        ),
        startDate = "2018-01-01",
        endDate = "2023-01-01",
        ask = FALSE
      )
      message("Data_6Tribes_5y")
      message(dim(Data_6Tribes_5y))
      usethis::use_data(
        Data_6Tribes_5y,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )

      # =======================================
      # Harmonize Data_6Tribes_5y
      # =======================================
      harmonized_data <- subset(
        Data_6Tribes_5y,
        Data_6Tribes_5y$TADA.ActivityMediaName %in% c("WATER")
      )
      harmonized_data <- TADA_RunKeyFlagFunctions(
        harmonized_data,
        clean = "both"
      )
      rm(Data_6Tribes_5y)

      harmonized_data <- harmonized_data |>
        TADA_FlagMethod(clean = TRUE) |>
        TADA_FlagAboveThreshold(clean = TRUE) |>
        TADA_FlagBelowThreshold(clean = TRUE) |>
        TADA_FindPotentialDuplicatesMultipleOrgs(dist_buffer = 100) |>
        TADA_FindPotentialDuplicatesSingleOrg() |>
        dplyr::filter(!(MeasureQualifierCode %in% c("D", "H", "ICA", "*"))) |>
        TADA_SimpleCensoredMethods(
          nd_method = "multiplier",
          nd_multiplier = 0.5,
          od_method = "as-is",
          od_multiplier = "null"
        ) |>
        dplyr::filter(
          TADA.ResultMeasureValueDataTypes.Flag != "Text" &
            TADA.ResultMeasureValueDataTypes.Flag != "NA - Not Available" &
            !is.na(TADA.ResultMeasureValue)
        )

      Data_6Tribes_5y_Harmonized <- TADA_HarmonizeSynonyms(harmonized_data)
      message("Data_6Tribes_5y_Harmonized")
      message(dim(Data_6Tribes_5y_Harmonized))
      usethis::use_data(
        Data_6Tribes_5y_Harmonized,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      rm(Data_6Tribes_5y_Harmonized, harmonized_data)

      # =======================================
      # Generate Data_R5_TADAPackageDemo
      # =======================================
      Data_R5_TADAPackageDemo <- TADA_DataRetrieval(
        startDate = "2019-05-01",
        endDate = "2019-05-07",
        statecode = c("IL", "IN", "MI", "MN", "OH", "WI"),
        ask = FALSE
      )
      message("Data_R5_TADAPackageDemo")
      message(dim(Data_R5_TADAPackageDemo))
      usethis::use_data(
        Data_R5_TADAPackageDemo,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      rm(Data_R5_TADAPackageDemo)

      # =======================================
      # Module 3 Vignette Example Data
      # =======================================
      Data_WV <- TADA_DataRetrieval(
        startDate = "2020-03-14",
        huc = "02070004",
        applyautoclean = FALSE,
        ask = FALSE
      )
      # Filter for surface water data (optional)
      Data_WV <- TADA_MediaFilter(
        Data_WV,
        clean = TRUE,
        surface_water = FALSE,
        ground_water = TRUE,
        sediment = TRUE,
        other = TRUE
      )
      # Remove single organization duplicates (required)
      Data_WV <- TADA_FindPotentialDuplicatesSingleOrg(Data_WV)
      Data_WV <- dplyr::filter(Data_WV, TADA.SingleOrgDup.Flag == "Unique")
      # Perform autocleaning (required)
      Data_WV <- TADA_AutoClean(Data_WV)
      # Handle censored results (required)
      Data_WV <- TADA_SimpleCensoredMethods(
        Data_WV,
        nd_method = "multiplier",
        nd_multiplier = 0.5,
        od_method = "as-is",
        od_multiplier = "null"
      )
      # Remove multiple organization duplicates (optional)
      Data_WV <- TADA_FindPotentialDuplicatesMultipleOrgs(Data_WV)
      Data_WV <- dplyr::filter(Data_WV, TADA.ResultSelectedMultipleOrgs == "Y")
      # Convert special characters
      Data_WV <- TADA_ConvertSpecialChars(
        Data_WV,
        col = "TADA.ResultMeasureValue",
        clean = TRUE
      )
      # Remove results with quality control issues (required)
      Data_WV <- TADA_RunKeyFlagFunctions(Data_WV, clean = TRUE)
      # Flag above and below threshold (do not remove)
      Data_WV <- TADA_FlagAboveThreshold(
        Data_WV,
        clean = FALSE,
        flaggedonly = FALSE
      )
      Data_WV <- TADA_FlagBelowThreshold(
        Data_WV,
        clean = FALSE,
        flaggedonly = FALSE
      )
      # Harmonize synonyms
      Data_WV <- TADA_HarmonizeSynonyms(Data_WV)
      # Save example data
      Data_HUC8_02070004_Mod1Output <- Data_WV
      message("Data_HUC8_02070004_Mod1Output")
      message(dim(Data_HUC8_02070004_Mod1Output))
      usethis::use_data(
        Data_HUC8_02070004_Mod1Output,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      rm(Data_HUC8_02070004_Mod1Output, Data_WV)

      # =======================================
      # Generate Data_MT_MissoulaCounty
      # =======================================
      Data_MT_MissoulaCounty <- TADA_DataRetrieval(
        startDate = "2020-01-01",
        endDate = "2022-12-31",
        statecode = "MT",
        characteristicName = c("Escherichia", "Escherichia coli", "pH"),
        countycode = "Missoula County",
        ask = FALSE
      ) |>
        TADA_RunKeyFlagFunctions() |>
        TADA_SimpleCensoredMethods() |>
        TADA_HarmonizeSynonyms()

      message("Data_MT_MissoulaCounty")
      message(dim(Data_MT_MissoulaCounty))
      usethis::use_data(
        Data_MT_MissoulaCounty,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )

      # =======================================
      # Generate Data_MT_AUMLRef
      # =======================================
      # Retrieve and clean crosswalk from ATTAINS
      attains.existing.MT <- TADA_GetATTAINSAUMLCrosswalk(org_id = "MTDEQ")
      clean.existing.attains.MT <- TADA_UpdateATTAINSAUMLCrosswalk(
        org_id = "MTDEQ"
      )

      # Create a user-supplied crosswalk for demonstration purposes
      user_supplied_cw <- clean.existing.attains.MT |>
        dplyr::select(
          ATTAINS.AssessmentUnitIdentifier,
          ATTAINS.MonitoringLocationIdentifier,
          ATTAINS.WaterType
        ) |>
        dplyr::filter(
          ATTAINS.MonitoringLocationIdentifier %in%
            c(
              "MDEQ_WQ_WQX-C04CKFKR05",
              "MDEQ_WQ_WQX-C04KNDYC01",
              "MDEQ_WQ_WQX-C04KNDYC02",
              "MDEQ_WQ_WQX-C04KNDYC04",
              "MDEQ_WQ_WQX-C04KNDYC54"
            )
        ) |>
        dplyr::rename(
          AssessmentUnitIdentifier = ATTAINS.AssessmentUnitIdentifier,
          MonitoringLocationIdentifier = ATTAINS.MonitoringLocationIdentifier,
          WaterType = ATTAINS.WaterType
        ) |>
        # Add a new assessment unit for demonstration
        dplyr::bind_rows(c(
          AssessmentUnitIdentifier = "NEW:EX_MDEQ_WQ_WQX",
          MonitoringLocationIdentifier = "NARS_WQX-NWC_MT-10184",
          WaterType = "LAKE, FRESHWATER"
        ))

      MT_AUMLRef <- TADA_CreateAUMLCrosswalk(
        Data_MT_MissoulaCounty,
        au_ref = user_supplied_cw,
        org_id = "MTDEQ",
        fill_ATTAINS_catch = TRUE,
        return_nearest = TRUE,
        batch_upload = TRUE
      )

      Data_MT_AUMLRef <- MT_AUMLRef

      message("Data_MT_AUMLRef")
      message(dim(Data_MT_AUMLRef))
      usethis::use_data(
        Data_MT_AUMLRef,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )

      # =======================================
      # Generate Data_MT_AU_UsesRef
      # =======================================
      Data_MT_AU_UsesRef <- TADA_AssignUsesToAU(
        AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
        org_id = "MTDEQ"
      )

      message("Data_MT_AU_UsesRef")
      message(dim(Data_MT_AU_UsesRef))
      usethis::use_data(
        Data_MT_AU_UsesRef,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )

      # =======================================
      # Generate Data_MT_AU_UsesRef_Water
      # =======================================
      Data_MT_AU_UsesRef_Water <- TADA_AssignUsesToAU(
        waterUseRef = TADA_AssignUsesToWaterType(org_id = "MTDEQ"),
        AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
        org_id = "MTDEQ"
      )

      message("Data_MT_AU_UsesRef_Water")
      message(dim(Data_MT_AU_UsesRef_Water))

      usethis::use_data(
        Data_MT_AU_UsesRef_Water,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      rm(
        attains.existing.MT,
        clean.existing.attains.MT,
        user_supplied_cw,
        MT_AUMLRef
      )
    },
    error = function(e) {
      message("An error occurred during data update: ", e$message)
    }
  )
}
