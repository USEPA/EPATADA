#' Update All TADA Reference Files (Internal)
#'
#' This internal function updates all TADA reference files by calling a series of update functions.
#' It is used to ensure that all reference data is current and accurate across various
#' datasets and geospatial layers.
#'
#' @details
#' The function sequentially calls several internal functions that update different sets
#' of reference data. Some updates may take longer than others, particularly the
#' `.TADA_UpdateATTAINSParamUseOrgRef()` function.
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
  # Update All Reference Files

  # ATTAINSRefTables.R
  tryCatch(
    {
      .TADA_UpdateATTAINSOrgIDsRef()
      .TADA_UpdateATTAINSParamUseOrgRef() # takes a long time, this is the Assessments Profile (Expert Query National Extract)
    },
    error = function(e) {
      message("Error updating ATTAINS reference tables: ", e$message)
    }
  )

  # WQPWQXRefTables.R
  tryCatch(
    {
      .TADA_UpdateWQXCharValRef()
      .TADA_UpdateMeasureUnitRef()
      .TADA_UpdateDetCondRef()
      .TADA_UpdateDetLimitRef()
      .TADA_UpdateActivityTypeRef()
      .TADA_UpdateCharacteristicRef()
      .TADA_UpdateMeasureQualifierCodeRef()
      .TADA_UpdateMonLocTypeRef()
      .TADA_UpdateWQPOrganizationRef()
      .TADA_UpdateWQXCharAliasRef()
    },
    error = function(e) {
      message("Error updating WQPWQX reference tables: ", e$message)
    }
  )

  # CriteriaRefTables.R
  tryCatch(
    {
      .TADA_CST_UpdateWorkbook()
    },
    error = function(e) {
      message(
        "Error updating EPA Criteria Search Tool Excel workbook: ",
        e$message
      )
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

  # TADARefTables.R
  tryCatch(
    {
      .TADA_UpdateTADACharAliasRef()
      .TADA_UpdateTADAUsesAliasRef()
    },
    error = function(e) {
      message(
        "Error updating TADA characteristic or use alias reference files: ",
        e$message
      )
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
      dim(Data_Nutrients_UT)
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
      # Generate Data_TribalNations
      # =======================================
      Data_TribalNations <- TADA_DataRetrieval(
        organization = c(
          "REDLAKE_WQX", # good HMW example, Red Lake Band of Chippewa Indians
          "SFNOES_WQX", # Sac & Fox Nation
          # "PUEBLO_POJOAQUE", # Pueblo of Pojoaque
          "FONDULAC_WQX", # good HMW example, Minnesota Chippewa Tribe (Fond du Lac Band)
          "PUEBLOOFTESUQUE", # Pueblo of Tesuque
          # "CNENVSER", # The Chickasaw Nation
          # "PENOBSCOTINDIANNATIONDNR", # Penobscot Indian Nation
          "UTEMTN", # R8, Ute Mountain Ute Tribe (Colorado)
          "BLCKFEET" # R8, Blackfeet Nation (Montana)
        ),
        # last 5 years
        startDate = "2021-01-01",
        endDate = "2025-12-31",
        ask = FALSE
      )
      message("Data_TribalNations")
      dim(Data_TribalNations)
      usethis::use_data(
        Data_TribalNations,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )

      # =======================================
      # Harmonize Data_TribalNations
      # =======================================

      # Filter for surface water data only
      harmonized_data <- TADA_MediaFilter(
        Data_TribalNations,
        clean = TRUE,
        surface_water = FALSE,
        ground_water = TRUE,
        sediment = TRUE,
        other = TRUE
      )

      rm(Data_TribalNations)
      
      # Remove single organization duplicates
      harmonized_data <- TADA_FindPotentialDuplicatesSingleOrg(harmonized_data)
      harmonized_data <- dplyr::filter(harmonized_data, TADA.SingleOrgDup.Flag == "Unique")
      
      # Handle censored results
      harmonized_data <- TADA_SimpleCensoredMethods(
        harmonized_data,
        nd_method = "multiplier",
        nd_multiplier = 0.5,
        od_method = "as-is",
        od_multiplier = "null"
      )

      # Convert special characters
      harmonized_data <- TADA_ConvertSpecialChars(
        harmonized_data,
        col = "TADA.ResultMeasureValue",
        clean = TRUE
      )
      
      # Remove results with quality control issues
      harmonized_data <- TADA_RunKeyFlagFunctions(harmonized_data, clean = TRUE)
      
      # Flag above and below threshold
      harmonized_data <- TADA_FlagAboveThreshold(
        harmonized_data,
        clean = FALSE,
        flaggedonly = FALSE
      )
      harmonized_data <- TADA_FlagBelowThreshold(
        harmonized_data,
        clean = FALSE,
        flaggedonly = FALSE
      )
      
      # Harmonize synonyms
      harmonized_data <- TADA_HarmonizeSynonyms(harmonized_data)
      Data_TribalNations_Harmonized <- TADA_HarmonizeSynonyms(harmonized_data)
      
      message("Data_TribalNations_Harmonized")
      dim(Data_TribalNations_Harmonized)
      
      usethis::use_data(
        Data_TribalNations_Harmonized,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
     
      rm(Data_TribalNations_Harmonized, harmonized_data)

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
      dim(Data_R5_TADAPackageDemo)
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
      dim(Data_HUC8_02070004_Mod1Output)
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
      dim(Data_MT_MissoulaCounty)
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
      dim(Data_MT_AUMLRef)
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
        Data_MT_AUMLRef$TADA_with_ATTAINS,
        AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
        org_id = "MTDEQ"
      )

      message("Data_MT_AU_UsesRef")
      dim(Data_MT_AU_UsesRef)
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
        Data_MT_AUMLRef$TADA_with_ATTAINS,
        waterUseRef = TADA_AssignUsesToWaterType(org_id = "MTDEQ"),
        AUMLRef = Data_MT_AUMLRef$ATTAINS_crosswalk,
        org_id = "MTDEQ"
      )

      message("Data_MT_AU_UsesRef_Water")
      dim(Data_MT_AU_UsesRef_Water)

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
      # =======================================
      # Generate wqx3_fullPhysChem for use in WQX3-Migration.Rmd
      # =======================================
      wqx3_fullPhysChem <- dataRetrieval::readWQPdata(
        statecode = "Illinois",
        countycode = "DeWitt",
        characteristicName = "Nitrogen",
        service = "ResultWQX3",
        dataProfile = "fullPhysChem",
        ignore_attributes = TRUE
      )

      message("wqx3_fullPhysChem")
      dim(wqx3_fullPhysChem)

      usethis::use_data(
        wqx3_fullPhysChem,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      rm(wqx3_fullPhysChem)
      
      # =======================================
      # Generate Data_Penobscot
      # =======================================    
      
      Data_Penobscot <- TADA_DataRetrieval(
        siteid = c(
          "PENOBSCOTINDIANNATIONDNR-130-BM1",
          "PENOBSCOTINDIANNATIONDNR-4-CH1",
          "PENOBSCOTINDIANNATIONDNR-5-DC1",
          "PENOBSCOTINDIANNATIONDNR-6-DP1",
          "PENOBSCOTINDIANNATIONDNR-76-DP3",
          "PENOBSCOTINDIANNATIONDNR-13-EM1",
          "PENOBSCOTINDIANNATIONDNR-14-EM2",
          "PENOBSCOTINDIANNATIONDNR-15-GB1",
          "PENOBSCOTINDIANNATIONDNR-16-GB2",
          "PENOBSCOTINDIANNATIONDNR-17-GB3",
          "PENOBSCOTINDIANNATIONDNR-18-GF1",
          "PENOBSCOTINDIANNATIONDNR-19-GF2",
          "PENOBSCOTINDIANNATIONDNR-113-GW1",
          "PENOBSCOTINDIANNATIONDNR-114-GW2",
          "PENOBSCOTINDIANNATIONDNR-115-GW3",
          "PENOBSCOTINDIANNATIONDNR-129-GWTR1",
          "PENOBSCOTINDIANNATIONDNR-20-LE1",
          "PENOBSCOTINDIANNATIONDNR-21-LE2",
          "PENOBSCOTINDIANNATIONDNR-22-LE3",
          "PENOBSCOTINDIANNATIONDNR-23-LI1",
          "PENOBSCOTINDIANNATIONDNR-24-LT1",
          "PENOBSCOTINDIANNATIONDNR-25-MD1",
          "PENOBSCOTINDIANNATIONDNR-26-MD2",
          "PENOBSCOTINDIANNATIONDNR-27-MI1",
          "PENOBSCOTINDIANNATIONDNR-28-MI2",
          "PENOBSCOTINDIANNATIONDNR-29-MI3",
          "PENOBSCOTINDIANNATIONDNR-30-MS1",
          "PENOBSCOTINDIANNATIONDNR-31-MW1",
          "PENOBSCOTINDIANNATIONDNR-32-MW2",
          "PENOBSCOTINDIANNATIONDNR-33-NL1",
          "PENOBSCOTINDIANNATIONDNR-117-OR1",
          "PENOBSCOTINDIANNATIONDNR-34-OT1",
          "PENOBSCOTINDIANNATIONDNR-35-OT2",
          "PENOBSCOTINDIANNATIONDNR-36-OT3",
          "PENOBSCOTINDIANNATIONDNR-37-PA1",
          "PENOBSCOTINDIANNATIONDNR-38-PA2",
          "PENOBSCOTINDIANNATIONDNR-39-PA3",
          "PENOBSCOTINDIANNATIONDNR-40-PA4",
          "PENOBSCOTINDIANNATIONDNR-41-PI1",
          "PENOBSCOTINDIANNATIONDNR-42-PI2",
          "PENOBSCOTINDIANNATIONDNR-43-PI3",
          "PENOBSCOTINDIANNATIONDNR-44-PS1",
          "PENOBSCOTINDIANNATIONDNR-46-SH2",
          "PENOBSCOTINDIANNATIONDNR-47-SL1",
          "PENOBSCOTINDIANNATIONDNR-81-TCOS1",
          "PENOBSCOTINDIANNATIONDNR-87-THOB",
          "PENOBSCOTINDIANNATIONDNR-135-TPIR14",
          "PENOBSCOTINDIANNATIONDNR-105-TPIR15",
          "PENOBSCOTINDIANNATIONDNR-123-TPIR16",
          "PENOBSCOTINDIANNATIONDNR-100-TPIR3",
          "PENOBSCOTINDIANNATIONDNR-103-TPIR6",
          "PENOBSCOTINDIANNATIONDNR-104-TPIR9",
          "PENOBSCOTINDIANNATIONDNR-86-TPOB",
          "PENOBSCOTINDIANNATIONDNR-91-TPUS1",
          "PENOBSCOTINDIANNATIONDNR-92-TPUS2",
          "PENOBSCOTINDIANNATIONDNR-131-VZI1",
          "PENOBSCOTINDIANNATIONDNR-132-VZTR1",
          "PENOBSCOTINDIANNATIONDNR-48-WB1",
          "PENOBSCOTINDIANNATIONDNR-49-WBU1",
          "PENOBSCOTINDIANNATIONDNR-50-WD1",
          "PENOBSCOTINDIANNATIONDNR-51-WD2",
          "PENOBSCOTINDIANNATIONDNR-75-WD3",
          "PENOBSCOTINDIANNATIONDNR-52-WE1",
          "PENOBSCOTINDIANNATIONDNR-53-WE2",
          "PENOBSCOTINDIANNATIONDNR-54-WE3",
          "PENOBSCOTINDIANNATIONDNR-56-WL1"
        ),
        characteristicName = "Escherichia coli",
        ask = FALSE,
        applyautoclean = TRUE
      )
      
      message("Data_Penobscot")
      dim(Data_Penobscot)
      
      usethis::use_data(
        Data_Penobscot,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
      
      # =======================================
      # Generate Data_Participatory_Scientists
      # =======================================
      selected_orgs <- c(
        "CONNRIVERCONSERVANCY",
        "CT_NERR",
        "BANTAMLAKE_WQX",
        "CTVOLMON",
        "CT_NERR"
      )

      Data_Participatory_Scientists <- EPATADA::TADA_DataRetrieval(
        organization = selected_orgs,
        ask = FALSE,
        applyautoclean = TRUE
      )

      message("Data_Participatory_Scientists")
      dim(Data_Participatory_Scientists)

      usethis::use_data(
        Data_Participatory_Scientists,
        internal = FALSE,
        overwrite = TRUE,
        compress = "xz",
        version = 3,
        ascii = FALSE
      )
    },
    error = function(e) {
      message("An error occurred during data update: ", e$message)
    }
  )
}
