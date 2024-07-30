#' Create Simple Numeric Criteria Ref Data Frame
#'
#' This function creates a data frame with the TADA.CharacteristicName, TADA.MethodSpeciationName, 
#' and TADA.ResultSampleFractionText found in a TADA data frame. It also adds blank columns for
#' TADA.UserStandardUnit and TADA.UserStandardValue which users can fill in with the value and unit
#' for the simple numeric criteria they want to compare TADA data frame results to.
#' The columns created by TADA_AutoClean are required to run this function. If they are not present
#' in the data frame, the function will stop and print an error message. It is recommended to
#' perform all data harmonization, conversion, and cleaning before using this function to ensure
#' that all characteristic/speciation/fraction combinations are accurately reflected in the simple
#' numeric criteria ref data frame.
#' 
#' # Should param(s) be added to easily export this as an Excel or csv file for users to edit?
#'
#' @param .data TADA dataframe
#'
#' @return A data frame with columns for TADA.CharacteristicName, TADA.MethodSpeciationName,
#' TADA.ResultSampleFractionText, TADA.UserStandardUnit, and TADA.UserStandardValue. All values for
#' TADA.UserStandardUnit and TADA.UserStandardValue are NA and must be filled in by the user before
#' use in TADA_SimpleCriteriaComparison.
#' 
#' @export
#'
#' @examples
#' # create criteria reference for Utah nutrients example data set
#' UT_CriteriaRef <- TADA_CreateSimpleCriteriaRef(Data_Nutrients_UT)
#' 

TADA_CreateSimpleCriteriaRef <- function(.data) {
  
  .data <- .data %>%
    dplyr::select(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
    dplyr::distinct() %>%
    dplyr::mutate(TADA.UserStandardValue = NA,
                  TADA.UserStandardUnit = NA)
  
  return(.data)
  
}

#' Compare Simple Numeric Criteria to TADA_Results
#'
#' This function compares TADA results with user-supplied simple numeric criteria.
#' The columns created by TADA_AutoClean are required to run this function. If
#' they are not present in the data frame, the function will stop and print an error message.
#'
#' @param .data TADA dataframe
#'
#' @param criteria.ref A user-supplied data frame with the columns TADA.CharacteristicName,
#' TADA.MethodSpeciationName, TADA.ResultSampleFractionText, TADA.UserStandardValue, and
#' TADA.UserStandardValues. All unique combinations of TADA.CharacteristicName,
#' TADA.MethodSpeciationName, and TADA.ResultSampleFractionText the user wants to compare
#' against simple numeric criteria should have their own row. TADA.UserStandardUnits should
#' contain the units of the numeric criteria in uppercase and TADA.UserStandardValue should
#' contain the numeric value of the criteria.
#
#' @return A TADA data frame with an added TADA.SimpleCriteriaComparison.Flag.
#' @export
#'
#' @examples
#' # create criteria reference for example
#' TADA.CharacteristicName <- c("BARIUM", "CHLORIDE", "CHLORIDE", "SILVER")
#' TADA.ResultSampleFractionText <- c("TOTAL", "TOTAL", "TOTAL", "TOTAL")
#' TADA.MethodSpeciationName <- c(NA, NA, "AS CL", NA)
#' TADA.UserStandardValue <- c(5000, 500, 500, 160)
#' TADA.UserStandardUnits <- c("UG/L", "MG/L", "MG/L", "UG/L")
#' example.ref <- data.frame(TADA.CharacteristicName, TADA.MethodSpeciationName, TADA.ResultSampleFractionText, TADA.UserStandardValue, TADA.UserStandardUnits)
#'
#' # remove intermediate objects
#' rm(
#'   TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.UserStandardValue,
#'   TADA.UserStandardUnits, TADA.MethodSpeciationName
#' )
#'
#' # create example data set
#' testdat <- TADA_DataRetrieval(
#'   statecode = "IL",
#'   startDate = "2020-01-01",
#'   endDate = "2020-06-30"
#' )
#'
#' # simple criteria comparison example with unit conversion
#' SimpleCriteriaEx <- TADA_SimpleCriteriaComparison(testdat, criteria.ref = example.ref, convert.units = TRUE)
#'
TADA_SimpleCriteriaComparison <- function(.data, criteria.ref = NULL, convert.units = TRUE) {
  # check for required columns
  req_cols <- c(
    "TADA.CharacteristicName", "TADA.MethodSpeciationName",
    "TADA.ResultSampleFractionText", "TADA.ResultMeasure.MeasureUnitCode"
  )

  TADA_CheckColumns(.data, expected_cols = req_cols)

  # check for user provided criteria reference, stop if not provided
  if (is.null(criteria.ref)) {
    stop("There is no user-supplied reference providing numeric criteria. The function cannot be run.")
  }

  # check for user provided criteria reference, continue if provided
  if (!is.null(criteria.ref)) {
    # create vector of TADA.ComparableDataIdentifier from TADA data frame
    tada.combs <- .data %>%
      dplyr::select(TADA.ComparableDataIdentifier) %>%
      dplyr::distinct()

    # create vector of characteristic/fraction pairs from user-supplied criteria reference
    ref.combs <- criteria.ref %>%
      dplyr::rename(TADA.ResultMeasure.MeasureUnitCode = TADA.UserStandardUnits) %>%
      TADA_CreateComparableID() %>%
      dplyr::select(TADA.ComparableDataIdentifier) %>%
      dplyr::distinct()

    # find pairs that are included in TADA data frame but not user-supplied criteria reference
    diff.combs <- tada.combs %>%
      dplyr::anti_join(ref.combs, by = dplyr::join_by(TADA.ComparableDataIdentifier)) %>%
      dplyr::pull()

    diff.combs <- stringi::stri_replace_last(stringi::stri_paste(diff.combs, collapse = ", "), " and ", fixed = ", ")

    print(paste("TADA_SimpleCriteriaComparison: The following characteristic/fraction pairs do not have user-provided numeric critieria: ", diff.combs, ".", sep = ""))

    # join user-supplied criteria reference to TADA data frame
    .data <- .data %>%
      dplyr::left_join(criteria.ref, by = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText", "TADA.MethodSpeciationName"))

    # compare TADA.ResultMeasure.MeasureUnitCode units to TADA.UserStandardUnits and identify mismatches
    unit.check <- .data %>%
      dplyr::filter(!is.na(TADA.UserStandardUnits)) %>%
      dplyr::filter(TADA.ResultMeasure.MeasureUnitCode != TADA.UserStandardUnits) %>%
      dplyr::select(TADA.ComparableDataIdentifier) %>%
      dplyr::distinct()

    # create string of characteristic/fraction pairs with mismatches
    unit.print <- stringi::stri_replace_last(stringi::stri_paste(dplyr::pull(unit.check), collapse = ", "), " and ", fixed = ", ")

    # if user does not want to convert units within this function
    if (convert.units == FALSE) {
      # if any rows with unit mismatches are present, stop function
      if (nrow(unit.check) > 0) {
        stop(paste("TADA_SimpleCriteriaComparison: The result units for the following TADA.ComparableDataIdentifier(s) in the TADA data frame do not match the units in the user-provided numeric criteria: ",
          unit.print, ". Correct numeric criteria input or use convert.units = TRUE to automatically convert results to match user-provided criteria units.",
          sep = ""
        ))
      }

      # if no rows with unit mismatches are present, continue with function
      if (nrow(unit.check) == 0) {
        print("TADA_SimpleCriteriaComparison: The result units for all TADA.ComparableDataIdentifiers in the TADA data frame match the units in the user-provided numeric criteria.")
      }
    }

    # if user does want to convert units within this function
    if (convert.units == TRUE) {
      # Import TADA specific conversion reference, created by HRM on 4/30/2024
      tada.unit.ref <- utils::read.csv(system.file("extdata", "TADAPriorityCharConvertRef.csv", package = "EPATADA"))
      # Make all codes and target units uppercase
      tada.unit.ref <- tada.unit.ref %>%
        dplyr::mutate(
          TADA.Target.ResultMeasure.MeasureUnitCode = toupper(Target.Unit),
          TADA.ResultMeasure.MeasureUnitCode = toupper(tada.unit.ref$Code)
        ) %>%
        dplyr::rename(
          TADA.WQXUnitConversionFactor = Conversion.Factor,
          TADA.WQXUnitConversionCoefficient = Conversion.Coefficient
        ) %>%
        dplyr::select(
          TADA.ResultMeasure.MeasureUnitCode, TADA.Target.ResultMeasure.MeasureUnitCode,
          TADA.WQXUnitConversionFactor, TADA.WQXUnitConversionCoefficient
        )

      # create wqx unit ref
      wqx.ref <- TADA_GetMeasureUnitRef() %>%
        dplyr::mutate(
          Code = toupper(Code),
          Target.Unit = toupper(Target.Unit)
        ) %>%
        dplyr::rename(
          TADA.ResultMeasure.MeasureUnitCode = Code,
          TADA.Target.ResultMeasure.MeasureUnitCode = Target.Unit,
          TADA.WQXUnitConversionFactor = Conversion.Factor,
          TADA.WQXUnitConversionCoefficient = Conversion.Coefficient
        ) %>%
        dplyr::select(-Domain, -Description, -Unique.Identifier, -Last.Change.Date)

      # add tada unit ref to wqx unit ref
      all.unit.ref <- wqx.ref %>%
        dplyr::full_join(tada.unit.ref, dplyr::join_by(
          TADA.ResultMeasure.MeasureUnitCode,
          TADA.Target.ResultMeasure.MeasureUnitCode, TADA.WQXUnitConversionFactor,
          TADA.WQXUnitConversionCoefficient
        ))

      # check if any rows contain unit mismatches
      if (nrow(unit.check) > 0) {
        # print message identifying characteristic/fraction pairs with unit mismatches
        print(paste("TADA_SimpleCriteriaComparison: The result units for the following TADA.ComparableDataIdentifiers in the TADA data frame do not match the units in the user-provided numeric criteria: ",
          unit.print, ". Result units will be converted to match user-supplied criteria units using TADA_ConvertResultUnits.",
          sep = ""
        ))

        # filter .data to create data frame of results that require conversion
        convert.data <- .data %>%
          dplyr::filter(TADA.ComparableDataIdentifier %in% unit.check$TADA.ComparableDataIdentifier)

        # filter .data to create date frame of results that do not require conversion
        other.data <- .data %>%
          dplyr::filter(!ResultIdentifier %in% convert.data$ResultIdentifier)

        # create ref based on results
        criteria.convert <- convert.data %>%
          dplyr::select(
            TADA.CharacteristicName, TADA.MethodSpeciationName,
            TADA.ResultSampleFractionText
          ) %>%
          dplyr::distinct() %>%
          dplyr::left_join(criteria.ref, by = dplyr::join_by(
            TADA.CharacteristicName, TADA.MethodSpeciationName,
            TADA.ResultSampleFractionText
          ))

        # create ref based on results
        result.ref <- convert.data %>%
          dplyr::select(
            TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
            ResultMeasure.MeasureUnitCode, TADA.ResultSampleFractionText
          ) %>%
          dplyr::distinct()

        # create ref based on detection limits (needed in order to work with TADA_ConvertResultUnits)
        det.ref <- convert.data %>%
          dplyr::select(
            TADA.CharacteristicName, TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
            DetectionQuantitationLimitMeasure.MeasureUnitCode, TADA.ResultSampleFractionText
          ) %>%
          dplyr::distinct() %>%
          dplyr::filter(!is.na(TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode)) %>%
          dplyr::rename(
            TADA.ResultMeasure.MeasureUnitCode = TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode,
            ResultMeasure.MeasureUnitCode = DetectionQuantitationLimitMeasure.MeasureUnitCode
          )

        # combine refs from results and detection limits to create ref to use with TADA_ConvertResultUnits
        unit.ref <- result.ref %>%
          dplyr::full_join(det.ref, by = dplyr::join_by(
            TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode,
            TADA.MethodSpeciationName, ResultMeasure.MeasureUnitCode, TADA.ResultSampleFractionText
          )) %>%
          dplyr::distinct() %>%
          dplyr::left_join(criteria.convert, by = dplyr::join_by(TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.MethodSpeciationName)) %>%
          dplyr::rename(TADA.Target.ResultMeasure.MeasureUnitCode = TADA.UserStandardUnits) %>%
          dplyr::left_join(all.unit.ref, by = dplyr::join_by(TADA.ResultMeasure.MeasureUnitCode, TADA.Target.ResultMeasure.MeasureUnitCode)) %>%
          dplyr::select(
            TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
            TADA.Target.ResultMeasure.MeasureUnitCode, ResultMeasure.MeasureUnitCode, TADA.WQXUnitConversionFactor,
            TADA.WQXUnitConversionCoefficient
          )

        # convert results in need of conversion using TADA_ConvertResultUnits and the unit ref created in this function
        convert.data <- TADA_ConvertResultUnits(convert.data, ref = unit.ref)

        # convert result units using TADA_ConvertResultUnits
        .data <- convert.data %>%
          dplyr::full_join(other.data, by = names(convert.data)) %>%
          TADA_CreateComparableID() %>%
          TADA_OrderCols()
      }

      # if all units in user-provided criteria ref match results in TADA data frame, do not make any changes to TADA data frame
      if (nrow(unit.check) == 0) {
        print("TADA_SimpleCriteriaComparison: The result units for all characteristic/fraction pairs in the TADA data frame match the units in the user-provided numeric criteria.")

        .data <- .data
      }
    }

    # create TADA.SimpleCriteria.Flag column to identify results where user-provided criteria are exceeded
    .data <- .data %>%
      dplyr::mutate(TADA.SimpleCriteria.Flag = ifelse(TADA.ResultMeasureValue > TADA.UserStandardValue,
        "Exceeds", "Does Not Exceed"
      ))
  }
}

#' Create Reference Data Frame to Pair Hardness, pH, or Temperature Results
#'
#' This function creates a data frame that shows all combinations of TADA.CharacteristicName,
#' TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
#' and TADA.ResultSampleFractionText for hardness, pH, or temperature for users to edit and use 
#' in NAMEOFPAIRINGFUNCTION.
#' 
#' @param .data TADA dataframe
#'
#' @param char Character string. Default is "hardness". Other options are "pH" and "temp". When
#' char = "hardness", the TADA data frame is filtered for all hardness TADA.CharacteristicNames before
#' the unique combinations of TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
#' TADA.MethodSpeciationName, and TADA.ResultSampleFractionText are iidentified. For char = "ph" and
#' char = "temp", the same process occurs for TADA.CharacteristicNames for pH, and water temperature,
#' respectively.
#
#' @return A data frame with four columns, ADA.CharacteristicName,
#' TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
#' and TADA.ResultSampleFractionText. 
#'
#' @export
#'
#' @examples
#' # create ref for hardness for example tribal data
#' HardnessRef <- TADA_CreatePairRef(Data_6Tribes_5y_Harmonized, char = "hardness")

TADA_CreatePairRef <- function(.data, char = "hardness") {
  
  if(char == "hardness") {
    
    char.ref <- TADA_GetCharacteristicRef() %>%
      dplyr::mutate(CharacteristicName = toupper(CharacteristicName))
    
    char.ref <- char.ref %>%
      dplyr::filter(grepl("HARDNESS", CharacteristicName))
    
    .data <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% char.ref$CharacteristicName)
  }
  
  if(char == "pH") {

    .data <- .data %>%
      dplyr::filter(TADA.CharacteristicName == "PH")
  }
  
  if(char == "temp") {
    
    .data <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c(	"TEMPERATURE", "TEMPERATURE, WATER"))
  }

  .data <- .data %>%
    dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
                  TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
    dplyr::distinct() %>%
    dplyr::group_by(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
                    TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
    dplyr::mutate(Rank = dplyr::cur_group_id())
  
  return(data)
}

#' Pair Results with Hardness, pH, or Temperature
#'
#' This function pairs TADA results with hardness, pH, and temperature results from the same 
#' MonitoringLocation within an user-specified time window to facilitate the calculation of numeric 
#' criteria The columns created by TADA_AutoClean are required to run this function. If they are not
#' present in. the data frame, the function will stop and print an error message.
#' 
#' Users can provide a pairing reference file (can be created using TADA_CreatePairRef) to specify
#' which combinations of TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnit, 
#' TADA.MethodSpeciatioName, and TADA.ResultSampleFraction should be used for hardness, pH, and temperature.
#' If no ref is specified, all possible combinations for hardness, pH, or temperature will be used.
#'
#' @param .data TADA dataframe
#'
#' @param hardness Boolean argument. If hardness = TRUE, hardness results will be paired with TADA
#' results. If hardness = FALSE, hardness results will not be paired. Default = TRUE.
#' 
#' @param ph Boolean argument. If ph = TRUE, hardness results will be paired with TADA
#' results. If ph = FALSE, ph results will not be paired. Default = TRUE.
#' 
#' @param temp Boolean argument. If temp = TRUE, temperature results will be paired with TADA
#' results. If temp = FALSE, temperature results will not be paired. Default = TRUE.
#
#' @return A TADA data frame with up to seven additional columns, depending on params. If hardness =
#' TRUE, columns for TADA.Hardness.MeasureValue, TADA.Hardness.MeasureUnitCode, 
#' @export
#'
#' @examples
#'
TADA_PairForCriteriaCalc <- function(.data, hardness = TRUE, ph = TRUE, temp = TRUE,
                                     ref = "null", hours_range = 4) {
  
  if(ref == "null"){
    
    if(hardness == TRUE) {
    
    hardness.data <- .data %>%
      dplyr::filter(!is.na(ActivityStartDateTime),
                    grepl("HARDNESS", TADA.CharacteristicName))
    
    hardness.subset <- hardness.data %>%
      dplyr::select(TADA.MonitoringLocationIdentifier, TADA.CharacteristicName,
                    TADA.ResultMeasure.MeasureUnitCode, ActivityStartDateTime,
                    ActivityIdentifier) %>%
      dplyr::rename(TADA.HardnessName = TADA.CharacteristicName,
                    TADA.HardnessUnitCode = TADA.ResultMeasure.MeasureUnitCode,
                    HardnessActivityStartDateTime = ActivityStartDateTime) %>%
      # what should default ranking be here?
      dplyr::mutate(Rank = dplyr::case_when(TADA.HardnessName == "HARDNESS, CA, MG" ~ 1,
                                     TADA.HardnessName == "TOTAL HARDNESS" ~ 2,
                                     TADA.HardnessName == "HARDNESS, CARBONATE" ~ 3))
    }
    
    if(ph == TRUE) {
    ph.data <- .data %>%
      dplyr::filter(TADA.CharacteristicName == "PH")
    }
    
    if(temp == TRUE) {
    temp.data <- .data %>% 
      dplyr::filter(TADA.CharacteristicName %in% c("TEMPERATURE", "TEMPERATURE, WATER"))
    }
    
    pair.hardness.activityid <- .data %>%
      dplyr::filter(ActivityIdentifier %in% hardness.subset$ActivityIdentifier) %>%
      dplyr::left_join(hardness.subset, by = dplyr::join_by(ActivityIdentifier, TADA.MonitoringLocationIdentifier),
                       relationship = "many-to-many") %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::mutate(NCount = length(TADA.ResultMeasureValue)) %>%
      dplyr::arrange(ResultIdentifier, Rank) %>%
      dplyr::slice_min(Rank) %>%
      dplyr::ungroup()
    
    # need to rank hardness characteristic choices
    
    #start2 <- Sys.time()
    
    pair.hardness.ml.time <- .data %>%
      dplyr::filter(!ResultIdentifier %in% pair.hardness.activityid$ResultIdentifier,
                    !is.na(ActivityStartDateTime),
                    TADA.MonitoringLocationIdentifier %in% hardness.subset$TADA.MonitoringLocationIdentifier) %>%
      dplyr::left_join(hardness.subset, relationship = "many-to-many",
                       by = dplyr::join_by(TADA.MonitoringLocationIdentifier)) %>%
      dplyr::group_by(ResultIdentifier) %>%
      # Figure out fastest time comparison method - needs to be absolute time comparison
      dplyr::mutate(timediff = abs(difftime(as.POSIXct(HardnessActivityStartDateTime), as.POSIXct(ActivityStartDateTime), units = c("hours")))) %>%
      dplyr::filter(timediff <= 4) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::mutate(NCount = length(TADA.ResultMeasureValue)) %>%
      dplyr::arrange(ResultIdentifier, Rank) %>%
      dplyr::slice_min(Rank) %>%
      dplyr::ungroup() %>%
      dplyr::select(-timediff)
      
      #end2 <- Sys.time()
      
      #end2-start2
    
    # combine paired hardness dfs
    hardness.pairs <- pair.hardness.activityid %>%
      dplyr::full_join(pair.hardness.ml.time, names(pair.hardness.activityid))
    
    
    

  }

  } 
