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

#' Create Reference Data Frame to Pair Characteristic Results For Use in Numeric Criteria Equations
#'
#' This function creates a data frame that shows all combinations of TADA.CharacteristicName,
#' TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
#' and TADA.ResultSampleFractionText for commonly paired characteristics (such as pH, temperature,
#' hardness, salinity, and chloride).
#' 
#' @param .data TADA dataframe
#' 
#' @param ph Boolean argument. When ph = TRUE, pH is included in this reference data frame. When 
#' ph = FALSE, pH is not included in the reference data frame.
#' 
#' @param hardness Boolean argument. When hardness = TRUE, hardness is included in this reference 
#' data frame. When hardness = FALSE, hardness is not included in the reference data frame.
#' 
#' @param temp Boolean argument. When temp = TRUE, water temperature is included in this reference 
#' data frame. When temp = FALSE, water temperature is not included in the reference data frame.
#' 
#' @param salinity Boolean argument. When salinity = TRUE, salinity is included in this reference 
#' data frame. When salinity = FALSE, salinity is not included in the reference data frame.
#' 
#' @param chloride Boolean argument. When chloride = TRUE, salinity is included in this reference 
#' data frame. When chloride = FALSE, chloride is not included in the reference data frame.
#'
#' @param other_char Character argument. The user provides a data frame TADA.CharacteristicNames and
#' the pairing group they belong to. #Needs more details.
#
#' @return A data frame with six columns, TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
#' TADA.MethodSpeciationName, TADA.ResultSampleFractionText, TADA.PairingGroup, and 
#' TADA.PairingGroupRank. 
#'
#' @export
#'
#' @examples
#' # create ref for hardness for example tribal data
#' HardnessRef <- TADA_CreatePairRef(Data_6Tribes_5y_Harmonized,  ph = FALSE, hardness = TRUE,
#' temp = FALSE, chloride = FALSE, salinity = FALSE, other_char = "null")
#' 

TADA_CreatePairRef <- function(.data, ph = TRUE, hardness = TRUE, temp = TRUE,
                               chloride = TRUE, salinity = TRUE, other_char = "null") {
  
  # create data frame to store pair refs
  pair.ref <- data.frame(matrix(ncol = 6, nrow = 0))
  # name columns in pair.ref df
  colnames(pair.ref) <- c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode", 
                          "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText",
                          "TADA.PairingGroup", "TADA.PairingGroupRank")
  
  prep.ref <- function(.data) {
    
    .data <- .data %>%
      dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
                    TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
      dplyr::distinct() %>%
      dplyr::group_by(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
                      TADA.MethodSpeciationName, TADA.ResultSampleFractionText) %>%
      # need a better way to assign rank? make specific to each char and remove from this function?
      dplyr::mutate(TADA.PairingGroup.Rank = dplyr::cur_group_id())
  }
  
  
  if(hardness == TRUE) {
    
    char.ref <- TADA_GetCharacteristicRef() %>%
      dplyr::mutate(CharacteristicName = toupper(CharacteristicName)) %>%
      dplyr::filter(grepl("HARDNESS", CharacteristicName))
    
    hard.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% char.ref$CharacteristicName) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Hardness")
    
    pair.ref <- rbind(pair.ref, hard.ref)
    
    rm(char.ref, hard.ref)
  }
  
  if(ph == TRUE) {

    ph.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName == "PH") %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "pH")
    
    pair.ref <- rbind(pair.ref, ph.ref)
    
    rm(ph.ref)
  }
  
  if(temp == TRUE) {
    
    temp.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c(	"TEMPERATURE", "TEMPERATURE, WATER")) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Temperature")
    
    pair.ref <- rbind(pair.ref, temp.ref)
    
    rm(temp.ref)
  }
  
  if(salinity == TRUE) {
    
    salinity.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c("SALINITY")) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Salinity")
    
    pair.ref <- rbind(pair.ref, salinity.ref)
    
    rm(salinity.ref)
  }
  
  
  if(chloride == TRUE) {
    
    chloride.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c("CHLORIDE")) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Chloride")
    
    pair.ref <- rbind(pair.ref, chloride.ref)
    
    rm(chloride.ref)
  }
    
  if(!is.data.frame(other_char) & other_char != "null") {
      stop("TADA_CreatePairRef: 'other_char' must be a data frame with three columns. The first column
           contains TADA.CharactersticName, the second column contains TADA.PairingGroup, and the
           third column contains TADA.PairingGroup.Rank")
    }
    
    # add code for adding other characteristic
    if(is.data.frame(other_char)) {
      
      pair.ref <- rbind(pair.ref, other_char)
    }

  # remove any duplicate rows
  pair.ref <- pair.ref %>%
    dplyr::distinct()
  
  # check to see if there are any rows in pair.ref
  
  if(nrow(pair.ref) == 0) {
    stop("None of the specified pairing characteristics were found in the TADA data frame.")
  }
  
  return(pair.ref)
}

#' Pair Results for Numeric Criteria Calculation
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
#' @param ref Write description of what columns need to be in this ref or null option
#
#' @return A TADA data frame with six additional columns added for each pairing group specified
#' in the pairing ref.
#' @export
#'
#' @examples
#' 
#'  AL_df <- TADA_DataRetrieval(startDate = "2010-11-30",
#'  endDate =  "2010-12-01",
#'  statecode = "AL" )
#'  
#'  AL_PairRef <- TADA_PairForCriteriaCalc(AL_df)
#'
TADA_PairForCriteriaCalc <- function(.data, ref = "null", hours_range = 4) {
  
 # if user does not supply a pairing ref, create the default ref
   if(ref == "null"){
    
    ref <- TADA_CreatePairRef(.data)
   }
  
  # check to see if user-supplied ref is a df
  # if(!is.character(ref)) {
  #   
  #   if(!is.data.frame(ref)) {
  #     
  #   stop("TADA_PairForCriteriaCalc: 'ref' must be a data frame with six columns: TADA.CharacteristicName,
  #        TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
  #        TADA.PairingGroup.Rank, and TADA.PairingGroup.")
  #   }
  #   
  #   if(is.data.frame(ref)) {
  #     
  #     col.names <- c("TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode", 
  #                    "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText",
  #                    "TADA.PairingGroup.Rank", "TADA.PairingGroup")
  #     
  #     ref.names <- names(ref)
  #     
  #     if(length(setdiff(col.names, ref.names)) > 0) {
  #       
  #       stop("TADA_PairForCriteriaCalc: 'ref' must be a data frame with six columns: TADA.CharacteristicName,
  #        TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
  #        TADA.PairingGroup.Rank, and TADA.PairingGroup.")
  #     }
  #   }
  #   
  # }

    
    # create list of pairing groups
    list.groups <- ref %>%
      dplyr::ungroup() %>%
      dplyr::select(TADA.PairingGroup) %>%
      dplyr::distinct() %>%
      dplyr::pull()
    
    # find number of groups
    n.groups <- length(list.groups)
    
    # create number list based on n.groups (for mapping across all groups)
    n.groups.list <- seq(1, n.groups, 1)
    
    # create pairing function to be mapped across all groups
    
    pairing <- function(.data, group.pos) {
    
    # create group ID for naming new columns
    group.id <- list.groups[group.pos]
    
    ref.subset <- ref %>%
      dplyr::ungroup() %>%
      dplyr::filter(TADA.PairingGroup == list.groups[group.pos]) 
    
   # create subset of data for pairing
    pair.subset <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% ref.subset$TADA.CharacteristicName,
                    !is.na(ActivityStartDateTime)) %>%
      dplyr::select(TADA.CharacteristicName, TADA.ResultMeasureValue, TADA.ResultMeasure.MeasureUnitCode,
                    ActivityIdentifier, MonitoringLocationIdentifier, ActivityStartDateTime,
                    TADA.ResultSampleFractionText, TADA.MethodSpeciationName) %>%
      dplyr::left_join(ref.subset, relationship = "many-to-many", 
                       dplyr::join_by(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, 
                                      TADA.ResultSampleFractionText, TADA.MethodSpeciationName)) %>%
      dplyr::rename_with(~paste0("TADA.", group.id, "Name"), TADA.CharacteristicName) %>%
      dplyr::rename_with(~paste0("TADA.", group.id, ".ResultMeasureValue"), TADA.ResultMeasureValue) %>%
      dplyr::rename_with(~paste0("TADA.", group.id, ".MeasureUnitCode"), TADA.ResultMeasure.MeasureUnitCode) %>%
      dplyr::rename_with(~paste0("TADA.", group.id, "ActivityStartDateTime"), ActivityStartDateTime) %>%
      dplyr::rename_with(~paste0("TADA.", group.id, "ResultSampleFractionText"), TADA.ResultSampleFractionText) %>%
      dplyr::rename_with(~paste0("TADA.", group.id, "MethodSpeciationName"), TADA.MethodSpeciationName)
      
      
    
    # identify dynamically named columns
    pair_char_name <- paste0("TADA.", group.id, "Name")
    pair_result_val <- paste0("TADA.", group.id, ".ResultMeasureValue")
    pair_units <- paste0("TADA.", group.id, ".MeasureUnitCode")
    pair_datetime <- paste0("TADA.", group.id, "ActivityStartDateTime")
    pair_fraction <- paste0("TADA.", group.id, "ResultSampleFractionText")
    pair_speciation <- paste0("TADA.", group.id, "MethodSpeciationName")
      
    # NOTE - CAN REMOVE NCOUNT COLS WHEN DONE TESTING (HRM 8/20/24)  

    # pair by activity id
    pair.activityid <- .data %>%
      dplyr::filter(ActivityIdentifier %in% pair.subset$ActivityIdentifier) %>%
      dplyr::left_join(pair.subset, by = dplyr::join_by(ActivityIdentifier),
                       relationship = "many-to-many") %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::slice_min(order_by = TADA.PairingGroup.Rank) %>%
      dplyr::ungroup() %>%
      dplyr::select(-TADA.PairingGroup.Rank) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::select(ResultIdentifier, 
                    !!rlang::sym(pair_datetime),
                    !!rlang::sym(pair_result_val),
                    !!rlang::sym(pair_units),
                    !!rlang::sym(pair_fraction),
                    !!rlang::sym(pair_speciation))
    
    # drop activity id from pair subset
    pair.subset2 <- pair.subset %>%
      dplyr::select(-ActivityIdentifier)
  
    # pair by monitoring location and time
    pair.ml.time <- .data %>%
      dplyr::filter(!ResultIdentifier %in% pair.activityid$ResultIdentifier,
                    !is.na(ActivityStartDateTime),
                    MonitoringLocationIdentifier %in% pair.subset$MonitoringLocationIdentifier) %>%
      dplyr::left_join(pair.subset2, relationship = "many-to-many",
                       by = dplyr::join_by(MonitoringLocationIdentifier)) %>%
      dplyr::group_by(ResultIdentifier) %>%
      # Figure out fastest time comparison method - needs to be absolute time comparison
      dplyr::mutate(timediff = abs(difftime(as.POSIXct(!!rlang::sym(pair_datetime)), as.POSIXct(ActivityStartDateTime), units = c("hours")))) %>%
      dplyr::filter(timediff <= hours_range) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::arrange(ResultIdentifier,TADA.PairingGroup.Rank, dplyr::desc(timediff)) %>%
      dplyr::slice_min(TADA.PairingGroup.Rank) %>%
      dplyr::slice_min(timediff) %>%
      dplyr::ungroup() %>%
      dplyr::select(-timediff, -TADA.PairingGroup.Rank) %>%
      dplyr::select(ResultIdentifier, 
                    !!rlang::sym(pair_datetime),
                    !!rlang::sym(pair_result_val),
                    !!rlang::sym(pair_units),
                    !!rlang::sym(pair_fraction),
                    !!rlang::sym(pair_speciation)) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::slice_sample(n = 1)
    
    # combine paired dfs
    all.pairs <- pair.activityid %>%
      rbind(pair.ml.time) %>%
      dplyr::distinct()
    
    return(all.pairs)
 
    }
    
   # find pairs for all groups included in pairing ref
   all.groups <- purrr::map(n.groups.list, ~ pairing(.data, group.pos = .x))
   
   # convert list of dfs to a single df to join with .data
   all.groups <- purrr::reduce(all.groups, function(left, right) {
     dplyr::left_join(left, right, by = "ResultIdentifier")
   })
   
   
   # join with .data
  .data <- dplyr::left_join(.data, all.groups, by = "ResultIdentifier")
   
 
   return(.data)
  }

