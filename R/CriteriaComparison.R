#' Create Reference Dataframe to Pair Characteristic Results For Use in Numeric Criteria Equations (UNDER ACTIVE DEVELOPMENT)
#'
#' This function creates a dataframe that shows all combinations of TADA.CharacteristicName,
#' TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
#' and TADA.ResultSampleFractionText for commonly paired characteristics (such as pH, temperature,
#' hardness, salinity, and chloride).
#'
#' @param .data TADA dataframe
#'
#' @param ph Boolean argument. When ph = TRUE, pH is included in this reference dataframe. When
#' ph = FALSE, pH is not included in the reference dataframe.
#'
#' @param hardness Boolean argument. When hardness = TRUE, hardness is included in this reference
#' dataframe. When hardness = FALSE, hardness is not included in the reference dataframe.
#'
#' @param temp Boolean argument. When temp = TRUE, water temperature is included in this reference
#' dataframe. When temp = FALSE, water temperature is not included in the reference dataframe.
#'
#' @param salinity Boolean argument. When salinity = TRUE, salinity is included in this reference
#' dataframe. When salinity = FALSE, salinity is not included in the reference dataframe.
#'
#' @param chloride Boolean argument. When chloride = TRUE, salinity is included in this reference
#' dataframe. When chloride = FALSE, chloride is not included in the reference dataframe.
#'
#' @param other_char Character argument. The user provides a dataframe TADA.CharacteristicNames and
#' the pairing group they belong to. #Needs more details.
#
#' @return A dataframe with six columns, TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode,
#' TADA.MethodSpeciationName, TADA.ResultSampleFractionText, TADA.PairingGroup, and
#' TADA.PairingGroupRank.
#'
#' @export
#'
#' @examples
#' # create ref for hardness for example tribal data
#' HardnessRef <- TADA_CreatePairRef(Data_6Tribes_5y_Harmonized,
#'   ph = FALSE, hardness = TRUE,
#'   temp = FALSE, chloride = FALSE, salinity = FALSE, other_char = "null"
#' )
#'
TADA_CreatePairRef <- function(.data, ph = TRUE, hardness = TRUE, temp = TRUE,
                               chloride = TRUE, salinity = TRUE, other_char = "null") {
  # create dataframe to store pair refs
  pair.ref <- data.frame(matrix(ncol = 6, nrow = 0))

  # name columns in pair.ref df
  colnames(pair.ref) <- c(
    "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText",
    "TADA.PairingGroup", "TADA.PairingGroupRank"
  )

  prep.ref <- function(.data) {
    .data <- .data %>%
      # group by characteristic and related fields
      dplyr::group_by(
        TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode,
        TADA.MethodSpeciationName, TADA.ResultSampleFractionText
      ) %>%
      # count number of results in TADA df for each group
      dplyr::mutate(NCount = length(TADA.ResultMeasureValue)) %>%
      # ungroup results
      dplyr::ungroup() %>%
      # retain required columns
      dplyr::select(
        TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode,
        TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
        NCount
      ) %>%
      # retain only distinct rows
      dplyr::distinct() %>%
      # arrange from largest to smallest number of results
      dplyr::arrange(dplyr::desc(NCount)) %>%
      # assign rank (largest NCount gets highest rank)
      dplyr::mutate(TADA.PairingGroup.Rank = dplyr::row_number()) %>%
      # remove NCount column
      dplyr::select(-NCount)
  }


  if (hardness == TRUE) {
    # create character reference from WQX characteristics containing "HARDNESS" in name
    char.ref <- TADA_GetCharacteristicRef() %>%
      dplyr::mutate(CharacteristicName = toupper(CharacteristicName)) %>%
      dplyr::filter(grepl("HARDNESS", CharacteristicName))

    # filter TADA df for hardness results
    hard.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% char.ref$CharacteristicName) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Hardness")

    # add hardness to pair.ref
    pair.ref <- rbind(pair.ref, hard.ref)

    # remove intermediate objects
    rm(char.ref, hard.ref)
  }

  if (ph == TRUE) {
    # filter TADA df for pH results
    ph.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName == "PH") %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "pH")

    # add pH to pair ref
    pair.ref <- rbind(pair.ref, ph.ref)

    # remove intermediate object
    rm(ph.ref)
  }

  if (temp == TRUE) {
    # filter TADA df for temperature results
    temp.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c("TEMPERATURE", "TEMPERATURE, WATER")) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Temperature")

    # add temperature to pair ref
    pair.ref <- rbind(pair.ref, temp.ref)

    # remove intermediate object
    rm(temp.ref)
  }

  if (salinity == TRUE) {
    # filter TADA df for salinity results
    salinity.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c("SALINITY")) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Salinity")

    # add salinity to pair ref
    pair.ref <- rbind(pair.ref, salinity.ref)

    # remove intermediate object
    rm(salinity.ref)
  }


  if (chloride == TRUE) {
    # filter TADA df for chloride results
    chloride.ref <- .data %>%
      dplyr::filter(TADA.CharacteristicName %in% c("CHLORIDE")) %>%
      prep.ref() %>%
      dplyr::mutate(TADA.PairingGroup = "Chloride")

    # add chloride to pair.ref
    pair.ref <- rbind(pair.ref, chloride.ref)

    # remove intermediate object
    rm(chloride.ref)
  }

  if (!is.data.frame(other_char) & other_char != "null") {
    stop("TADA_CreatePairRef: 'other_char' must be a dataframe with three columns. The first column
           contains TADA.CharacteristicName, the second column contains TADA.PairingGroup, and the
           third column contains TADA.PairingGroup.Rank")
  }

  # add code for adding other characteristic
  if (is.data.frame(other_char)) {
    pair.ref <- rbind(pair.ref, other_char)
  }

  # remove any duplicate rows
  pair.ref <- pair.ref %>%
    dplyr::distinct()

  # check to see if there are any rows in pair.ref

  if (nrow(pair.ref) == 0) {
    stop("None of the specified pairing characteristics were found in the TADA dataframe.")
  }

  return(pair.ref)
}

#' Pair Results for Numeric Criteria Calculation (UNDER ACTIVE DEVELOPMENT)
#'
#' This function pairs TADA results with results from specified characteristics from the same
#' MonitoringLocation within a user-specified time window to facilitate the calculation of numeric
#' criteria. The columns created by TADA_AutoClean are required to run this function. If they are not
#' present in the dataframe, the function will stop and print an error message.
#'
#' Users can provide a pairing reference file (can be created using TADA_CreatePairRef) to specify
#' which combinations of TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnit,
#' TADA.MethodSpeciationName, and TADA.ResultSampleFractionText should be used for hardness, pH,
#' temperature, salinity, chloride or other user-defined groups. If no ref is specified, all possible
#' combinations for hardness, pH, temperature, salinity and chloride will be used.
#'
#' @param .data TADA dataframe
#'
#' @param ref Write description of what columns need to be in this ref or null option
#'
#' @param hours_range Numeric argument. The time difference allowed between the paired characteristic
#' and the result.
#
#' @return A TADA dataframe with six additional columns added for each pairing group specified
#' in the pairing ref.
#' @export
#'
#' @examples
#'
#' AL_df <- TADA_DataRetrieval(
#'   startDate = "2010-11-30",
#'   endDate = "2010-12-01",
#'   statecode = "AL"
#' )
#'
#' AL_PairRef <- TADA_PairForCriteriaCalc(AL_df)
#'
TADA_PairForCriteriaCalc <- function(.data, ref = "null", hours_range = 4) {
  # check to see if user-supplied ref is a df
  if (!is.character(ref)) {
    if (!is.data.frame(ref)) {
      stop("TADA_PairForCriteriaCalc: 'ref' must be a dataframe with six columns: TADA.CharacteristicName,
         TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
         TADA.PairingGroup.Rank, and TADA.PairingGroup.")
    }

    if (is.data.frame(ref)) {
      col.names <- c(
        "TADA.CharacteristicName", "TADA.ResultMeasure.MeasureUnitCode",
        "TADA.MethodSpeciationName", "TADA.ResultSampleFractionText",
        "TADA.PairingGroup.Rank", "TADA.PairingGroup"
      )

      ref.names <- names(ref)

      if (length(setdiff(col.names, ref.names)) > 0) {
        stop("TADA_PairForCriteriaCalc: 'ref' must be a dataframe with six columns: TADA.CharacteristicName,
         TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName, TADA.ResultSampleFractionText,
         TADA.PairingGroup.Rank, and TADA.PairingGroup.")
      }
    }
  }

  # if user does not supply a pairing ref, create the default ref
  if (!is.data.frame(ref)) {
    if (ref == "null") {
      ref <- TADA_CreatePairRef(.data)
    }
  }


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
      dplyr::filter(
        TADA.CharacteristicName %in% ref.subset$TADA.CharacteristicName,
        !is.na(ActivityStartDateTime)
      ) %>%
      dplyr::select(
        TADA.CharacteristicName, TADA.ResultMeasureValue, TADA.ResultMeasure.MeasureUnitCode,
        ActivityIdentifier, MonitoringLocationIdentifier, ActivityStartDateTime,
        TADA.ResultSampleFractionText, TADA.MethodSpeciationName
      ) %>%
      dplyr::left_join(ref.subset,
        relationship = "many-to-many",
        dplyr::join_by(
          TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode,
          TADA.ResultSampleFractionText, TADA.MethodSpeciationName
        )
      ) %>%
      dplyr::rename_with(~ paste0("TADA.", group.id, "Name"), TADA.CharacteristicName) %>%
      dplyr::rename_with(~ paste0("TADA.", group.id, ".ResultMeasureValue"), TADA.ResultMeasureValue) %>%
      dplyr::rename_with(~ paste0("TADA.", group.id, ".MeasureUnitCode"), TADA.ResultMeasure.MeasureUnitCode) %>%
      dplyr::rename_with(~ paste0("TADA.", group.id, "ActivityStartDateTime"), ActivityStartDateTime) %>%
      dplyr::rename_with(~ paste0("TADA.", group.id, "ResultSampleFractionText"), TADA.ResultSampleFractionText) %>%
      dplyr::rename_with(~ paste0("TADA.", group.id, "MethodSpeciationName"), TADA.MethodSpeciationName)



    # identify dynamically named columns
    pair_char_name <- paste0("TADA.", group.id, "Name")
    pair_result_val <- paste0("TADA.", group.id, ".ResultMeasureValue")
    pair_units <- paste0("TADA.", group.id, ".MeasureUnitCode")
    pair_datetime <- paste0("TADA.", group.id, "ActivityStartDateTime")
    pair_fraction <- paste0("TADA.", group.id, "ResultSampleFractionText")
    pair_speciation <- paste0("TADA.", group.id, "MethodSpeciationName")

    # pair by activity id
    pair.activityid <- .data %>%
      dplyr::filter(ActivityIdentifier %in% pair.subset$ActivityIdentifier) %>%
      dplyr::left_join(pair.subset,
        by = dplyr::join_by(ActivityIdentifier),
        relationship = "many-to-many"
      ) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::slice_min(order_by = TADA.PairingGroup.Rank) %>%
      dplyr::ungroup() %>%
      dplyr::select(-TADA.PairingGroup.Rank) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::slice_sample(n = 1) %>%
      dplyr::select(
        ResultIdentifier,
        !!rlang::sym(pair_datetime),
        !!rlang::sym(pair_result_val),
        !!rlang::sym(pair_units),
        !!rlang::sym(pair_fraction),
        !!rlang::sym(pair_speciation)
      )

    # drop activity id from pair subset
    pair.subset2 <- pair.subset %>%
      dplyr::select(-ActivityIdentifier)

    # pair by monitoring location and time
    pair.ml.time <- .data %>%
      dplyr::filter(
        !ResultIdentifier %in% pair.activityid$ResultIdentifier,
        !is.na(ActivityStartDateTime),
        MonitoringLocationIdentifier %in% pair.subset$MonitoringLocationIdentifier
      ) %>%
      dplyr::left_join(pair.subset2,
        relationship = "many-to-many",
        by = dplyr::join_by(MonitoringLocationIdentifier)
      ) %>%
      dplyr::group_by(ResultIdentifier) %>%
      # Figure out fastest time comparison method - needs to be absolute time comparison
      dplyr::mutate(timediff = abs(difftime(as.POSIXct(!!rlang::sym(pair_datetime)), as.POSIXct(ActivityStartDateTime), units = c("hours")))) %>%
      dplyr::filter(timediff <= hours_range) %>%
      dplyr::group_by(ResultIdentifier) %>%
      dplyr::arrange(ResultIdentifier, TADA.PairingGroup.Rank, dplyr::desc(timediff)) %>%
      dplyr::slice_min(TADA.PairingGroup.Rank) %>%
      dplyr::slice_min(timediff) %>%
      dplyr::ungroup() %>%
      dplyr::select(-timediff, -TADA.PairingGroup.Rank) %>%
      dplyr::select(
        ResultIdentifier,
        !!rlang::sym(pair_datetime),
        !!rlang::sym(pair_result_val),
        !!rlang::sym(pair_units),
        !!rlang::sym(pair_fraction),
        !!rlang::sym(pair_speciation)
      ) %>%
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

  rm(all.groups)


  return(.data)
}
