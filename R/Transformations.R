#' Harmonize Synonyms
#'
#' This function joins a synonym reference table to the dataset to convert
#' synonymous data to a unified naming format for easier aggregation, analysis,
#' and visualization. Users may populate the function with a dataset-specific
#' synonym table created from TADA_GetSynonymRef and reviewed/customized by the
#' user (recommended), or the default TADA-provided synonym table, containing
#' suggested synonym naming for some priority characteristics. Where a suggested
#' characteristic name, fraction, speciation, or unit is present, the function
#' will convert the TADA.CharacteristicName, TADA.ResultSampleFractionText,
#' TADA.MethodSpeciationName, and/or TADA.ResultMeasure.MeasureUnitCode to
#' the target format. In cases where a target unit or speciation differs from
#' the existing unit or speciation, the reference table will also apply
#' multiplication conversion factors to the TADA.ResultMeasureValue.
#'
#' @param .data TADA dataframe
#' @param ref Optional argument to specify which dataframe to use as a reference
#' file. The primary use for this argument is when a user has generated a
#' synonym reference file unique to their data, and they made changes to
#' that file.
#' @param np_speciation Boolean. Determines whether the user wants to convert
#'   nitrogen and phosphorus subspecies to speciation 'as N' and 'as P', where
#'   speciation conversions are provided. Defaults to TRUE. For example, if
#'   np_speciation is TRUE, all Nitrate with TADA.MethodSpeciationName = as
#'   NO3 will be converted to as N using molecular weight conversion factors.
#'
#' @return The input TADA dataframe with the TADA.CharacteristicName,
#'   TADA.ResultSampleFractionText, TADA.MethodSpeciationName, and
#'   TADA.ResultMeasure.MeasureUnitCode columns converted to the target values,
#'   if supplied. Also includes additional columns
#'   TADA.CharacteristicNameAssumptions, TADA.FractionAssumptions, and
#'   TADA.SpeciationAssumptions populated with additional notes about the conversion
#'   logic, and a TADA.Harmonized.Flag, indicating whether TADA columns were
#'   changed in this function.
#'
#' @export
#'
#' @examples
#' # Load example dataset:
#' data(Data_6Tribes_5y)
#'
#' # Create a synonym reference table for flagged, cleaned dataframe:
#' Data_6Tribes_5yClean <- subset(Data_6Tribes_5y, !is.na(Data_6Tribes_5y$TADA.ResultMeasureValue))
#' Data_6Tribes_5yClean <- TADA_FlagFraction(Data_6Tribes_5yClean, clean = TRUE)
#' Data_6Tribes_5yClean <- TADA_FlagResultUnit(Data_6Tribes_5yClean, clean = "invalid_only")
#' Data_6Tribes_5yClean <- TADA_FlagSpeciation(Data_6Tribes_5yClean, clean = "invalid_only")
#' Data_6Tribes_5yClean <- TADA_FlagMethod(Data_6Tribes_5yClean, clean = TRUE)
#' CreateRefTable <- TADA_GetSynonymRef(Data_6Tribes_5yClean)
#'
#' # Append synonym reference table columns to dataframe and transform/convert
#' # data to the USER SUPPLIED reference table values:
#' Data_6Tribes_5yClean_Harmonized <- TADA_HarmonizeSynonyms(Data_6Tribes_5yClean, ref = CreateRefTable)
TADA_HarmonizeSynonyms <- function(.data, ref, np_speciation = TRUE) {
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")

  # check .data has the required columns
  expected_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)

  # define which columns are expected in ref
  expected_ref_cols <- c(
    "TADA.CharacteristicName",
    "Target.TADA.CharacteristicName",
    "TADA.CharacteristicNameAssumptions",
    "TADA.ResultSampleFractionText",
    "Target.TADA.ResultSampleFractionText",
    "TADA.FractionAssumptions",
    "TADA.MethodSpeciationName",
    "Target.TADA.MethodSpeciationName",
    "TADA.SpeciationAssumptions",
    "Target.TADA.SpeciationConversionFactor",
    "TADA.ResultMeasure.MeasureUnitCode",
    "Target.TADA.ResultMeasure.MeasureUnitCode",
    "Target.TADA.UnitConversionFactor",
    "Target.TADA.UnitConversionCoefficient",
    "TADA.UnitConversionRef",
    "HarmonizationGroup"
  )

  # if class(ResultMeasureValue) != numeric, run special char function - EDH - should not be needed at this point but doesn't hurt.
  if (!is.numeric(.data$TADA.ResultMeasureValue)) {
    stop("TADA.ResultMeasureValue is not numeric. This column must be numeric before proceeding.")
  }

  # define harm.ref
  # if input for ref exists, use that data
  if (!missing(ref)) {
    # check ref is data.frame
    TADA_CheckType(ref, "data.frame")

    # check ref has all of the required columns
    TADA_CheckColumns(ref, expected_ref_cols)

    harm.ref <- ref
  }

  # if input for ref does not exist, use raw harmonization template
  if (missing(ref)) {
    # use output of TADA_GetSynonymRef which uses the TADA HarmonizationTemplate.csv in the extdata folder
    harm.ref <- TADA_GetSynonymRef(.data)
  }

  # find places where metadata will be changed and add targets
  harm.ref$TADA.Harmonized.Flag <- ifelse(!is.na(harm.ref$Target.TADA.CharacteristicName) | !is.na(harm.ref$Target.TADA.ResultSampleFractionText) | !is.na(harm.ref$Target.TADA.MethodSpeciationName) | !is.na(harm.ref$Target.TADA.ResultMeasure.MeasureUnitCode), TRUE, FALSE)

  .data <- .data[, !names(.data) %in% c("TADA.ComparableDataIdentifier")]

  # join harm.ref to .data
  flag.data <- merge(.data, harm.ref,
    by = expected_cols[!expected_cols %in% "TADA.ResultMeasureValue"],
    all.x = TRUE
  )

  # TADA.CharacteristicName
  # replace TADA.CharacteristicName with Target.TADA.CharacteristicName
  clean.data <- flag.data %>%
    # use TADA suggested name where there is a suggested name, use original name if no suggested name
    dplyr::mutate(TADA.CharacteristicName = dplyr::case_when(
      !is.na(Target.TADA.CharacteristicName) ~ Target.TADA.CharacteristicName,
      is.na(Target.TADA.CharacteristicName) ~ TADA.CharacteristicName
    ))

  # TADA.ResultSampleFractionText
  # replace ResultSampleFractionText with Target.TADA.ResultSampleFractionText
  clean.data <- clean.data %>%
    # use TADA suggested frac where there is a suggested frac, use original frac if no suggested frac
    dplyr::mutate(TADA.ResultSampleFractionText = dplyr::case_when(
      !is.na(Target.TADA.ResultSampleFractionText) ~ Target.TADA.ResultSampleFractionText,
      !is.na(TADA.ResultSampleFractionText) & is.na(Target.TADA.ResultSampleFractionText) & !is.na(TADA.FractionAssumptions) ~ Target.TADA.ResultSampleFractionText,
      is.na(Target.TADA.ResultSampleFractionText) ~ TADA.ResultSampleFractionText
    ))

  # Handle instances with DO where the speciation is listed "AS O2" but it should be NA
  clean.data$TADA.MethodSpeciationName <- ifelse(!is.na(clean.data$TADA.MethodSpeciationName) & is.na(clean.data$Target.TADA.MethodSpeciationName) & !is.na(clean.data$TADA.SpeciationAssumptions), clean.data$Target.TADA.MethodSpeciationName, clean.data$TADA.MethodSpeciationName)

  # ResultMeasure.MeasureUnitCode
  # replace ResultMeasure.MeasureUnitCode with Target.TADA.ResultMeasure.MeasureUnitCode
  clean.data <- clean.data %>%
    # use TADA suggested unit where there is a suggested unit, use original unit if no suggested unit
    dplyr::mutate(TADA.ResultMeasure.MeasureUnitCode = dplyr::case_when(
      !is.na(Target.TADA.ResultMeasure.MeasureUnitCode) ~ Target.TADA.ResultMeasure.MeasureUnitCode,
      is.na(Target.TADA.ResultMeasure.MeasureUnitCode) ~ TADA.ResultMeasure.MeasureUnitCode
    )) %>%
    # if conversion factor exists, multiply by ResultMeasureValue
    dplyr::rowwise() %>%
    dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
      !is.na(Target.TADA.UnitConversionFactor) & !is.na(Target.TADA.ResultMeasure.MeasureUnitCode) &
        !is.na(Target.TADA.UnitConversionCoefficient)
      ~ ((Target.TADA.UnitConversionFactor * TADA.ResultMeasureValue) + Target.TADA.UnitConversionCoefficient),
      !is.na(Target.TADA.UnitConversionFactor) & !is.na(Target.TADA.ResultMeasure.MeasureUnitCode) &
        is.na(Target.TADA.UnitConversionCoefficient)
      ~ ((Target.TADA.UnitConversionFactor * TADA.ResultMeasureValue)),
      is.na(Target.TADA.UnitConversionFactor) ~ TADA.ResultMeasureValue
    ))

  # TADA.MethodSpeciationName
  # replace MethodSpeciationName with Target.TADA.MethodSpeciationName

  if (np_speciation == TRUE) {
    clean.data <- clean.data %>%
      # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
      dplyr::mutate(TADA.MethodSpeciationName = dplyr::case_when(
        !is.na(Target.TADA.MethodSpeciationName) ~ Target.TADA.MethodSpeciationName,
        is.na(Target.TADA.MethodSpeciationName) ~ TADA.MethodSpeciationName
      )) %>%
      # if conversion factor exists, multiply by ResultMeasureValue
      dplyr::rowwise() %>%
      dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
        !is.na(Target.TADA.SpeciationConversionFactor) ~
          (Target.TADA.SpeciationConversionFactor * TADA.ResultMeasureValue),
        is.na(Target.TADA.SpeciationConversionFactor) ~ TADA.ResultMeasureValue
      ))
  } else {
    clean.data <- clean.data %>%
      # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
      dplyr::mutate(TADA.MethodSpeciationName = dplyr::case_when(
        !is.na(Target.TADA.MethodSpeciationName) & is.na(Target.TADA.SpeciationConversionFactor) ~ Target.TADA.MethodSpeciationName,
        is.na(Target.TADA.MethodSpeciationName) ~ TADA.MethodSpeciationName
      ))
  }

  # remove conversion columns
  clean.data <- clean.data %>%
    dplyr::select(-c(
      "Target.TADA.CharacteristicName",
      "Target.TADA.ResultSampleFractionText",
      "Target.TADA.MethodSpeciationName",
      "Target.TADA.SpeciationConversionFactor",
      "Target.TADA.ResultMeasure.MeasureUnitCode",
      "Target.TADA.UnitConversionFactor",
      "Target.TADA.UnitConversionCoefficient",
      "TADA.UnitConversionRef",
      "HarmonizationGroup"
    ))

  clean.data$TADA.Harmonized.Flag <- ifelse(is.na(clean.data$TADA.Harmonized.Flag), FALSE, clean.data$TADA.Harmonized.Flag)

  # return clean.data
  clean.data <- TADA_CreateComparableID(clean.data)
  clean.data <- TADA_OrderCols(clean.data)
  return(clean.data)
}


#' Calculate Total Nitrogen and Phosphorus
#'
#' This function applies the [Nutrient Aggregation logic](https://echo.epa.gov/trends/loading-tool/resources/nutrient-aggregation#nitrogen)
#' from ECHO's Water Pollutant Loading Tool to add nitrogen subspecies together
#' to approximate a total nitrogen value on a single day at a single site.
#' Before summing subspecies, this function runs TADA_AggregateMeasurements to
#' obtain the max value of a characteristic-fraction-speciation at a given site,
#' date, and depth. Where necessary, it uses conversion factors to convert
#' nitrogen subspecies expressed as nitrate, nitrite, ammonia, ammonium, etc. to
#' as nitrogen based on the atomic weights of the different elements in the
#' compound. The reference table is contained within the package but may be
#' edited/customized by users. Nutrient equations are as follows:
#'
#' NITROGEN:
#' 1. TOTAL N (UNFILTERED)
#' 2. TOTAL N (FILTERED) + TOTAL N (PARTICULE)
#' 3. TOTAL KJELDAHL NITROGEN + NITRATE + NITRITE
#' 4. ORGANIC N + AMMONIA + NITRATE + NITRITE
#' 5. OTHER NITROGEN FORMS
#'
#' PHOSPHORUS:
#' 1. TOTAL PHOSPHORUS
#' 2. PHOSPHATE
#' 3. OTHER PHOSPHORUS FORMS
#'
#' Equations are applied in the order above. The function looks for groups of
#' nutrients that exactly match each equation before looking for every
#' combination within each equation (for example, a group of nitrogen subspecies
#' including AMMONIA and NITRATE will be passed over in an intial sweep of
#' groups of subspecies containing ORG N, AMMONIA, NITRATE, and NITRITE, but
#' will be caught as the function moves down the hierarchy of equations to fewer
#' and fewer subspecies). Eventually, even groups with only one subspecies will
#' be used to represent a TOTAL N value for that site/day/depth.
#'
#' @param .data TADA dataframe, ideally harmonized using TADA_HarmonizeSynonyms.
#'   If user wants to consider grouping N or P subspecies across multiple
#'   organizations, user should have run TADA_FindNearbySites and grouped all
#'   nearby sites to one common MonitoringLocationIdentifier,
#'   TADA.LatitudeMeasure, TADA.LongitudeMeasure, etc.
#' @param sum_ref Optional. A custom summation reference dataframe the user has
#'   loaded into the R environment. Dataframe must have same columns as default
#'   TADA.summation reference table.
#' @param daily_agg The function used to aggregate to a single
#'   characteristic-unit-fraction-speciation at the same location and depth on
#'   the same day for multiple measurements. Defaults to 'max', but can be set
#'   to 'min' or 'mean'.
#'
#' @return Input TADA dataframe with additional rows representing total N and P
#'   summation values from adding up subspecies. Note that for total phosphorus,
#'   these additional rows are simply a re-classification of phosphorus or
#'   phosphate into the total phosphorus as P format. These new rows share the
#'   same date and monitoring location as the subspecies, but an additional note
#'   is added in the TADA.NutrientSummation.Flag column describing how the total
#'   was derived. Also adds TADA.NutrientSummationGroup and
#'   TADA.NutrientSummationEquation columns, which can be used to trace how the
#'   total was calculated and from which subspecies.
#'
#' @export

TADA_CalculateTotalNP <- function(.data, sum_ref, daily_agg = c("max", "min", "mean")) {
  # check to make sure daily_agg is populated with allowable value
  daily_agg <- match.arg(daily_agg)

  # check required columns for TADA dataset
  req_cols <- c(
    "TADA.CharacteristicName",
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpeciationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "TADA.ResultMeasureValue",
    "ActivityStartDate",
    "MonitoringLocationIdentifier",
    "ActivityTypeCode"
  )
  TADA_CheckColumns(.data, expected_cols = req_cols)

  # bring in custom reference df if provided
  if (!missing(sum_ref)) {
    ref_cols <- names(TADA_GetNutrientSummationRef())
    TADA_CheckColumns(sum_ref, expected_cols = ref_cols)
  } else {
    sum_ref <- TADA_GetNutrientSummationRef()
  }

  # check if QC flag function run and print warning if not
  if (!"TADA.ActivityType.Flag" %in% names(.data)) {
    "Warning: TADA dataset does not have the TADA.ActivityType.Flag column, which indicates QC replicates have not been handled/reviewed. This function is not built to handle QC replicate samples and will use them to aggregate to a daily max and total nutrient value."
  }

  # Get grouping cols for daily aggregation
  # create nutrient groups by site, date, and depth
  depths <- names(.data)[grepl("DepthHeightMeasure", names(.data))]
  depths <- depths[grepl("TADA.", depths)]
  grpcols <- c(
    "ActivityStartDate",
    # "ActivityStartDateTime", #does not make sense to include for daily agg
    "ActivityRelativeDepthName",
    "MonitoringLocationIdentifier",
    "MonitoringLocationName",
    "TADA.LongitudeMeasure",
    "TADA.LatitudeMeasure",
    "ActivityMediaSubdivisionName",
    "TADA.ActivityMediaName",
    "TADA.ComparableDataIdentifier",
    "TADA.ResultMeasure.MeasureUnitCode",
    depths
  )

  dat <- suppressMessages(TADA_AggregateMeasurements(.data, grouping_cols = grpcols, agg_fun = daily_agg, clean = TRUE))

  # join data to summation table and keep only those that match for summations
  sum_dat <- merge(dat, sum_ref, all.x = TRUE)
  sum_dat <- subset(sum_dat, !is.na(sum_dat$NutrientGroup))

  ## REMINDER FOR TADA TEAM: NEED TO ENSURE ALL COMBOS PRESENT IN TABLE

  # If the join results in matching rows
  if (dim(sum_dat)[1] > 0) {
    thecols <- grpcols[!grpcols %in% c("TADA.ComparableDataIdentifier")]

    # # find nearby sites
    # nearsites = unique(sum_dat[,c("MonitoringLocationIdentifier","TADA.LatitudeMeasure","TADA.LongitudeMeasure")])
    # nearsites = TADA_FindNearbySites(nearsites)
    # nearsites = subset(nearsites, !nearsites$TADA.NearbySiteGroups%in%c("No nearby sites"))

    # create nutrient group ID's.
    sum_dat <- sum_dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(thecols))) %>%
      dplyr::mutate(TADA.NutrientSummationGroup = dplyr::cur_group_id())

    # bring in equations
    eqns <- utils::read.csv(system.file("extdata", "NP_equations.csv", package = "TADA"))


    # dataframe to hold results
    summeddata <- data.frame()
    grps <- vector()

    for (i in 1:length(unique(eqns$Nutrient))) {
      nut <- unique(eqns$Nutrient)[i]
      nutqns <- subset(eqns, eqns$Nutrient == nut)
      for (j in 1:length(unique(nutqns$EQN))) {
        eqnum <- unique(nutqns$EQN)[j]
        eqn <- subset(nutqns, nutqns$EQN == eqnum)$SummationName
        nutrient <- ifelse(nut == "N", "Total Nitrogen as N", "Total Phosphorus as P")
        # for each equation, see if any groups contain all required subspecies, and for each pick the variant with the lowest rank.
        # combine group with other groups and remove group ID from consideration for the next equation
        out <- sum_dat %>%
          dplyr::filter(!TADA.NutrientSummationGroup %in% grps) %>%
          dplyr::group_by(TADA.NutrientSummationGroup) %>%
          dplyr::filter(all(eqn %in% SummationName)) %>% # this line ensures that ALL subspecies are present within an equation group, not just one or more
          dplyr::filter(SummationName %in% eqn) %>%
          dplyr::mutate(TADA.NutrientSummationEquation = paste0(unique(SummationName), collapse = " + "))

        out <- out %>%
          dplyr::group_by(TADA.NutrientSummationGroup, SummationName) %>%
          dplyr::slice_min(SummationRank, with_ties = FALSE)
        out$TADA.NutrientSummation.Flag <- paste0("Used to calculate ", nutrient, ".")
        out$nutrient <- nutrient
        summeddata <- plyr::rbind.fill(summeddata, out)
        grps <- c(grps, unique(out$TADA.NutrientSummationGroup))
      }
    }

    # check to make sure group isn't entirely non-detects


    # Convert speciation if needed
    summeddata$TADA.ResultMeasureValue <- ifelse(!is.na(summeddata$SummationSpeciationConversionFactor), summeddata$TADA.ResultMeasureValue * summeddata$SummationSpeciationConversionFactor, summeddata$TADA.ResultMeasureValue)
    summeddata$TADA.MethodSpeciationName <- ifelse(!is.na(summeddata$SummationSpeciationConversionFactor) & summeddata$nutrient == "Total Nitrogen as N", "AS N", summeddata$TADA.MethodSpeciationName)
    summeddata$TADA.MethodSpeciationName <- ifelse(!is.na(summeddata$SummationSpeciationConversionFactor) & summeddata$nutrient == "Total Phosphorus as P", "AS P", summeddata$TADA.MethodSpeciationName)

    # Get to total N or P

    totncols <- c(thecols, "TADA.NutrientSummationGroup", "TADA.NutrientSummationEquation")
    TotalN <- summeddata %>%
      dplyr::filter(nutrient == "Total Nitrogen as N") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) %>%
      dplyr::summarise(TADA.ResultMeasureValue = sum(TADA.ResultMeasureValue)) %>%
      dplyr::mutate(TADA.CharacteristicName = "TOTAL NITROGEN, MIXED FORMS", TADA.ResultSampleFractionText = "UNFILTERED", TADA.MethodSpeciationName = "AS N", TADA.NutrientSummation.Flag = "Nutrient summation from one or more subspecies.")
    TotalP <- summeddata %>%
      dplyr::filter(nutrient == "Total Phosphorus as P") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) %>%
      dplyr::summarise(TADA.ResultMeasureValue = sum(TADA.ResultMeasureValue)) %>%
      dplyr::mutate(TADA.CharacteristicName = "TOTAL PHOSPHORUS, MIXED FORMS", TADA.ResultSampleFractionText = "UNFILTERED", TADA.MethodSpeciationName = "AS P", TADA.NutrientSummation.Flag = "Nutrient summation from one subspecies.")

    # if summation is zero....include anyway?

    Totals <- plyr::rbind.fill(TotalN, TotalP)
    Totals$ResultIdentifier <- paste0("TADA-", sample(100000000:1000000000, dim(Totals)[1])) # give TADA ResultIdentifier

    # combine all data back into input dataset and get rid of unneeded columns
    .data <- merge(.data, summeddata, all.x = TRUE)
    .data <- plyr::rbind.fill(.data, Totals)
    .data <- .data %>% dplyr::select(-c(SummationFractionNotes, SummationSpeciationNotes, SummationSpeciationConversionFactor, SummationName, SummationRank, SummationNote, nutrient, NutrientGroup))
    .data$TADA.NutrientSummation.Flag[is.na(.data$TADA.NutrientSummation.Flag)] <- "Not used to calculate Total N or P."
  } else {
    # if there are no data to sum
    .data$TADA.NutrientSummation.Flag <- "Not used to calculate Total N or P."
    print("No Total N or P subspecies exist in dataset. Returning input dataset with TADA.NutrientSummation.Flag set to 'Not used to calculate Total N or P'")
  }

  # order columns
  .data <- TADA_CreateComparableID(.data)
  .data <- TADA_OrderCols(.data)
}
