#' Generate Unique Synonym Reference Table
#'
#' Function generates a synonym reference table containing all unique
#' combinations of TADA.CharacteristicName, TADA.ResultSampleFractionText,
#' TADA.MethodSpecificationName, and TADA.ResultMeasure.MeasureUnitCode. The
#' function also joins in some TADA-specific suggested synonyms for nutrients
#' and priority parameters. These target synonyms (denoted in the reference
#' table with the prefix "Target.") are intended to help the user aggregate
#' synonymous data that may be uploaded with slightly different metadata
#' conventions and prepare nutrient data for total N and P summations. Users can
#' review how their input data relates to target synonyms for
#' TADA.CharacteristicName, TADA.ResultSampleFractionText,
#' TADA.MethodSpecificationName, and TADA.ResultMeasure.MeasureUnitCode. Once
#' the synonym table is created, users may optionally edit the target columns in
#' the reference table to meet their needs. Additionally, the function assumes
#' the user has already removed any data containing invalid
#' characteristic-unit-fraction-speciation combinations (i.e. user has already
#' run TADA_FlagFraction, TADA_FlagSpeciation, TADA_FlagResultUnit, etc.).
#' 
#' @param .data TADA dataframe. If a data frame is not provided, the function will return the default internal reference table.
#' 
#' @param download Boolean argument; when download = TRUE, the output is
#' downloaded to the current working directory.
#'
#' @return Synonym Reference Table unique to the input dataframe
#'
#' @export
#' 
#' @examples 
#' # Load example dataset:
#' data(Data_6Tribes_5y)
#' 
#' # Create a synonym reference table for flagged, cleaned dataframe:
#' Data_6Tribes_5yClean = subset(Data_6Tribes_5y, !is.na(Data_6Tribes_5y$TADA.ResultMeasureValue))
#' Data_6Tribes_5yClean = TADA_FlagFraction(Data_6Tribes_5yClean, clean = TRUE)
#' Data_6Tribes_5yClean = TADA_FlagResultUnit(Data_6Tribes_5yClean, clean = "invalid_only")
#' Data_6Tribes_5yClean = TADA_FlagSpeciation(Data_6Tribes_5yClean, clean = "invalid_only")
#' Data_6Tribes_5yClean = TADA_FlagMethod(Data_6Tribes_5yClean, clean = TRUE)
#' CreateRefTable <- TADA_GetSynonymRef(Data_6Tribes_5yClean)
#' 
#' # Create and download (to your working directory) a synonym reference
#' # table for dataframe: 
#' \dontrun{
#' DownloadRefTable <- TADA_GetSynonymRef(Data_6Tribes_5yClean, download = TRUE)
#' }
#' 
#' # Get internal synonym reference table
#' reference <- TADA_GetSynonymRef()

TADA_GetSynonymRef <- function(.data, download = FALSE) {
  
  if(missing(.data)){
    ref = utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv", package = "TADA"))
    return(ref)
  }
  
  # check .data is data.frame
  TADA_CheckType(.data, "data.frame", "Input object")
  # check download is boolean
  TADA_CheckType(download, "logical")
  
  # check .data has the required columns
  expected_cols <- c(
    "TADA.ActivityMediaName",
    "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
    "TADA.MethodSpecificationName",
    "TADA.ResultMeasure.MeasureUnitCode"
    )
  TADA_CheckColumns(.data, expected_cols)
  
  if(!any(c("TADA.MethodSpeciation.Flag","TADA.SampleFraction.Flag","TADA.ResultUnit.Flag")%in%names(.data))){
    print("Warning: This dataframe is missing TADA QC flagging columns, indicating that you have not yet run the TADA_FlagResultUnit, TADA_FlagFraction, or TADA_FlagSpeciation functions. It is highly recommended you run these flagging functions and remove Invalid combinations before proceeding to this step.")
  }
  
  # check to see if any invalid data flags exist
  check_inv = .data[,names(.data)%in%c("TADA.MethodSpeciation.Flag","TADA.SampleFraction.Flag","TADA.ResultUnit.Flag")]
  check_inv = check_inv %>% tidyr::pivot_longer(cols = names(check_inv), names_to = "Flag Column") %>% dplyr::filter(value == "Invalid")
  
  if(dim(check_inv)[1]>0){
    check_inv = check_inv %>% dplyr::group_by(Flag) %>% dplyr::summarise("Result Count" = length(value))
    print("Warning: Your dataframe contains invalid metadata combinations in the following flag columns:")
    print(as.data.frame(check_inv))
  }
  
  # execute function after checks are passed
  # define raw harmonization table as an object
  harm.raw <- utils::read.csv(system.file("extdata", "HarmonizationTemplate.csv", package = "TADA"))
  
  join.data <- merge(unique(.data[, expected_cols]),
                     harm.raw,
                     by = expected_cols,
                     all.x = TRUE)
  
  # trim join.data to include only unique combos of char-frac-spec-unit
  unique.data <- join.data %>% dplyr::distinct()
  
  unique.data = unique.data[,names(harm.raw)]
  
  # if download = TRUE, download unique.data as a csv to the working directory
  if (download == TRUE) {
    utils::write.csv(unique.data, "HarmonizationRefTable.csv", row.names = FALSE)
  }
  
  # return unique.data
  return(unique.data)
}

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
#' TADA.MethodSpecificationName, and/or TADA.ResultMeasure.MeasureUnitCode to
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
#'   np_speciation is TRUE, all Nitrate with TADA.MethodSpecificationName = as
#'   NO3 will be converted to as N using molecular weight conversion factors.
#' 
#' @return The input TADA dataframe with the TADA.CharacteristicName,
#'   TADA.ResultSampleFractionText, TADA.MethodSpecificationName, and
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
#' Data_6Tribes_5yClean = subset(Data_6Tribes_5y, !is.na(Data_6Tribes_5y$TADA.ResultMeasureValue))
#' Data_6Tribes_5yClean = TADA_FlagFraction(Data_6Tribes_5yClean, clean = TRUE)
#' Data_6Tribes_5yClean = TADA_FlagResultUnit(Data_6Tribes_5yClean, clean = "invalid_only")
#' Data_6Tribes_5yClean = TADA_FlagSpeciation(Data_6Tribes_5yClean, clean = "invalid_only")
#' Data_6Tribes_5yClean = TADA_FlagMethod(Data_6Tribes_5yClean, clean = TRUE)
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
    "TADA.ActivityMediaName",
    "TADA.CharacteristicName", "TADA.ResultSampleFractionText",
    "TADA.MethodSpecificationName",
    "TADA.ResultMeasureValue",
    "TADA.ResultMeasure.MeasureUnitCode"
  )
  TADA_CheckColumns(.data, expected_cols)
  
  # additional columns that may be in harmonization ref
  # columns to keep from .data if exist
  # harmonization_cols <- c(
  #   "TADA.SampleFraction.Flag", "TADA.MethodSpeciation.Flag",
  #   "TADA.ResultUnit.Flag", "TADA.AnalyticalMethod.Flag"
  # )
  
  
  #define which columns are expected in ref
  expected_ref_cols <- c(
    "TADA.ActivityMediaName",
    "TADA.CharacteristicName", 
    "TADA.ResultSampleFractionText",
    "TADA.MethodSpecificationName",
    "TADA.ResultMeasure.MeasureUnitCode",
    "Target.TADA.CharacteristicName", 
    "Target.TADA.ResultSampleFractionText",
    "Target.TADA.MethodSpecificationName",
    "Target.TADA.ResultMeasure.MeasureUnitCode",
    "Target.TADA.SpeciationConversionFactor",
    "Target.TADA.UnitConversionFactor"
  )
  
  # if class(ResultMeasureValue) != numeric, run special char function - EDH - should not be needed at this point but doesn't hurt.
  if (!is.numeric(.data$TADA.ResultMeasureValue) ) {
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
    # use output of HarmonizationRefTable which uses the TADA HarmonizationTemplate.csv in the extdata folder
    harm.ref <- TADA_GetSynonymRef(.data, download=FALSE)
  }
  
  # find places where metadata will be changed and add FLAG column.
  harm.ref$TADA.Harmonized.Flag = ifelse(!is.na(harm.ref$Target.TADA.CharacteristicName)|!is.na(harm.ref$Target.TADA.ResultSampleFractionText)|!is.na(harm.ref$Target.TADA.MethodSpecificationName)|!is.na(harm.ref$Target.TADA.ResultMeasure.MeasureUnitCode),TRUE,FALSE) 

  .data = .data[,!names(.data)%in%c("TADA.ComparableDataIdentifier")]
  
  # # join by expected cols and (if in dataset) harmonization cols
  # joincols = names(.data)[names(.data)%in%c(expected_cols, harmonization_cols)]
  
  # join harm.ref to .data - EDH - there are some columns ("ResultAnalyticalMethod.MethodName","SampleCollectionMethod.MethodName","ResultCommentText","MonitoringLocationTypeName")in the example harmonization table that are also in .data that would result in erroneous joins if not excluded from join below
  flag.data <- merge(.data, harm.ref,
                     by = expected_cols[!expected_cols%in%"TADA.ResultMeasureValue"],
                     all.x = TRUE)
  
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
      is.na(Target.TADA.ResultSampleFractionText) ~ TADA.ResultSampleFractionText
    ))
  
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
      !is.na(Target.TADA.UnitConversionFactor) ~
        (Target.TADA.UnitConversionFactor * TADA.ResultMeasureValue),
      is.na(Target.TADA.UnitConversionFactor) ~ TADA.ResultMeasureValue
    ))
  
  # TADA.MethodSpecificationName
  # replace MethodSpecificationName with Target.TADA.MethodSpecificationName
  
  if(np_speciation==TRUE){
    clean.data <- clean.data %>%
      # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
      dplyr::mutate(TADA.MethodSpecificationName = dplyr::case_when(
        !is.na(Target.TADA.MethodSpecificationName) ~ Target.TADA.MethodSpecificationName,
        is.na(Target.TADA.MethodSpecificationName) ~ TADA.MethodSpecificationName
      )) %>%
      # if conversion factor exists, multiply by ResultMeasureValue
      dplyr::rowwise() %>%
      dplyr::mutate(TADA.ResultMeasureValue = dplyr::case_when(
        !is.na(Target.TADA.SpeciationConversionFactor) ~
          (Target.TADA.SpeciationConversionFactor * TADA.ResultMeasureValue),
        is.na(Target.TADA.SpeciationConversionFactor) ~ TADA.ResultMeasureValue
      ))
      
  }else{
    clean.data <- clean.data %>%
      # use TADA suggested spec where there is a suggested spec, use original spec if no suggested spec
      dplyr::mutate(TADA.MethodSpecificationName = dplyr::case_when(
        !is.na(Target.TADA.MethodSpecificationName) & is.na(Target.TADA.SpeciationConversionFactor) ~ Target.TADA.MethodSpecificationName,
        is.na(Target.TADA.MethodSpecificationName) ~ TADA.MethodSpecificationName
      ))
  }
  
  # remove conversion columns
  clean.data <- clean.data %>%
    dplyr::select(-c(
      "Target.TADA.CharacteristicName", 
      "Target.TADA.ResultSampleFractionText",
      "Target.TADA.MethodSpecificationName",
      "Target.TADA.ResultMeasure.MeasureUnitCode",
      "Target.TADA.SpeciationConversionFactor",
      "Target.TADA.UnitConversionFactor",
      "Target.TADA.SpeciationConversionFactor",
      "Target.TADA.UnitConversionFactor",
      "HarmonizationGroup"
    ))
  
  clean.data$TADA.Harmonized.Flag = ifelse(is.na(clean.data$TADA.Harmonized.Flag),FALSE,clean.data$TADA.Harmonized.Flag)
  
  # return clean.data
  clean.data = TADA_CreateComparableID(clean.data)
  clean.data <- TADA_OrderCols(clean.data)
  return(clean.data)
}


#' Calculate Total Nitrogen or Phosphorus
#' 
#' This function uses the [Nutrient Aggregation logic](https://echo.epa.gov/trends/loading-tool/resources/nutrient-aggregation#nitrogen)
#' from ECHO's Water Pollutant Loading Tool to add nitrogen subspecies together
#' to approximate a total nitrogen value on a single day at a single site. Where
#' necessary, it uses conversion factors to convert nitrogen subspecies
#' expressed as nitrate, nitrite, ammonia, ammonium, etc. to as nitrogen based
#' on the atomic weights of the different elements in the compound. The
#' reference table is contained within the package but may be edited/customized
#' by users. Future development may include total P summations as well.
#' 
#' @param .data TADA dataframe. If user wants to consider grouping nitrogen
#'   subspecies across multiple organizations, user should have run
#'   TADA_FindNearbySites and grouped all nearby sites to one common
#'   MonitoringLocationIdentifier, TADA.LatitudeMeasure, TADA.LonigtudeMeasure,
#'   etc.
#' @param sum_ref Optional. A custom summation reference dataframe the user has
#'   loaded into the R environment. Dataframe must have same columns as default
#'   TADA.summation reference table.
#' @param daily_agg The function used to aggregate to a single
#'   characteristic-unit-fraction-speciation at the same location and depth on
#'   the same day for multiple measurements. Defaults to 'max', but can be set
#'   to 'min' or 'mean'.
#' 
#' @return Input TADA dataframe with additional rows representing total N
#'   summation values from adding up subspecies. These new rows share the same
#'   date and monitoring location as the subspecies, but an additional note is
#'   added in the TADA.NutrientSummation.Flag column describing how the total was
#'   derived.
#'   
#' @export

TADA_CalculateTotalNP <- function(.data, sum_ref, daily_agg){
  
  # check required columns for TADA dataset
  req_cols = c("TADA.CharacteristicName",
               "TADA.ResultSampleFractionText",
               "TADA.MethodSpecificationName",
               "TADA.ResultMeasure.MeasureUnitCode",
               "TADA.ResultMeasureValue",
               "ActivityStartDate",
               "MonitoringLocationIdentifier",
               "ActivityTypeCode")
  TADA_CheckColumns(.data, expected_cols = req_cols)
  
  # notification
  print("Note: function currently only calculates total nitrogen. ")
  
  # bring in custom reference df if provided
  if(!missing(sum_ref)){
    ref_cols = names(TADA_GetNutrientSummationRef())
    TADA_CheckColumns(sum_ref, expected_cols = ref_cols)
  }else{
    sum_ref = TADA_GetNutrientSummationRef()
  }
  
  # Get grouping cols for daily aggregation
  # create nutrient groups by site, date, and depth
  depths = names(.data)[grepl("DepthHeightMeasure", names(.data))]
  depths = depths[grepl("TADA.", depths)]
  grpcols = c("ActivityStartDate", "MonitoringLocationIdentifier","ActivityMediaSubdivisionName","TADA.ComparableDataIdentifier","ActivityTypeCode","TADA.ResultMeasure.MeasureUnitCode",depths)
  
  dat = TADA_AggregateMeasurements(.data, grouping_cols = grpcols, agg_fun = daily_agg, clean = TRUE)

  # join data to summation table and keep only those that match for summations
  sum_dat = merge(dat, sum_ref, all.x = TRUE)
  sum_dat = subset(sum_dat, !is.na(sum_dat$SummationRank))
  
  ## REMINDER FOR TADA TEAM: NEED TO ENSURE ALL COMBOS PRESENT IN TABLE
  
  # If the join results in matching rows
  if(dim(sum_dat)[1]>0){
    
    thecols = grpcols[!grpcols%in%c("TADA.ComparableDataIdentifier")]
    # # find nearby sites
    # nearsites = unique(sum_dat[,c("MonitoringLocationIdentifier","TADA.LatitudeMeasure","TADA.LongitudeMeasure")])
    # nearsites = TADA_FindNearbySites(nearsites)
    # nearsites = subset(nearsites, !nearsites$TADA.NearbySiteGroups%in%c("No nearby sites"))
    
    # create nutrient group ID's.
    sum_dat = sum_dat %>% dplyr::group_by(dplyr::across(dplyr::all_of(thecols))) %>% dplyr::mutate(TADA.NutrientSummationGroup = dplyr::cur_group_id())
    
    # Create list of summation equations in order of preference, can also accommodate phosphorus
    eqns = list(N1 = "TOTAL N", 
                N2 = c("TKN","NITRATE","NITRITE"), 
                N2_a = c("TKN","NITRATE + NITRITE"), 
                N3 = c("ORG N","AMMON","NITRATE","NITRITE"), 
                N3_a = c("ORG N","AMMON","NITRATE + NITRITE"), 
                N4 = c("AMMON","NITRATE","NITRITE"),
                N4_a = c(c("AMMON","NITRATE + NITRITE")))
    
    # dataframe to hold results
    summeddata = data.frame()
    grps = vector()
    
    # for each equation, see if any groups contain all required subspecies, and for each pick the variant with the lowest rank.
    # combine group with other groups and remove group ID from consideration for the next equation
    for(i in 1:length(eqns)){
      eq = eqns[[i]]
      nutrient = ifelse(grepl("N", names(eqns[i])),"Total Nitrogen as N","Total Phosphorus as P")
      out = sum_dat %>% dplyr::filter(!TADA.NutrientSummationGroup%in%grps) %>% dplyr::group_by(TADA.NutrientSummationGroup) %>% dplyr::filter(all(eq%in%SummationName)) %>% dplyr::filter(SummationName%in%eq) %>% dplyr::mutate(TADA.NutrientSummationEquation = paste0(eq, collapse = " + "))
      out = out %>% dplyr::group_by(TADA.NutrientSummationGroup, SummationRank) %>% dplyr::slice_min(SummationRank)
      out$TADA.NutrientSummation.Flag = paste0("Used to calculate ",nutrient,".")
      summeddata = plyr::rbind.fill(summeddata, out)
      grps = c(grps, unique(out$TADA.NutrientSummationGroup))
    }
    
    # Convert speciation if needed
    summeddata$TADA.ResultMeasureValue = ifelse(!is.na(summeddata$SummationSpeciationConversionFactor), summeddata$TADA.ResultMeasureValue * summeddata$SummationSpeciationConversionFactor, summeddata$TADA.ResultMeasureValue)
    summeddata$TADA.MethodSpecificationName = ifelse(!is.na(summeddata$SummationSpeciationConversionFactor), "AS N", summeddata$TADA.MethodSpecificationName)
    
    # Get to total N or P
    totncols = c(thecols, "TADA.NutrientSummationGroup","TADA.NutrientSummationEquation")
    TotalN = summeddata %>% dplyr::group_by(dplyr::across(dplyr::all_of(totncols))) %>% dplyr::summarise(TADA.ResultMeasureValue = sum(TADA.ResultMeasureValue)) %>% dplyr::mutate(TADA.CharacteristicName = "TOTAL NITROGEN, MIXED FORMS", TADA.ResultSampleFractionText = "UNFILTERED",TADA.MethodSpecificationName = "AS N", TADA.NutrientSummation.Flag = "Nutrient summation from subspecies.")
    
    # combine all data back into input dataset and get rid of unneeded columns
    .data = merge(.data, summeddata, all.x = TRUE)
    .data = plyr::rbind.fill(.data, TotalN)
    .data = .data %>% dplyr::select(-c(SummationFractionNotes, SummationSpeciationNotes, SummationSpeciationConversionFactor, SummationName, SummationRank, SummationNote))
    .data$TADA.NutrientSummation.Flag[is.na(.data$TADA.NutrientSummation.Flag)] = "Not used to calculate Total N or P."
  }else{
    # if there are no data to sum
    .data$TADA.NutrientSummation.Flag = "Not used to calculate Total N or P."
    print("No Total N or P subspecies exist in dataset. Returning input dataset with TADA.NutrientSummation.Flag set to 'Not used to calculate Total N or P'")
  }
  
  # order columns
  .data = TADA_CreateComparableID(.data)
  .data = TADA_OrderCols(.data)
  
}