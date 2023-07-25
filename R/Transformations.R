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
#' @param .data TADA dataframe
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
#' data(Data_Nutrients_UT)
#' 
#' # Create a synonym reference table for dataframe:
#' CreateRefTable <- TADA_GetSynonymRef(Data_Nutrients_UT)
#' 
#' # Create and download (to your working directory) a synonym reference
#' # table for dataframe: 
#' \dontrun{
#' DownloadRefTable <- TADA_GetSynonymRef(Data_Nutrients_UT, download = TRUE)
#' }
#' 

TADA_GetSynonymRef <- function(.data, download = FALSE) {
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
#' data(Data_Nutrients_UT)
#' 
#' # Append synonym reference table columns to dataframe and transform/convert
#' # data to the reference table values:
#' 
#' # Append synonym reference table columns to dataframe and transform/convert
#' # data to the USER SUPPLIED reference table values:
#' UniqueHarmonizationRef <- TADA_GetSynonymRef(Data_Nutrients_UT, download = FALSE)
#' Nutrients_Harmonized_UserSuppliedRef <- TADA_HarmonizeSynonyms(Data_Nutrients_UT, ref = UniqueHarmonizationRef)

TADA_HarmonizeSynonyms <- function(.data, ref) {
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
      "Target.TADA.UnitConversionFactor"
    ))
  
  # return clean.data
  clean.data = TADA_CreateComparableID(clean.data)
  clean.data <- TADA_OrderCols(clean.data)
  return(clean.data)
  
}
