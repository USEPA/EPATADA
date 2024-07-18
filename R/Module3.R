TADA_SimpleCriteriaComparison <- function(.data, criteria.ref, convert.units = TRUE) {
  
  # check for user provided criteria reference, stop if not provided
  if(is.null(criteria.ref)) {
    stop("There is no user-supplied reference providing numeric criteria. The function cannot be run.")
  }
  
  # check for user provided criteria refrence, continue if provided
  if(!is.null(criteria.ref)) {
    
  # function to create a vector of char.frac.pairs
    char.frac.pairs <- function(.data) {
    .data <- .data %>%
    dplyr::select(TADA.CharacteristicName, TADA.ResultSampleFractionText) %>%
    dplyr::distinct() %>%
    dplyr::mutate(pair = paste(TADA.CharacteristicName, " (", TADA.ResultSampleFractionText, ")", sep = "")) %>%
    dplyr::select(pair) %>%
    dplyr::distinct() %>%
    dplyr::pull()
    
    return(.data)
  }
    
   # create vector of characteristic/fraction pairs from TADA data frame
   tada.pairs <- char.frac.pairs(.data)
    
   # create vector of characteristic/fraction pairs from user-supplied criteria reference
   ref.pairs <- char.frac.pairs(criteria.ref)
  
   # find pairs that are included in tada.pairs
   diff.pairs <- setdiff(tada.pairs, ref.pairs)
   diff.pairs <- stringi::stri_replace_last(stringi::stri_paste(diff.pairs, collapse = ", "), " and ", fixed = ", ")
   
   print(paste("TADA_SimpleCriteriaComparison: The following characteristic/fraction pairs do not have user-provided numeric critieria: ", diff.pairs, ".", sep = "" ))
   
   # join user-supplied criteria reference to TADA data frame
   .data <- .data %>%
     dplyr::left_join(criteria.ref, by = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText"))
   
   # compare TADA.ResultMeasure.MeasureUnitCode units to TADA.UserStandardUnits and identify mismatches
   unit.check <- .data %>%
     dplyr::select(TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.ResultMeasure.MeasureUnitCode, TADA.UserStandardUnits) %>%
     dplyr::distinct() %>%
     dplyr::filter(!is.na(TADA.UserStandardUnits)) %>%
     dplyr::filter(TADA.ResultMeasure.MeasureUnitCode != TADA.UserStandardUnits) 
     
   # create string of characteristic/fraction pairs with mismatches
   unit.pairs <- char.frac.pairs(unit.check)
   unit.print <- stringi::stri_replace_last(stringi::stri_paste(unit.pairs, collapse = ", "), " and ", fixed = ", ")
   
   # if user does not want to convert units within this function
   if(convert.units == FALSE) {
       
     
     # if any rows with unit mismatches are present, stop function
     if(nrow(unit.check) > 0) {
       
       stop(paste("TADA_SimpleCriteriaComparison: The result units for the following characteristic/fraction pairs in the TADA data frame do not match the units in the user-provided numeric criteria: ",
                  unit.print, ". Correct numeric criteria input or use convert.units = TRUE to automatically convert results to match user-provided criteria units."))
     }
     
     # if no rows with unit mismatches are present, continue with function
     if(nrow(unit.check) == 0) {
       print("TADA_SimpleCriteriaComparison: The result units for all characteristic/fraction pairs in the TADA data frame match the units in the user-provided numeric criteria.")
     }
   }
   
   # if user does want to convert units within this function
   if(convert.units == TRUE) {
    
     # check if any rows contain unit mismatches
     if(nrow(unit.check) > 0) {
       
       unit.print <- stringi::stri_replace_last(stringi::stri_paste(unit.check, collapse = ", "), " and ", fixed = ", ")
       
       # print message identifying characteristic/fraction pairs with unit mismatches
       stop(paste("TADA_SimpleCriteriaComparison: The result units for the following characteristic/fraction pairs in the TADA data frame do not match the units in the user-provided numeric criteria: ",
                  unit.print, ". Result units will be converted to match user-supplied criteria units using TADA_ConvertResultUnits"))
       
       # filter .data to create data frame of results that require conversion
       convert.data <- .data %>%
         dplyr::mutate(pair = paste(TADA.CharacteristicName, " (", TADA.ResultSampleFractionText, ")", sep = "")) %>%
         dplyr::filter(pair %in% unit.pairs,
                       TADA.ResultMeasure.MeasureUnitCode != TADA.UserStandardUnits)
       
       # filter .data to create date frame of results that do not require conversion
       other.data <- .data %>%
         dplyr::filter(!ResultIdentifier %in% convert.data)
       
       # create wqx unit ref
       wqx.ref <- TADA_GetMeasureUnitRef() %>%
         dplyr::mutate(Code = toupper(Code),
                       Target.Unit = toupper(Target.Unit)) %>%
         dplyr::rename(TADA.ResultMeasure.MeasureUnitCode = Code,
                       TADA.Target.ResultMeasure.MeasureUnitCode = Target.Unit,
                       TADA.WQXUnitConversionFactor = Conversion.Factor,
                       TADA.WQXUnitConversionCoefficent = Conversion.Coefficient) %>%
         dplyr::select(-Domain, -Description, -Unique.Identifier, -Last.Change.Date)
       
       # create data frame of TADA.CharacteristicName, TADA.ResultSampleFractionText, and TADA.Target.ResultMeasure.MeasureUnitCode combinations requiring conversion
       criteria.convert <- criteria.ref %>%
         dplyr::mutate(pair = paste(TADA.CharacteristicName, " (", TADA.ResultSampleFractionText, ")", sep = "")) %>%
         dplyr::filter(pair %in% unit.pairs) %>%
         dplyr::select(-pair, -TADA.UserStandardValue) %>%
         dplyr::rename(TADA.Target.ResultMeasure.MeasureUnitCode = TADA.UserStandardUnits)
       
       # create unit ref for use in TADA_ConvertResultUnits
       unit.ref <- convert.data %>%
         dplyr::select(TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode, TADA.MethodSpeciationName,
                       ResultMeasure.MeasureUnitCode, TADA.ResultSampleFractionText) %>%
         dplyr::distinct() %>%
         dplyr::left_join(criteria.convert, by = c("TADA.CharacteristicName", "TADA.ResultSampleFractionText")) %>%
         dplyr::left_join(wqx.ref, by = c("TADA.ResultMeasure.MeasureUnitCode, TADA.Target.ResultMeasure.MeasureUnitCode"))
         
       
       convert.data <- .data %>%
         TADA_ConvertResultUnits()
       
       
     }
     
     if(nrow(unit.check) == 0) {
       print("TADA_SimpleCriteriaComparison: The result units for all characteristic/fraction pairs in the TADA data frame match the units in the user-provided numeric criteria.")
     }
   }
   
  }
}

# 
# TADA.CharacteristicName <- c("BARIUM", "CHLORIDE", "DICAMBA", "SILVER")
# 
# TADA.ResultSampleFractionText <- c("TOTAL", "TOTAL", "TOTAL", "TOTAL")
# 
# TADA.UserStandardValue <- c(5000, 500, 1500, 160)
#   
# TADA.UserStandardUnits <- c("UG/L", "UG/L", "UG/L", "UG/L")

criteria.ref <- data.frame(TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.UserStandardValue, TADA.UserStandardUnits)

testdat <- TADA_DataRetrieval(statecode = "IL",
                              startDate = "2020-01-01",
                              endDate = "2020-06-30")

testdat2 <- testdat %>%
  dplyr::filter(TADA.CharacteristicName %in% criteria.ref$TADA.CharacteristicName)

.data <- testdat

sort(unique(testdat$TADA.CharacteristicName))

rm(TADA.CharacteristicName, TADA.ResultSampleFractionText, TADA.UserStandardUnits, TADA.UserStandardValue, TADA.CharacteristicName)
