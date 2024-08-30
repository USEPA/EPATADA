#' Find Target Units That Cannot be Converted For Priority Characteristics


# Run to load a random testing data of Characteristics
# test_convertUnit <- TADA_RandomTestingData()

# Create a User defined UnitRef dataframe - Finds all combinations of Characteristics and Units
test_convertUnitRef <- TADA_CreateUnitRef(test_convertUnit)
test_convertUnit3 <- TADA_ConvertResultUnits(test_convertUnit, ref = test_convertUnitRef)

# read in extdata of TADA Priority Charactersitics and its respective units
tada_priority <- read.csv(system.file("extdata","TADAPriorityCharUnitRef.csv", package = "EPATADA"))
tada_priority_ref <- read.csv(system.file("extdata","TADAPriorityCharConvertRef.csv", package = "EPATADA"))

# Name of unique characteristic Names to be filtered by those found in the Priority Characteristics list/ref file
columns <- unique(tada_priority$TADA.CharacteristicName)
test_for_missing_units <- dplyr::filter(test_convertUnit2, TADA.CharacteristicName %in% columns)

# Looks at all Priority Characteristic units that are not currently able to be converted from either TADAPriorityCharUnitRef.csv or WQXUnitRef.csv
# Must consider if these conversions are able to be converted or if they're errors that should be excluded/invalid?
test_for_missing_units2 <- dplyr::anti_join(test_for_missing_units, tada_priority, by = c("TADA.CharacteristicName", "TADA.Target.ResultMeasure.MeasureUnitCode"))
# test_for_missing_units3 <- dplyr::filter(test_for_missing_units2, ! c("TADA.CharacteristicName", "TADA.Target.ResultMeasure.MeasureUnitCode") %in% columns)

# Add a count of number of parameter & units that are being pulled in. This will help to determine how often this is occuring and whether 
# it should be flagged or not or if adding it as an additional unit is valid.