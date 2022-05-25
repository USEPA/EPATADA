#' Update Existing Data in sysdata.rda
#' 
#' Function is for internal use only. It is used in other internal 
#' functions which are used to update internal data (e.g. reference tables). 
#' This function was adapted from a stackoverflow.com thread, which can be
#' accessed [here](https://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file).
#'
#' @param ... Objects to be updated in sysdata.rda.
#' @param list Argument indicating the data class of the list.
#'
#' @return Updated sysdata.rda file
#' 

UpdateInternalData <- function(..., list = character()) {
 
  # check object inputs are of class data.frame
  if("data.frame" %in% class(...) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # load existing sysdata.rda
  sysdata.prev  <- load("R/sysdata.rda")
  # create a list of input objects (objects to update in sysdata)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  # attribute data to each object in var.names
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  # save new object to sysdata.rda
  save(list = unique(c(sysdata.prev, var.names), compress = "xz"), file = "R/sysdata.rda")
}

#' WQX QAQC Characteristic Validation Reference Table
#' 
#' Function updates the raw Water Quality Exchange (WQX) QAQC Characteristic 
#' Validation reference table, as well as the cleaned reference table 
#' (WQXcharValRef) in the sysdata.rda file. The WQXcharValRef data frame 
#' contains information for four functions: InvalidFraction, InvalidResultUnit, 
#' InvalidSpeciation, and UncommonAnalyticalMethodID. 
#'
#' @return Updated sysdata.rda with updated WQXcharValRef object
#' 

UpdateWQXCharValRef <- function(){
  
  # read raw csv from url
  raw.data <- utils::read.csv(url("https://cdx2.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV"))
  # filter data to include only accepted (valid) values and remove extraneous columns
  WQXcharValRef <- raw.data %>%
    dplyr::select(-c("Domain", "Unique.Identifier", "Note.Recommendation",
                     "Last.Change.Date"))
  # replace "Status" values with Valid, Invalid, Unknown
  WQXcharValRef["Status"][WQXcharValRef["Status"] == "Accepted"] <- "Valid"
  WQXcharValRef["Status"][WQXcharValRef["Status"] == "Rejected"] <- "Invalid"
  WQXcharValRef["Status"][WQXcharValRef["Status"] == "Nonstandardized" |
                             WQXcharValRef["Status"] == "InvalidMediaUnit" |
                             WQXcharValRef["Status"] == "InvalidChar" |
                             WQXcharValRef["Status"] == "MethodNeeded"] <- "Nonstandardized" 
  # write reference table to inst/extdata
      # write to sysdata.rda: UpdateInternalData(WQXcharValRef)
  utils::write.csv(WQXcharValRef, file = "inst/extdata/WQXcharValRef.csv", row.names = FALSE)
}

#' Update Measure Unit Reference Table
#' 
#' Function reads in the latest WQX MeasureUnit Domain table, adds 
#' additional target unit information, and writes the data to sysdata.rda.
#'
#' @return sysdata.rda with updated WQXunitRef object (unit conversion reference
#' table)
#' 

UpdateMeasureUnitRef <- function(){
  
  # read in raw WQX QAQC Characteristic Validation csv
  WQXunitRef <- utils::read.csv(url("https://cdx2.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"))
  # add m and ft as target units for "Length Distance" (Description field) rows
    # target.unit = m
  target.m <- data.frame(Domain = rep("Measurement Unit(MeasureUnitCode)", 13),
                              Unique.Identifier = rep(NA, 13),
                              Code = c("Angst", "cm", "dm", "feet", "ft", "in",
                                       "km", "m", "mi", "mm", "nm", "nmi", "yd"),
                              Description = c("Length Distance, Angstroms",
                                              "Length Distance, Centimeters",
                                              "Length Distance, Decimeters",
                                              "Length Distance, Feet",
                                              "Length Distance, Feet",
                                              "Length Distance, Inches",
                                              "Length Distance, Kilometers",
                                              "Length Distance, Meters",
                                              "Length Distance, Miles",
                                              "Length Distance, Millimeters",
                                              "Length Distance, Nanometers",
                                              "Length Distance, Nautical miles",
                                              "Length Distance, Yards"),
                            Last.Change.Date = rep("3/24/2022 12:00:00 PM", 13),
                            Target.Unit = rep("m", 13),
                            Conversion.Factor = c(1e-10, 0.01, 0.1, 0.3048, 0.3048,
                                                  0.0254, 1000, 1, 1609.34, 0.001,
                                                  1e-9, 1825, 0.9144),
                            Conversion.Coefficient = rep(0, 13)
                            )
    # target.unit = ft
  target.ft <- data.frame(Domain = rep("Measurement Unit(MeasureUnitCode)", 13),
                         Unique.Identifier = rep(NA, 13),
                         Code = c("Angst", "cm", "dm", "feet", "ft", "in",
                                  "km", "m", "mi", "mm", "nm", "nmi", "yd"),
                         Description = c("Length Distance, Angstroms",
                                         "Length Distance, Centimeters",
                                         "Length Distance, Decimeters",
                                         "Length Distance, Feet",
                                         "Length Distance, Feet",
                                         "Length Distance, Inches",
                                         "Length Distance, Kilometers",
                                         "Length Distance, Meters",
                                         "Length Distance, Miles",
                                         "Length Distance, Millimeters",
                                         "Length Distance, Nanometers",
                                         "Length Distance, Nautical miles",
                                         "Length Distance, Yards"),
                         Last.Change.Date = rep("3/24/2022 12:00:00 PM", 13),
                         Target.Unit = rep("ft", 13),
                         Conversion.Factor = c(3.28084e-10, 0.0328084, 0.328084,
                                               1, 1, 0.08333, 3280.84, 3.28084,
                                               5280, 0.00328084, 3.2808e-9,
                                               6076.12, 3),
                         Conversion.Coefficient = rep(0, 13)
                         )
     # add data to WQXunitRef
  WQXunitRef <- plyr::rbind.fill(WQXunitRef, target.m, target.ft)
    
  # write reference table to sysdata.rda
      #UpdateInternalData(WQXunitRef)
  utils::write.csv(WQXunitRef, file = "inst/extdata/WQXunitRef.csv", row.names = FALSE)
}


#' Generate Unique Harmonization Reference Table
#' 
#' Function generates a harmonization reference table that is specific to
#' the input dataset. Users can review how their input data relates to standard
#' TADA values for CharacteristicName, ResultSampleFractionText, 
#' MethodSpecicationName, and ResultMeasure.MeasureUnitCode and they can optionally
#' edit the reference file to meet their needs.
#'
#' @param .data TADA dataframe
#' @param download Boolean argument; when download = TRUE, the output is 
#' downloaded to the current working directory.
#'
#' @return Harmonization Reference Table unique to the input dataset
#' 
#' @export

HarmonizationRefTable <- function(.data, download = FALSE){
  
  # check that .data object is compatible with TADA
  # check .data is of class data.frame
  if(("data.frame" %in% class(.data)) == FALSE) {
    stop("Input object must be of class 'data.frame'")
  }
  # check .data has any of the required columns
  if(all(c("CharacteristicName", "ResultSampleFractionText",
           "MethodSpecificationName",
           "ResultMeasure.MeasureUnitCode") %in% 
         colnames(.data)) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  # check if download is boolean
  if(is.logical(download) == FALSE){
    stop("download argument must be Boolean (TRUE or FALSE)")
  }
  # execute function after checks are passed
  if(all(c("CharacteristicName", "ResultSampleFractionText",
           "MethodSpecificationName",
           "ResultMeasure.MeasureUnitCode") %in% 
         colnames(.data)) == TRUE) {
    
    # define raw harmonization table as an object
    harm.raw <- read.csv(system.file("extdata", "HarmonizationTemplate.csv",
                            package = "TADA"))

    # join harmonization table to .data
      # if WQX QA Char Val flags are in .data, include them in the join
    if(all(c("WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
             "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity") %in% 
           colnames(.data)) == TRUE) {
      join.data <- merge(.data[, c("CharacteristicName", "ResultSampleFractionText",
                                   "MethodSpecificationName",
                                   "ResultMeasure.MeasureUnitCode", 
                                   "WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
                                   "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity")],
                         harm.raw, 
                         by.x = c("CharacteristicName", "ResultSampleFractionText",
                                  "MethodSpecificationName",
                                  "ResultMeasure.MeasureUnitCode"),
                         by.y = c("CharacteristicName", "ResultSampleFractionText",
                                  "MethodSpecificationName",
                                  "ResultMeasure.MeasureUnitCode"), 
                         all.x = TRUE)
      # otherwise, execute the join with no additional columns
    } else {
      join.data <- merge(.data[, c("CharacteristicName", "ResultSampleFractionText",
                                   "MethodSpecificationName",
                                   "ResultMeasure.MeasureUnitCode")],
                         harm.raw, 
                         by.x = c("CharacteristicName", "ResultSampleFractionText",
                                  "MethodSpecificationName",
                                  "ResultMeasure.MeasureUnitCode"),
                         by.y = c("CharacteristicName", "ResultSampleFractionText",
                                  "MethodSpecificationName",
                                  "ResultMeasure.MeasureUnitCode"), 
                         all.x = TRUE)
    }
    
    # trim join.data to include only unique combos of char-frac-spec-unit
    unique.data <- join.data %>%
      dplyr::filter(!duplicated(join.data[, c("CharacteristicName", 
                                              "ResultSampleFractionText", 
                                              "MethodSpecificationName",
                                              "ResultMeasure.MeasureUnitCode")]))
      
    # reorder columns to match harm.raw
      # include WQX QA flag columns, if they exist
    if(all(c("WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
             "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity") %in% 
           colnames(.data)) == TRUE) {
      # get .data column names
      col.order <- colnames(harm.raw)
      # add WQX.SampleFractionValidity column to the list
      col.order <- append(col.order, c("WQX.SampleFractionValidity", "WQX.MethodSpeciationValidity",
                                       "WQX.ResultUnitValidity", "WQX.AnalyticalMethodValidity"))
      # reorder columns in flag.data
      unique.data <- unique.data[, col.order]
    } else {
      unique.data <- unique.data[, colnames(harm.raw)]
    }
    
    # remove extraneous characters in first column
    colnames(unique.data)[1] <- gsub('^','',colnames(unique.data)[1])
    
    # flag potential duplicates
    unique.data$TADA.ComparableDataID <- as.integer(seq_len(nrow(unique.data)))
      
    # if download = TRUE, download unique.data as a csv to the working directory
    if(download == TRUE){
      utils::write.csv(unique.data, "HarmonizationRefTable.csv")
    }
      
    # return unique.data
    return(unique.data)
  }
}