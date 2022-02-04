#' Update Existing Data in sysdata.rda
#' 
#' This function is for internal use only. It is used in other internal 
#' functions which are used to update internal data (e.g. reference tables). 
#' This function was adapted from a stackoverflow.com thread, which can be
#' accessed [here](https://stackoverflow.com/questions/11813096/updating-an-existing-rdata-file).
#'
#' @param ... Objects to be updated in sysdata.rda.
#' @param list Argument indicating the data class of the list.
#'
#' @return Updated sysdata.rda file
#' 

TADAupdateInternalData <- function(..., list = character()) {
  
  # check object inputs are of class data.frame
  if(class(...) != "data.frame") {
    stop("Input object must be of class 'data.frame'")
  }
  # load existing sysdata.rda
  sysdata.prev  <- load("R/sysdata.rda")
  # create a list of input objects (objects to update in sysdata)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  # attribute data to each object in var.names
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  # save new object to sysdata.rda
  save(list = unique(c(sysdata.prev, var.names)), file = "R/sysdata.rda")
}

#' Sample Fraction Reference Table
#' 
#' This function should be run when package is loaded to library (how do we do
#' this?)
#'
#'
#' @return sysdata.rda with updated frac.ref object (sample fraction
#' reference table)
#' 

TADAupdateFractionRef <- function(){
  
  # read in raw WQX QAQC Characteristic Validation csv
  raw.data <- utils::read.csv("inst/extdata/QAQCCharacteristicValidation.CSV")
  # filter data to include only valid characteristic-fraction pairs
  frac.ref <- dplyr::filter(raw.data, Domain.1 == "CharacteristicFraction")
  # write reference table to sysdata.rda
  TADAupdateInternalData(frac.ref) 
}

#' Update Unit Conversion Reference Table
#' 
#' This function should be run when package is loaded to library (how do we do
#' this?)
#'
#' @return sysdata.rda with updated unit.ref object (unit conversion reference
#' table)
#' 

TADAupdateUnitRef <- function(){
  
  # read in raw WQX QAQC Characteristic Validation csv
  unit.ref <- utils::read.csv("inst/extdata/TADA_Unit_Conversions_ref.csv")
  # write reference table to sysdata.rda
  TADAupdateInternalData(unit.ref) 
}