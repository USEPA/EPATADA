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
#' This function updates the raw WQX QAQC Characteristic Validation reference
#' table, as well as the cleaned reference table (WQXcharVal.ref) in the 
#' sysdata.rda file. The WQXcharVal.ref data frame contains information for four
#' functions: InvalidFraction, InvalidResultUnit, InvalidSpeciation, and 
#' UncommonAnalyticalMethodID. 
#' This function is run when package is loaded to library (how?)
#'
#'
#' @return Updated raw QAQCCharacteristicValidation.CSV and sysdata.rda with
#'  updated WQXcharVal.ref object
#' 

UpdateWQXCharValRef <- function(){
  
  # read raw csv from url
  raw.data <- utils::read.csv(url("https://cdx2.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV"))
  # filter data to include only accepted (valid) values and remove extraneous columns
  WQXcharVal.ref <- raw.data %>%
    dplyr::select(-c("Domain", "Unique.Identifier", "Note.Recommendation",
                     "Last.Change.Date"))
  # replace "Status" values with Valid, Invalid, Unknown
  WQXcharVal.ref["Status"][WQXcharVal.ref["Status"] == "Accepted" |
                             WQXcharVal.ref["Status"] == "InvalidChar" |
                             WQXcharVal.ref["Status"] == "InvalidMediaUnit"] <- "Valid"
  WQXcharVal.ref["Status"][WQXcharVal.ref["Status"] == "Rejected"] <- "Invalid"
  WQXcharVal.ref["Status"][WQXcharVal.ref["Status"] == "MethodNeeded"] <- "Unknown" 
  # write reference table to sysdata.rda
  TADAupdateInternalData(WQXcharVal.ref)
}

#' Update Unit Conversion Reference Table
#' 
#' This function should be run when package is loaded to library (how do we do
#' this?)
#'
#' @return sysdata.rda with updated unit.ref object (unit conversion reference
#' table)
#' 

UpdateUnitRef <- function(){
  
  # read in raw WQX QAQC Characteristic Validation csv
  unit.ref <- utils::read.csv("inst/extdata/TADA_Unit_Conversions_ref.csv")
  # write reference table to sysdata.rda
  TADAupdateInternalData(unit.ref) 
}
