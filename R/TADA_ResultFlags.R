#' Sample Fraction Flag
#' 
#' **placeholder text for function description
#'
#' @param .data TADA dataset
#'
#' @return Full TADA dataset with potentially incorrect sample fractions flagged
#' @export
#' 


TADAfractionFlag <- function(.data){
  
  # check that .data object is compatible with TADA
  if(TADAprofileCheck(.data) == FALSE) {
    stop("The dataframe does not contain the required fields to use TADA. Use either the full physical/chemical profile downloaded from WQP or download the TADA profile template available on the EPA TADA webpage.")
  }
  
  # read in sample fraction reference table from sysdata.rda
  frac.ref <- TADA:::frac.ref
  # Flag incorrect characteristic-sample fraction data
  # make cont.data data frame
  check.data <- dplyr::filter(.data,
                              !(toupper(.data$CharacteristicName) %in% frac.ref$Characteristic & 
                                  toupper(.data$ResultSampleFractionText) %in% frac.ref$Value))
  # append SampleFracFlag column
  check.data$SampleFracFlag <- 1
  # join cont.data to flag.data
  flag.data <- merge(.data, check.data, all.x = TRUE) 
}