% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResultFlagsDependent.R
\name{TADA_FlagResultUnit}
\alias{TADA_FlagResultUnit}
\title{Check Result Unit Validity}
\usage{
TADA_FlagResultUnit(
  .data,
  clean = c("suspect_only", "nonstandardized_only", "both", "none"),
  flaggedonly = FALSE
)
}
\arguments{
\item{.data}{TADA dataframe}

\item{clean}{Character argument with options "suspect_only", "nonstandardized_only",
"both", or "none." The default is clean = "suspect_only" which removes rows of
data flagged as having "Suspect" characteristic-media-result unit combinations. When
clean = "nonstandardized_only", the function removes rows of data flagged as
having "NonStandardized" characteristic-media-result unit combinations. When
clean = "both", the function removes rows of data flagged as either "Suspect" or
"NonStandardized". And when clean = "none", the function does not remove any "Suspect"
or "NonStandardized" rows of data.}

\item{flaggedonly}{Boolean argument; filters dataframe to show only "Suspect"
characteristic-media-result unit combinations when flaggedonly = TRUE. Default
is flaggedonly = FALSE.}
}
\value{
This function adds the TADA.ResultUnit.Flag to a TADA dataframe. This column
flags each CharacteristicName, ActivityMediaName, and ResultMeasure/MeasureUnitCode
combination in your dataframe as either "NonStandardized", "Suspect", "Pass", or
"Not Reviewed".
When clean = "none" and flaggedonly = TRUE, the dataframe is filtered to show only
the "Suspect" and "NonStandardized" data; the column TADA.ResultUnit.Flag is
still appended. When clean = "suspect_only" and flaggedonly = FALSE, "Suspect"
rows are removed from the dataframe, but "NonStandardized" rows are retained. When
clean = "nonstandardized_only" and flaggedonly = FALSE, "NonStandardized" rows
are removed, but "Suspect" rows are retained. The default is clean = "suspect_only"
and flaggedonly = FALSE.
}
\description{
Function checks the validity of each characteristic-media-result unit
combination in the dataframe. When clean = "suspect_only", rows with Suspect
characteristic-media-result unit combinations are removed. Default is
clean = "suspect_only". When flaggedonly = TRUE, dataframe is filtered to show only
rows with "Suspect" or "NonStandardized" characteristic-media-result unit combinations.
Default is flaggedonly = FALSE.
}
\details{
The “Not Reviewed” value within "TADA.ResultUnit.Flag" means
that the EPA WQX team has not yet reviewed the combinations
(see https://cdx.epa.gov/wqx/download/DomainValues/QAQCCharacteristicValidation.CSV).
The WQX team plans to review and update these new combinations quarterly.
}
\examples{
# Load example dataset:
data(Data_Nutrients_UT)

# Remove data with Suspect characteristic-media-result unit combinations
# from dataframe, but retain "NonStandardized" combinations flagged in new
# column 'TADA.ResultUnit.Flag':
SuspectUnit_clean <- TADA_FlagResultUnit(Data_Nutrients_UT)

# Remove data with "NonStandardized" characteristic-media-result unit
# combinations from dataframe but retain Suspect combinations flagged in
# new column 'TADA.ResultUnit.Flag:
NonstandardUnit_clean <- TADA_FlagResultUnit(Data_Nutrients_UT,
  clean = "nonstandardized_only"
)

# Remove both Suspect and "NonStandardized" characteristic-media-result
# unit combinations from dataframe:
ResultUnit_clean <- TADA_FlagResultUnit(Data_Nutrients_UT, clean = "both")

# Flag, but do not remove, data with Suspect or "NonStandardized"
# characteristic-media-result unit combinations in new column titled
# "TADA.ResultUnit.Flag":
SuspectUnit_flags <- TADA_FlagResultUnit(Data_Nutrients_UT, clean = "none")

# Show only Suspect characteristic-media-result unit combinations:
SuspectUnit_flaggedonly <- TADA_FlagResultUnit(Data_Nutrients_UT,
  clean = "nonstandardized_only", flaggedonly = TRUE
)

# Show only "NonStandardized" characteristic-media-result unit combinations:
NonstandardUnit_flaggedonly <- TADA_FlagResultUnit(Data_Nutrients_UT,
  clean = "suspect_only", flaggedonly = TRUE
)

}
