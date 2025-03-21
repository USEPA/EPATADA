% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoFilter.R
\name{TADA_AnalysisDataFilter}
\alias{TADA_AnalysisDataFilter}
\title{TADA_AnalysisDataFilter}
\usage{
TADA_AnalysisDataFilter(
  .data,
  clean = FALSE,
  surface_water = TRUE,
  ground_water = FALSE,
  sediment = FALSE
)
}
\arguments{
\item{.data}{A TADA profile object}

\item{clean}{Boolean argument; TRUE removes all results not flagged for use in
analysis workflow. TADA.UseForAnalysis.Flag column displaying the media type (as
determined by this function) and "Yes"/"No" will be added when clean = FALSE.
Results flagged "Yes" are identified as usable for further analysis. Default = FALSE.}

\item{surface_water}{Boolean argument; specifies whether surface water
results should be flagged or removed in the returned dataframe. Default is
surface_water = TRUE, surface water results are identified as usable for analysis.}

\item{ground_water}{Boolean argument; specifies whether ground water
results should be flagged or removed in the returned dataframe. Default is
ground_water = FALSE, ground water results are identified as not usable for analysis.}

\item{sediment}{Boolean argument; specifies whether sediment results should
be flagged or removed in the returned dataframe. Default is sediment = FALSE,
sediment results are identified as not usable for analysis.}
}
\value{
If clean = TRUE, returns the dataframe with only the media types
selected as usable (set to TRUE in function input) by the user.
If clean = FALSE, returns the dataframe and an additional column,
TADA.UseForAnalysis.Flag, indicating the media type (as determined by this function)
and which results should be included or excluded from assessments based on user input.
}
\description{
With default settings (clean = FALSE), this function creates a TADA.UseForAnalysis.Flag
column which flags any data that are NOT surface water results for
removal (TADA.UseForAnalysis.Flag = "No") and flags surface water results
for use in analysis (TADA.UseForAnalysis.Flag = "Yes"). If desired, a user
can change the function input to clean = TRUE, and then the function will
filter the dataframe to remove rows that are not going to be used in analyses,
and retain only the media types selected by the user.Setting clean = TRUE, means
that all results not flagged for use in the analysis workflow will be removed
and the TADA.UseForAnalysis.Flag column will not be added.
}
\details{
It uses ActivityMediaSubdivisionName, AquiferName,
LocalAqfrName, ConstructionDateText, WellDepthMeasure.MeasureValue,
WellDepthMeasure.MeasureUnitCode, WellHoleDepthMeasure.MeasureValue, and
WellHoleDepthMeasure.MeasureUnitCode to identify groundwater samples. Users
can select whether sediment, groundwater and/or surface water should be included.
An additional column, TADA.UseForAnalysis.Flag, specifies whether each row should
be included in the analysis workflow and why. Setting clean = TRUE, means
that all results not flagged for use in the analysis workflow will be removed
and the TADA.UseForAnalysis.Flag column will not be added.
}
\examples{
\dontrun{
data(Data_6Tribes_5y_Harmonized)
# Returns data with ONLY surface water results retained and no TADA.UseForAnalysis.Flag column
Data_6Tribes_Assessment1 <- TADA_AnalysisDataFilter(Data_6Tribes_5y_Harmonized,
  clean = TRUE,
  surface_water = TRUE, ground_water = FALSE, sediment = FALSE
)

# Returns dataframe with ONLY surface water results identified as usable and adds
# TADA.UseForAnalysis.Flag column.
Data_6Tribes_Assessment2 <- TADA_AnalysisDataFilter(Data_6Tribes_5y_Harmonized,
  clean = FALSE,
  surface_water = TRUE, ground_water = FALSE, sediment = FALSE
)
unique(Data_6Tribes_Assessment2$TADA.UseForAnalysis.Flag)
}

}
