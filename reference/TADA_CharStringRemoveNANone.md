# Remove NAs and NONEs in Strings for Figure Titles and Axis Labels

Returns a vector of string(s) that removes common NA strings found in
columns such as TADA.ComparableDataIdentifier. Can also accommodate
handling of certain NA texts found in any general character string or a
vector of strings.

## Usage

``` r
TADA_CharStringRemoveNANone(char_string)
```

## Arguments

- char_string:

  Character argument. Could be a single string or vector of strings that
  contains common "NA" strings (ex: "(NA", "(NA)", "\_NA", etc.)

## Value

A vector string that has removed NAs from its value.

## Details

This function is meant as an internal function to remove NAs from figure
titles and axis labels for the TADA package.

## Examples

``` r
# Removes NAs based on each TADA.ComparableDataIdentifier found in a dataset.
utils::data(Data_Nutrients_UT)
unique(Data_Nutrients_UT$TADA.ComparableDataIdentifier)
#>  [1] "NITRATE_DISSOLVED_NONE_NONE"         "NITROGEN_SUSPENDED_NONE_MG/L"       
#>  [3] "NITRATE_DISSOLVED_AS N_MG/L"         "NITRATE_DISSOLVED_AS NO3_MG/L"      
#>  [5] "NITROGEN_SUSPENDED_NONE_NONE"        "NITROGEN_TOTAL_AS N_MG/L"           
#>  [7] "NITROGEN_DISSOLVED_AS N_MG/L"        "NITRATE_NONE_UNKNOWN_MG/L"          
#>  [9] "AMMONIA_TOTAL RECOVERABLE_AS N_MG/L" "AMMONIA_UNFILTERED_AS N_MG/L"       
#> [11] "NITRATE_TOTAL_AS N_NONE"             "NITRATE_UNFILTERED_AS N_NONE"       
#> [13] "AMMONIA_TOTAL_AS N_MG/L"             "NITRATE_TOTAL_AS N_MG/L"            
#> [15] "NITRATE_UNFILTERED_AS N_MG/L"       
UT_Titles <- TADA_CharStringRemoveNANone(unique(Data_Nutrients_UT$TADA.ComparableDataIdentifier))
unique(UT_Titles)
#>  [1] "NITRATE DISSOLVED"                   "NITROGEN SUSPENDED MG/L"            
#>  [3] "NITRATE DISSOLVED AS N MG/L"         "NITRATE DISSOLVED AS NO3 MG/L"      
#>  [5] "NITROGEN SUSPENDED"                  "NITROGEN TOTAL AS N MG/L"           
#>  [7] "NITROGEN DISSOLVED AS N MG/L"        "NITRATE UNKNOWN MG/L"               
#>  [9] "AMMONIA TOTAL RECOVERABLE AS N MG/L" "AMMONIA UNFILTERED AS N MG/L"       
#> [11] "NITRATE TOTAL AS N"                  "NITRATE UNFILTERED AS N"            
#> [13] "AMMONIA TOTAL AS N MG/L"             "NITRATE TOTAL AS N MG/L"            
#> [15] "NITRATE UNFILTERED AS N MG/L"       
```
