# Get WQX Characteristic Domain Table (internal-only)

Loads the package-installed internal reference table from inst/extdata
and caches it for the session. No network is used. Arguments
download_only and refresh are kept for backward compatibility but are
ignored.

## Usage

``` r
TADA_GetCharacteristicRef(download_only = FALSE, refresh = FALSE)
```

## Arguments

- download_only:

  Ignored. Present for backward compatibility.

- refresh:

  Ignored. Present for backward compatibility.

## Value

data.frame with columns: CharacteristicName, Comparable.Name,
CAS.Number, Char_Flag
