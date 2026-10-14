# .checkNHD

Check NHD web services. For use in functions that rely on fetchNHD. This
helper function will stop the function if web service is not available.

## Usage

``` r
.checkNHD(resolution = c("Hi", "Med"), timeout_sec = 10)
```

## Arguments

- resolution:

  Character argument. Options are "Hi" or "Med" resolution. Default is
  resolution = "Hi".

- timeout_sec:

  Numeric argument. The number of seconds allowed before timeout.
