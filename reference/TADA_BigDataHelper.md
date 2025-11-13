# Assist with large WQP data pulls using dataRetrieval

This is a helper function that takes large WQP (waterqualitydata.us)
queries and splits them up into multiple, smaller queries. By default it
pulls data for up to 300 sites or 250,000 data records at a time and
then joins the separate pulls back together to produce a single TADA
compatible dataframe as the output. Computer memory may limit the size
of data frames that your R console will be able to hold in one session.

## Usage

``` r
TADA_BigDataHelper(record_summary, WQPquery, maxrecs = 250000, maxsites = 300)
```

## Arguments

- record_summary:

  MonitoringLocationIdentifier and resultCount columns from the output
  of
  [`dataRetrieval::whatWQPdata`](https://rdrr.io/pkg/dataRetrieval/man/whatWQPdata.html)
  for the WQP query being used.

- WQPquery:

  A named list of query terms to supply dataRetrieval functions.

- maxrecs:

  Maximum number of records to query at once.

- maxsites:

  Maximum number of sites to query at once.

## Value

TADA-compatible dataframe
