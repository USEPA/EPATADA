# List Criteria and Methodologies Files Available in the TADACommunityHub

Retrieves the complete listing of Criteria and Methodologies files from
the TADACommunityHub repository. The ATTAINS.OrganizationIdentifier or
display_name returned can be used as function inputs in the
TADA_GetCriteriaFile() function to retrieve a Criteria and Methodologies
File.

## Usage

``` r
TADA_ListCriteriaFiles()
```

## Value

A data frame with four columns:

- ATTAINS.OrganizationIdentifier

- display_name

- file_name

- file_path

## Examples

``` r
criteriaFiles <- TADA_ListCriteriaFiles()
```
