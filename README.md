
# Epidemiological Data Management (EPIDM)

<!-- badges: start -->
<!-- badges: end -->

The goal of EPIDM is provide standard methods for the management and transformation of UK Public Health data.

## Installation

Currently this is only avaiable via [GitHub](https://github.com/alexbhatt/epidm)

``` r

devtools::install_github("alexbhatt/epidm")

```

## Functions 

### Utilities 

 + __clean_sql__: import a SQL query, and remove out any comments
 + __csv_from_zip__: pointing to a zip file from the web, extract a csv or xlsx; NHS/ODS files are often available in this format
 
### Data management

 + __valid_nhs__: the UK NHS checksum algorithm to validate an NHS number
 + __patient_id__: a multistage deterministic algorithm for grouping patient records using common UK patient identifiers
 + __continuous_inpatient_spells__: grouping of episodes/spells from HES/SUS following the NHSD methodology
 + __group_time__: grouping of time events where they overlap; applies to both rolling infection windows (eg. all samples within 14-days of the last) or overlapping time intervals (eg. HES/SUS mega spells)
