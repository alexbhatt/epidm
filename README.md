
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

 + __clean_sql__: import a SQL query, and remove out any comments, including multi line ones
 + __csv_from_zip__: pointing to a zip file from the web, extract a csv or xlsx; NHS/ODS files are often available in this format, user choice of reader after extraction
 + __lookup_recode__: a fast method of using two column lookups to recode data
 + __valid_nhs__: the UK NHS checksum algorithm to validate an NHS number
 
### Data management

 + __uk_patient_id__: a multistage deterministic algorithm for grouping patient records using common UK patient identifiers
 + __continuous_inpatient_spells__: grouping of episodes/spells from HES/SUS following the NHSD methodology
 + __group_time__: grouping of time events where they overlap; applies to both rolling infection windows (eg. all samples within 14-days of the last) or overlapping time intervals (eg. HES/SUS mega spells)
 + __proxy_episode_dates__: cleanup of dates for inpatient spells when they are missing or not yet discharged
 + __inpatient_codes__: cleanup for inpatient ICD9/10 or OPCS codes for analysis
 + __infection_episode__: grouping of single point time events where they overlap given a window (in days) using either a static or rolling window; primarily used for infection episodes to deduplicate multiple positive tests
