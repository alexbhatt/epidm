
# Epidemiological Data Management (epidm)

<!-- badges: start -->
<!-- badges: end -->

The goal of `epidm` is provide standard methods for the management and transformation of UK Public Health data.

## Installation

Currently this is only available via [GitHub](https://github.com/alexbhatt/epidm)

``` r

devtools::install_github("alexbhatt/epidm")

```
## Purpose

The `epidm` package has been developed to share standard methods for the 
processing of epidemiological data in the UK. Key data assets this focuses on include

### Infection data

The [Second Generation Surveillance System (SGSS)](https://sgss.phe.org.uk/) is a data asset held by the National Institute of Public Health (formerly Public Health England) which routinely and automatically collects laboratory data from across England.

Laboratories return data on organisms isolated from samples such as the organism species, specimen type, sampling date and antimicrobial susceptibility testing results. These data are routinely used for public health surveillance and epidemiology in England. 

### Hospital data

The functions for dealing with hospital data within `epidm` are designed to help clean, process, and link the hospital in a meaningful way. These methods have been used to help understand [Healthcare Associated COVID-19 in England](https://www.medrxiv.org/content/10.1101/2021.02.16.21251625v1).

#### [Emergency Care Data Set (ECDS)](https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/emergency-care-data-set-ecds)
ECDS is the national data set for urgent and emergency care and is reported daily by NHS Trusts.

#### [Secondary Use Services (SUS)](https://digital.nhs.uk/services/secondary-uses-service-sus)
SUS is the single, comprehensive repository for healthcare data in England which enables a range of reporting and analyses to support the NHS in the delivery of healthcare services.
SUS is reported monthly, by the 21st of each month for the previous months data.

#### [Hospital Episode Statistics (HES)](https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics)
HES is a data warehouse containing details of all admissions, outpatient appointments and A and E attendances at NHS hospitals in England.
Hes is reported quarterly, and is a 'cleaned' version of SUS and ECDS.
