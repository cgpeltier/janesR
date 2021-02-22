# janesR
<img src=images/hex-janesR2.png align="right" alt="" width="120" />

janesR is an R wrapper for the Janes API that allows Janes users to easily pull data from the API into tibbles. These tibbles are then ready for data analysis and visualization, export into CSV or XLSX, or integration into users' systems. Documentation for the API can be found at the [Janes Developer site](https://developer.janes.com/). 

You can install janesR from github: 

```{r}
devtools::install_github("cgpeltier/janesR")
```
## Endpoints
The package currently supports the following Janes API endpoints:

* Airports: `endpoint = "airports"`
* Bases: `endpoint = "bases"`
* Companies: `endpoint = "companies"`
* Country Risk: `endpoint = "countryrisk"`
* Defence Programs: `endpoint = "defenceprogrammes"`
* Early Warning Sites: `endpoint = "ewsites"`
* Equipment: `endpoint = "equipment"`
* Equipment Relationships: `endpoint = "equipmentrelationships"`
* Events: `endpoint = "events"`
* Inventories: `endpoint = "inventories"`
* Markets Forecast: `endpoint = "marketforecasts"`
* News: `endpoint = "news"`
* Nuclear Sites: `endpoint = "nuclearsites"`
* ORBATs: `endpoint = "orbats"`
* Reference: `endpoint = "references"`
* SAM Sites: `endpoint = "samsites"`
* Satellite Images: `endpoint = "satelliteImages"`

## Getting Started

To get started, and to use any of the above functions, first use the ` save_janes_key` function to save your Janes API key to your R environment:

```{r}
save_janes_key("JANES_API_KEY_GOES_HERE")
```
## Using janesR

As of 19 February 2021, all API endpoint-specific functions have been replaced with a single function: `get_janes`. Users specify an endpoint and, optionally, a country:

```{r}
## Pull all Russian ORBATs data
ru_orbat <- get_janes(country = "RU", endpoint = "orbats")

## Pull all Russian SAM sites data
ru_sam <- get_janes(country = "RU", endpoint = "samsites")

## Pull all Belgian, Chinese, and Canadian inventories
inventories <- get_janes(country = c("BE", "CN", "CA"), endpoint = "inventories")
```

As in the inventories example above, multiple countries can be queried by passing the country ISO codes in a vector. 

There are also functions available for saving data from the above endpoints into XML and JSON files: `get_janes_json` and `get_janes_xml`. 

**Parallel processing**

Users can now use parallel processing to significantly speed up API pulls (by ~75%, depending on users' computers and internet connection speeds) by using the optional argument `parallel = TRUE`.

```{r}
## Pull all bases data using parallel processing

n_cores <- availableCores() - 1
plan(multiprocess, workers = n_cores)

all_bases <- get_janes(endpoint = "bases", parallel = TRUE)
```

**XMLs from the Reference/News endpoints**

All Janes endpoints *except* the Reference and News endpoints natively return JSON data. Reference and News instead return XML versions of Janes online documents. As a result, using the argument `endpoint = "references"` or `endpoint = "news"` instead saves XMLs of the documents that fit your search criteria to your working directory. 

It is still possible to use parallel processing when saving XMLs. 

<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


