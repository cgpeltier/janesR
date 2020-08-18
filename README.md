# janesR
<img src=images/hex-janesR2.png align="right" alt="" width="120" />

janesR is a wrapper for the Janes API that allows Janes users to easily pull data from the API into tibbles in R. Documentation for the API can be found at the [Janes Developer site](https://developer.janes.com/). 

You can install janesR from github: 
```{r}
devtools::install_github("cgpeltier/janesR")
```
## Using janesR 
The package currently supports the following API endpoints:

* Airports: `get_janes_airports`
* Bases: `get_janes_bases`
* Companies: `get_janes_companies`
* Country Risk: `get_janes_country_risks` 
* Defence Programs: `get_janes_programs`
* Equipment: `get_janes_equipment`
* Equipment Relationships: `get_janes_equipment_relationships`
* Intelligence Events: `get_janes_events`
* Inventories: `get_janes_inventories`
* News: `get_janes_news`
* ORBATs: `get_janes_orbats`

If you'd like to save your output as a JSON file, I recommend using the `jsonlite::toJSON` function on the returned data frame. 

Functions for the other API endpoints will be added shortly. 

To get started, and to use any of the above functions, first use the ` save_janes_key` function to save your Janes API key to your R environment:
```{r}
save_janes_key("JANES_API_KEY_GOES_HERE")
```

<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


