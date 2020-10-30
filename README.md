# janesR
<img src=images/hex-janesR2.png align="right" alt="" width="120" />

janesR is a wrapper for the Janes API that allows Janes users to easily pull data from the API into tibbles in R. Documentation for the API can be found at the [Janes Developer site](https://developer.janes.com/). 

You can install janesR from github: 
```{r}
devtools::install_github("cgpeltier/janesR")
```
## Using janesR 
The package currently supports the following Janes API endpoints:

* Airports: `get_janes_airports`
* Bases: `get_janes_bases`
* Companies: `get_janes_companies`
* Country Risk: `get_janes_country_risks` 
* Defence Programs: `get_janes_programs`
* Early Warning Sites: `get_janes_ew`
* Equipment: `get_janes_equipment`
* Equipment Relationships: `get_janes_equipment_relationships`
* Events: `get_janes_events`
* Intelligence Events: `get_janes_events_intel`
* Inventories: `get_janes_inventories`
* Markets Forecast: `get_janes_markets_forecast`
* News: `get_janes_news`
* ORBATs: `get_janes_orbats`
* SAM Sites: `get_janes_sam`
* Satellite Images: `get_janes_sat_images`
* Terrorist Events: `get_janes_events_jtic` 

There are also functions available for saving data from the above endpoints into XML and JSON files: `get_janes_json` and `get_janes_xml`. 

To get started, and to use any of the above functions, first use the ` save_janes_key` function to save your Janes API key to your R environment:

```{r}
save_janes_key("JANES_API_KEY_GOES_HERE")
```

# Examples

```{r}
ru_orbat <- get_janes_orbats(country = "RU")
ru_sam <- get_janes_sam(country = "RU")
inventories <- get_janes_inventories(country = c("BE", "CN", "CA"))
```

As in the inventories example above, multiple countries can be queried by passing the country ISO codes in a vector. 


<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


