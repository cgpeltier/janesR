# janesR
<img src=images/hex-janesR2.png align="right" alt="" width="120" />

janesR is a wrapper for the Janes API that allows Janes users to easily pull data from the API into tibbles in R. Documentation for the API can be found at the [Janes Developer site](https://developer.janes.com/). 

The package currently supports the following API endpoints:

* Airports
* Bases
* Defence Programs
* Equipment
* Inventory
* Orbats
* News

Functions for the other API endpoints will be added shortly. Additional options for more customized API calls will also be added to existing janesR functions.

You can install janesR from github: 
```{r}
devtools::install_github("cgpeltier/janesR")
```

<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->


