#' @title get_page_range
#' @description Pulls Janes page ranges for all data endpoints. Helper function
#'
#' @param country Country filter for news
#' @param branch Military branch
#' @param type Depends on endpoint
#' @param endpoint One of 6 options currently
#' @param query Search term
#' @param environment Of search, i.e. "Air"
#' @param operator_force Operator force
#'
#' @return Janes page ranges for a given search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"


get_page_range <- function(country = NULL, branch = NULL, type = NULL,
                           operator_force = NULL, query = NULL, environment = NULL,
                           post_date = NULL, event_type = NULL,
                           endpoint = c("inventories", "equipment", "orbats",
                                        "bases", "airports", "countryrisks",
                                        "companies", "events", "equipmentrelationships",
                                        "references", "samsites", "ewsites",
                                        "satelliteImages")){

    if (endpoint == "references") {endpoint2 <- endpoint } else
        {endpoint2 <- paste0("data/", endpoint)}

    countries <- paste0(country, collapse = ")%3Cor%3Ecountryiso(")

    response <- httr::GET(url = paste0("https://developer.janes.com/api/v1/",
                                    endpoint2,"?q=",
                                    str_replace_all(query, " ", "%20"),
                                    "&f=countryiso(",
                                    countries,
                                    ")%3Cand%3ESOURCE_TYPE(",
                                    str_replace_all(event_type, " ", "%20"),
                                    ")%3Cand%3EpostDate(",
                                    str_replace_all(post_date, "::", "%3A%3A"),
                                    ")%3cand%3Ebranch(",
                                    stringr::str_replace_all(branch, " ", "%20"),
                                    ")%3Cand%3EoperatorForce(",
                                    stringr::str_replace_all(operator_force, " ", "%20"),
                                    ")%3cand%3Etype(",
                                    type,
                                    ")%3Cand%3Eenvironment(",
                                    environment,
                                    ")&num=100"),
                       httr::add_headers(Authorization = Sys.getenv("JANES_KEY"))) %>%
            httr::content()

    range_temp <- ceiling(response[["metadata"]][["recordCount"]] / 100)

    seq(1:range_temp)

}


#' @export
