#' @title get_page_range
#' @description Pulls Janes page ranges for all data endpoints. Helper function
#'
#' @param country Country filter for news
#' @param branch Military branch
#' @param type Of base
#' @param endpoint One of 6 options currently
#'
#' @return Janes page ranges for a given search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"


get_page_range <- function(country = NULL, branch = NULL, type = NULL,
                           endpoint = c("inventories", "equipment", "orbats",
                                             "bases", "airports")){
  request <- httr::GET(url = paste0("https://developer.janes.com/api/v1/data/",
                                    endpoint, "?f=",
                                    "&f=countryiso(",
                                    country, ")%3cand%3Ebranch(",
                                    stringr::str_replace_all(branch, " ", "%20"),
                                    ")%3cand%3etype(",
                                    type, ")",
                                    "&num=100"),
                       httr::add_headers(Authorization = janes_key))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  range_temp <- ceiling(jsonlite::fromJSON(response)[["metadata"]][["recordCount"]] / 100)
  seq(1:range_temp)
}


#' @export
