#' @title get_janes_info
#' @description Pulls Janes data info for all data endpoints. Helper function.
#'
#' @param country Country filter for news
#' @param branch Military branch
#' @param type Of base
#' @param endpoint One of 6 options currently
#' @param x For iteration on URLs
#'
#' @return Helper function to return Janes news article page range related to search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"


get_janes_info <- function(x, country = NULL, branch = NULL, type = NULL,
                                endpoint = c("inventories", "equipment", "orbats",
                                             "bases", "airports", "defenceprogrammes")){
    request <- GET(url = paste0("https://developer.janes.com/api/v1/data/",
                                endpoint, "?&f=countryiso(",
                                country, ")%3cand%3Ebranch(",
                                str_replace_all(branch, " ", "%20"),
                                ")%3cand%3etype(",
                                type, ")",
                                "&num=100", "&pg=", x),
                   add_headers(Authorization = janes_key))
    response <- content(request, as = "text", encoding = "UTF-8")
    fromJSON(response)[["results"]]
}




#' @export
