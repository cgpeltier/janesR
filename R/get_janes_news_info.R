#' @title get_janes_news_info
#' @description Pulls Janes news articles page range for a given search. Helper function.
#'
#' @param country Country filter for news
#' @param query Keyword search for news
#' @param x Page number; used for iteration in other functions.
#'
#' @return Helper function to return Janes news article page range related to search.
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom stringr str_remove

get_janes_news_info <- function(country = NULL, query = NULL, x){
  request <- httr::GET(url = paste0("https://developer.janes.com/api/v1/news?q=",
                                    stringr::str_replace_all(query, " ", "%20"),
                                    "&f=countryiso(",
                                    country, ")", "&num=100",
                                    "&pg=", x),
                       httr::add_headers(Authorization = janes_key))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(response)[["results"]]
}

#' @export
