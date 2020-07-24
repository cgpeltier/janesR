#' @title get_news_page_range
#' @description Pulls Janes news articles page range for a given search. Helper function.
#'
#' @param country Country filter for news
#' @param query Keyword search for news
#'
#' @return Helper function to return Janes news article page range related to search.
#' @importFrom dplyr "%>%"
#' @importFrom httr GET
#' @export GET
#' @importFrom httr content
#' @export httr content
#' @importFrom jsonlite fromJSON
#' @export fromJSON
#' @importFrom stringr str_replace_all
#' @export str_replace_all
#' @export get_news_page_range


get_news_page_range <- function(country = NULL, query = NULL){
  request <- httr::GET(url = paste0("https://developer.janes.com/api/v1/news?q=",
                                    stringr::str_replace_all(query, " ", "%20"),
                                    "&f=countryiso(",
                                    country, ")", "&num=100"),
                       add_headers(Authorization = janes_key))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  range_temp <- ceiling(jsonlite::fromJSON(response)[["metadata"]][["recordCount"]] / 100)
  seq(1:range_temp)
}
