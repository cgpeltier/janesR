#' @title get_janes_news_text
#' @description Pulls Janes news articles page range for a given search. Helper function.
#'
#' @param x urls to pull from
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


get_janes_news_text <- function(x){
  request <- httr::GET(url = x, httr::add_headers(Authorization = janes_key))
  response <- httr::content(request)

  response %>%
    xml2::xml_children() %>%
    xml2::xml_children() %>%
    xml2::xml_find_all("//janes:para") %>%
    xml2::xml_text() %>%
    paste(collapse = " ")
}

#' @export

