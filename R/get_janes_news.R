#' @title get_janes_news
#' @description Pulls Janes news articles page range for a given search. Helper function.
#'
#' @param country Country filter for news
#' @param query Keyword search for news
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
#' @export


get_janes_news_info <- function(country = NULL, query = NULL, x){
  httr::request <- GET(url = paste0("https://developer.janes.com/api/v1/news?q=",
                              stringr::str_replace_all(query, " ", "%20"),
                              "&f=countryiso(",
                              country, ")", "&num=100",
                              "&pg=", x),
                 httr::add_headers(Authorization = janes_key))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(response)[["results"]]
}



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


get_janes_news <- function(country = NULL, query = NULL){
  page_range <- get_news_page_range(country = country, query = query)
  news <- purrr::map(page_range, ~ get_janes_news_info(.x, country = country,
                                                query = query)) %>%
    bind_rows()

  news %>%
    mutate(news_text = purrr::map(url, get_janes_news_text)) %>%
    flatten(news_text) %>%
    mutate(postDate = lubridate::ymd(stringr::str_remove(postDate, "T.+")),
           news_text = stringr::str_replace_all(news_text, "\\s{2,}", " "))
}

#' @export
