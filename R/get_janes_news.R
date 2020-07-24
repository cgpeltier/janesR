#' @title get_janes_news
#' @description Pulls Janes news articles for a given search.
#'
#' @param country Country filter for news
#' @param query Keyword search for news
#'
#' @return Returns Janes news articles in dataframe related to search.
#' @importFrom dplyr "%>%"
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @export



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


get_janes_news_info <- function(country = NULL, query = NULL, x){
  request <- httr::GET(url = paste0("https://developer.janes.com/api/v1/news?q=",
                              stringr::str_replace_all(query, " ", "%20"),
                              "&f=countryiso(",
                              country, ")", "&num=100",
                              "&pg=", x),
                 add_headers(Authorization = janes_key))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(response)[["results"]]
}



get_janes_news_text <- function(x){
  request <- httr::GET(url = x, add_headers(Authorization = janes_key))
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
    dplyr::mutate(news_text = purrr::map(url, get_janes_news_text)) %>%
    purrr:flatten(news_text) %>%
    dplyr::mutate(postDate = lubridate::ymd(stringr::str_remove(postDate, "T.+")),
           news_text = stringr::str_replace_all(news_text, "\\s{2,}", " "))
}
