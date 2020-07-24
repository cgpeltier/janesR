#' @title get_janes_news
#' @description Pulls Janes news articles for a given search.
#'
#' @param country Country filter for news
#' @param query Keyword search for news
#'
#' @return Returns Janes news articles in dataframe related to search.
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom httr "GET"
#' @importFrom httr "content"
#' @importFrom jsonlite "fromJSON"
#' @importFrom xml2 "xml_children"
#' @importFrom xml2 "xml_find_all"
#' @importFrom xml2 "xml_text"
#'
#'
#'



get_news_page_range <- function(country = NULL, query = NULL){
  request <- GET(url = paste0("https://developer.janes.com/api/v1/news?q=",
                              str_replace_all(query, " ", "%20"),
                              "&f=countryiso(",
                              country, ")", "&num=100"),
                 add_headers(Authorization = janes_key))
  response <- content(request, as = "text", encoding = "UTF-8")
  range_temp <- ceiling(fromJSON(response)[["metadata"]][["recordCount"]] / 100)
  seq(1:range_temp)
}


get_janes_news_info <- function(country = NULL, query = NULL, x){
  request <- GET(url = paste0("https://developer.janes.com/api/v1/news?q=",
                              str_replace_all(query, " ", "%20"),
                              "&f=countryiso(",
                              country, ")", "&num=100",
                              "&pg=", x),
                 add_headers(Authorization = janes_key))
  response <- content(request, as = "text", encoding = "UTF-8")
  fromJSON(response)[["results"]]
}



get_janes_news_text <- function(x){
  request <- GET(url = x, add_headers(Authorization = janes_key))
  response <- content(request)

  response %>%
    xml_children() %>%
    xml_children() %>%
    xml_find_all("//janes:para") %>%
    xml_text() %>%
    paste(collapse = " ")
}


get_janes_news <- function(country = NULL, query = NULL){
  page_range <- get_news_page_range(country = country, query = query)
  news <- map(page_range, ~ get_janes_news_info(.x, country = country,
                                                query = query)) %>%
    bind_rows()
  news %>%
    mutate(news_text = map(url, get_janes_news_text)) %>%
    flatten(news_text) %>%
    mutate(postDate = ymd(str_remove(postDate, "T.+")),
           news_text = str_replace_all(news_text, "\\s{2,}", " "))
}
