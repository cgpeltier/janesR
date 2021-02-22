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
#' @importFrom purrr map
#' @importFrom jsonlite flatten
#' @importFrom dplyr bind_rows
#' @export

get_janes_news <- function(country = NULL, query = NULL){
    page_range <- get_page_range(country = country, query = query, endpoint = "news")

    news <- purrr::map(page_range, ~ get_janes_info(.x, country = country,
                                                  query = query, endpoint = "news")) %>%
      dplyr::bind_rows()

    news %>%
      dplyr::mutate(news_text = purrr::map(url, get_janes_news_text)) %>%
      jsonlite::flatten(news_text) %>%
      dplyr::mutate(postDate = lubridate::ymd(stringr::str_remove(postDate, "T.+")),
             news_text = stringr::str_replace_all(news_text, "\\s{2,}", " "))
}

