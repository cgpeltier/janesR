#' @title get_janes_info_programs
#' @description Pulls Janes data info for all data endpoints. Helper function.
#'
#' @param operator_country Country filter for news
#' @param x For iteration on URLs
#'
#' @return Helper function to return Janes news article page range related to search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"


get_janes_info <- function(x, operator_country = NULL){
  request <- GET(url = paste0("https://developer.janes.com/api/v1/data/defenceprogrammes?f=operatorCountry(",
                              operator_country = stringr::str_replace_all(operator_country, " ", "%20"),
                              ")&num=100", "&pg=", x),
                 add_headers(Authorization = janes_key))
  response <- content(request, as = "text", encoding = "UTF-8")
  fromJSON(response)[["results"]]
}




#' @export

