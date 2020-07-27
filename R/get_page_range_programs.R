#' @title get_page_range_programs
#' @description Pulls Janes page ranges for defense programs. Helper function
#'
#' @param operator_country Country filter for news
#'
#' @return Janes page ranges for a given search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"


get_page_range <- function(country = NULL){
  request <- httr::GET(url = paste0("https://developer.janes.com/api/v1/data/defenceprogrammes?f=operatorCountry(",
                                    operator_country = stringr::str_replace_all(operator_country, " ", "%20"),
                                    ")&num=100"),
                       httr::add_headers(Authorization = janes_key))
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  range_temp <- ceiling(jsonlite::fromJSON(response)[["metadata"]][["recordCount"]] / 100)
  seq(1:range_temp)
}


#' @export
