#' @title get_janes_data_xml
#' @description Pulls Janes data for all data endpoints. Helper function.
#'
#' @param x For iteration on URLs
#'
#' @return Janes data. Used in all "get_janes_x" functions.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text


get_janes_data_xml <- function(x){
  request <- GET(url = x, add_headers(Authorization = Sys.getenv("JANES_KEY")))
  request %>%
    content()

}

#' @export



