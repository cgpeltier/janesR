#' @title save_janes_key
#' @description saves_janes_key
#'
#' @param key API key
#'
#' @return Janes page ranges for a given search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"


save_janes_key <- function(key){
    apikey <- paste0("JANES_KEY3=", "'", key, "'")
    file <- file.path(path.expand("~"), ".Renviron")
    cat(apikey, file = file, append = TRUE, fill = TRUE)
}


