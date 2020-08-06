#' @title save_janes_key
#' @description Saves Janes key to environment
#'
#' @param key Janes API key
#'
#' @return Saves Janes API key to environment.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @export



save_janes_key <- function(key){
    Sys.setenv(JANES_KEY = key)

    # apikey <- paste0("JANES_KEY=", key)
    # file <- file.path(path.expand("~"), ".Renviron")
    # cat(apikey, file = file, append = TRUE, fill = TRUE)
}


#' @export
