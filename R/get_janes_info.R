#' @title get_janes_info
#' @description Pulls Janes data info for all data endpoints. Helper function.
#'
#' @param country Country filter for news
#' @param branch Military branch
#' @param type Depends on endpoint
#' @param endpoint One of 6 options currently
#' @param query Search term
#' @param environment Of search, i.e. "Air"
#' @param operator_force Operator force
#' @param market Markets Forecast market
#' @param event_type Event type - JTIC or Intel Event
#' @param end_user_country JMF end user country
#' @param query Query
#' @param post_date Event post date
#' @param start_date Event start date
#' @param overall_family Overall equipment family
#'
#' @return Helper function to return Janes news article page range related to search.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#'


get_janes_info <- function(x, country = NULL, branch = NULL, type = NULL,
                           operator_force = NULL, query = NULL, environment = NULL,
                           post_date = NULL, start_date = NULL, event_type = NULL,
                           market = NULL, end_user_country = NULL,
                           overall_family = NULL,
                           endpoint = c("inventories", "equipment", "orbats", "news",
                                        "bases", "airports", "countryrisks",
                                        "companies", "events", "equipmentrelationships",
                                        "references", "samsites", "ewsites",
                                        "satelliteImages", "marketforecasts")){


    if(endpoint %in% c("references", "news")){
            endpoint2 <- endpoint
    }else{
      endpoint2 <- paste0("data/", endpoint)}



    countries <- paste0(country, collapse = ")%3Cor%3Ecountryiso(")

    request <- GET(url = paste0("https://developer.janes.com/api/v1/",
                                endpoint2, "?q=",
                                query,
                                "&f=countryiso(",
                                countries,
                                ")%3Cand%3Emarket(",
                                str_replace_all(market, " ", "%20"),
                                ")%3Cand%3EENDUSERCOUNTRY(",
                                str_replace_all(end_user_country," ", "%20"),
                                ")%3Cand%3ESOURCE_TYPE(",
                                str_replace_all(event_type, " ", "%20"),
                                ")%3Cand%3EPOST_DATE(",
                                str_replace_all(post_date, "::", "%3A%3A"),
                                ")%3Cand%3Estart_Date(",
                                str_replace_all(start_date, "::", "%3A%3A"),
                                ")%3cand%3Ebranch(",
                                str_replace_all(branch, " ", "%20"),
                                ")%3Cand%3EoperatorForce(",
                                stringr::str_replace_all(operator_force, " ", "%20"),
                                ")%3cand%3etype(",
                                type,
                                ")%3Cand%3Eoverallfamily(",
                                overall_family,
                                ")%3Cand%3Eenvironment(",
                                environment,
                                ")&num=1000", "&pg=", x),
                   add_headers(Authorization = Sys.getenv("JANES_KEY")))
    response <- content(request, as = "text", encoding = "UTF-8")
    fromJSON(response)[["results"]]


}




#' @export
