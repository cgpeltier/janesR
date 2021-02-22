#' @title get_janes
#' @description General interface to Janes data
#'
#' @param country Country filter for equipment
#' @param parallel Whether to use parallel processing
#' @param endpoint What endpoint to pull data from

#'
#' @return Janes structured data
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom jsonlite flatten
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr unnest_auto
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom janitor clean_names
#' @importFrom janitor remove_empty
#' @importFrom dplyr starts_with
#' @importFrom dplyr any_of
#' @importFrom purrr pluck
#' @importFrom dplyr pull
#' @importFrom dplyr bind_cols
#' @importFrom furrr future_map_dfr
#' @export


## add:
# Check on multi-country search support
# IE vs JTIC events filter
# News and ref support
# Support for defence programmes
# Handle JMF and JDP programme stuff

get_janes <- function(country = NULL, parallel = FALSE,
                      endpoint = c("inventories", "equipment", "orbats",
                                   "bases", "airports", "countryrisks",
                                   "companies", "events", "equipmentrelationships",
                                   "samsites", "ewsites","nuclearsites",
                                   "satelliteImages", "marketforecasts",
                                   "defenceprogrammes")) {

  countries <- paste0(country, collapse = ")%3Cor%3Ecountryiso(")


  if(parallel == FALSE){

      GET(paste0("https://developer.janes.com/api/v1/data/", endpoint,
                 "?f=countryiso(", countries, ")&num=100000"),
          add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
        content(as = "text") %>%
        fromJSON() %>%
        pluck(2) %>%
        tibble() %>%
        dplyr::pull(url) %>%
        map_dfr(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                  content(as = "text") %>%
                  fromJSON() %>%
                  tibble()) %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider(".") %>%
        unnest_all2() %>%
        unnest_all2() %>%
        unnest_all2() %>%
        unnest_all2() %>%
        unnest_all2()
  }

  else {

    GET(paste0("https://developer.janes.com/api/v1/data/", endpoint,
               "?f=countryiso(", country, ")&num=100000"),
        add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
      content(as = "text") %>%
      fromJSON() %>%
      pluck(2) %>%
      tibble() %>%
      dplyr::pull(url) %>%
      future_map_dfr(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                content(as = "text") %>%
                fromJSON() %>%
                tibble()) %>%
      conditional_unnest_wider(".") %>%
      conditional_unnest_wider(".") %>%
      unnest_all2() %>%
      unnest_all2() %>%
      unnest_all2() %>%
      unnest_all2() %>%
      unnest_all2()


  }


}



#' @export
