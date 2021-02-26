#' @title get_janes
#' @description General interface to Janes data
#'
#' @param country Country filter for equipment
#' @param parallel Whether to use parallel processing
#' @param endpoint What endpoint to pull data from

#'
#' @return Janes data
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom purrr safely
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
# IE vs JTIC events filter
# Handle JMF and JDP programme stuff
# support for queries




get_janes <- function(country = NULL, parallel = FALSE,
                      post_date = NULL, include_del = FALSE,
                      source_type = NULL,
                      endpoint = c("inventories", "equipment", "orbats",
                                   "bases", "airports", "countryrisks",
                                   "companies", "events", "equipmentrelationships",
                                   "samsites", "ewsites","nuclearsites",
                                   "satelliteImages", "marketforecasts",
                                   "defenceprogrammes", "references", "news")) {

  countries <- paste0(country, collapse = ")%3Cor%3Ecountryiso(")
  post_dates <- str_replace(post_date, "::", "%3A%3A")
  sources <- str_replace_all(source_type, " ", "%20")

  if(endpoint %in% c("references", "news")) {
      ## to save all XMLs

      request_url <- paste0("https://developer.janes.com/api/v1/", endpoint,
                          "?f=countryiso(", countries, ")%3Cand%3EpostDate(",
                          post_dates, ")&num=100000&includeDeleted=", include_del)


      if(parallel == FALSE){

        GET(request_url, add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
            content(as = "text") %>%
            fromJSON() %>%
            pluck(2) %>%
            tibble() %>%
            dplyr::pull(url) %>%
            purrr::map(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                         content() %>%
                         xml2::as_xml_document() %>%
                         xml2::write_xml(., stringr::str_extract(.x, "(?<=\\/)[^\\/]+$"), ".xml"))

        } else {

          GET(request_url, add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
            content(as = "text") %>%
            fromJSON() %>%
            pluck(2) %>%
            tibble() %>%
            dplyr::pull(url) %>%
            furrr::future_map(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                                content() %>%
                                xml2::as_xml_document() %>%
                                xml2::write_xml(., stringr::str_extract(.x, "(?<=\\/)[^\\/]+$"), ".xml"))



      }



  } else if(endpoint == "events") {

      request_url <- paste0("https://developer.janes.com/api/v1/data/", endpoint,
                            "?f=countryiso(", countries, ")%3Cand%3EpostDate(",
                            post_dates, ")%3Cand%3Esource_type(", sources,
                            ")&num=100000&includeDeleted=", include_del)

      if(parallel == FALSE){

        GET(request_url, add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
          content(as = "text") %>%
          fromJSON() %>%
          pluck(2) %>%
          tibble() %>%
          dplyr::pull(url) %>%
          map(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                content(as = "text") %>%
                fromJSON() %>%
                tibble())  %>%
          tibble() %>%
          conditional_unnest_wider(".") %>%
          conditional_unnest_wider(".") %>%
          unnest_all2() %>%
          unnest_all2() %>%
          unnest_all2() %>%
          unnest_all2() %>%
          unnest_all2() %>%
          unnest_all2()


    } else {

       GET(request_url, add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
         content(as = "text") %>%
         fromJSON() %>%
         pluck(2) %>%
         tibble() %>%
         dplyr::pull(url) %>%
         future_map(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
               content(as = "text") %>%
               fromJSON() %>%
               tibble())  %>%
         tibble() %>%
         conditional_unnest_wider(".") %>%
         conditional_unnest_wider(".") %>%
         unnest_all2() %>%
         unnest_all2() %>%
         unnest_all2() %>%
         unnest_all2() %>%
         unnest_all2() %>%
         unnest_all2()

     }



    } else {
      ## for all JSON data

      request_url <- paste0("https://developer.janes.com/api/v1/data/", endpoint,
                            "?f=countryiso(", countries, ")%3Cand%3EpostDate(",
                            post_dates, ")%3Cand%3Esource_type(", sources,
                            ")&num=100000&includeDeleted=", include_del)


      if(parallel == FALSE){

        GET(request_url, add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
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
          unnest_all2() %>%
          unnest_all2()

      }

      else {

        GET(request_url, add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
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
          unnest_all2() %>%
          unnest_all2()

        }
      }



}



#' @export
