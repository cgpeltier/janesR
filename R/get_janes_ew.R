#' @title get_janes_ew
#' @description Pulls Janes EW sites data
#'
#' @param country Country in which EW site is located
#'
#' @return Janes equipment data.
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
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @export


#
# get_janes_ew <- function(country = NULL){
#   page_range <- get_page_range(country = country, endpoint = "ewsites")
#   ewsites <- map(page_range, ~ get_janes_info(x = .x, country = country,
#                                                endpoint = "ewsites")) %>%
#       bind_rows()
#   ewsites_data <- map(ewsites$url, get_janes_data)
#
#   ewsites_data %>%
#       tibble() %>%
#       conditional_unnest_wider(".") %>%
#       conditional_unnest_wider(".") %>%
#       conditional_unnest_wider("installation") %>%
#       conditional_unnest_wider("featureTypes") %>%
#       conditional_unnest_wider("featureType") %>%
#       conditional_unnest_wider("location") %>%
#       conditional_unnest_wider("associatedEquipments") %>%
#       conditional_unnest_wider("associatedEquipment") %>%
#       conditional_unnest_wider("equipmentId") %>%
#       conditional_unnest_wider("familyRootId") %>%
#       conditional_unnest_wider("equipmentName") %>%
#       conditional_unnest_wider("numberOfItems") %>%
#       conditional_unnest_wider("equipmentType") %>%
#       conditional_unnest_wider("rangeInMeters") %>%
#       janitor::clean_names()




get_janes_ew <- function(country = NULL){

  GET(paste0("https://developer.janes.com/api/v1/data/ewsites?f=countryiso(", country, ")&num=100000"),
      add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
      content(as = "text") %>%
      fromJSON() %>%
      pluck(2) %>%
      tibble() %>%
      pull(url) %>%
      map_dfr(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                  content(as = "text") %>%
                  fromJSON() %>%
                  tibble()) %>%
      conditional_unnest_wider(".") %>%
      unnest_all2() %>%
      unnest_all2() %>%
      unnest_all2() %>%
      unnest_all2()



}


