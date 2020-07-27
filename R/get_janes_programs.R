#' @title get_janes_programs
#' @description Pulls Janes defense programs data
#'
#' @param operator_country Country to pull programs from
#'
#' @return Janes defense programs data.
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
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom dplyr if_else
#' @importFrom tidyr fill
#' @importFrom tidyr pivot_wider
#' @export



get_janes_programs <- function(operator_country = NULL){
  page_range <- get_page_range_programs(operator_country = operator_country)
  programs <- map(page_range, ~ get_janes_info_programs(x = .x, operator_country = operator_country)) %>%
      bind_rows()
  programs_data <- map(programs$url, get_janes_data)

  programs_data %>%
      unlist() %>%
      enframe() %>%
      unnest(cols = c("name", "value")) %>%
      mutate(id = if_else(name == "..programme.id", value, NA_character_)) %>%
      fill(id) %>%
      pivot_wider(names_from = name, values_from = value, id_cols = id) %>%
      janitor::clean_names() %>%
      janitor::remove_empty()
}


#' @export
