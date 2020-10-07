#' @title get_col_classes
#' @description Get col classes of
#'
#' @param country Country in which SAM site is located
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
#' @importFrom purrr imap_dfr
#' @importFrom stringr str_c


get_col_classes <-function(data){
  data %>%
    imap_dfr(~ tibble(colname = .y, classes = class(.x) %>% str_c(collapse = ", "))) %>%
    View("col_classes")

}



#' @export




