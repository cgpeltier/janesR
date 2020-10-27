#' @title get_janes_markets_forecast
#' @description Pulls Janes Markets Forecast data.
#'
#' @param end_user_country Country in which base is located
#' @param query Search term
#' @param market JMF market to query
#' @param subsystems Include only subsystems data for a given search. Logical.
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



get_janes_markets_forecast <- function(end_user_country = NULL, query = NULL, market = NULL,
                                       subsystems = FALSE){

  page_range <- get_page_range(end_user_country = end_user_country, endpoint = "marketforecasts",
                               query = str_replace_all(query, " ", "%20"),
                               market = market)

  jmf <- map(page_range, ~ get_janes_info(x = .x, end_user_country = end_user_country,
                                          endpoint = "marketforecasts",
                                          query = str_replace_all(query, " ", "%20"),
                                          market = market)) %>%
    bind_rows()


  jmf_data <- map(jmf$url, get_janes_data)

  jmf_data2 <- jmf_data %>%
      tibble() %>%
      conditional_unnest_wider(".") %>%
      tibble() %>%
      conditional_unnest_wider(".") %>%
      conditional_unnest_wider(".") %>%
      conditional_unnest_wider("platform") %>%
      conditional_unnest_wider("supplier") %>%
      conditional_unnest_wider("endUser") %>%
      conditional_unnest_wider("lifeCycle") %>%
      conditional_unnest_wider("marketsSpecificAttributes") %>%
      conditional_unnest_wider2("finalAssembly") %>%
      conditional_unnest_wider2("country") %>%
      conditional_unnest_wider("slsInformation") %>%
      conditional_unnest_wider("documentation") %>%
      conditional_unnest_wider("finalAssembly_country") %>%
      conditional_unnest_wider2("name") %>%
      conditional_unnest_wider2("percentageOfForecast") %>%
      conditional_unnest_wider2("slsForecast") %>%
      conditional_unnest_wider2("productionForecast") %>%
      conditional_unnest_wider2("rAndDForecast") %>%
      conditional_unnest_wider2("totalProgramForecast") %>%
      conditional_unnest_wider2("unitsForecast") %>%
      conditional_unnest_wider2("slsForecast") %>%
      conditional_unnest_wider2("productionForecast") %>%
      conditional_unnest_wider2("slsForecast_values") %>%
      conditional_unnest_wider2("productionForecast_values") %>%
      conditional_unnest_wider2("rAndDForecast_values") %>%
      conditional_unnest_wider2("totalProgramForecast_values") %>%
      conditional_unnest_wider2("unitsForecast_values")


  if(subsystems == TRUE){
      jmf_data3 <- jmf_data2 %>%
        conditional_unnest_wider2("subsystemSuppliers") %>%
        conditional_unnest_wider2("subSystemType") %>%
        conditional_unnest_wider("subsystemSuppliers_name") %>%
        conditional_unnest_wider("subsystemSuppliers_subSystemType") %>%
        conditional_unnest_wider("subsystemSuppliers_percentageOfForecast") %>%
        conditional_unnest_wider("subsystemSuppliers") %>%
        conditional_unnest_wider2("subsystemSuppliers_slsForecast") %>%
        conditional_unnest_wider2("subsystemSuppliers_productionForecast") %>%
        conditional_unnest_wider2("subsystemSuppliers_rAndDForecast") %>%
        conditional_unnest_wider2("subsystemSuppliers_totalProgramForecast") %>%
        conditional_unnest_wider2("subsystemSuppliers_unitsForecast") %>%
        conditional_unnest_wider2("subsystemSuppliers_slsForecast_values") %>%
        conditional_unnest_wider2("subsystemSuppliers_productionForecast_values") %>%
        conditional_unnest_wider2("subsystemSusubsystemSuppliers_rAndDForecast_valuesppliers") %>%
        conditional_unnest_wider2("subsystemSuppliers_totalProgramForecast_values") %>%
        conditional_unnest_wider2("subsystemSuppliers_unitsForecast_values") %>%
        select(-where(is.list)) %>%
        janitor::clean_names()


        jmf_data3 %>%
            select(id1 = id, starts_with("subsystem_suppliers")) %>%
            mutate(across(everything(), as.character),
                   id2 = id1, id3 = id1, id4 = id1, id5 = id1, id6 = id1, id7 = id1,
                   id8 = id1, id9 = id1, id10 = id1, id11 = id1, id12 = id1,
                   id13 = id1, id14 = id1, id15 = id1, id16 = id1, id17 = id1,
                   id18 = id1, id19 = id1, id20 = id1) %>%
            pivot_longer(names_to = c(".value", "subsystem_suppliers"),
                         names_pattern = "([A-Za-z_]+)(\\d{1,2}$)",
                         cols = everything()) %>%
            filter(!is.na(subsystem_suppliers_name)) %>%
            select(-subsystem_suppliers)


  } else{ jmf_data2 %>% janitor::clean_names() %>% select(-where(is.list)) }

}


#' @export
