#' @title get_janes_sat_images
#' @description Pulls Janes satellite images
#'
#' @param country Country of satellite image target
#' @param query Keyword search
#' @param pics If TRUE, returns satellite images as JPG files. If FALSE, returns metadata
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
#' @importFrom purrr safely
#' @importFrom dplyr filter
#' @importFrom magick image_read
#' @importFrom magick image_write
#' @export



get_janes_sat_images <- function(country = NULL, query = NULL, pics = FALSE){
    page_range <- get_page_range(country = country, endpoint = "satelliteImages",
                                 query = str_replace_all(query, " ", "%20"))

    images <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                               endpoint = "satelliteImages",
                                               query = str_replace_all(query, " ", "%20"))) %>%
        bind_rows()

    images_data <- map(images$url, get_janes_data)

    images_data2 <- images_data %>%
        tibble() %>%
        unnest_wider(".") %>%
        unnest_wider(".") %>%
        unnest_wider("image") %>%
        filter(!is.na(id)) %>%
        select(-any_of("...1"))


    if(pics == TRUE){
        images_data2 <- images_data2 %>%
            mutate(url = paste0("https://developer.janes.com/api/v1/images/",
                                str_extract(source, "(?<=\\/)[^\\/]+$")),
                   url = str_remove(url, ".jpg"),
                   url = str_remove(url, "(?<=\\/)x"))


        map(images_data2$url, purrr::safely(~ GET(url = .x,
                                           add_headers(Authorization = Sys.getenv("JANES_KEY"))) %>%
                                        content() %>%
                                        magick::image_read() %>%
                                        magick::image_write(path = paste0(getwd(), "/sat_image_",
                                                                          str_extract(.x, "(?<=\\/)[^\\/]+$"), ".jpg"))))

    } else {
      images_data2

      }

}




