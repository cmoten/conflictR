#'Tidy ACLED Data into a time series
#'
#' @param file path for ACLED data
#'
#' @return tstibble with columns for Quarter, EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY, ACTOR1
#'
#' @example
#' get_acled_ts(data_file_path)
#'
#' @export

get_acled_ts <- function(data_file_path){
  acled_data <- readxl::read_excel(data_file_path)

  acled_ts_tibble <- acled_data %>%
    select(EVENT_DATE, EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY, ACTOR1) %>%
    dplyr::mutate(Quarter = tsibble::yearquarter(EVENT_DATE)) %>%
    dplyr::select(-EVENT_DATE) %>%
    group_by(Quarter, EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY, ACTOR1) %>%
    count() %>%
    tsibble::as_tsibble(key = c(EVENT_TYPE, SUB_EVENT_TYPE, COUNTRY, ACTOR1),
                        index = Quarter) %>%
    tsibble::fill_gaps(n = 0) %>%
    dplyr::ungroup()

  return(acled_ts_tibble)
}
