#' Prepare SINAN violence microdata
#'
#' Recodes supported fields from SINAN violence notifications into descriptive
#' values and normalizes escaped Unicode text. Columns not explicitly recoded
#' are retained, but the returned tibble contains character columns.
#'
#' @param data A data frame returned by [fetch_datasus()] with
#'   `information_system = "SINAN-VIOL"`, or a compatible layout.
#' @param municipality_data Logical scalar retained for API compatibility. It is
#'   not currently used by this processing function.
#'
#' @examples
#' \dontrun{
#'   sinan_viol_raw <- fetch_datasus(
#'     year_start = 2023,
#'     year_end = 2023,
#'     information_system = "SINAN-VIOL"
#'   )
#'   sinan_viol <- process_sinan_viol(sinan_viol_raw)
#' }
#'
#' @return A tibble with character columns. Supported codes are replaced with
#'   descriptions.
#'
#' @seealso [fetch_datasus()]
#'
#' @export
process_sinan_viol <- function(data, municipality_data = TRUE) {
  variables_names <- names(data)

  if ("TP_NOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TP_NOT = dplyr::recode_values(
          .data$TP_NOT,
          "1" ~ "Negativa",
          "2" ~ "Individual",
          "3" ~ "Surto",
          "4" ~ "Agregado",
          default = .data$TP_NOT
        )
      ) %>%
      dplyr::mutate(TP_NOT = as.factor(.data$TP_NOT))
  }

  data <- tibble::as_tibble(data)
  data <- droplevels(data)
  data <- suppressWarnings(tibble::as_tibble(lapply(
    X = data,
    FUN = stringi::stri_unescape_unicode
  )))

  return(data)
}
