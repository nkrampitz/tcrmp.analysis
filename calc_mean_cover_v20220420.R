#' TCRMP Benthic Cover Summary
#' @description Summarize benthic cover collected by the US Virgin
#'   Islands Territorial Coral Reef Monitoring Program.
#'
#' @param df a data frame containing raw benthic cover data.
#' @param sum_by a character vector containing variable(s) by which to
#'   summarize benthic cover.
#'
#' @import dplyr
#' @importFrom rlang syms
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#'
#' @export calc_mean_cover
#'
#' @examples
#' calc_mean_cover(tcrmp_benthic, c("SampleYear", "Location"))
#'
#' calc_mean_cover(tcrmp_benthic, "Location")

calc_mean_cover <- function(df, sum_by) {

  # Bind variables locally to function
  AA <- ClionaCov <- Transect <- Type <- Value <- NULL

  # Calculate mean benthic cover
  benthic_mean <- df |>
    # Group specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate mean benthic cover for all columns (cover categories)
    summarise(across(AA:ClionaCov, ~mean(.x, na.rm = TRUE), .names = "{.col}")) |>
    # Convert from wide to long format
    pivot_longer(c(AA:ClionaCov), names_to = "Cover", values_to = "Value") |>
    # Add column indicating values are mean
    add_column(Type = "Avg", .after = "Cover")

  # Calculate SEM by Location for each sampling period
  benthic_sem <- df |>
    # Group specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate SEM for all columns (cover categories)
    summarise(across(AA:ClionaCov, ~sd(.x, na.rm = TRUE)/sqrt(length(which(Transect > 0))), .names = "{.col}")) |>
    # Convert from wide to long format
    pivot_longer(c(AA:ClionaCov), names_to = "Cover", values_to = "Value") |>
    # Add column indicating values are SEM
    add_column(Type = "SEM", .after = "Cover")

  # Combine mean benthic cover and SEM into one data frame
  rbind(benthic_mean, benthic_sem) |>
    # Split mean and SEM into their own columns
    spread(Type, Value)

}
