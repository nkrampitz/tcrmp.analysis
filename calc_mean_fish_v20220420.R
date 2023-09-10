#' TCRMP Fish Summary
#' @description Summarizes raw fish data collected by the US Virgin
#'   Islands Territorial Coral Reef Monitoring Program (TCRMP).
#'
#' @param df a data frame containing raw fish data.
#' @param data_type a character value that specifies what kind of data
#'   is contained in \code{df}.
#' @param sum_by a character vector containing variable(s) by which to
#'   summarize fish data.
#'
#' @import dplyr
#' @importFrom rlang syms
#' @importFrom stats sd
#' @importFrom tibble add_column
#'
#' @export calc_mean_fish
#'
#' @examples
#' calc_mean_fish(tcrmp_fish, "Abundance",
#'                c("SampleYear", "Location"))
#'
#' calc_mean_fish(tcrmp_fish, "Abundance", "Location")
#'
#'
#' calc_mean_fish(tcrmp_fish, "Biomass",
#'                c("SampleYear", "Location"))
#'
#' calc_mean_fish(tcrmp_fish, "Biomass", "Location")

calc_mean_fish <- function(df, data_type, sum_by) {

  # Bind variables locally to function
  Location <- SampleYear <- Period <- Transect <- Total <- Tot <- Metric <- NULL

  if(any(grepl("Name", sum_by)) == TRUE) {

    # Calculate mean fish and SEM by species
    df |>
      # Filter for correct metric
      filter(Metric == data_type) |>
      # Group by specified variables
      group_by(!!!syms(sum_by)) |>
      # Calculate mean fish and SEM
      summarise(Avg = mean(Total, na.rm = TRUE),
                SEM = sd(Total, na.rm = TRUE)/sqrt(length(which(Transect > 0)))) |>
      # Create new column indicating values are abundance
      add_column(Metric = data_type, .after = 1)

  } else {

    # Calculate mean fish and SEM
    df |>
      # Filter for correct metric
      filter(Metric == data_type) |>
      # Group by location, sampling period, and transect
      group_by(Location, SampleYear, Period, Transect) |>
      # Calculate total abundance per transect
      summarise(Tot = sum(Total, na.rm = TRUE)) |>
      # Group by specified variables
      group_by(!!!syms(sum_by)) |>
      # Calculate mean abundance and SEM
      summarise(Avg = mean(Tot, na.rm = TRUE),
                SEM = sd(Tot, na.rm = TRUE)/sqrt(length(which(Transect > 0)))) |>
      # Create new column indicating values are abundance
      add_column(Metric = data_type, .after = 1)

  }

}
