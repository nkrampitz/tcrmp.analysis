#' TCRMP \emph{Diadema antillarum} Density Summary
#' @description Calculates mean \emph{Diadema antillarum} density and SEM
#'   from data collected by the US Virgin Islands Territorial Coral Reef
#'   Monitoring Program (TCRMP).
#'
#' @param df a data frame containing raw \emph{Diadema antillarum} data.
#' @param sum_by a character vector containing variable(s) by which to
#'   summarize \emph{Diadema antillarum} density.
#'
#' @import dplyr
#' @importFrom rlang syms
#' @importFrom stats sd
#'
#' @export calc_mean_dens
#'
#' @examples
#' calc_mean_dens(tcrmp_diadema, c("SampleYear", "Location"))
#'
#' calc_mean_dens(tcrmp_diadema, "Location")

calc_mean_dens <- function(df, sum_by) {

  # Bind variables locally to function
  DiadDens <- Transect <- NULL

  # Calculate Diadema density
  df |>
    # Group by specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate mean Diadema density and SEM
    summarise(Avg = mean(DiadDens, na.rm = TRUE),
              SEM = sd(DiadDens, na.rm = TRUE)/sqrt(length(Transect)))

}
