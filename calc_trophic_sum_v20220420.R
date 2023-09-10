#' TCRMP Fish Trophic Group Summary
#' @description Summarizes raw fish abundance data collected by the US Virgin
#'   Islands Territorial Coral Reef Monitoring Program (TCRMP) by trophic
#'   group.
#'
#' @param df a data frame containing raw fish abundance data.
#' @param sum_by a character vector containing variable(s) by which to
#'   summarize fish abundance by trophic group.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang syms
#' @importFrom tidyr pivot_wider
#'
#' @export calc_trophic_sum
#'
#' @examples
#' calc_trophic_sum(tcrmp_fish_abun, c("SampleYear", "Location"))
#'
#' calc_trophic_sum(tcrmp_fish_abun, "Location")

calc_trophic_sum <- function(df, sum_by) {

  # Bind variables locally to function
  TrophicGroup <- Richness <- CommonName <- unk <- `NA` <- TotSpp <- herb <- Prop <- . <- NULL

  # Calculate species richness by trophic group
  trophic_rich <- df |>
    # Group by specified variables and trophic group
    group_by(!!!syms(sum_by), TrophicGroup) |>
    # Calculate species richness
    summarise(Richness = length(unique(CommonName))) |>
    # Convert from long to wide format
    pivot_wider(names_from = TrophicGroup, values_from = Richness) |>
    # Change `NA` to unk trophic level
    rename(unk = `NA`) |>
    # Calculate total species richness
    mutate(TotSpp = rowSums(across(herb:unk), na.rm = TRUE)) %>%
    # Convert NA to 0
    mutate_at(vars(herb:unk), ~replace(., is.na(.), 0))

  # Calculate proportion of total species richness
  trophic_prop <- trophic_rich  %>%
    # Calculate proportion of total species richness by trophic group
    mutate_at(vars(herb:unk), ~(./TotSpp)*100) |>
    # Remove total species richness
    select(!TotSpp) |>
    # Convert from wide to long format
    pivot_longer(c(herb:unk), names_to = "TrophicGroup", values_to = "Proportion")

  # Convert from wide to long format
  pivot_longer(trophic_rich, c(herb:unk), names_to = "TrophicGroup", values_to = "Richness") |>
    # Remove total species richness
    select(!TotSpp) %>%
    # Join species richness by trophic level with proportion of total species richness
    left_join(., trophic_prop)

}
