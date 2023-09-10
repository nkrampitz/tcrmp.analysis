#' TCRMP Coral Health Summary
#' @description Summarize coral health assessment data collected by the
#'   US Virgin Islands Territorial Coral Reef Monitoring Program.
#'
#' @param df a data frame containing raw coral health assessment data.
#' @param sum_by a character vector containing variable(s) by which to
#'   summarize coral health assessment data.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang syms
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer spread
#'
#' @export calc_mean_health
#'
#' @examples
#' calc_mean_health(tcrmp_health, c("SampleYear", "Location"))
#'
#' calc_mean_health(tcrmp_health, "Location")

calc_mean_health <- function(df, sum_by) {

  # Bind variables locally to function
  Location <- SampleYear <- Period <- SampleMonth <- Transect <- Dict <- Damage <- OldMort <- TotMort <- BLP <- BL <- SPP <- P <- VP <- SP <- TotBL <- ATL <- Dis1 <-
    Dis2 <- BBD <- DCOR <- DSD <- IMS <- PLA <- SCTLD <- UNK <- WBD <- YBD <- TotDis <- Other <- Metric <- DataType <- Type <- . <- Category <- Value <- Cliona <- Spo <-
    Pred <- NULL

  # Calculate raw prevalence of coral health metrics by transect
  health_prev_raw <- df |>
    # Separate each transect individually
    group_by(Location, SampleYear, Period, SampleMonth, Transect) |>
    # Calculate prevalence of coral health metrics
    summarise(across(c(Dict:Damage, OldMort:TotMort), ~mean(!is.na(.x) == TRUE) * 100, .names = "{.col}"),
              BL = length(which(BLP == "BL" | BLP == "PB" | !is.na(BL) == TRUE)) / length(SPP) * 100,
              P = length(which(BLP == "P" | !is.na(P) == TRUE)) / length(SPP) * 100,
              VP = length(which(BLP == "VP" | !is.na(VP) == TRUE)) / length(SPP) * 100,
              SP = length(which(BLP == "SP" | !is.na(SP) == TRUE)) / length(SPP) * 100,
              TotBL = mean(!is.na(BLP) | !is.na(TotBL) == TRUE) * 100,
              ATL = length(which(Dis1 == "ATL" | Dis2 == "ATL" | !is.na(ATL == TRUE))) / length(SPP) * 100,
              BBD = length(which(Dis1 == "BBD" | Dis2 == "BBD" | !is.na(BBD == TRUE))) / length(SPP) * 100,
              DCOR = length(which(Dis1 == "DCOR" | Dis2 == "DCOR" | !is.na(DCOR == TRUE))) / length(SPP) * 100,
              DSD = length(which(Dis1 == "DSD" | Dis2 == "DSD" | !is.na(DSD == TRUE))) / length(SPP) * 100,
              IMS = length(which(Dis1 == "IMS" | Dis2 == "IMS" | !is.na(IMS == TRUE))) / length(SPP) * 100,
              PLA = length(which(Dis1 == "PLA" | Dis2 == "PLA" | !is.na(PLA == TRUE))) / length(SPP) * 100,
              SCTLD = length(which(Dis1 == "SCTLD" | Dis2 == "SCTLD" | !is.na(SCTLD == TRUE))) / length(SPP) * 100,
              UNK = length(which(Dis1 == "UNK" | Dis2 == "UNK" | !is.na(UNK == TRUE))) / length(SPP) * 100,
              WBD = length(which(Dis1 == "WBD" | Dis2 == "WBD" | !is.na(WBD == TRUE))) / length(SPP) * 100,
              YBD = length(which(Dis1 == "YBD" | Dis2 == "YBD" | !is.na(YBD == TRUE))) / length(SPP) * 100,
              TotDis = mean(!is.na(TotDis) | !is.na(Dis1) | !is.na(Dis2) == TRUE) * 100) |>
    # Convert old mortality from 2020 SCTLD to NA (data not collected)
    # Convert SCTLD from earlier than 2019 to NA (didn't exist)
    # Convert all interactions prior to 2008 to NA (data not collected)
    mutate(OldMort = ifelse(SampleYear == 2020 & Period == "SCTLD" & OldMort == 0, NA, OldMort),
           SCTLD = ifelse(SampleYear < 2019, NA, SCTLD),
           across(Dict:Other, ~ifelse(SampleYear < 2008, NA, .x), .names = "{.col}"))

  # Calculate mean prevalence of coral health metrics
  health_prev_mean <- health_prev_raw |>
    # Group specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate mean prevalence
    summarise(across(Dict:TotDis, ~mean(.x, na.rm = TRUE), .names = "{.col}")) |>
    # Convert from wide to long format
    pivot_longer(c(Dict:TotDis), names_to = "Metric", values_to = "Value") |>
    # Add column indicating values refer to prevalence
    add_column(DataType = "Prevalence", .after = 1) |>
    # Add column indicating values are the mean
    add_column(Type = "Avg", .after = "DataType")

  # Calculate prevalence of coral health metrics SEM
  health_prev_sem <- health_prev_raw |>
    # Group specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate prevalence SEM
    summarise(across(Dict:TotDis, ~sd(.x, na.rm = TRUE) / sqrt(length(which(Transect > 0))), .names = "{.col}")) |>
    # Convert from wide to long format
    pivot_longer(c(Dict:TotDis), names_to = "Metric", values_to = "Value") |>
    # Add column indicating values refer to prevalence
    add_column(DataType = "Prevalence", .after = 1) |>
    # Add column indicating values are SEM
    add_column(Type = "SEM", .after = "DataType")

  # Establish categories for coral health metrics
  health_data_cat <- tibble(Category = c(rep("Interaction", 18), "Damage", rep("Mortality", 3), rep("Bleaching", 5), rep("Disease", 11)),
                            Metric = colnames(health_prev_raw)[6:ncol(health_prev_raw)])

  # Join mean prevalence and SEM
  health_prev_mean_sem <- rbind(health_prev_mean, health_prev_sem) |>
    # Split mean and SEM into their own columns
    tidyr::spread(Type, Value) %>%
    # Join mean prevalence and SEM with health metric categories
    left_join(., health_data_cat, by = "Metric") |>
    # Move coral health metric category column to better location
    relocate(Category, .after = "DataType") %>%
    # Convert NaN to NA
    mutate_all(~ifelse(is.nan(.), NA, .))

  # Calculate mean extent of coral health metrics
  health_ext_mean <- df |>
    # Convert old mortality from 2020 SCTLD to NA (data not collected)
    # Convert SCTLD from earlier than 2019 to NA (didn't exist)
    # Convert all interactions prior to 2008 to NA (data not collected)
    mutate(OldMort = ifelse(SampleYear == 2020 & Period == "SCTLD" & OldMort == 0, NA, OldMort),
           SCTLD = ifelse(SampleYear < 2019, NA, SCTLD),
           across(Dict:Other, ~ifelse(SampleYear < 2008, NA, .x), .names = "{.col}")) |>
    # Group specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate mean extent
    summarise(across(c(Dict:Cliona, Spo:Pred, BL:TotMort, ATL:TotDis), ~mean(.x, na.rm = TRUE), .names = "{.col}")) |>
    # Convert from wide to long format
    pivot_longer(c(Dict:TotDis), names_to = "Metric", values_to = "Value") |>
    # Add new column indicating values refer to extent
    add_column(DataType = "Extent", .after = 1) |>
    # Add column indicating values are the mean
    add_column(Type = "Avg", .after = "DataType")

  # Calculate extent of coral health metrics SEM
  health_ext_sem <- df |>
    # Convert old mortality from 2020 SCTLD to NA (data not collected)
    # Convert SCTLD from earlier than 2019 to NA (didn't exist)
    # Convert all interactions prior to 2008 to NA (data not collected)
    mutate(OldMort = ifelse(SampleYear == 2020 & Period == "SCTLD" & OldMort == 0, NA, OldMort),
           SCTLD = ifelse(SampleYear < 2019, NA, SCTLD),
           across(Dict:Other, ~ifelse(SampleYear < 2008, NA, .x), .names = "{.col}")) |>
    # Group specified variables
    group_by(!!!syms(sum_by)) |>
    # Calculate extent SEM
    summarise(across(c(Dict:Cliona, Spo:Pred, BL:TotMort, ATL:TotDis), ~sd(.x, na.rm = TRUE) / sqrt(length(which(.x > 0))), .names = "{.col}")) |>
    # Convert from wide to long format
    pivot_longer(c(Dict:TotDis), names_to = "Metric", values_to = "Value") |>
    # Add new column indicating values refer to extent
    add_column(DataType = "Extent", .after = 1) |>
    # Add new column indicating values are SEM
    add_column(Type = "SEM", .after = "DataType")

  # Join mean extent and SEM
  health_ext_mean_sem <- rbind(health_ext_mean, health_ext_sem) |>
    # Split mean and SEM into their own columns
    spread(Type, Value) %>%
    # Join mean extent and SEM with coral health metric categories
    left_join(., health_data_cat, by = "Metric") |>
    # Move coral health metric category column to better location
    relocate(Category, .after = "DataType") %>%
    # Convert NaN to NA
    mutate_all(~ifelse(is.nan(.), NA, .))

  # Join prevalence and extent summaries
  rbind(health_prev_mean_sem, health_ext_mean_sem)

}
