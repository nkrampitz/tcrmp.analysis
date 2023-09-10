#' Temperature Summary Figures
#' @description Creates daily temperature and DHW summary figures
#'   from data for a specified range of years.
#'
#' @param df an data frame containing mean daily temperature and DHW.
#' @param bl_th a data frame containing the bleaching thresholds (BT) and
#'   monthly maximum mean (MMM) for all locations (Location) in \code{df}.
#' @param sample_date an optional data frame containing sample dates
#'   (SampleDate) for each location (Location) in \code{df}.
#' @param sample_years vector of years (\code{c(start, end)}) to include
#'   in summary figures.
#' @param path file pathway to where summary \code{.jpg}s should be
#'   saved.
#'
#' @details The figures created match the format of those published in
#'   the annual TCRMP report. Annual reports can be found at:
#'   (\url{https://sites.google.com/site/usvitcrmp/tcrmp-reports})
#'
#' @importFrom cowplot plot_grid
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate ceiling_date floor_date year
#' @importFrom stringr str_replace_all
#'
#' @examples \dontrun{
#'
#' temp <- calc_dhw("C:/Users/User/Documents/TemperatureFiles",
#'                  tcrmp_bl_th, "TCRMP")
#'
#' create_fig_temp(temp, tcrmp_bl_th, tcrmp_benthic_sample_date,
#'                 c(2005, 2020), "C:/Users/User/Documents")
#' }

create_fig_temp <- function(df, bl_th, sample_date, sample_years, path) {

  # Bind variables locally to function
  DHW <- Temperature <- Sample <- SampleYear <- Location <- SampleDate <- BT <- NULL

  # Add sample dates, if appropriate
  if (missing(sample_date) == TRUE) {

    temp_sample <- df |>
      # Add column empty column indicating DHW on sample dates
      mutate(Sample = NA) |>
      # Convert "Sample" to numeric with no values
      mutate_at("Sample", as.numeric) |>
      # Keep relevant columns
      select(Location, SampleDate, Temperature, DHW, Sample)

  } else {

    # Join mean daily temperature data with sample dates
    temp_sample <- left_join(df, sample_date, by = c("Location", "SampleDate")) |>
      # Add column replicating DHW on dates when sampling occurred
      mutate(Sample = ifelse(is.na(SampleYear) == TRUE, NA, ifelse(is.na(DHW) == TRUE, 0, DHW))) |>
      # Keep relevant columns
      select(Location, SampleDate, Temperature, DHW, Sample)

  }

  # Create vector of location names
  sites <- as.vector(unique(temp_sample$Location))

  # Define maximum temperature
  max_temp <- ceiling(max(temp_sample$Temperature, na.rm = TRUE))
  # Define minimum temperature
  min_temp <- floor(min(temp_sample$Temperature, na.rm = TRUE))
  # Define maximum DHW
  max_dhw <- ceiling(max(temp_sample$DHW, na.rm = TRUE))

  # Create location-specific mean daily temperature and DHW figures
  for (s in sites) {

    # Define sample date range
    sample_date_range <- filter(temp_sample, year(SampleDate) >= sample_years[1] & year(SampleDate) <= sample_years[2] & Location == s)
    # Define minimum sample date
    sample_date_min <- floor_date(min(sample_date_range$SampleDate) - ((as.numeric(min(format(sample_date_range$SampleDate, "%Y"))) - sample_years[1]) * 365), unit = "year")
    # Define maximum sample date
    sample_date_max <- ceiling_date(max(sample_date_range$SampleDate) + ((sample_years[2] - as.numeric(max(format(sample_date_range$SampleDate, "%Y")))) * 365), unit = "year")

    # Create mean daily temperature figure
    daily_temp_fig <- sample_date_range |>
      ggplot(aes(x = SampleDate, y = Temperature)) +
      geom_line(size = 0.7, color = "navy", na.rm = TRUE) +
      geom_hline(yintercept = subset(bl_th, Location == s, BT)[[1]], size = 1.3, color = "cyan") +
      geom_text(aes(sample_date_min, subset(bl_th, Location == s, BT)[[1]], label = "BT"), vjust = -1, size = 6) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = "none",
            axis.title.y = element_text(size = 20, face = "bold"), axis.text.y = element_text(size = 18),
            axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) +
      ylab(expression(bold('Temperature ('*~degree*C*')'))) +
      scale_y_continuous(limits = c(min_temp, max_temp)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"), date_labels = "%Y")
    # Create DHW figure
    dhw_fig <- sample_date_range |>
      ggplot(aes(x = SampleDate, y = DHW)) +
      geom_line(size = 0.8, na.rm = TRUE) +
      geom_point(aes(x = SampleDate, y = Sample, size = 2.5), shape = 16) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = "none",
            axis.title = element_text(size = 20, face = "bold"), axis.ticks.length = unit(0.25, "cm"),
            axis.text.y = element_text(size = 18),
            axis.text.x = element_text(size = 18, angle = 60, vjust = 0.5)) +
      ylab("Degree Heating Weeks") +
      xlab("Year") +
      geom_hline(yintercept = 4, linetype = "dashed", size = 1.3, color = "red") +
      geom_hline(yintercept = 8, linetype = "dashed", size = 1.3, color = "red") +
      geom_text(aes(sample_date_min, 4, label = "4 DHW"), vjust = -1, size = 6) +
      geom_text(aes(sample_date_min, 8, label = "8 DHW"), vjust = -1, size = 6) +
      scale_y_continuous(limits = c(0, max_dhw)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"), date_labels = "%Y")
    # Combine individual panes
    temp_fig <- plot_grid(daily_temp_fig, dhw_fig, ncol = 1, nrow = 2, align = "v", rel_heights = c(1, 1.4))
    # Save combined figure to file pathway
    jpeg(file.path(path, str_replace_all(paste0(subset(temp_sample, Location == s)$Location[1], ".jpg"), pattern = " ", replacement = "_")), width = 800, height = 800)
    print(temp_fig)
    dev.off()

  }

}
