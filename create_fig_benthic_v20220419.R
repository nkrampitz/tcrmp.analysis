#' TCRMP Report Benthic Cover Summary Figures
#' @description Creates summary figures of benthic cover categories
#'   from data collected by the US Virgin Islands Territorial Coral
#'   Reef Monitoring Program (TCRMP) for a specified range of years.
#'
#' @param df a data frame containing mean benthic cover and
#'   SEM for each benthic cover category by location for each sampling
#'   period.
#' @param cover a vector of benthic cover categories for which figures
#'   should be created.
#' @param sample_years a vector of years (\code{c(start, end)}) to include
#'   in summary figures.
#' @param path a file pathway to where summary \code{.jpg}s should be
#'   saved.
#'
#' @details The figures created match the format of those published in
#'   the annual TCRMP report. Annual reports can be found at:
#'   (\url{https://sites.google.com/site/usvitcrmp/tcrmp-reports})
#'
#' @importFrom cowplot draw_label ggdraw plot_grid
#' @import dplyr
#' @import ggplot2
#' @importFrom grDevices dev.off jpeg
#' @importFrom lubridate ceiling_date floor_date
#' @importFrom magrittr "%>%"
#' @importFrom plyr round_any
#'
#' @examples \dontrun{
#'
#' ben_cov <- calc_mean_cover(tcrmp_benthic,
#'                            c("SampleYear", "Location"))
#'
#' create_fig_benthic(ben_cov, c("Coral", "OFAV", "Spo"),
#'                    c(2005, 2020), "C:/Users/User/Documents")
#' }

create_fig_benthic <- function(df, cover, sample_years, path) {

  # Bind variables locally to function
  Cover <- . <- Location <- SampleYear <- Period <- SampleMonth <- Avg <- min_date <- max_date <- max_vals <- SEM <- Max <- ReefComplex <- SampleDate <- NULL

  # Clean full benthic cover data set
  df <- df |>
    # Filter benthic categories from full data set
    filter(Cover %in% cover) %>%
    # Combine benthic cover data with TCRMP location metadata
    left_join(., tcrmp::tcrmp_metadata, by = "Location") %>%
    # Combine benthic cover data and TCRMP location metadata with TCRMP benthic sample dates
    left_join(., tcrmp::tcrmp_benthic_sample_date, by = c("Location", "SampleYear", "Period", "SampleMonth")) |>
    # Filter data to include only that which falls between the sample years
    filter(SampleYear >= sample_years[1] & SampleYear <= sample_years[2])

  # Define the starting and ending dates for the x-axis
  min_date <- floor_date(min(df$SampleDate) - ((as.numeric(min(df$SampleYear)) - sample_years[1]) * 365), unit = "year")
  max_date <- ceiling_date(max(df$SampleDate) + ((sample_years[2] - as.numeric(max(df$SampleYear))) * 365), unit = "year")

  # Define the maximum values for each benthic cover category
  max_vals <- df |>
    mutate(Max = Avg + SEM) |>
    group_by(Cover) |>
    summarise(MaxVal = max(Max, na.rm = TRUE))

  # Function to create figures
  cpx_plot <- function(cpx, cov, shapes, shape_fill) {

    # Filter data to between sample years by reef complex and benthic cover category
    filter(df, ReefComplex == cpx, Cover == cov, !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = Location)) +
      geom_line(size = 1, color = "black") +
      geom_point(aes(shape = Location, fill = Location), size = 6, color = "black") +
      scale_fill_manual(values = shape_fill, labels = as.vector(unique(df$Location[which(df$ReefComplex == cpx)])), name = cpx) +
      scale_shape_manual(values = shapes, labels = as.vector(unique(df$Location[which(df$ReefComplex == cpx)])), name = cpx) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.key = element_blank(), legend.position = "left", legend.text = element_text(size = 18), legend.title = element_text(size = 20, face = "bold"),
            legend.key.size = unit(0.65, "cm"),
            axis.title.y = element_text(size = 20, face = "bold"), axis.text.y = element_text(size = 16, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "bold", angle = 60, vjust = 0.6)) +
      ylab("Cover (%)") +
      geom_errorbar(aes(ymin = Avg - SEM, ymax = Avg + SEM), width = 100) +
      scale_y_continuous(limits = c(0, y_max)) +
      scale_x_date(limits = c(min_date, max_date + (365/3)), breaks = function(x) seq.Date(from = min_date, max_date, by = "1 year"), date_labels = "%Y") +
      coord_cartesian(ylim = c(0, y_max))

  }

  # Create benthic cover summary figures
  for (c in cover) {

    # Define the max value of the y-axis
    y_max <- round_any(max_vals$MaxVal[which(max_vals$Cover == c)], 20, f = ceiling)
    # Create nearshore summary figure
    near_benthic_fig <- cpx_plot(cpx = "Nearshore",
                                 cov = c,
                                 shapes = c(21, 25, 22, 23, 24, 21, 25,
                                            22, 23, 24, 21, 25, 22),
                                 shape_fill = c("darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue", "darkorchid4",
                                                "darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue"))
    # Create offshore summary figure
    off_benthic_fig <- cpx_plot(cpx = "Offshore",
                                cov = c,
                                shapes = c(21, 25, 22, 23, 24, 21, 21,
                                           25, 22, 23, 24),
                                shape_fill = c("darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue", "darkorchid4",
                                               "darkred", "darkorange", "yellow2", "forestgreen"))
    # Create mesophotic summary figure
    meso_benthic_fig <- cpx_plot(cpx = "Mesophotic",
                                 cov = c,
                                 shapes = c(21, 25, 22, 23, 24, 21, 21,
                                            25, 22, 23),
                                 shape_fill = c("darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue", "darkorchid4",
                                                "darkred", "darkorange", "yellow2"))
    # Join individual reef complex figures into one multipane figure
    benthic_cpx_fig <- plot_grid(near_benthic_fig, off_benthic_fig, meso_benthic_fig, ncol = 1, align = "v")
    # Assign the x-axis label
    x_title <- ggdraw() + draw_label("Year", fontface = "bold", size = 22)
    # Join multipane reef complex figure and x-axis label
    benthic_cpx_fig <- plot_grid(benthic_cpx_fig, x_title, nrow = 2, align = "v", rel_heights = c(1, 0.05))
    # Save finished figure to file pathway
    jpeg(file.path(path, paste0("TCRMP", sample_years[2], "_", c, ".jpg")), width = 825, height = 1010)
    print(benthic_cpx_fig)
    dev.off()

  }

}
