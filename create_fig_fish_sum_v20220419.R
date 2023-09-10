#' TCRMP Report Fish Summary Figures
#' @description Creates summary figures of fish abundance and biomass
#'   from data collected by the US Virgin Islands Territorial Coral
#'   Reef Monitoring Program (TCRMP) for a specified range of years.
#'
#' @param df a data frame containing mean abundance, biomass, and
#'   SEM by location for each sampling period.
#' @param sample_years vector of years (\code{c(start, end)}) to include
#'   in overall summary figures.
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
#' abun <- calc_mean_fish(tcrmp_fish_abun, "Abundance",
#'                        c("SampleYear", "Location"))
#' biom <- calc_mean_fish(tcrmp_fish_biom, "Biomass",
#'                        c("SampleYear", "Location"))
#' abun_biom <- rbind(abun, biom)
#'
#' create_fig_fish_sum(abun_biom, c(2005, 2020),
#'                     "C:/Users/User/Documents")
#' }

create_fig_fish_sum <- function(df, sample_years, path) {

  # Bind variables locally to function
  Location <- Code <- ReefComplex <- Avg <- SEM <- Metric <- Max <- SampleYear <- y_max <- lab <- . <- fish_metric <- NULL

  # Clean fish abundance and biomass data set
  df <- df %>%
    # Combine benthic cover data with TCRMP location metadata
    left_join(., tcrmp::tcrmp_metadata, by = "Location") |>
    # Filter data to include only that which falls between the sample years
    filter(SampleYear >= sample_years[1] & SampleYear <= sample_years[2]) |>
    # Convert SampleYear to a numeric variable
    mutate_at("SampleYear", as.numeric)

  # Define the maximum values for each benthic cover category
  max_vals <- df |>
    mutate(Max = Avg + SEM) |>
    group_by(Metric) |>
    summarise(MaxVal = max(Max, na.rm = TRUE))

  # Function to generate figures
  cpx_plot <- function(cpx, metric, shapes, shape_fill) {

    filter(df, ReefComplex == cpx, Metric == metric, !is.na(Avg)) |>
      ggplot(aes(x = SampleYear, y = Avg, group = Location)) +
      geom_line(size = 1, color = "black") +
      geom_point(aes(shape = Location, fill = Location), size = 6, color = "black") +
      scale_fill_manual(values = shape_fill, labels = as.vector(unique(df$Location[which(df$ReefComplex == cpx)])), name = cpx) +
      scale_shape_manual(values = shapes, labels = as.vector(unique(df$Location[which(df$ReefComplex == cpx)])), name = cpx) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.key = element_blank(), legend.position = "left", legend.text = element_text(size = 18), legend.title = element_text(size = 20, face = "bold"),
            legend.key.size = unit(0.65, "cm"),
            axis.title.y = element_text(size = 20, face = "bold"), axis.text.y = element_text(size = 16, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_text(angle = 60, size = 16, face = "bold", vjust = 0.6)) +
      ylab(lab)+
      geom_errorbar(aes(ymin = Avg - SEM, ymax = Avg + SEM), width = 0.25) +
      scale_y_continuous(limits = c(0, y_max)) +
      scale_x_continuous(limits = c(sample_years[1], sample_years[2] + 1), breaks = seq(sample_years[1], sample_years[2])) +
      coord_cartesian(ylim = c(0, y_max), xlim = c(sample_years[1], sample_years[2]))

  }

  # Define unique values of Metric
  fish_metric <- as.vector(unique(df$Metric))

  for(m in fish_metric) {

    # Deine the y-axis unites and max abundance value
    if(m == "Abundance") {

      lab <- expression(bold("Fish/100"~m^2))
      y_max <- round_any(max_vals$MaxVal[which(max_vals$Metric == "Abundance")], 500, f = ceiling)

    } else {

      lab <- expression(bold("kg/100"~m^2))
      y_max <- round_any(max_vals$MaxVal[which(max_vals$Metric == "Biomass")], 50, f = ceiling)

    }

    # Create nearshore figure
    near_fish_fig <- cpx_plot(cpx = "Nearshore",
                              metric = m,
                              shapes = c(21, 25, 22, 23, 24, 21, 25,
                                         22, 23, 24, 21, 25, 22),
                              shape_fill = c("darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue", "darkorchid4",
                                             "darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue"))
    # Create offshore figure
    off_fish_fig <- cpx_plot(cpx = "Offshore",
                             metric = m,
                             shapes = c(21, 25, 22, 23, 24, 21, 21,
                                        25, 22, 23, 24),
                             shape_fill = c("darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue","darkorchid4",
                                            "darkred", "darkorange", "yellow2", "forestgreen"))
    # Create mesophotic figure
    meso_fish_fig <- cpx_plot(cpx = "Mesophotic",
                              metric = m,
                              shapes = c(21, 25, 22, 23, 24, 21, 21,
                                         25, 22, 23),
                              shape_fill = c("darkred", "darkorange", "yellow2", "forestgreen", "darkcyan", "darkblue", "darkorchid4",
                                             "darkred", "darkorange", "yellow2"))
    # Join individual reef complex figures into one multipane figure
    fish_cpx_fig <- plot_grid(near_fish_fig, off_fish_fig, meso_fish_fig, ncol = 1, align = "v")
    # Assign the x-axis label
    x_title <- ggdraw() + draw_label("Year", fontface = "bold", size = 22)
    # Join multipane reef complex figure and x-axis label
    fish_cpx_fig <- plot_grid(fish_cpx_fig, x_title, nrow = 2, align = "v", rel_heights = c(1, 0.05))
    # Save finished figure to file pathway
    jpeg(file.path(path, paste0("TCRMP", sample_years[2], "_", m, ".jpg")), width = 825, height = 1010)
    print(fish_cpx_fig)
    dev.off()

  }

}
