#' TCRMP Coral Health Summary Figures
#' @description Creates summary figures of coral health metrics from
#'   data collected by the US Virgin Islands Territorial Coral Reef
#'   Monitoring Program (TCRMP) for a specified range of years.
#'
#' @param benthic_df a data frame containing mean TCRMP benthic cover
#'   and SEM.
#' @param health_df a data frame containing mean prevalence, extent, and
#'   SEM of TCRMP coral health metrics.
#' @param sample_years vector of years (\code{c(start, end)}) to include
#'   in summary figures.
#' @param path file pathway to where summary \code{.jpg}s should be
#'   saved.
#' @param gbf_incl determines whether a location specific \code{.jpg}
#'   should be created (\code{TRUE}) for the monitoring location Ginsburg
#'   Fringe. If no benthic data was collected at this location for the
#'   \code{sample_year} indicated, this parameter should be set to \code{FALSE}.
#'
#' @details The figures created match the format of those published in
#'   the annual TCRMP report. Annual reports can be found at:
#'   (\url{https://sites.google.com/site/usvitcrmp/tcrmp-reports})
#' @details The default values of \code{gbf_incl} is \code{TRUE}.
#'
#' @importFrom cowplot ggdraw plot_grid
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate ceiling_date floor_date
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{
#'
#' hlth <- calc_mean_health(tcrmp_health,
#'                          c("SampleYear", "Location"))
#' ben_cov <- calc_mean_cover(tcrmp_benthic,
#'                            c("SampleYear", "Location"))
#'
#' create_fig_health(ben_cov, hlth, c(2005, 2020),
#'                   "C:/Users/User/Documents")
#' create_fig_health(ben_cov, hlth, c(2005, 2020),
#'                   "C:/Users/User/Documents", gbf_incl = FALSE)
#' }

create_fig_health <- function(benthic_df, health_df, sample_years, path, gbf_incl = TRUE) {

  # Bind variables locally to function
  Cover <- Category <- SEM <- DataType <- Metric <- Location <- SampleYear <- Period <- SampleMonth <- Avg <- SampleDate <- . <- NULL

  # Define summary benthic cover categories
  benthic_sum_cats <- tibble(Cover = c("CoralCov", "OrbicellaCov", "AgariciaCov", "OtherCoralCov", "MC", "PA", "SS",
                                       "EAC", "MacaCov", "CyanCov", "GorgCov", "SpoCov", "ClionaCov"),
                             Category = c(rep("Coral", 7), rep("OtherBenthic", 6)))

  # Clean benthic cover summary data and join with benthic sample dates
  benthic_df <- benthic_df |>
    # Filter summary data frame to contain only summary benthic cover categories
    filter(Cover %in% benthic_sum_cats$Cover) |>
    # Remove SEM values for coral cover (not needed for figure)
    # Add data description columns
    mutate(SEM = ifelse(Cover == "CoralCov", NA, SEM),
           DataType = "BenthicCover") %>%
    # Join benthic cover data with summary benthic cover category labels
    left_join(., benthic_sum_cats, by = "Cover") |>
    # Rename "Cover" column to "Metric" (for later joining with health data)
    rename(Metric = Cover) %>%
    # Join benthic cover data with benthic sample dates
    left_join(., tcrmp::tcrmp_benthic_sample_date, by = c("Location", "SampleYear", "Period", "SampleMonth")) |>
    # Convert data descriptors to factors
    mutate(DataType = factor(DataType, levels = c("BenthicCover")),
           Category = factor(Category, levels = c("Coral", "OtherBenthic")),
           Metric = factor(Metric, levels = c("CoralCov", "OrbicellaCov", "AgariciaCov", "OtherCoralCov", "MC", "PA", "SS",
                                              "EAC", "MacaCov", "CyanCov", "GorgCov", "SpoCov", "ClionaCov")))

  # Clean coral health summary data and join with coral health sample dates
  health_df <- health_df |>
    # Reorder columns
    select(Location, SampleYear, Period, SampleMonth, Metric, Avg, SEM, DataType, Category) %>%
    # Join summary coral health data with coral health sample dates
    left_join(., tcrmp::tcrmp_health_sample_date, by = c("Location", "SampleYear", "Period", "SampleMonth")) |>
    # Remove sample period "Juvenile" (not part of regular TCRMP)
    filter(Period != "Juvenile") |>
    # Keep only data needed for figures
    filter(Category %in% c("Bleaching", "Mortality", "Disease"),
           Metric %in% c("TotBL", "TotDis", "BBD", "DCOR", "DSD", "IMS", "SCTLD", "PLA", "YBD", "UNK", "OldMort", "RecMort")) |>
    # Convert data descriptors to factors
    mutate(DataType = factor(DataType, levels = c("Prevalence", "Extent")),
           Category = factor(Category, levels = c("Bleaching", "Mortality", "Disease")),
           Metric = factor(Metric, levels = c("TotBL", "TotDis", "BBD", "DCOR", "DSD", "IMS", "SCTLD", "PLA", "YBD", "UNK", "OldMort", "RecMort")))

  # Join benthic cover and coral health data
  df <- rbind(benthic_df, health_df)

  # Define names for categories
  # Define coral species names
  coral_names <- c("All Coral", expression(paste(bolditalic("Orbicella"), bold(" spp."))), expression(paste(bolditalic("Agaricia"), bold(" spp."))),
                   "Other Corals", expression(paste(bolditalic("Montastrea cavernosa"))), expression(paste(bolditalic("Porites astreoides"))),
                   expression(paste(bolditalic("Siderastrea siderea"))))
  # Define other benthic cover category names
  benthic_names <- c("Epilithic Algae", "Macroalgae", "Cyanobacteria", "Gorgonians", "Sponges", "Clionids")
  # Define disease names
  disease_names <- c("All Disease", "Black Band", "Lesion", "Dark Spots", "Intercostal", "SCTLD", "White Disease", "Yellow Band", "Unknown")

  # Create vector of TCRMP location names
  sites <- as.vector(unique(df$Location[df$Location != "Ginsburg Fringe"]))

  # Create benthic and coral health summary figures
  for (s in sites) {

    # Define sample date range
    sample <- subset(df, SampleYear >= sample_years[1] & SampleYear <= sample_years[2] & Location == s)

    # Define minimum sample date
    if(as.numeric(min(sample$SampleYear)) < as.numeric(min(format(sample$SampleDate, "%Y")))) {

      sample_date_min <- floor_date(min(sample$SampleDate) - (((as.numeric(min(sample$SampleYear)) - sample_years[1]) * 365) + 365), unit = "year")

    } else {

      sample_date_min <- floor_date(min(sample$SampleDate) - ((as.numeric(min(sample$SampleYear)) - sample_years[1]) * 365), unit = "year")

    }

    # Define maximum sample date
    if(as.numeric(max(sample$SampleYear)) < as.numeric(max(format(sample$SampleDate, "%Y")))) {

      sample_date_max <- ceiling_date(max(sample$SampleDate), unit = "year")

    } else {

      sample_date_max <- ceiling_date(max(sample$SampleDate) + ((sample_years[2] - as.numeric(max(sample$SampleYear))) * 365), unit = "year")

    }

    # Create coral cover summary figure
    loc_ben_coral <- filter(sample, DataType == "BenthicCover", Category == "Coral", !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = Metric)) +
      geom_ribbon(aes(ymin = 0, ymax = Avg, alpha = Metric), fill = "bisque2") +
      scale_alpha_manual(values = c(0.3, rep(0, 6)), labels = coral_names) +
      geom_line(aes(color = Metric), size = 0.8) +
      scale_color_manual(values = c("gray", rep("black", 6)), labels = coral_names) +
      geom_point(aes(fill = Metric, shape = Metric, size = Metric), color = "black") +
      scale_fill_manual(values = c("gray", "darkred", "darkorange", "yellow2", "darkgreen", "darkblue", "darkorchid4"), labels = coral_names) +
      scale_shape_manual(values = c(rep(21, 3), 25, 24, rep(22, 2)), labels = coral_names) +
      scale_size_manual(values = c(0, rep(6, 6)), labels = coral_names) +
      guides(alpha = guide_legend(ncol = 2), color = guide_legend(ncol = 2), fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2), size = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.7, 0.93), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(), legend.text.align = 0,
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      ylab("Benthic Cover (%)") +
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 60))
    # Create other benthic cover summary figure
    loc_ben_benthic <- filter(sample, DataType == "BenthicCover", Category == "OtherBenthic", !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = Metric)) +
      geom_line(size = 0.8, color = "black") +
      geom_point(aes(fill = Metric, shape = Metric), size = 6, color = "black") +
      scale_fill_manual(values = c("darkred", "darkorange", "yellow2", "darkgreen", "darkblue", "darkorchid4"), labels = benthic_names) +
      scale_shape_manual(values = c(rep(21, 2), 25, 24, rep(22, 2)), labels = benthic_names) +
      guides(fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.7, 0.93), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(), legend.text.align = 0,
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 20, face = "bold", angle = 60, vjust = 0.6), axis.ticks.x = element_blank()) +
      ylab("Benthic Cover (%)") +
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 100))
    # Create coral bleaching summary figure
    loc_coral_bl <- filter(sample, Category == "Bleaching", !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = DataType)) +
      geom_point(aes(shape = DataType, fill = DataType), size = 6, color = "black") +
      scale_shape_manual(values = c(21, 24), labels = c("Prevalence", "Extent"), name = "Bleaching") +
      scale_fill_manual(values = c("darkred", "darkorange"), labels = c("Prevalence", "Extent"), name = "Bleaching") +
      guides(shape = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.8, 0.97), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(),
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      ylab("Bleaching (%)") +
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 105))
    # Create coral disease summary figure
    loc_coral_dis <- filter(sample, DataType == "Prevalence", Category == "Disease", !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = Metric)) +
      geom_line(aes(linetype = Metric), size = 4, color="gray") +
      scale_linetype_manual(values = c("solid", rep("blank", 8)), labels = disease_names, name= "Disease") +
      geom_point(aes(shape = Metric, fill = Metric), size = 6, color = "black") +
      scale_fill_manual(values = c("gray", "darkred", "darkorange", "yellow2", "forestgreen", "black", "darkcyan", "darkblue", "darkorchid4"),
                        labels = disease_names, name = "Disease") +
      scale_shape_manual(values = c(rep(21, 3), 25, 24, 22, 22, 23, 23), labels = disease_names, name = "Disease") +
      guides(linetype = guide_legend(ncol = 2), fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.75, 0.9), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(),
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      ylab("Prevalence (%)")+
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 65))
    # Create coral mortality summary figure
    loc_coral_mort <- filter(sample, DataType == "Prevalence", Category == "Mortality", !is.na(Avg) == TRUE, Metric != "TotMort") |>
      ggplot(aes(x = SampleDate, y = Avg, group = Metric)) +
      geom_line(aes(linetype = Metric), size = 0.8) +
      scale_linetype_manual(values = c("solid", "blank"), labels = c("Old", "Recent"), name = "Partial Mortality") +
      geom_point(aes(shape = Metric, fill = Metric), size = 6, color = "black") +
      scale_fill_manual(values = c("darkred", "darkorange"), labels = c("Old", "Recent"), name = "Partial Mortality") +
      scale_shape_manual(values = c(21, 24), labels = c("Old", "Recent"), name = "Partial Mortality") +
      guides(linetype = guide_legend(ncol = 2), fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.85, 0.97), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(),
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 20, face = "bold", angle = 60, vjust = 0.6)) +
      ylab("Prevalence (%)") +
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 105))
    # Combine individual benthic cover figure panes
    loc_ben_cov_fig <- plot_grid(loc_ben_coral, loc_ben_benthic, nrow = 2, align = "v")
    # Combine individual coral health figures panes
    loc_coral_fig <- plot_grid(loc_coral_bl, loc_coral_dis, loc_coral_mort, nrow = 3, align = "v")
    # Combine benthic cover and coral health panes
    loc_coral_sum_fig <- plot_grid(loc_ben_cov_fig, loc_coral_fig, nrow = 1, ncol = 2)
    # Define x-axis title
    x_title <- ggdraw() + draw_label("Year", fontface = "bold", size = 22)
    # Combine summary figure with x-axis title
    loc_coral_sum_fig <- plot_grid(loc_coral_sum_fig, x_title, nrow = 2, align = "v", rel_heights = c(1, 0.05))
    # Save figure to file pathway
    jpeg(file.path(path, paste0("TCRMP", sample_years[2], "_", filter(tcrmp::tcrmp_metadata, Location == s)$Code[1], ".jpg")), width = 1200, height = 1500)
    print(loc_coral_sum_fig)
    dev.off()

  }

  if(gbf_incl == TRUE) {

    # Define sample date range
    sample <- subset(df, SampleYear >= sample_years[1] & SampleYear <= sample_years[2] & Location == "Ginsburg Fringe")

    # Define minimum sample date
    if(as.numeric(min(sample$SampleYear)) < as.numeric(min(format(sample$SampleDate, "%Y")))) {

      sample_date_min <- lubridate::floor_date(min(sample$SampleDate) - (((as.numeric(min(sample$SampleYear)) - sample_years[1]) * 365) + 365), unit = "year")

    } else {

      sample_date_min <- lubridate::floor_date(min(sample$SampleDate) - ((as.numeric(min(sample$SampleYear)) - sample_years[1]) * 365), unit = "year")

    }

    # Define maximum sample date
    if(as.numeric(max(sample$SampleYear)) < as.numeric(max(format(sample$SampleDate, "%Y")))) {

      sample_date_max <- lubridate::ceiling_date(max(sample$SampleDate), unit = "year")

    } else {

      sample_date_max <- lubridate::ceiling_date(max(sample$SampleDate) + ((sample_years[2] - as.numeric(max(sample$SampleYear))) * 365), unit = "year")

    }

    # Create coral cover summary figure
    gbf_ben_coral <- filter(sample, DataType == "BenthicCover", Category == "Coral", !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = Metric)) +
      geom_ribbon(aes(ymin = 0, ymax = Avg, alpha = Metric), fill = "bisque2") +
      scale_alpha_manual(values = c(0.3, rep(0, 6)), labels = coral_names) +
      geom_line(aes(color = Metric), size = 0.8) +
      scale_color_manual(values = c("gray", rep("black", 6)), labels = coral_names) +
      geom_point(aes(fill = Metric, shape = Metric, size = Metric), color = "black") +
      scale_fill_manual(values = c("gray", "darkred", "darkorange", "yellow2", "darkgreen", "darkblue", "darkorchid4"), labels = coral_names) +
      scale_shape_manual(values = c(rep(21, 3), 25, 24, rep(22, 2)), labels = coral_names) +
      scale_size_manual(values = c(0, rep(6, 6)), labels = coral_names) +
      guides(alpha = guide_legend(ncol = 2), color = guide_legend(ncol = 2), fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2), size = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.7, 0.93), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(), legend.text.align = 0,
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 20, face = "bold", angle = 60, vjust = 0.6), axis.ticks.x = element_blank()) +
      ylab("Benthic Cover (%)") +
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 60))
    # Create other benthic cover summary figure
    gbf_ben_benthic <- filter(sample, DataType == "BenthicCover", Category == "OtherBenthic", !is.na(Avg) == TRUE) |>
      ggplot(aes(x = SampleDate, y = Avg, group = Metric)) +
      geom_line(size = 0.8, color = "black") +
      geom_point(aes(fill = Metric, shape = Metric), size = 6, color = "black") +
      scale_fill_manual(values = c("darkred", "darkorange", "yellow2", "darkgreen", "darkblue", "darkorchid4"), labels = benthic_names) +
      scale_shape_manual(values = c(rep(21, 2), 25, 24, rep(22, 2)), labels = benthic_names) +
      guides(fill = guide_legend(ncol = 2), shape = guide_legend(ncol = 2)) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = c(0.7, 0.93), legend.key = element_blank(), legend.key.size = unit(0.65, "cm"), legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_blank(), legend.text.align = 0,
            axis.title.y = element_text(size = 22, face = "bold"), axis.text.y = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 20, face = "bold", angle = 60, vjust = 0.6), axis.ticks.x = element_blank()) +
      ylab("Benthic Cover (%)") +
      geom_errorbar(aes(ymax = Avg + SEM, ymin = Avg - SEM), width = 100) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
      scale_x_date(limits = c(sample_date_min, sample_date_max + (365/3)), breaks = function(x) seq.Date(from = sample_date_min, sample_date_max, by = "1 year"),
                   date_labels = "%Y", expand = c(0, 182.5)) +
      coord_cartesian(ylim = c(0, 100))
    # Combine individual figure panes
    gbf_ben_cov_fig <- plot_grid(gbf_ben_coral, gbf_ben_benthic, ncol = 2, align = "h")
    # Define x-axis title
    x.title <- ggdraw() + draw_label("Year", fontface = "bold", size = 22)
    # Combine summary figure and x-axis title
    gbf_ben_cov_fig <- plot_grid(gbf_ben_cov_fig, x.title, nrow = 2, align = "v", rel_heights = c(1, 0.05))
    # Save summary figure to file pathway
    jpeg(file.path(path, paste0("TCRMP", sample_years[2], "_", "GBF.jpg")), width = 1200, height = 750)
    print(gbf_ben_cov_fig)
    dev.off()

  }

}
