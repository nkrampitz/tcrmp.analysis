#' TCRMP Location-Specific Fish Summary Figures
#' @description Creates location-specific summary figures of
#'   fish community metrics collected by the US Virgin Islands
#'   Territorial Coral Reef Monitoring Program (TCRMP).
#'
#' @param abun_df a data frame containing fish abundance by species.
#' @param biom_df a data frame containing mean fish biomass and SEM by
#'   species.
#' @param trophic_df a data frame containing fish trophic level species
#'   richness and proportion of total richness.
#' @param sample_year sample year for which location specific summary
#'   figures should be created.
#' @param path file pathway to where summary \code{.jpg}s should be
#'   saved.
#' @param gbf_incl determines whether a location specific \code{.jpg}
#'   should be created (\code{TRUE}) for the monitoring location Ginsburg
#'   Fringe. If no fish data was collected at this location for the
#'   \code{sample_year} indicated, this parameter should be set to \code{FALSE}.
#'
#' @details The figures created match the format of those published in
#'   the annual TCRMP report. Annual reports can be found at:
#'   (\url{https://sites.google.com/site/usvitcrmp/tcrmp-reports})
#' @details The default value of \code{gbf_incl} is \code{TRUE}.
#'
#' @importFrom cowplot plot_grid
#' @import dplyr
#' @import ggplot2
#' @importFrom utils head
#'
#' @examples \dontrun{
#'
#' abun <- calc_mean_fish(tcrmp_fish_abun, "Abundance",
#'                        c("SampleYear", "Location"))
#' biom <- calc_mean_fish(tcrmp_fish_biom, "Biomass",
#'                        c("SampleYear", "Location"))
#' trophic <- calc_trophic_sum(tcrmp_fish_abun,
#'                             c("SampleYear", "Location"))
#'
#' create_fig_fish_ind(abun, biom, trophic, 2020,
#'                     "C:/Users/User/Documents")
#' create_fig_fish_ind(abun, biom, trophic, 2020,
#'                     "C:/Users/User/Documents", gbf_incl = FALSE)
#' }

create_fig_fish_ind <- function(abun_df, biom_df, trophic_df, sample_year, path, gbf_incl = TRUE) {

  # Bind variables locally to function
  Location <- SampleYear <- Period <- TrophicGroup <- Abundance <- Biomass <- CommonName <- BiomassSEM <- Richness <- Proportion <- Code <- NULL

  # Join fish abundance by species with mean biomass and SEM by species
  abun_biom_df <- left_join(abun_df, biom_df, by = c("Location", "SampleYear", "Period", "TrophicGroup", "CommonName")) |>
    # Sort by location and sampling period and arrange species in decreasing order by abundance
    arrange(Location, SampleYear, Period, TrophicGroup, desc(Abundance))

  # Create vector of site names
  if(gbf_incl == TRUE) {

    sites <- as.vector(unique(abun_biom_df$Location))

  } else {

    sites <- as.vector(unique(abun_biom_df$Location[which(abun_biom_df$Location != "Ginsburg Fringe")]))

  }

  # Function to determine which rows to include in trophic group biomass figures (top 10 most abundant species)
  row_incl <- function(group) {

    filter(abun_biom_df, Location == s, TrophicGroup == group, SampleYear == sample_year) |>
      head(10)

  }

  # Function to create trophic group biomass figures
  loc_plot <- function(group, fill, title) {

    ggplot(row_incl(group), aes(x = reorder(CommonName, -Abundance), y = Biomass + 1)) +
      geom_bar(position = "dodge", stat = "identity", color = "black", fill = fill, width = 0.8) +
      ylab(expression(bold(paste("Biomass (g/100 ", m^2, ")")))) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            axis.title.y = element_text(size = 18, face = "bold"), axis.text.y = element_text(size = 16),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "bold", angle = 60, vjust = 0.95, hjust = 0.95),
            plot.title = element_text(size = 17, face = "bold", hjust = 0.5)) +
      ggtitle(title) +
      geom_errorbar(aes(ymin = ifelse(is.na(log10((Biomass + 1) - SEM)) == TRUE, 1, (Biomass + 1) - SEM), ymax = (Biomass + 1) + SEM), width = 0.2) +
      scale_y_continuous(trans = "log10", breaks = c(1, 11, 101, 1001, 10001, 50001), labels = c(0, 10, 100, 1000, 10000, 50000), expand = c(0, 0)) +
      coord_cartesian(ylim = c(1, 50001))

  }

  # Create summary figures by location
  for(s in sites) {

    # Create herbivore summary figure
    loc_herb <- loc_plot(group = "herb", fill = "forestgreen", title = "Herbivores")
    # Create invertivore summary figure
    loc_inv <- loc_plot(group = "inv", fill = "purple3", title = "Invertivores")
    # Create planktivore summary figure
    loc_plank <- loc_plot(group = "plank", fill = "dodgerblue", title = "Planktivores")
    # Create piscivore summary figure
    loc_pisc <- loc_plot(group = "pisc", fill = "firebrick", title = "Piscivores")
    # Create species richness by trophic group figure
    loc_trophic_rich <- trophic_df |>
      filter(Location == s, SampleYear == sample_year) |>
      ggplot(aes(x = TrophicGroup, y = Richness, fill = TrophicGroup)) +
      geom_bar(position = "dodge", stat = "identity", color = "black", width = 0.8) +
      scale_fill_manual(values = c("forestgreen", "purple3", "gold", "firebrick", "dodgerblue", "wheat4")) +
      theme(panel.background = element_blank(), axis.line = element_line(),
            legend.position = "none",
            axis.title.y = element_text(size = 18, face = "bold"), axis.text.y = element_text(size = 16),
            axis.title.x = element_blank(), axis.text.x = element_text(size = 16, face = "bold", angle = 60, vjust = 0.95, hjust = 0.95)) +
      ylab("Species Richness") +
      scale_y_continuous(limits = c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35), expand = c(0, 0))
    # Create proportion of total species richness by trophic group figure
    loc_trophic_comp <- trophic_df |>
      filter(Location == s, SampleYear == sample_year) |>
      ggplot(aes(x = "", y = Proportion, fill = TrophicGroup)) +
      geom_bar(stat = "identity", color = "black", width = 0.5) +
      scale_fill_manual(values = c("forestgreen", "purple3", "gold", "firebrick", "dodgerblue", "wheat4"),
                        labels = c(" Herbivores", " Invertivores", " Omnivores", " Piscivores", " Planktivores", " Spongivores")) +
      theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_blank(),axis.line = element_blank(),
            legend.title = element_blank(), legend.text = element_text(size = 16, face = "bold"),
            axis.title = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
      coord_polar(theta = "y", start = 0)
    # Combine individual figure panes
    loc_fish <- plot_grid(loc_herb, loc_plank, loc_inv, loc_pisc, loc_trophic_rich, loc_trophic_comp,nrow = 3, ncol = 2, labels = LETTERS, label_size = 18, align = "h")
    # Save combined figures to file pathway
    jpeg(file.path(path, paste0("TCRMP", sample_year, "_", filter(tcrmp::tcrmp_metadata, Location == s)$Code[1], ".jpg")), width = 1000, height = 1336)
    print(loc_fish)
    dev.off()

  }

}
