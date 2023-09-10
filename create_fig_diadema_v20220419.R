#' TCRMP \emph{Diadema antillarum} Density Summary Figure
#' @description Creates a figure displaying mean \emph{Diadema antillarum}
#'   density from data collected by the US Virgin Islands Territorial Coral
#'   Reef Monitoring Program (TCRMP) for a specified sample year.
#'
#' @param df a data frame containing mean TCRMP \emph{Diadema antillarum}
#'   density and SEM.
#' @param sample_year year for which the summary figure should be created.
#' @param path file pathway to where summary \code{.jpg}s should be
#'   saved.
#'
#' @details The figure created matches the format of that published in
#'   the annual TCRMP report. Annual reports can be found at:
#'   (\url{https://sites.google.com/site/usvitcrmp/tcrmp-reports})
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stats reorder
#'
#' @examples \dontrun{
#'
#' diad_dens <- calc_mean_dens(tcrmp_diadema,
#'                             c("SampleYear", "Location"))
#'
#' create_fig_diadema(diad_dens, 2020, "C:/Users/User/Documents")
#' }

create_fig_diadema <- function(df, sample_year, path) {

  # Bind variables locally to function
  SampleYear <- Avg <- Location <- SEM <- NULL

  # Create average Diadema density figure
  diadema_fig <- df |>
    filter(SampleYear == sample_year) |>
    ggplot(aes(y = Avg + 1, x = reorder(Location, -Avg))) +
    geom_bar(position = "dodge", stat = "identity", width = 0.5) +
    theme(panel.background = element_blank(), axis.line = element_line(),
          legend.position = "none",
          axis.title.y = element_blank(), axis.title.x = element_text(size = 22), axis.text = element_text(size = 20, face = "bold")) +
    ylab(expression(bold(paste(bolditalic("Diadema antillarum"), "/100 ", m^2)))) +
    geom_errorbar(aes(ymax = (Avg + 1) + SEM, ymin = 0), width = 0.5) +
    scale_y_continuous(trans = "log10", breaks = c(1, 2, 6, 16, 51, 101), labels = c(0, 1, 5, 15, 50, 100), expand = expansion(add = c(0, 0.15))) +
    coord_flip()
  # Save finished figure to file pathway
  jpeg(file.path(path, paste0("TCRMP", sample_year, "_Diadema", ".jpg")), width = 1000, height = 1200)
  print(diadema_fig)
  dev.off()

}
