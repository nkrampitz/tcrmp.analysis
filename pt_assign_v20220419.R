#' Random Point Assignment
#' @description Superimposes random points to images and assigns letter values
#'   to individual points for the primary purpose of identifying benthic cover.
#'
#' @param pts specifies the number of random points to superimpose on images.
#' @param path pathway to folder in which images are located and to which new
#'   images with assigned points will be saved.
#' @param color (optional) specifies the color of points and text.
#'
#' @details The maximum number of points that can be assigned to a single image
#'   is 52 (labels: A-Z, a-z).
#' @details The pathway should direct to a folder containing only images for
#'   point assignment.
#' @details Dimensions of input images are automatically detected and new images
#'   of the same size with assigned points are saved under the original file
#'   name with the ending "\code{_pts}".
#' @details The default \code{color} is red. However, \code{color} may take any
#'   single color (i.e. \code{"blue"}, \code{"yellow"}) or base color ramp
#'   (i.e. \code{\link[grDevices:heat.colors]{heat.colors}},
#'   \code{\link[grDevices:rainbow]{rainbow}}, etc.). If using a color ramp,
#'   \code{n} is equal to the number of points.
#'
#' @importFrom graphics text
#' @importFrom grDevices dev.copy
#' @importFrom imager load.image
#' @importFrom jpeg readJPEG
#' @importFrom spatstat.geom owin runifrect
#' @importFrom tools file_path_sans_ext
#'
#' @examples \dontrun{
#'
#' pt_assign(20, "C:/Users/User/Documents/Images")
#'
#' pt_assign(50, "C:/Users/User/Documents/Images", heat.colors(50))
#' }

pt_assign <- function(pts, path, color) {

  # Create point labels
  labs <- if(pts <= 26) {
    LETTERS[seq(from = 1, to = pts)]
  } else {
    c(LETTERS[seq(from = 1, to = 26)], letters[seq(from = 1, to = pts - 26)])
  }

  # List image files located in the file pathway
  files <- dir(path, full.names = T)

  # Define point and text color if different from default
  if (missing(color)) {
    color <- "red"
  } else {
    color <- color
  }

  # Load images from file list
  clips <- lapply(files, load.image)

  # Assign random points and save images
  for (c in seq_along(clips)) {
    # Read image characteristics
    f <- readJPEG(files[c])
    # Assign random points within the dimensions of image
    points <- runifrect(pts, win = owin(c(0, dim(f)[2]), c(0, dim(f)[1])))
    # Create new file name with "_pts" extension
    name <- paste0(tools::file_path_sans_ext(files[c]), "_", "pts.jpg")
    # Plot image
    plot(clips[[c]], axes = F)
    # Plot points on image
    points(points, pch = 3, col = color, cex = 4.5, lwd = 2.5)
    # Plot point labels
    text(points, labels = labs, col = color, pos = 4, font = 2, cex = 6.5, offset = 1, xpd = TRUE)
    # Save plot under new file name to file pathway
    dev.copy(jpeg, name, width = dim(f)[2], height = dim(f)[1])
    dev.off()
  }

}
