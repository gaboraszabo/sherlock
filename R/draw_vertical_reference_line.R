#' Draw vertical reference line
#'
#' @description
#' Draws a vertical reference line or multiple reference lines to plots
#'
#' @param reference_line input x coordinate of reference line(s). for multiple reference lines, concatenate individual values into a vector  (required)
#' @param color change reference line color. options are "grey", "blue" and "red". by default, it is set to "grey" (optional)
#' @param linetype change line type. identical to linetype ggplot2 aesthetic. by default, it is set to "dashed" (optional)
#' @param size change line thickness. identical to size ggplot2 aesthetic. by default, it is set to 0.7 (optional)
#'
#' @return A vertical reference line plotted on top of 'ggplot' object
#'
#' @export

draw_vertical_reference_line <- function(reference_line, color = "grey", linetype = "dashed", size = 0.7) {

  if(color == "grey") {
    ref_line <- ggplot2::geom_vline(xintercept = reference_line, color = "grey50", linetype = linetype, alpha = 0.5, size = size)
  }

  if(color == "blue") {
    ref_line <- ggplot2::geom_vline(xintercept = reference_line, color = "#40506e", linetype = linetype, alpha = 0.7, size = size)
  }

  if(color == "red") {
    ref_line <- ggplot2::geom_vline(xintercept = reference_line, color = "#700808", linetype = linetype, alpha = 0.5, size = size)
  }

  return(ref_line)

}
