#' Draw horizontal reference line
#'
#' @description
#' Draws horizontal reference line(s) to plots
#'
#' @param reference_line input y coordinate of reference line(s). for multiple reference lines, concatenate individual values into a vector  (required)
#' @param color change reference line color. options are "grey", "blue" and "red". by default, it is set to "grey" (optional)
#' @param linetype change line type. identical to linetype ggplot2 aesthetic. by default, it is set to "dashed" (optional)
#' @param size change line thickness. identical to size ggplot2 aesthetic. by default, it is set to 0.7 (optional)
#'
#' @return A ggplot multi-vari plot object
#'
#' @examples
#' library(ggh4x)
#'
#' multi_vari_data %>%
#'     draw_multivari_plot(response = force, factor_1 = cycle, factor_2 = fixture, factor_3 = line) +
#'     draw_horizontal_reference_line(reference_line = c(19.3, 23.5))
#'
#' @export

draw_horizontal_reference_line <- function(reference_line, color = "grey", linetype = "dashed", size = 0.5) {

  if(color == "grey") {
    ref_line <- ggplot2::geom_hline(yintercept = reference_line, color = "grey50", linetype = linetype, alpha = 0.5, size = size)
  }

  if(color == "blue") {
    ref_line <- ggplot2::geom_hline(yintercept = reference_line, color = "#40506e", linetype = linetype, alpha = 0.7, size = size)
  }

  if(color == "red") {
    ref_line <- ggplot2::geom_hline(yintercept = reference_line, color = "#700808", linetype = linetype, alpha = 0.5, size = size)
  }

  return(ref_line)

}