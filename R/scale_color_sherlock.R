#' Sherlock Color Palettes
#'
#' @description
#' Set color scheme to one of the Sherlock color palettes
#'
#' @param palette color palette to be used (required)
#'
#' @return Plot color scheme uses one of the Sherlock color palettes
#'
#' @export


scale_color_sherlock <- function(palette = 1) {

  if(palette == 1) {

    color_scheme <- ggplot2::scale_color_manual(values = c("#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A",
                                           "#3971CB", "#D76213", "#111111", "#9A0000",
                                           "#335F34", "#8E5816", "#624187", "#141B7A"))
  }

  if(palette == 2) {
    color_scheme <- "#3971CB"
  }

  return(color_scheme)

}
