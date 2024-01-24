#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author ben
#' @export
make_theme <- function() {

  custom_theme <- theme(
    axis.title = element_text(size = 10), 
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12), 
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )
  
  return(custom_theme)

}
