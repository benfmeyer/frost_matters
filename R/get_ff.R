#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lpjg_outputs
#' @return
#' @author ben
#' @export
get_ff <- function(lpjg_outputs) {

  ff <- lpjg_outputs |> 
    filter(type == "frost", run == "frost") |> 
    group_by(lon, lat) |> 
    summarize(output = sum(output))

  ggplot(ff, aes(lon, lat, fill = output))+
    geom_tile()+
    scale_fill_scico(palette = "lapaz")
}
