#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param file
#' @return
#' @author ben
#' @export
get_npp <- function(lpjg_outputs) {

  npp_tb <- lpjg_outputs |> 
    filter(type == "anpp", run %in% c("frost", "baseline")) |> 
    rename(npp = output) |> 
    group_by(lon, lat) |> 
    group_by(lon, lat, run) |> 
    mutate(cnpp = cumsum(npp))
  
  return(npp_tb)

}
