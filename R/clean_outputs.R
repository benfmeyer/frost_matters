#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lpjg_outputs_raw
#' @return
#' @author ben
#' @export
clean_outputs <- function(lpjg_outputs_raw) {

  fpc_check <- lpjg_outputs_raw[[2]] |> 
    filter(type == "fpc") |> 
    group_by(lon, lat) |> 
    summarize(mean = mean(output)) |> 
    filter(mean >= 0.1) |> 
    select(lon, lat)
  
  lpjg_outputs <- reduce(lpjg_outputs_raw, bind_rows) |> 
    right_join(fpc_check)
  
  return(lpjg_outputs)

}
