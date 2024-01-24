#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param full_data_station
#' @return
#' @author ben
#' @export
make_key <- function(full_data_station) {

  full_data_station |> 
    select(lat, lon, site_id)
  
}
