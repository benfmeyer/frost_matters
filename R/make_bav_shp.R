#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author ben
#' @export
make_bav_shp <- function() {

  bav_shp <- rnaturalearth::ne_states(country = "Germany", returnclass = "sf") |> 
    filter(name == "Bayern") |> 
    select(geometry)
  
  return(bav_shp)

}
