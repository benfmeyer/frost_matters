#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param frost_sensitivity_files
#' @return
#' @author ben
#' @export
read_fs <- function(path, key) {

  frost_t<- str_extract(path, "\\d{1}\\.\\d{1}") |>  
    as.numeric() |> 
    multiply_by(-1) |> 
    replace_na(-9999)
  
  out <- path |>  
    read_table() |> 
    select(Lon, Lat, Year, anpp = Fag_syl) |>  
    janitor::clean_names(case = "snake") |> 
    mutate(frost_tolerance = frost_t,
           type = if_else(frost_tolerance == -9999, "lpjg", "lpjg_frost"),
           lon = round(lon, 4),
           lat = round(lat, 4)) |> 
    full_join(key)
  
  return(out)
  

}
