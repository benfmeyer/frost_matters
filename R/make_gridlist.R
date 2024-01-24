#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param target_coords
#' @return
#' @author ben
#' @export
make_gridlist <- function(sites) {

  nsites <- sites |> 
    select(lon, lat) |> 
    unique()
  
  land_ids <- 1:nrow(nsites)
    
  base <- "/home/benfmeyer/Documents/PhD/LPJ-GUESS4.0.1/LPJ-GUESS_Drivers/gridlists/"
  name <- "frost_validation_gridlist.txt"
  file_path <- paste0(base, name)
  write.table(land_ids, file_path, col.names = FALSE, row.names = FALSE)
  return(file_path)
}
