#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param path
#' @return
#' @author ben
#' @export
get_dittmar_data <- function() {

  load("data/dittmar/dittmar_data.rda")
  
  dittmar_remove <- dittmar_tidy |> 
    filter(raw < 0) |> 
    pull(tree_id) |> 
    unique()
  
  dittmar_tidy <- rename(dittmar_tidy, "rwl" = "raw") |>  
    dplyr::select(-spline, -dbh_avail) |> 
    filter(tree_id %!in% dittmar_remove)
  
    
  dittmar_meta_clean <- dittmar_meta |>  
    dplyr::select(-contributor, -contributor_id, -country, -site) |> 
    rename("elev" = "alt")
  
  dittmar_tidy <- full_join(dittmar_tidy, dittmar_meta_clean)
  return(dittmar_tidy)

}
