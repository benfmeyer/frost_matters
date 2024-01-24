#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author ben
#' @export
get_principe_data <- function() {

  load("data/principe/data_jk.rda")
  
  principe_remove <- tidy_jk |> 
    filter(raw < 0) |> 
    pull(tree_id) |> 
    unique()
  
  principe_tidy <- rename(tidy_jk, "rwl" = "raw") %>% 
    dplyr::select(-spline, -dbh_avail) |> 
    filter(tree_id %!in% principe_remove)
  
  principe_meta_clean <- meta_jk   %>% 
    dplyr::select(-contributor, -contributor_id, -country, -site) %>% 
    rename("elev" = "alt")
  
  principe_tidy <- full_join(principe_tidy, principe_meta_clean) %>% 
    as_tibble()
  
  return(principe_tidy)

}
