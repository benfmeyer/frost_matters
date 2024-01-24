#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param full_data_grid
#' @param ...
#' @return
#' @author ben
#' @export
detrend_rwl <- function(full_data_grid, ...) {

  .tbrm_tbl <- function(ttbl){
     ttbl %>% 
      group_by(year) %>% 
      summarize(across(.cols = everything(), .fns = ~tbrm(.x)))
  }
  
  .detrend_tbl <- function(x, colName, ...) {
    colName <- enquo(colName)
    arg_list <- list(...)
    x %>% 
      dplyr::select(year, tree_id, !!colName) %>% 
      pivot_wider(names_from = tree_id, values_from = !!colName) %>% 
      arrange(year) %>% 
      column_to_rownames(var = "year") %>% 
      detrend(...) %>% 
      rownames_to_column(var = "year") %>% 
      mutate(year = as.numeric(year)) %>% 
      pivot_longer(-year, names_to = "tree_id",
                   values_to = str_to_lower(arg_list[["method"]])) 
  } 
  
  dl <- list(...)
  dd <- list()
  for (i in 1:length(dl[["method"]])){
  dd[[i]] <- full_data_grid %>% 
    mutate(data = map(data, ~inner_join(.x, .detrend_tbl(.x, rwl, method = dl[["method"]][[i]]))))
  }
  
  dd_ij <- dd %>% 
    map(~unnest(.x, data)) %>% 
    reduce(inner_join) %>% 
    group_nest(lon, lat, site_id) %>% 
    mutate(data = map(data, ~dplyr::select(.x, -tree_id)),
           data = map(data, .tbrm_tbl),
           data = map(data, ~filter(.x, year >= 1951)))
    
  
  return(dd_ij)

}


