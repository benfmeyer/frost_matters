#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author ben
#' @export
get_meyer_data <- function() {

  paths <- list.files(pattern = "Beech_Complete.rwl", recursive = T, ignore.case = T)
  raw_rwl <- map(paths, read.rwl) %>% 
    map(~dplyr::select(., str_sort(names(.))))
  
  dbh <- list.files(path = "data/meyer/Beech", pattern = "\\.csv$",
                    recursive = T, full.names = T) %>% 
    map(read_csv) %>% 
    map(~mutate(., DBH = DBH*10)) %>% 
    map(~(set_names(., c("tree", "dbh")))) %>% 
    map(~(arrange(., tree))) %>% 
    map(~mutate(., tree_id2 = tree, .before = 1)) %>% 
    map(column_to_rownames, "tree") %>% 
    map2(raw_rwl, ~dplyr::filter(., tree_id2 %in% names(.y)))
  
  bai <- map2(raw_rwl, dbh, bai.out) %>% 
    map(rownames_to_column) %>% 
    map(rename, "year" = "rowname") %>% 
    map(~pivot_longer(., -year, names_to = "tree", values_to = "bai")) %>% 
    bind_rows() %>% 
    mutate(year = as.numeric(year))
    
  clean_rwl <- raw_rwl %>% 
    map(rownames_to_column) %>% 
    map(rename, "year" = "rowname") %>% 
    map(~pivot_longer(., -year, names_to = "tree", values_to = "rwl")) %>% 
    bind_rows() %>% 
    mutate(year = as.numeric(year))
 

  with_bai <- inner_join(clean_rwl, bai) %>% 
    mutate(site_id = rep("COBB"),
           tree_id = paste0(site_id, "_", tree),
           elev = 320,
           lat = 50.28,
           lon = 11.00) 
  
  return(with_bai)
  
}
