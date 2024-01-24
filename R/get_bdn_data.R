#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author ben
#' @export
get_bdn_data <- function() {

   .rename_cols <- function(x) {
     old_names <- colnames(x)
     prefix <- str_extract(old_names, "\\w{4}") %>% 
       unique()
     suffix <- str_replace(old_names, prefix, "")
     new_names <- paste0(prefix, str_pad(suffix, 4, pad = "0"))
     x %>% setNames(new_names)
   }
   
   .tree_mean <- function(x){
     ids <- read.ids(x, c(4, 3, 1))
     site <- str_sub(colnames(x), 1L, 4L) %>% unique()
     tm <- treeMean(x, ids, F)
     colnames(tm) <- paste0(site, "_", colnames(tm))
     return(tm)
   }
  
  rwl_raw <- list.files("data/bdn/", pattern = ".rwl", full.names = T) %>% 
    map(read.rwl) %>% 
    map(.rename_cols) %>% 
    map(.tree_mean) %>% 
    map(rownames_to_column, "year") %>% 
    map(mutate, year = as.numeric(year)) %>% 
    reduce(full_join) %>% 
    pivot_longer(-year, names_to = c("site_id", "tree"), names_sep = "_", values_to = "rwl") %>% 
    unite(tree_id, site_id, tree, remove = F) %>% 
    drop_na()
  
  meta_data <- readxl::read_excel("data/bdn/BDN_FROST.xlsx") %>% 
    select(site_id = `User_core-Code`, lon, lat, elev = `Elevation [m] (3)`) %>% 
    mutate(site_id = str_sub(site_id, 1L, 4L))
    
  rwl_clean <- full_join(rwl_raw, meta_data)
    
  return(rwl_clean)
}



