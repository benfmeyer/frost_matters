#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lpjg_output_files
#' @return
#' @author ben
#' @export
join_outputs <- function(lpjg_output_files) {

  run_spec <- str_extract(lpjg_output_files[[1]][[1]], "(?<=\\/)\\w*(?=\\/\\w*.out)")
  
  pft_spec <- c("ddddd", "dddddd")
  
  file_specs <- lpjg_output_files[[1]] %>% 
    map(spec_table) %>% 
    map(as.character)  
  
  clean_files <- lpjg_output_files[[1]][which(str_detect(file_specs, "c+", negate = T))]
  clean_specs <- clean_files %>% 
    map(spec_table) %>% 
    map(as.character)
  
  pft_indices <- which(clean_specs %in% pft_spec)
  
  pft_files <- clean_files[pft_indices]
  
  pft_names <- pft_files %>% 
    str_extract("\\w*(?=.out)")
   
  pft_out <-  pft_files %>% 
    map(read_table) %>% 
    map(janitor::clean_names) %>% 
    map(select, lon, lat, year, output = fag_syl) %>% 
    map2(pft_names, ~mutate(.x, type = .y)) %>% 
    reduce(bind_rows) %>% 
    mutate(run = run_spec, level = "pft")
  
  gridcell_indices <- which(!(clean_specs %in% pft_spec))
  
  gridcell_files <- clean_files[gridcell_indices] 
  
  gridcell_names <- gridcell_files %>% 
    str_extract("\\w*(?=.out)")
  
  gridcell_out <- gridcell_files %>% 
    map(read_table) %>% 
    map(janitor::clean_names) %>% 
    map2(gridcell_names, ~mutate(.x, type = .y)) %>% 
    reduce(full_join) %>% 
    pivot_longer(-c(lon, lat, year, type), names_to = "variable", values_to = "output") %>% 
    drop_na() %>% 
    mutate(run = run_spec, level = "gridcell")
  
  out <- full_join(pft_out, gridcell_out)
  
  return(list(pft_out))

}
