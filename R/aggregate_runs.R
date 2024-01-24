#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lpj
#' @param lpj_frost
#' @param rwi
#' @param select_res
#' @return
#' @author ben
#' @export
aggregate_runs <- function(lpjg, rwi) {

  .prep_rwi <- function(rwi) {
    rwi |> 
      unnest(cols = data) |> 
      select(-bai, -elev) |> 
      pivot_longer(c(rwl, spline), names_to = "idx", values_to = "val") |> 
      mutate(type = "trw")
  }
  
  .prep_lpjg <- function(lpjg) {
    
    out <- lpjg |> 
      rename(val = anpp) |> 
      mutate(idx = "anpp", type = if_else(frost_tolerance != -9999, "lpjg_frost", "lpjg")) 
    
    return(out)
  }
  

  rwi_cl <- .prep_rwi(rwi)
  lpjg_cl <- .prep_lpjg(lpjg)  
  
  com_raw <- bind_rows(rwi_cl, lpjg_cl) 
  
  overlap_yrs <- com_raw |> 
    filter(type == "trw") |> 
    group_by(site_id) |> 
    reframe(year = unique(year))
  
  com <- left_join(overlap_yrs, com_raw, multiple = "all")
  
  return(com)

}
