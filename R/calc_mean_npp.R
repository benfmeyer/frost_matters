#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param npp
#' @return
#' @author ben
#' @export
calc_mean_npp <- function(npp) {

  mnpp <- npp |> 
    select(-level, -cnpp, -type) |> 
    group_by(year, run) |> 
    summarize(across(
      .cols = npp,
      .fns = list(mean = mean, lower = qnts_lower, upper = qnts_upper),
      .names = "{.col}_{.fn}"
    )) 

}
