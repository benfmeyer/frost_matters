#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lpjg_outputs
#' @return
#' @author ben
#' @export
calc_npp_loss <- function(npp) {

  npp_loss <- npp |> 
    select(-level, -npp, -type) |>  
    pivot_wider(names_from = run, values_from = cnpp) |> 
    mutate(loss = frost - baseline) |>
    group_by(year) |> 
    summarize(across(
      .cols = loss,
      .fns = list(mean = mean, lower = qnts_lower, upper = qnts_upper),
      .names = "{.col}_{.fn}"
    ))
  
  return(npp_loss)

}
