#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cmass
#' @return
#' @author ben
#' @export
calc_cmass_loss <- function(cmass) {

  cmass_loss <- cmass |> 
    select(-level) |>
    group_by(type) |> 
    group_split() |> 
    map(~pivot_wider(.x, names_from = run, values_from = output)) |> 
    map(~mutate(.x, loss = frost - baseline)) |>
    bind_rows() |> 
    group_by(year, type) |> 
    summarize(across(
      .cols = loss,
      .fns = list(mean = mean, lower = qnts_lower, upper = qnts_upper),
      .names = "{.fn}"
    ))
  
  return(cmass_loss)

}
