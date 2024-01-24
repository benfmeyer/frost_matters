#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lpjg_outputs
#' @return
#' @author ben
#' @export
get_cmass <- function(lpjg_outputs) {

  cmass <- lpjg_outputs |> 
    filter(str_detect(type,"cmass"), run %in% c("frost", "baseline")) |> 
    mutate(output = case_when(
      type == "cmass_debt" ~ output * -1,
      TRUE ~ output
    ))
  
  return(cmass)

}
