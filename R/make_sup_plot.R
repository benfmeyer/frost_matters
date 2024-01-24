#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dittmar
#' @param meyer
#' @param bdn
#' @param principe
#' @param sens_trw
#' @param custom_theme
#' @return
#' @author ben
#' @export
make_sup_plot <- function(dittmar, meyer, bdn, principe, sens_trw, custom_theme) {

  dt <- dittmar$site_id |> 
    unique()
  pr <- principe$site_id |>
    unique()
  bd <- bdn$site_id |> 
    unique()
  my <- meyer$site_id |> 
    unique()
  
  soi <- sens_trw |> 
    mutate(ds = case_when(
      site_id %in% dt ~ "dittmar",
      site_id %in% pr ~ "principe",
      site_id %in% bd ~ "bdn",
      site_id %in% my ~ "meyer"
    )) |> 
    mutate(frost_tolerance = as.character(frost_tolerance),
           frost_tolerance = replace_na(frost_tolerance, "RWI"))
  
  rt <- function(x, n) {
    pre_dr <- rollmean(lag(x), k = n, align = "right", fill = NA)
    idx <- x / pre_dr
  }
  
  res <- soi |> group_by(site_id, frost_tolerance) |> 
    mutate(res = rt(val, 2)) |> 
    filter(case_when(
      ds %in% c("meyer", "principe") ~ year  == 2011,
      ds == "dittmar" ~ year  ==  1953,
      ds == "bdn" ~ year == 2011
    ), idx != "rwl") 
  
  
  pal <- nationalparkcolors::park_palette("Saguaro")[c(3,2)]
  order <- c(seq(-0.3, -5.5, -0.2), "RWI")  
  
  rp <- res |> 
    filter(frost_tolerance != 0) |> 
    ggplot(aes(factor(frost_tolerance, order), res, fill = type))+
    geom_boxplot(outlier.shape = NA)+
    theme_clean()+
    facet_grid(year~.)+
    scale_fill_manual(values = pal, labels = c("LPJ-GUESS-FROST", "RWI"))+
    scale_color_manual(values = pal, guide = "none")+
    labs(x = "LSF Year", y = "Resistance", fill = "Dataset")+
    geom_hline(yintercept = 1, color = "red")+
    custom_theme+
    theme(axis.text.x = element_text(angle = 90))
  
  return(rp)

}
