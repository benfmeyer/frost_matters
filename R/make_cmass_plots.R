#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cmass
#' @param cmass_loss
#' @return
#' @author ben
#' @export
make_cmass_plots <- function(cmass_loss, custom_theme) {

  pal <- nationalparkcolors::park_palette("Arches", 5)
  
  cl_plot <- cmass_loss |> 
    filter(type == "cmass") |> 
    ggplot(aes(year, mean))+
    geom_line(color = "red")+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "grey")+
    theme_clean()+
    ylab(expression(Delta~carbon~mass~(~kg~C~m^-2)))+
    xlab("Year")+
    custom_theme+
    scale_y_continuous(breaks = c(-2.5, -2, -1.5, -1, -0.5, 0), limits = c(-2.5, 0.7))
  
  stacked_plot <- cmass_loss |> 
    filter(type != "cmass") |> 
    ggplot(aes(year, mean, fill = type))+
    geom_area()+
    scale_fill_manual(values = pal, labels = c("Storage", "Heartwood", "Leaf", "Root", "Sapwood"))+
    theme_clean()+
    labs(y = expression(Delta~carbon~mass~(~kg~C~m^-2)), x = "Year", fill = "Pool")+
    custom_theme+
    theme(legend.position = "bottom")+
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))
  
  plot_list <- list(cl_plot, stacked_plot) |> 
    set_names(c("closs", "stack"))
  
  return(plot_list)

}
