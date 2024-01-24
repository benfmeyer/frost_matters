#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param npp
#' @return
#' @author ben
#' @export
make_npp_plots <- function(mnpp, npp_loss, custom_theme) {

  pal <- nationalparkcolors::park_palette("Saguaro")[c(5,1)]
  
  npp_plot <- ggplot(mnpp, aes(year, npp_mean, fill = run))+
    geom_ribbon(aes(ymin = npp_lower, ymax = npp_upper), alpha = 0.2)+
    geom_line(linewidth = 1, aes(color = run))+
    theme_clean()+
    labs(y = expression(Annual~NPP~(~kg~C~m^-2)), x = "Year", color = "Run", fill = "Run")+
    scale_fill_manual(values = pal, labels = c( "LPJ-GUESS", "LPJ-GUESS-FROST"))+
    scale_color_manual(values = pal, labels = c("LPJ-GUESS", "LPJ-GUESS-FROST"))+
    custom_theme+
    theme(panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
          panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"))
  
  nl_plot <- ggplot(npp_loss, aes(year, loss_mean))+
    geom_line(color = "red")+
    geom_ribbon(aes(ymin = loss_lower, ymax = loss_upper), alpha = 0.3, fill = "grey")+
    theme_clean()+
    labs(y = expression(Delta~NPP~(~kg~C~m^-2)), x = "Year")+
    custom_theme+
    scale_y_continuous(breaks = c(-2.5, -2, -1.5, -1, -0.5, 0), limits = c(-2.7, 0.25))
  
  plot_list <- list(npp_plot, nl_plot) |> 
    set_names(c("npp", "npp_loss"))
  
  return(plot_list)

}
