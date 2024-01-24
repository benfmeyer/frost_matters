#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param full_data_station
#' @return
#' @author ben
#' @export
make_sample_depth_plot <- function(full_data_station, full_detrended) {

  depth_tb <- full_data_station |>
    unnest(cols = c(data)) |> 
    group_by(year, site_id) |>
    summarize(sample_depth = n())

  chronology_raw <- full_detrended |> 
    unnest(cols = c(data)) 
  
  chron_sample_depth <- left_join(chronology_raw, depth_tb) |> 
    select(site_id, year, spline, sample_depth)
  
  ylim_primary <- c(0.2, 1.8)
  ylim_secondary <- c(0, 30)

  b <- diff(ylim_primary)/diff(ylim_secondary)
  a <- ylim_primary[1] - b * ylim_secondary[1]
  
  depth_plot <- ggplot(chron_sample_depth, aes(year, spline))+
    geom_area(aes(y = a + sample_depth * b), fill = "grey", alpha = 0.5)+
    geom_line()+
    facet_wrap(~site_id, ncol = 4)+
    scale_y_continuous(sec.axis = sec_axis(~(. - a)/b, name = "Sample Depth"), 
                       expand = expansion(add = c(0.05, 0)))+
    coord_cartesian(xlim = c(1951,2020), ylim = c(0.2, 1.8), expand = F)+
    geom_vline(xintercept = c(1953, 2011), linetype = "dashed")+
    labs(x = "Year", y = "Ring-width index")+
    theme_clean()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
