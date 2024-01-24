#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wzm_data
#' @param dittmar
#' @param meyer
#' @param bdn
#' @param principe
#' @param fs_trw
#' @param custom_theme
#' @return
#' @author ben
#' @export
make_wzm_plot <- function(wzm_data, meyer, bdn, principe, bav_shp, custom_theme) {

  wzm_poly <- read_sf(wzm_data[[2]])
  wzm_tif <- rast(wzm_data[[1]]) |> 
    as.data.frame(xy = T) |> 
    mutate(binned_pdm = cut(lyr.1, breaks = c(-0.65, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 1.41)))
  
  crds <- bind_rows(meyer, bdn, principe) |> 
    select(site_id, lat, lon) |> 
    unique()
  
  ggplot()+
    geom_sf(data = bav_shp, fill = "grey95")+
    geom_tile(data = wzm_tif, aes(x, y, fill = binned_pdm))+
    geom_point(data = crds, aes(lon, lat), color = "black", size = 2)+
    geom_sf(data = wzm_poly, fill = NA, color = "black", linewidth = 0.5)+
    scale_fill_scico_d(palette = "roma", direction = 1)+
    coord_sf()+
    theme_few()+
    labs(x = "Longitude", y = "Latitude", fill = "PDM")+
    custom_theme+
    theme(legend.position = "right")
}
