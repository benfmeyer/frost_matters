#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cmass
#' @param bav_shp_file
#' @param dem_file
#' @return
#' @author ben
#' @export
make_map <- function(lpjg_outputs, bav_shp, dem_file, bav_grid_file, custom_theme) {

  bav_grid <- rast(bav_grid_file)
  
  dem <- rast(dem_file) |> 
    terra::project(bav_grid)
  
  dem_masked <- dem / bav_grid
  
  dem_mat <- as.matrix(dem_masked)
  dem_crd <- terra::crds(dem_masked)
  
  dem_tibble <- cbind(dem_mat, dem_crd) |> 
    as_tibble() |> 
    set_names(c("elevation", "lon", "lat")) |> 
    filter(is.finite(elevation))
  

  cmass_end <- lpjg_outputs |> 
    filter(type == "cmass", year == 2020, run %in% c("baseline", "frost")) |> 
    select(lonx = lon, latx =lat, output, run) |> 
    pivot_wider(names_from = run, values_from = output) |> 
    mutate(cmass_loss = frost - baseline,
           cmass_loss_pc = abs(cmass_loss)/baseline)
  
  in_crd <- dem_tibble |> 
    select(lon , lat) 
  
  out_crd <- cmass_end |> 
    select(lonx, latx) 
  
  match_crd <- nn2(in_crd, out_crd, 1)[[1]]
  
  key <- in_crd[match_crd, ] |> 
    bind_cols(out_crd)
  
  dem_cmass <- left_join(key, dem_tibble) |> 
    left_join(cmass_end) |> 
    select(-lonx, -latx) |> 
    pivot_longer(-c(lon, lat), names_to = "var", values_to = "val")
  
  plot_vars <- dem_cmass |> 
    filter(var %in% c("elevation", "cmass_loss")) |> 
    group_by(var) |> 
    group_split()
  
  legend_text <- list(expression(Delta~carbon~mass~(kg~C~m^-2)),
                   "Elevation (m)")
  
  loss_bands <- dem_cmass |> 
    pivot_wider(names_from = var, values_from = val) |> 
    mutate(cl = cut(cmass_loss_pc, breaks = c(seq(0,0.2, by = 0.05), 1), include.lowest = T)) 
  
  loss_bx <- ggplot(loss_bands, aes(cl, elevation))+
    geom_boxplot()+
    theme_clean()+
    labs(x = "Biomass loss classes (%)", y = "Elevation")+
    scale_x_discrete(labels = c("[0, 5]", "(5, 10]", "(10, 15]", "(15, 20]", "> 20"))+
    custom_theme
  
  mask <- bav_shp |>
    st_bbox() |> 
    add(c(-0.15, -0.15, 0.15, 0.15)) |>
    st_as_sfc() |> 
    st_buffer(0.7) |>
    st_difference(bav_shp) |> 
    st_as_sf()
  
  .plot_map <- function(plot_vars, legend_text, dir, bav_shp, mask, custom_theme){
    ggplot(plot_vars, environment = environment())+
      geom_tile(aes(lon, lat, fill = val))+
      scale_fill_scico(palette = "imola", direction = dir)+
      scale_color_scico(palette = "imola", direction = dir)+
      geom_sf(data = bav_shp, fill = NA, color = "black", linewidth = 1)+
      geom_sf(data = mask, fill = "white", color = "white")+
      theme_few()+
      labs(y = "Latitude", x = "Longitude", fill = legend_text)+
      theme(legend.position = "bottom")+
      guides(fill = guide_colorbar(title.position = "top", barwidth = 10))+
      custom_theme
  }
  
  plot_list <- pmap(list(plot_vars, legend_text, list(1, -1)), .plot_map, bav_shp, mask, custom_theme)
  
  
  maps <- ((plot_list[[1]] | plot_list[[2]]) / loss_bx) + plot_annotation(tag_levels = "A")
  
  return(maps)
}
