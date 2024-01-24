#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param modis_path
#' @return
#' @author ben
#' @export
package_modis_climate <- function(modis_path, key) {
  
  clean_modis <- read_csv(modis_path) %>%
    rename(site_id = site) %>% 
    right_join(key) %>%  
    group_by(lon, lat) %>%  
    mutate(station = cur_group_id(),
           date = ymd(date),
           leap = if_else(month(date) == 2 & day(date) == 29, 1, 0)) %>% 
    filter(leap == 0, year(date) <= 2018) %>%  
    ungroup() %>%  
    group_by(date) %>%  
    mutate(time = cur_group_id() - 1)
  
  station_dim <- clean_modis %>%  
    ungroup()  %>%  
    select(station) %>% 
    unique() %>% 
    pull() %>% 
    ncdim_def("station", "station_id", .)
  
  time_dim <- clean_modis %>% 
    ungroup() %>% 
    select(time) %>% 
    unique() %>% 
    pull() %>% 
    ncdim_def("time", "days since 1950-01-01", ., calendar = "365_day")
  
  lon_var <- ncvar_def("lon", "degrees_east", list(station_dim))
  lat_var <- ncvar_def("lat", "degrees_north", list(station_dim))
  tmin_var <- ncvar_def("tmin", "K", list(station_dim, time_dim))
  temp_var <- ncvar_def("temp", "K", list(station_dim, time_dim))
  cc_var <- ncvar_def("insol", "1", list(station_dim, time_dim))
  prec_var <- ncvar_def("prec", "kg m-2", list(station_dim, time_dim))
  
  file_names <- c("tmin_modis_stations_lpjf.nc", "temp_modis_stations_lpjf.nc",
                  "prec_modis_stations_lpjf.nc", "cc_modis_stations_lpjf.nc")
  
  tmin_nc <- nc_create(file_names[[1]], list(tmin_var, lon_var, lat_var), force_v4 = T)
  temp_nc <- nc_create(file_names[[2]], list(temp_var, lon_var, lat_var), force_v4 = T)
  prec_nc <- nc_create(file_names[[3]], list(prec_var, lon_var, lat_var), force_v4 = T)
  cc_nc   <- nc_create(file_names[[4]], list(cc_var, lon_var, lat_var), force_v4 = T)
  
  
  # fill tmin -----------------------------------------------------------------------------------
  
  clean_modis %>% 
    filter(var == "TN") %>% 
    mutate(pre = pre + 273.15) %>%
    arrange(station) %>% 
    arrange(time) %>% 
    pull(pre) %>% 
    ncvar_put(tmin_nc, tmin_var, .)
  
  clean_modis %>%
    pull(lon) %>% 
    unique() %>% 
    ncvar_put(tmin_nc, lon_var, .)
  
  clean_modis %>%
    pull(lat) %>% 
    unique() %>% 
    ncvar_put(tmin_nc, lat_var, .)
  
  ncatt_put(tmin_nc, tmin_var, "coordinates", "lat lon")
  ncatt_put(tmin_nc, lon_var, "standard_name", "longitude")
  ncatt_put(tmin_nc, lat_var, "standard_name", "latitude")
  ncatt_put(tmin_nc, tmin_var, "standard_name", "air_temperature")
  
  # fill temp -----------------------------------------------------------------------------------
  
  clean_modis %>% 
    filter(var == "TM") %>% 
    mutate(pre = pre + 273.15) %>% 
    arrange(station) %>% 
    arrange(time) %>% 
    pull(pre) %>% 
    ncvar_put(temp_nc, temp_var, .)
  
  clean_modis %>% 
    pull(lon) %>% 
    unique() %>% 
    ncvar_put(temp_nc, lon_var, .)
  
  clean_modis %>% 
    pull(lat) %>% 
    unique() %>% 
    ncvar_put(temp_nc, lat_var, .)
  
  ncatt_put(temp_nc, temp_var, "coordinates", "lat lon")
  ncatt_put(temp_nc, lon_var, "standard_name", "longitude")
  ncatt_put(temp_nc, lat_var, "standard_name", "latitude")
  ncatt_put(temp_nc, temp_var, "standard_name", "air_temperature")
  
  # fill prec -----------------------------------------------------------------------------------
  
  clean_modis %>% 
    filter(var == "RR") %>%
    arrange(station) %>% 
    arrange(time) %>% 
    pull(pre) %>% 
    ncvar_put(prec_nc, prec_var, .)
  
  clean_modis %>% 
    pull(lon) %>% 
    unique() %>% 
    ncvar_put(prec_nc, lon_var, .)
  
  clean_modis %>% 
    pull(lat) %>% 
    unique() %>% 
    ncvar_put(prec_nc, lat_var, .)
  
  ncatt_put(prec_nc, prec_var, "coordinates", "lat lon")
  ncatt_put(prec_nc, lon_var, "standard_name", "longitude")
  ncatt_put(prec_nc, lat_var, "standard_name", "latitude")
  ncatt_put(prec_nc, prec_var, "standard_name", "precipitation_amount")
  
  # fill cc -------------------------------------------------------------------------------------
  
  clean_modis %>% 
    filter(var == "CC") %>%
    mutate(pre = pre/8) %>% 
    arrange(station) %>% 
    arrange(time) %>%  
    pull(pre) %>% 
    ncvar_put(cc_nc, cc_var, .)
  
  clean_modis %>% 
    pull(lon) %>% 
    unique() %>% 
    ncvar_put(cc_nc, lon_var, .)
  
  clean_modis %>% 
    pull(lat) %>% 
    unique() %>% 
    ncvar_put(cc_nc, lat_var, .)
  
  ncatt_put(cc_nc, cc_var, "coordinates", "lat lon")
  ncatt_put(cc_nc, lon_var, "standard_name", "longitude")
  ncatt_put(cc_nc, lat_var, "standard_name", "latitude")
  ncatt_put(cc_nc, cc_var, "standard_name", "cloud_area_fraction")
  
  list(tmin_nc, temp_nc, prec_nc, cc_nc) %>% map(nc_close)

  return(file_names)
}
