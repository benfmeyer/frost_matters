library(targets)
library(tarchetypes)

if(!dir.exists("data")) {
  curl::curl_download("https://zenodo.org/api/records/10562679/files/data.zip/content", "data.zip")
  unzip("data.zip")
}



tar_option_set(packages = c("tidyverse", "lpjreadr","terra", "zoo", "magrittr",
                            "here", "dplR", "RANN", "sf", "janitor",
                            "multcompView", "ncdf4", "patchwork", "scico", "ggthemes",
                            "nationalparkcolors", "rnaturalearth", "rnaturalearthhires"))

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)



## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  tar_target(
    dittmar,
    get_dittmar_data()
  ),
  
  tar_target(
    principe,
    get_principe_data()
  ),
  
  tar_target(
    meyer,
    get_meyer_data()
  ),

  tar_target(
    bdn,
    get_bdn_data()
  ),
  
  tar_target(
    full_data_station,
    list(dittmar, principe, meyer, bdn) |>  
      reduce(full_join) |> 
      select(-tree) |> 
      group_nest(lat, lon, site_id) |> 
      mutate(lon = round(lon, 4),
             lat = round(lat, 4))
  ),
  
  tar_target(
    sample_depth_plot,
    make_sample_depth_plot(full_data_station, full_detrended)
  ),
  
  tar_target(
    crd_site_key,
    make_key(full_data_station)
  ),
  
  
  tar_files_input(
    threshold_files,
    list.files("data/frost_sensitivity", "*anpp.out", recursive = T, full.names = T),
    format = "file"
  ),
  
  tar_target(
    frost_sensitivity,
    read_fs(threshold_files, crd_site_key), 
    pattern = map(threshold_files)
  ),
  
  tar_target(
    sens_trw,
    aggregate_runs(frost_sensitivity, full_detrended)
  ),

  tar_target(
    dem_file,
    "data/dem/eu_dem_v11_E40N20-30_WGS84_bav_box.nc",
    format = "file"
  ),
  
  tar_target(
    gridlist,
    make_gridlist(full_data_station),
    format = "file"
  ),
  
  tar_target(
    full_detrended,
    detrend_rwl(full_data_station, method = c("Spline"), nyrs = 30)
  ),
  
  tar_files_input(
    frost_validation_files,
    list.files("data/frost_validation", "*anpp.out", recursive = T, full.names = T),
    format = "file"
  ),
  
  tar_target(
    frost_validation,
    read_fs(frost_validation_files, crd_site_key),
    pattern = map(frost_validation_files)
  ),
  
  tar_target(
    fs_trw,
    aggregate_runs(frost_validation, full_detrended)
  ),
  
  tar_target(
    lpjg_files,
    list.files("data/runs_22012024_revision/baseline", pattern = "*.out", full.names = T),
    format = "file"
  ),
  
  tar_target(
    lpjg_frost_files,
    list.files("data/runs_22012024_revision/frost", pattern = "*.out", full.names = T),
    format = "file"
  ),

  tar_target(
    lpjg_output_files,
    cat_files(lpjg_frost_files, lpjg_files)
  ),
  
  tar_target(
    lpjg_outputs_raw,
    join_outputs(lpjg_output_files),
    pattern = map(lpjg_output_files)
  ),
  
  tar_target(
    lpjg_outputs,
    clean_outputs(lpjg_outputs_raw)
  ),
  
  tar_target(
    npp,
    get_npp(lpjg_outputs)
  ),
  
  tar_target(
    npp_loss,
    calc_npp_loss(npp)
  ),
  
  tar_target(
    mnpp,
    calc_mean_npp(npp)
  ),
  
  tar_target(
    custom_theme,
    make_theme()
  ),
  
  tar_target(
    npp_plots,
    make_npp_plots(mnpp, npp_loss, custom_theme)
  ),
  
  tar_target(
    cmass,
    get_cmass(lpjg_outputs)
  ),
  
  tar_target(
    cmass_loss,
    calc_cmass_loss(cmass)
  ),
  
  tar_target(
    cmass_plots,
    make_cmass_plots(cmass_loss, custom_theme)
  ),
  
  tar_target(
    bav_shp,
    make_bav_shp()
  ),
  
  tar_target(
    bav_grid_file,
    "data/remapping/LFU_WGS84_5km_mask.nc",
    format = "file"
  ),
  
  tar_target(
    frost_frequency,
    get_ff(lpjg_outputs)
  ),
  
  tar_target(
    cmass_map,
    make_map(lpjg_outputs, bav_shp, dem_file, bav_grid_file, custom_theme)
  ),
  
  tar_target(
    res_plot,
    make_res_plot(dittmar, meyer, bdn, principe, fs_trw, custom_theme)
  ),
  
  tar_target(
    wzm_data,
    c("data/FROST_PAPER_WZM_20231207/PDM20110525.tif", "data/FROST_PAPER_WZM_20231207/Frost.shp")
  ),
  
  tar_target(
    wzm_plot,
    make_wzm_plot(wzm_data, meyer, bdn, principe, bav_shp, custom_theme)
  ),
  
  tar_target(
    sup_plot,
    make_sup_plot(dittmar, meyer, bdn, principe, sens_trw, custom_theme)
  )
)