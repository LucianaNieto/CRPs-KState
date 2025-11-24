

load_ssurgo_data <- function(aoi, ssurgo_dir, counties) {
  library(sf)
  library(dplyr)
  library(soilDB)

  if (!dir.exists(file.path(ssurgo_dir, "SSURGO_MU"))) {
    message("SSURGO  Kansas  ", ssurgo_dir, " …")
    downloadSSURGO(states = "KS", dir = ssurgo_dir)
  }

  mu_path <- file.path(ssurgo_dir, "SSURGO_MU", "mupolygon.shp")
  mu_all <- st_read(mu_path, stringsAsFactors = FALSE, quiet = TRUE)

  mu <- mu_all %>%
    filter(STATE_NAME == "Kansas", COUNTY_NAME %in% counties) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON")

  comp <- st_read(file.path(ssurgo_dir, "SSURGO_CO", "component.shp"),
                  stringsAsFactors = FALSE, quiet = TRUE)
  hz   <- st_read(file.path(ssurgo_dir, "SSURGO_CO", "chorizon.shp"),
                  stringsAsFactors = FALSE, quiet = TRUE)

  soil_data <- comp %>%
    filter(mukey %in% mu$mukey) %>%
    inner_join(hz, by = "cokey") %>%
    filter(hzdept_r >= 0, hzdepb_r <= 15) %>%
    group_by(mukey, compname, taxorder, hydgrp, drainagecl, erocl) %>%
    summarize(
      texture = first(texdesc[order(hzdept_r)]),
      ph = mean(ph1to1h2o_r, na.rm = TRUE),
      organic_matter = mean(om_r, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(mukey) %>%
    summarize(
      soil_type       = first(taxorder[order(desc(compname))]),
      texture         = first(texture[order(desc(compname))]),
      ph              = mean(ph, na.rm = TRUE),
      organic_matter  = mean(organic_matter, na.rm = TRUE),
      hydrologic_group = first(hydgrp[order(desc(compname))]),
      drainage_class   = first(drainagecl[order(desc(compname))]),
      erosion_hazard   = first(erocl[order(desc(compname))]),
      .groups = "drop"
    )

  return(list(
    mapunits  = mu,
    soil_data = soil_data
  ))
}
