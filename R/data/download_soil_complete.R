
# Standalone script: fetch & cache USDA WSS soil data by county,
# with robust row‐count checking to avoid “missing value where TRUE/FALSE” errors.
# Usage: source("load_soils_data_safe.R") in an R session
# —————————————————————————————————————————————————————————————————————————————

#Configuration: paths & counties
AOI_GEOJSON     <- "data/spatial/kansas-counties.geojson"
PROCESSED_PATH  <- "data/processed/"
TIMEOUT_SECONDS <- 600

# COUNTIES <- c("WYANDOTTE","WOODSON","WILSON","WICHITA","WASHINGTON")
COUNTIES <- NULL

library(sf)
library(dplyr)
library(soilDB)

# Define load_soils_data() with safe row‐count logic

load_soils_data <- function(aoi, processed_path, counties = NULL, timeout_seconds = 600) {
  if (!dir.exists(processed_path)) dir.create(processed_path, recursive = TRUE)
  
  message("Processed path: ", processed_path)
  message("Counties parameter: ",
          if (is.null(counties)) "<NULL> (will use AOI)" else paste(counties, collapse = ", "))
  
  counties_all <- unique(aoi$name)
  if (is.null(counties)) {
    counties_to_return <- counties_all
  } else {
    counties_to_return <- intersect(counties, counties_all)
    if (length(counties_to_return) == 0)
      stop("None of the requested counties found in AOI.")
  }
  
  mapunits_file <- file.path(processed_path, "soils_kansas_mapunits.gpkg")
  data_file     <- file.path(processed_path, "soils_kansas_data.rds")
  
  message("Checking global cache:")
  message("  ", basename(mapunits_file), " exists? ", file.exists(mapunits_file))
  message("  ", basename(data_file),     " exists? ", file.exists(data_file))
  
  # If either global cache file is missing, fetch from USDA WSS API
  if (!file.exists(mapunits_file) || !file.exists(data_file)) {
    message("Cache missing → fetching from USDA WSS API …")
    
    # Helper to impose a timeout on expressions
    with_timeout <- function(expr, timeout) {
      setTimeLimit(elapsed = timeout, transient = TRUE)
      on.exit(setTimeLimit(elapsed = Inf, transient = FALSE))
      expr
    }
    
    all_mapunits <- list()
    all_soil_data <- list()
    
    for (county in counties_all) {
      message("\n— Processing county: ", county)
      county_aoi <- filter(aoi, name == county)
      
      # 1) Spatial query (suppress GDAL and soilDB messages)
      mapunits <- tryCatch({
        suppressMessages(
          with_timeout(
            SDA_spatialQuery(st_transform(county_aoi, 4326), what = "mupolygon"),
            timeout_seconds
          )
        )
      }, error = function(e) {
        message("  [ERROR] Spatial query failed for ", county, ": ", e$message)
        return(NULL)
      })
      
      # 2) Safe row count check
      if (is.null(mapunits)) {
        message("  → mapunits is NULL, skipping")
        next
      }
      row_count <- tryCatch(nrow(mapunits), error = function(e) NA)
      if (!inherits(mapunits, "sf") || is.na(row_count) || row_count == 0) {
        message(sprintf("  → no valid mapunits for %s (nrow = %s), skipping", county, row_count))
        next
      }
      message("  → fetched ", row_count, " mapunits")
      
      # 3) Clean & simplify geometries
      mapunits$county_name <- county
      mapunits <- st_make_valid(mapunits)
      valid_idx <- st_is_valid(mapunits)
      mapunits <- mapunits[valid_idx, ]
      mapunits <- mapunits[!st_geometry_type(mapunits) %in% "GEOMETRYCOLLECTION", ]
      mapunits <- st_cast(mapunits, "MULTIPOLYGON")
      mapunits <- tryCatch({
        st_simplify(mapunits, dTolerance = 10, preserveTopology = TRUE)
      }, error = function(e) {
        message("  → simplify failed: ", e$message)
        mapunits
      })
      mapunits <- st_transform(mapunits, 4326)
      
      # 4) Attribute query
      mukeys <- unique(mapunits$mukey)
      sql <- paste0(
        "SELECT component.mukey, component.cokey, component.compname, component.taxorder, ",
        "component.hydgrp, component.drainagecl, component.erocl, ",
        "chorizon.hzname, chorizon.hzdept_r, chorizon.hzdepb_r, ",
        "chtexturegrp.texdesc, chorizon.ph1to1h2o_r, chorizon.om_r ",
        "FROM component ",
        "LEFT JOIN chorizon ON component.cokey = chorizon.cokey ",
        "LEFT JOIN chtexturegrp ON chorizon.chkey = chtexturegrp.chkey ",
        "WHERE component.mukey IN (", paste(mukeys, collapse = ","), ")"
      )
      soil_raw <- tryCatch({
        suppressMessages(
          with_timeout(SDA_query(sql), timeout_seconds)
        )
      }, error = function(e) {
        message("  [ERROR] Attribute query failed for ", county, ": ", e$message)
        return(NULL)
      })
      if (is.null(soil_raw) || nrow(soil_raw) == 0) {
        message("  → no soil attributes, skipping")
        next
      }
      message("  → fetched ", nrow(soil_raw), " raw attribute rows")
      
      # 5) Summarize horizons 0–15 cm
      soil_summary <- soil_raw %>%
        as.data.frame() %>%
        filter(hzdept_r >= 0, hzdepb_r <= 15) %>%
        group_by(mukey, cokey, compname, taxorder, hydgrp, drainagecl, erocl) %>%
        summarize(
          texture        = first(texdesc, order_by = hzdept_r),
          ph             = mean(ph1to1h2o_r, na.rm = TRUE),
          organic_matter = mean(om_r,        na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(mukey) %>%
        summarize(
          soil_type        = first(taxorder,       order_by = desc(compname)),
          texture          = first(texture,        order_by = desc(compname)),
          ph               = mean(ph,              na.rm = TRUE),
          organic_matter   = mean(organic_matter,  na.rm = TRUE),
          hydrologic_group = first(hydgrp,         order_by = desc(compname)),
          drainage_class   = first(drainagecl,     order_by = desc(compname)),
          erosion_hazard   = first(erocl,          order_by = desc(compname)),
          .groups = "drop"
        )
      
      all_mapunits[[county]]  <- mapunits
      all_soil_data[[county]] <- soil_summary
    }
    
    if (length(all_mapunits) == 0) stop("No soil data could be fetched for any county.")
    
    # 6) Combine and dedupe
    mapunits_all  <- do.call(rbind, all_mapunits)
    soil_data_all <- do.call(rbind, all_soil_data) %>%
      distinct(mukey, .keep_all = TRUE)
    
    # 7) Write global cache
    st_write(mapunits_all, mapunits_file, delete_dsn = TRUE, quiet = TRUE)
    saveRDS(soil_data_all, data_file)
    message("Global cache written: ", basename(mapunits_file), ", ", basename(data_file))
    
    # 8) Write per-county clean caches
    for (county in counties_all) {
      slug   <- tolower(gsub(" ", "_", county))
      mu_ct  <- subset(mapunits_all, county_name == county)
      sd_ct  <- subset(soil_data_all, mukey %in% mu_ct$mukey)
      f_mu   <- file.path(processed_path, paste0("soils_kansas_", slug, "_clean.gpkg"))
      f_data <- file.path(processed_path, paste0("soils_kansas_", slug, "_clean.rds"))
      st_write(mu_ct, f_mu, delete_dsn = TRUE, quiet = TRUE)
      saveRDS(sd_ct, f_data)
      message("  Cached ", county, " → ", basename(f_mu), ", ", basename(f_data))
    }
    
  } else {
    message("Global cache found, skipping fetch.")
  }
  
  # 9) Load requested counties
  results_map  <- list()
  results_data <- list()
  for (ct in counties_to_return) {
    slug   <- tolower(gsub(" ", "_", ct))
    f_mu   <- file.path(processed_path, paste0("soils_kansas_", slug, "_clean.gpkg"))
    f_data <- file.path(processed_path, paste0("soils_kansas_", slug, "_clean.rds"))
    if (file.exists(f_mu) && file.exists(f_data)) {
      results_map[[ct]]  <- st_read(f_mu,   quiet = TRUE)
      results_data[[ct]] <- readRDS(f_data)
    } else {
      warning("Missing clean cache for county: ", ct)
    }
  }
  
  if (length(results_map) == 0) stop("No counties could be loaded.")
  
  final_map  <- do.call(rbind, results_map)
  final_data <- do.call(rbind, results_data)
  
  list(mapunits = final_map, soil_data = final_data)
}


aoi <- st_read(AOI_GEOJSON, quiet = TRUE)

result <- load_soils_data(
  aoi            = aoi,
  processed_path = PROCESSED_PATH,
  counties       = COUNTIES,
  timeout_seconds= TIMEOUT_SECONDS
)

message("\nLoaded map units:    ", nrow(result$mapunits))
message("Loaded soil data rows:", nrow(result$soil_data))
print(head(result$soil_data))
