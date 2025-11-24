load_kansas_counties <- function(spatial_path) {
  kansas_counties <- st_read(paste0(spatial_path, "kansas-counties.geojson"), quiet = TRUE)
  kansas_counties <- kansas_counties %>% mutate(name = toupper(trimws(name)))
  return(kansas_counties)
}