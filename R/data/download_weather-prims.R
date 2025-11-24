library(prism)
library(raster)
library(sf)
library(tigris)
library(dplyr)
library(lubridate)

# Set directory for downloading PRISM data
prism_set_dl_dir("prism_data")
options(prism_download_timeout = 600)

# Download monthly PRISM data (1985-2024, all variables)
variables <- c("ppt", "tmean", "tmin", "tmax", "vpdmin", "vpdmax")
years <- 1985:2024
months <- 1:12
for (var in variables) {
  for (year in years) {
    for (mon in months) {
      get_prism_monthlys(type = var, year = year, mon = mon, keepZip = FALSE)
    }
  }
}

# Load shapefile of Kansas counties
counties <- counties(state = "KS", cb = TRUE, class = "sf")

# Function to extract mean values by county
extract_county_means <- function(file_path, counties) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  r <- raster(file_path)
  means <- extract(r, counties, fun = mean, na.rm = TRUE, sp = TRUE)
  data.frame(
    county_fips = counties$GEOID,
    county_name = counties$NAME,
    value = means@data[[ncol(means@data)]]
  )
}

# Process all PRISM files
prism_files <- prism_archive_ls()
climate_data <- data.frame()

for (file in prism_files) {
  # Extract info from file name
  file_name <- basename(file)
  print(paste("Processing file:", file_name))
  
  # Build path to .bil file
  file_path <- file.path("prism_data", file_name, paste0(file_name, ".bil"))
  if (!file.exists(file_path)) {
    warning(paste(".bil file not found for:", file_path))
    next
  }
  
  # Extract metadata manually
  parts <- unlist(strsplit(file_name, "_"))
  type <- parts[2] # ppt, tmean, tmin, tmax, vpdmin, vpdmax
  date_str <- parts[5]
  year <- as.numeric(substr(date_str, 1, 4)) 
  month <- as.numeric(substr(date_str, 5, 6)) 
  
  # Verify values
  if (is.na(year) || is.na(month)) {
    warning(paste("Could not extract year or month for file:", file_name))
    next
  }
  
  # Create date
  date <- as.Date(sprintf("%04d-%02d-01", year, month), format = "%Y-%m-%d")
  
  # Filter for 1985-2024
  if (year >= 1985 && year <= 2024) {
    means <- tryCatch({
      extract_county_means(file_path, counties) %>%
        mutate(
          date = date,
          variable = type
        )
    }, error = function(e) {
      warning(paste("Error processing", file_name, ":", e$message))
      NULL
    })
    
    if (!is.null(means)) {
      climate_data <- bind_rows(climate_data, means)
      print(paste("Added", nrow(means), "rows for", file_name))
    }
  }
}

# Check if climate_data is empty
if (nrow(climate_data) == 0) {
  stop("No data processed. Check warnings and PRISM files.")
}

# Pivot data to have all variables as columns
climate_data <- climate_data %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value,
    values_fill = NA
  ) %>%
  arrange(county_fips, date)

# Save processed data
write.csv(climate_data, "prism_kansas_counties_1985_2024.csv", row.names = FALSE)
saveRDS(climate_data, "prism_kansas_counties_1985_2024.rds")