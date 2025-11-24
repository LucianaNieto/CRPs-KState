load_crp_history <- function(data_path) {
  crp_history_raw <- read_csv(paste0(data_path, "CRPHistoryCounty86-24-acres.csv"), show_col_types = FALSE)
  crp_history <- crp_history_raw %>%
    pivot_longer(cols = -c(STATE, COUNTY), names_to = "Year", values_to = "Acres") %>%
    mutate(
      STATE = toupper(trimws(STATE)),
      COUNTY = toupper(trimws(COUNTY)),
      Year = as.numeric(Year),
      Acres = as.numeric(gsub(",", "", Acres))
    ) %>%
    filter(STATE == "KANSAS") %>%
    distinct(STATE, COUNTY, Year, .keep_all = TRUE)
  return(crp_history)
}

load_crp_practices <- function(data_path) {
  practices_raw <- read_csv(paste0(data_path, "CRPPracticesbyCountyJAN24.csv"), show_col_types = FALSE)
  practices <- practices_raw %>%
    pivot_longer(cols = -c(STATE, COUNTY), names_to = "PracticeType", values_to = "Acres") %>%
    mutate(
      STATE = toupper(trimws(STATE)),
      COUNTY = toupper(trimws(COUNTY)),
      Acres = as.numeric(gsub(",", "", Acres))
    ) %>%
    filter(STATE == "KANSAS", !is.na(Acres))
  return(practices)
}

load_crp_expiration <- function(data_path) {
  expire_data <- read_csv(paste0(data_path, "EXPIRECOUNTY.csv"), show_col_types = FALSE)
  return(expire_data)
}