#
# get_water_chemistry_from_stations
# 
# IMPROVED by including VALUE_ID in the result from V_WATER_CHEMISTRY_SAMPLES 
#   (which gives us a link to AqM export files to VM)

get_water_chemistry_from_stations <- function(station_id, years, station_metadata = NULL){
  
  # Get station metadata
  if (is.null(station_metadata)){
    station_metadata <- get_nivabase_selection("PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME", 
                                               "PROJECTS_STATIONS",
                                               "STATION_ID",
                                               station_id)
  }
  
  year_string <- paste0("(", paste(years, collapse = ","), ")")
  
  # WATER_SAMPLES - 1-3 records per station/year
  df_water <- get_nivabase_selection(
    "WATER_SAMPLE_ID, STATION_ID, SAMPLE_POINT_ID, SAMPLE_DATE, DEPTH1, DEPTH2", 
    "WATER_SAMPLES",
    "STATION_ID",
    station_id,
    extra_sql = paste(" and extract(YEAR from SAMPLE_DATE) in ", year_string)
  ) %>%
    left_join(station_metadata, by = c("STATION_ID"))
  
  # xtabs(~STATION_CODE + year(SAMPLE_DATE), df_water)   
  
  # V_WATER_CHEMISTRY_SAMPLES - up to 359 records per station/year
  df_measurements_water_1 <- get_nivabase_selection(
    "WATER_SAMPLE_ID, VALUE, METHOD_ID, UNIT, NAME, LABORATORY, FLAG1, APPROVED, SAMPLE_DATE, DEPTH1, DEPTH2, STATION_ID, STATION_CODE, PROJECT_ID, PARAMETER, VALUE_ID", 
    "V_WATER_CHEMISTRY_SAMPLES",
    "STATION_ID", 
    station_metadata$STATION_ID) %>%
    mutate(Year = year(SAMPLE_DATE)) %>%
    filter(Year %in% years) %>%
    left_join(station_metadata, by = c("STATION_ID", "STATION_CODE", "PROJECT_ID"))
  
  # xtabs(~STATION_CODE + Year, df_waterchem)
  
  # Water samples from Nivabasen ---
  df_samples_water <- df_measurements_water_1 %>% 
    filter(Year %in% years) %>%
    count(STATION_CODE, WATER_SAMPLE_ID, name = "n_nivabasen") %>%
    # Order for making "Samplenumber"
    arrange(STATION_CODE, WATER_SAMPLE_ID) %>%
    # Add self-made "Samplenumber"- needed for join below
    group_by(STATION_CODE) %>%
    mutate(Samplenumber = seq_len(length(STATION_CODE))) %>%
    ungroup()
  
  # Water measurements from Labware ---
  df_measurements_labware_water <- get_nivabase_selection(
    "AQUAMONITOR_CODE, SAMPLE_TYPE, SAMPLE_NUMBER, DESCRIPTION, REPORTED_NAME, ENTRY_QUALIFIER, NUMERIC_ENTRY", 
    "LABWARE_IMPORT",
    "AQUAMONITOR_ID", 
    station_id,
    extra_sql = paste(
      " and extract(YEAR from SAMPLED_DATE) in", year_string,
      " and SAMPLE_TYPE = 'AVLØPSVANN'")
  ) %>%
    mutate(
      SAMPLE_TYPE = sub("<d8>", "Ø", SAMPLE_TYPE)
    )
  
  # Water samples from Labware ---
  df_samples_labware_water <- df_measurements_labware_water %>%
    count(AQUAMONITOR_CODE, SAMPLE_TYPE, SAMPLE_NUMBER, DESCRIPTION, 
          name = "n_labware") %>%
    # Order records for making "Samplenumber"
    arrange(AQUAMONITOR_CODE, SAMPLE_NUMBER) %>%
    # Add self-made "Samplenumber" 
    group_by(AQUAMONITOR_CODE) %>%
    mutate(Samplenumber = seq_len(length(AQUAMONITOR_CODE))) %>%
    ungroup()
  
  # samples_labware_water
  
  # Joining
  df_measurements_water <- df_measurements_water_1 %>%
    # Add "Samplenumber" (for joining with 'df_samples_labware_water') :      
    left_join(
      df_samples_water %>% select(STATION_CODE, WATER_SAMPLE_ID, Samplenumber),
      by = c("STATION_CODE", "WATER_SAMPLE_ID")) %>%
    # Add "SAMPLE_TYPE", "SAMPLE_NUMBER", "DESCRIPTION" and "n_labware" from Labware:      
    left_join(
      df_samples_labware_water,
      by = c("STATION_CODE" = "AQUAMONITOR_CODE", "Samplenumber" = "Samplenumber"))
  
  # Check!
  if (nrow(df_measurements_water) != nrow(df_measurements_water_1)){
    stop("nrow(final measurements) != nrow(measurements from V_WATER_CHEMISTRY_SAMPLES)!")
  }
  
  df_measurements_water
  
}
