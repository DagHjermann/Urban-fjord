

library(dplyr)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# SQL strings ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

sql_taxon <- paste(
  "NIVADATABASE.TAXONOMY_CODES left join NIVADATABASE.TAXONOMY",
  "on NIVADATABASE.TAXONOMY_CODES.NIVA_TAXON_ID = NIVADATABASE.TAXONOMY.NIVA_TAXON_ID")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for making SQLs ----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# . Making SQLs - create SQLs 
#
# Based on 'make_sql_methods' in script 71..functions  
# Adding DESCR, removing MATRIX, MATRIX_ID and METHOD_REF 
make_sql_methods2 <- function(name, unit, lab, descr = NA, cas = NA, method = NA){
  
  original_options <- options(useFancyQuotes = FALSE)
  
  txt <- paste0(
    "insert into NIVADATABASE.METHOD_DEFINITIONS ",
    "(NAME, DESCR, UNIT, LABORATORY, CAS, METHOD_REF)\n",  # \n for line shift
    "values (",
    ifelse(is.na(name), "NULL", sQuote(name)), ", ",
    ifelse(is.na(descr), "NULL", sQuote(descr)), ", ",
    ifelse(is.na(unit), "NULL", sQuote(unit)), ", ",
    ifelse(is.na(lab), "NULL", sQuote(lab)), ", ",
    ifelse(is.na(cas), "NULL", sQuote(cas)), ", ", 
    ifelse(is.na(method), "NULL", sQuote(method)), 
    ");"
  )
  options(original_options)
  txt
}

# Test
if (FALSE){
  
  make_sql_methods2(
    name = "Dechlorane plus syn",
    unit = "ng/g w.w.",
    lab = "NILU",
    descr = "bis(hexachlorocyclopentadieno)cyclooctane",
    cas = "135821-03-3")
  
  make_sql_methods2(
    name = "Dechlorane plus syn",
    unit = "ng/g w.w.",
    lab = "NILU",
    descr = "bis(hexachlorocyclopentadieno)cyclooctane",
    cas = "135821-03-3",
    method = "TEST")
  
  i <- 1
  make_sql_methods2(
    name = df_substances_sel[i,"Compound"],
    unit = "w.w.",
    lab = "NILU",
    descr = df_substances_sel[i,"Description"],
    cas = df_substances_sel[i, "CAS"])
}



#
# Make all SQLs for several user IDs, projects and role IDs
# Also adds "commit;" at the end
#
make_sql_access <- function(user_ids, project_ids, role_ids = c(0,3)){
  sql = ""
  for (user_id in user_ids){
    for (role_id in role_ids){
      sql <- paste0(
        sql,
        make_sql_access_severalprojects(user_id = user_id, 
                                        project_ids = project_ids, 
                                        role_id = role_id)
      )
    }}
  sql
}

# Test:
# make_sql_access(user_ids = c(151, 222), 
#                 project_ids = proj_ids, 
#                 role_ids = c(0,3)) %>% writeLines("clipboard")

# Results in:
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12465, 0);
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12460, 0);
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12447, 0);
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12466, 0);
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12464, 0);
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12465, 3);
# insert into NIVADATABASE.XACCESS (USERID, PROJECT_ID, ROLE_ID) values (151, 12460, 3);
# etc.


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for getting biota data from Nivabase ----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Get data from LABWARE_IMPORT (measurement level) using 
#
get_labware_import <- function(text_id = NULL, 
                               variablestring = "SAMPLE_TYPE, AQUAMONITOR_ID, AQUAMONITOR_CODE, TEXT_ID, DESCRIPTION, REPORTED_NAME"){
  text_id <- text_id[!is.na(text_id)]
  get_nivabase_selection(
    variablestring, 
    "LABWARE_IMPORT",
    "TEXT_ID", 
    text_id,
    values_are_text = TRUE) %>%
    mutate(
      SAMPLE_TYPE = sub("<d8>", "Ø", SAMPLE_TYPE)
    )
}


#
#
#
get_labware_check_sample <- function(text_id = NULL){
  text_id <- text_id[!is.na(text_id)]
  get_nivabase_selection(
    "TEXT_ID, STATUS, SPECIES, TISSUE, BIOTA_SAMPLENO, X_BULK_BIO, DESCRIPTION", 
    "LABWARE_CHECK_SAMPLE",
    "TEXT_ID", 
    text_id, values_are_text = TRUE)
}

# Get sample data from labware, including num 
# - Uses 'get_labware_import' to get data on measurement level
# - Is summarized to sample level (), bt inclupdes number of parameters (n_labware) 
# - Then uses 'get_labware_check_sample' to get AQUAMONITOR_CODE, which then is used to get
#   project + station info:  PROJECT_ID, STATION_ID
# 
# 
# Columns "NIVA-no.", "SAMPLE_TYPE", "AQUAMONITOR_ID", "AQUAMONITOR_CODE"
# Also includes 
# - Number of parameters in labware (n_labware) 
# - "SPECIES", "TISSUE", "BIOTA_SAMPLENO", "X_BULK_BIO" (= NA for non-biota)  
# - project + station info:  PROJECT_ID, STATION_ID

get_labware_samples <- function(text_id = NULL){
  
  # Make sure 'text_id' are unique and existing   
  text_id <- unique(text_id)
  text_id <- text_id[!is.na(text_id)]
  
  
  # Get data from LABWARE_IMPORT (measurement level)
  df_measurements_labware <- get_labware_import(text_id)
  
  # Make sample data   
  df_samples_labware_import <- df_measurements_labware %>%
    filter(REPORTED_NAME != "Uspesifikk organisk analyse") %>%
    distinct(SAMPLE_TYPE, AQUAMONITOR_ID, AQUAMONITOR_CODE, TEXT_ID, DESCRIPTION, REPORTED_NAME) %>%
    count(SAMPLE_TYPE, AQUAMONITOR_ID, AQUAMONITOR_CODE, TEXT_ID, DESCRIPTION, name = "n_labware")
  
  n1 <- nrow(df_samples_labware_import)
  
  # Get data from LABWARE_CHECK_SAMPLE (sample level)
  df_samples_check_labware <- get_labware_check_sample(text_id)
  
  n2 <- nrow(df_samples_check_labware)  
  
  # Check if the two has the same number of rows
  if (n1 != n2){
    txt <- paste0(
      "Number of samples from LABWARE_IMPORT (", n1, ") ",
      "differ from number of samples from LABWARE_CHECK_SAMPLE (", n2, ")!"
    )
    warning(txt)
  }
  
  # Get station metadata
  df_stations <- get_nivabase_selection("PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME", 
                                        "PROJECTS_STATIONS",
                                        "STATION_CODE",
                                        unique(df_samples_labware_import$AQUAMONITOR_CODE),
                                        values_are_text = TRUE)
  
  # Combine data from LABWARE_IMPORT and LABWARE_CHECK_SAMPLE   
  
  result <- df_samples_labware_import %>%                              # already has "NIVA-no.", "SAMPLE_TYPE", "AQUAMONITOR_ID", "AQUAMONITOR_CODE"
    full_join(df_samples_check_labware, by = c("TEXT_ID", "DESCRIPTION")) %>%  # adds "SPECIES", "TISSUE", "BIOTA_SAMPLENO", "X_BULK_BIO"
    left_join(df_stations, by = c("AQUAMONITOR_ID" = "STATION_ID")) # %>% filter(`Compartment spec.` == "Polychaetes") %>% nrow()
  
  # Check if the result has the same number of rows as the previous two
  # For instance, df_stations may have several projects for a single station  
  n3 <- nrow(result)  
  if ((n3 != n1) | (n3 != n2)){
    txt <- paste0(
      "Number of samples of result (", n3, ") ",
      "differ from number of samples of LABWARE_IMPORT (", n1, ") or LABWARE_CHECK_SAMPLE (", n2, ")!"
    )
    warning(txt)
  }
  
  result
  
}  

# Test
if (FALSE){
  # A cod muscle sample  
  get_labware_samples("NR-2019-10406")
}

#
# Get measurement-level data given station ID(s) and year(s)   
#
get_biota_chemistry_from_stations <- function(station_id, year){
  
  # Get all specimens of station   
  df_biota_specimens_1 <- get_nivabase_selection(
    "SPECIMEN_ID, DATE_CAUGHT, SAMPLE_POINT_ID, SPECIES_ID, STATION_ID, REMARK, TAXONOMY_CODE_ID, SPECIMEN_NO",
    "BIOTA_SINGLE_SPECIMENS", 
    "STATION_ID",
    station_id
  ) %>%
    mutate(YEAR = year(DATE_CAUGHT))
  cat("Number of specimens (all years): ", nrow(df_biota_specimens_1), "\n")
  
  # Add LATIN_NAME column
  df_biota_specimens_2 <- df_biota_specimens_1 %>%
    left_join(
      get_nivabase_selection("TAXONOMY_CODE_ID, LATIN_NAME", 
                             sql_taxon, 
                             "TAXONOMY_CODE_ID", 
                             unique(df_biota_specimens_1$TAXONOMY_CODE_ID), table_literal = TRUE), 
      by = "TAXONOMY_CODE_ID"
    )
  
  if (nrow(df_biota_specimens_1) != nrow(df_biota_specimens_2))
    warning("Adding LATIN_NAME changed the number of records")
  
  # xtabs(~LATIN_NAME + year(DATE_CAUGHT), df_biota_specimens)
  # xtabs(~STATION_ID + year(DATE_CAUGHT), df_biota_specimens)
  
  # Get sample-to-specimen table
  df_nb_biota_samplespec <- get_nivabase_selection("SPECIMEN_ID, SAMPLE_ID",
                                                   "BIOTA_SAMPLES_SPECIMENS",
                                                   "SPECIMEN_ID",
                                                   df_biota_specimens_2 %>%
                                                     filter(YEAR == year) %>%
                                                     pull(SPECIMEN_ID) %>%
                                                     unique()
  )
  cat("Number of sample-specimen combinations (given year(s)): ", nrow(df_nb_biota_samplespec), "\n")
  
  # Get samples
  df_nb_biota_samples1 <- get_nivabase_selection("SAMPLE_ID, TISSUE_ID, SPECIES_ID, SAMPLE_NO, REPNO",
                                                 "BIOTA_SAMPLES",
                                                 "SAMPLE_ID",
                                                 unique(df_nb_biota_samplespec$SAMPLE_ID))
  cat("Number of samples: ", nrow(df_nb_biota_samples1), "\n")
  
  # Summarize specimens for each sample (if >1 speciemen per sample)
  df_biota_specimens_3 <- df_nb_biota_samplespec %>%
    left_join(df_biota_specimens_2, by = "SPECIMEN_ID") %>%
    group_by(SAMPLE_ID, SPECIES_ID, STATION_ID, SAMPLE_POINT_ID, YEAR, TAXONOMY_CODE_ID) %>%
    summarize(across(.fns = ~paste(.x, collapse = ";")), .groups = "drop")
  
  df_nb_biota_samples2 <- df_nb_biota_samples1 %>%
    rename(SPECIES_ID_samp = SPECIES_ID) %>%
    left_join(df_biota_specimens_3, by = "SAMPLE_ID")
  
  if (nrow(df_nb_biota_samples2) != nrow(df_nb_biota_samples1)){
    txt <- paste(
      "df_nb_biota_samples1 has ", nrow(df_nb_biota_samples1), "rows \n",
      "df_nb_biota_samples2 has ", nrow(df_nb_biota_samples2), "rows \n",
      "It appears that SPECIES_ID, STATION_ID, SAMPLE_POINT_ID, YEAR, TAXONOMY_CODE_ID are not unique for each sample"
    )
    warning(txt)
  }
  
  df_nb_biota_tissue <- get_nivabase_selection("TISSUE_ID, TISSUE_NAME",
                                               "BIOTA_TISSUE_TYPES",
                                               "TISSUE_ID",
                                               unique(df_nb_biota_samples2$TISSUE_ID))
  cat("Number of tissues: ", nrow(df_nb_biota_tissue), "\n")
  
  df_nb_biota_chemvalues <- get_nivabase_selection("SAMPLE_ID, METHOD_ID, VALUE, FLAG1",
                                                   "BIOTA_CHEMISTRY_VALUES",
                                                   "SAMPLE_ID",
                                                   unique(df_nb_biota_samples1$SAMPLE_ID))
  cat("Number of chemical measurements: ", nrow(df_nb_biota_chemvalues), "\n")
  
  df_nb_biota_methods <- get_nivabase_selection("METHOD_ID, NAME, DESCR, UNIT, LABORATORY, CAS",
                                                "METHOD_DEFINITIONS",
                                                "METHOD_ID",
                                                unique(df_nb_biota_chemvalues$METHOD_ID)) %>%
    rename(PARAM = NAME)
  cat("Number of chemical methods: ", nrow(df_nb_biota_methods), "\n")
  
  result <- df_nb_biota_chemvalues %>%
    left_join(df_nb_biota_samples2, by = "SAMPLE_ID") %>%
    left_join(df_nb_biota_tissue, by = "TISSUE_ID") %>%
    left_join(df_nb_biota_methods, by = "METHOD_ID")
  
  cat("Number of rows in result: ", nrow(result), "\n")
  
  result
  
}

# TEST
if (FALSE){
  # debugonce(get_biota_chemistry_from_stations)
  get_biota_chemistry_from_stations(c(67413, 67066, 67067), 2019)
}


get_water_chemistry_from_stations <- function(station_id, year, station_metadata = NULL){
  
  # Get station metadata
  if (is.null(station_metadata)){
    station_metadata <- get_nivabase_selection("PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME", 
                                               "PROJECTS_STATIONS",
                                               "STATION_ID",
                                               station_id)
  }
  
  # WATER_SAMPLES - 1-3 records per station/year
  df_water <- get_nivabase_selection(
    "WATER_SAMPLE_ID, STATION_ID, SAMPLE_POINT_ID, SAMPLE_DATE, DEPTH1, DEPTH2", 
    "WATER_SAMPLES",
    "STATION_ID",
    station_id,
    extra_sql = paste(" and extract(YEAR from SAMPLE_DATE) = ", year)
  ) %>%
    left_join(station_metadata, by = c("STATION_ID"))
  
  # xtabs(~STATION_CODE + year(SAMPLE_DATE), df_water)   
  
  # V_WATER_CHEMISTRY_SAMPLES - up to 359 records per station/year
  df_measurements_water <- get_nivabase_selection(
    "WATER_SAMPLE_ID, VALUE, METHOD_ID, UNIT, NAME, LABORATORY, FLAG1, APPROVED, SAMPLE_DATE, DEPTH1, DEPTH2, STATION_ID, STATION_CODE, PROJECT_ID, PARAMETER", 
    "V_WATER_CHEMISTRY_SAMPLES",
    "STATION_ID", 
    station_metadata$STATION_ID) %>%
    mutate(Year = year(SAMPLE_DATE)) %>%
    filter(Year %in% year) %>%
    left_join(station_metadata, by = c("STATION_ID", "STATION_CODE", "PROJECT_ID"))
  
  # xtabs(~STATION_CODE + Year, df_waterchem)
  
  # Water samples from Nivabasen ----\
  df_samples_water <- df_measurements_water %>% 
    filter(Year == year) %>%
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
      " and extract(YEAR from SAMPLED_DATE) = ", year,
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
  df_samples_water <- full_join(
    df_samples_water,
    df_samples_labware_water, 
    by = c("STATION_CODE" = "AQUAMONITOR_CODE", "Samplenumber" = "Samplenumber"))
  
  # Checks!
  if (nrow(df_samples_water) != nrow(df_samples_water)){
    stop("nrow(final samples) != nrow(samples from nivabase)!")
  }
  if (nrow(df_samples_water) != nrow(df_samples_labware_water)){
    stop("nrow(final samples) != nrow(samples from labware)!")
  }
  
  df_samples_water
  
}

# get_nivabase_water


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
    "WATER_SAMPLE_ID, VALUE, METHOD_ID, UNIT, NAME, LABORATORY, FLAG1, APPROVED, SAMPLE_DATE, DEPTH1, DEPTH2, STATION_ID, STATION_CODE, PROJECT_ID, PARAMETER", 
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


get_sediment_chemistry_from_stations <- function(station_id, years, station_metadata = NULL){
  
  # Get station metadata
  if (is.null(station_metadata)){
    station_metadata <- get_nivabase_selection("PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME", 
                                               "PROJECTS_STATIONS",
                                               "STATION_ID",
                                               station_id)
  }
  if (nrow(station_metadata) == 0)
    stop("No stations found!")
  
  year_string <- paste0("(", paste(years, collapse = ","), ")")
  
  # SEDIMENT_SAMPLES - 1-3 records per station/year
  df_sed_samples <- get_nivabase_selection(
    "SAMPLE_ID, STATION_ID, SAMPLE_POINT_ID, SAMPLE_DATE, TOTAL_CORE_DEPTH, WATER_DEPTH", 
    "SEDIMENT_SAMPLES",
    "STATION_ID", 
    station_id,
    extra_sql = paste(" and extract(YEAR from SAMPLE_DATE) in", year_string)
  ) %>%
    mutate(Year = year(SAMPLE_DATE)) %>%
    left_join(station_metadata, by = c("STATION_ID"))
  
  
  if (nrow(df_sed_samples) == 0){
    
    message("No sediment samples found!")
    df_measurements_sediment <- NULL
    
  } else {
    
    # xtabs(~STATION_CODE + Year, df_sed_samples) 
    
    # SEDIMENT_SLICES
    df_sed_slice <- get_nivabase_selection(
      "SLICE_ID, SAMPLE_ID, DEPTH1, DEPTH2", 
      "SEDIMENT_SLICES",
      "SAMPLE_ID", 
      df_sed_samples$SAMPLE_ID)
    
    
    # SEDIMENT_CHEMISTRY_VALUES
    df_sed_values <- get_nivabase_selection(
      "*", 
      "SEDIMENT_CHEMISTRY_VALUES",
      "SLICE_ID", 
      df_sed_slice$SLICE_ID) 
    
    # Preliminary result without Labware columns  
    df_measurements_sediment_1 <- df_sed_samples %>%
      left_join(df_sed_slice, by = "SAMPLE_ID") %>%
      right_join(df_sed_values, by = "SLICE_ID")
    
    # Check!
    if (nrow(df_measurements_sediment_1) != nrow(df_sed_values)){
      stop("nrow(final measurements) != nrow(measurements from SEDIMENT_CHEMISTRY_VALUES)!")
    }
    
    # Samples from Nivabase (only for 'Samplenumber')
    df_samples_sediment <- df_measurements_sediment_1 %>% 
      count(STATION_CODE, SAMPLE_ID, SLICE_ID, name = "n_nivabasen") %>%
      # Order records for making "Samplenumber"
      arrange(STATION_CODE, SAMPLE_ID, SLICE_ID) %>%
      # Add self-made "Samplenumber"- needed for join below
      group_by(STATION_CODE) %>%
      mutate(Samplenumber = seq_len(length(STATION_CODE))) %>%
      ungroup() 
    
    # Sediment measurements from Labware ---
    df_measurements_labware_sediment <- get_nivabase_selection(
      "AQUAMONITOR_CODE, SAMPLE_TYPE, SAMPLE_NUMBER, DESCRIPTION, REPORTED_NAME, ENTRY_QUALIFIER, NUMERIC_ENTRY", 
      "LABWARE_IMPORT",
      "AQUAMONITOR_ID", 
      station_id,
      extra_sql = paste(
        " and extract(YEAR from SAMPLED_DATE) in", year_string,
        " and SAMPLE_TYPE in ('SEDIMENT', 'VANN')")
    ) %>%
      mutate(
        SAMPLE_TYPE = sub("<d8>", "Ø", SAMPLE_TYPE)
      )
    
    # Sediment samples from Labware ---
    df_samples_labware_sediment <- df_measurements_labware_sediment %>%
      count(AQUAMONITOR_CODE, SAMPLE_TYPE, SAMPLE_NUMBER, DESCRIPTION, 
            name = "n_labware") %>%
      # Order records for making "Samplenumber"
      arrange(AQUAMONITOR_CODE, SAMPLE_NUMBER) %>%
      # Add self-made "Samplenumber" 
      group_by(AQUAMONITOR_CODE) %>%
      mutate(Samplenumber = seq_len(length(AQUAMONITOR_CODE))) %>%
      ungroup()
    
    # Joining
    df_measurements_sediment <- df_measurements_sediment_1 %>%
      # Add "Samplenumber" (for joining with 'df_samples_labware_water') :      
      left_join(
        df_samples_sediment %>% select(STATION_CODE, SAMPLE_ID, SLICE_ID, Samplenumber),
        by = c("STATION_CODE", "SAMPLE_ID", "SLICE_ID")) %>%
      # Add "SAMPLE_TYPE", "SAMPLE_NUMBER", "DESCRIPTION" and "n_labware" from Labware:      
      left_join(
        df_samples_labware_sediment,
        by = c("STATION_CODE" = "AQUAMONITOR_CODE", "Samplenumber" = "Samplenumber"))
    
    # Check!
    if (nrow(df_measurements_sediment_1) != nrow(df_measurements_sediment)){
      stop("nrow(final measurements) changed after join!")
    }
    
  }  # if sediment samples found
  
  df_measurements_sediment
  
}




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for getting biota data from existing files----  
#
# For a given sample 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Excel file
get_biotadata_excel <- function(station, species, tissue, sampleno){
  df <- df_excel %>%
    left_join(samples_all %>%
                select(`NIVA-no.`, STATION_CODE, STATION_NAME, LATIN_NAME, TISSUE_NAME, BIOTA_SAMPLENO) %>%
                rename(SAMPLE_NO = BIOTA_SAMPLENO), 
              by = "NIVA-no."
    )
  df %>%
    filter(STATION_CODE %in% station & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & SAMPLE_NO %in% sampleno)
}
# Test
# get_biotadata_excel("IO", "Gadus morhua", "Lever", 1) %>% nrow() # 144

# LABWARE_IMPORT
get_biotadata_labware <- function(station, species, tissue, sampleno){
  df <- df_labware %>%
    left_join(samples_all %>%
                select(TEXT_ID, STATION_CODE, STATION_NAME, LATIN_NAME, TISSUE_NAME, BIOTA_SAMPLENO) %>%
                rename(SAMPLE_NO = BIOTA_SAMPLENO), 
              by = "TEXT_ID"
    )
  df %>%
    filter(STATION_CODE %in% station & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & SAMPLE_NO %in% sampleno)
}
# Test
# get_biotadata_labware("IO", "Gadus morhua", "Lever", 1) %>% nrow()  # 42

# NIVADATABASE
get_biotadata_nivadatabase <- function(station, species, tissue, sampleno){
  df_nivabase_biota %>%
    filter(STATION_CODE %in% station & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & SAMPLE_NO %in% sampleno)
}
# Test
# get_biotadata_nivadatabase("IO", "Gadus morhua", "Lever", 1) %>% nrow()  # 41


# Change ASCII code in server output to Norwegian letters
# Example: replace "<e6>" with "ø"
# From 'R selfmade functions'  

asciicode_to_norwegian <- function(txt){
  search <- c("<e6>", "<f8>", "<e5>", "<c6>", "<d8>", "<c5>")
  replace <- c("æ","ø","å","Æ","Ø","Å")
  for (i in seq_along(search)){
    txt <- sub(search[i], replace[i], txt)
  }
  txt
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# READING "WIDE" AQUAMONITOR DATA ----
#
# - Puts the data on 'long' format (two variables named Substance and Value)
# - Additional variables for Unit and Flag ("<" for values less than LOQ)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

read_broad_chemistry_table <- function(...,
                                       rows,
                                       number_of_key_cols = 12,
                                       number_of_total_cols = 183){
  
  # Read key columns (leftmost columns)
  # Read column names as well (col_names = TRUE)
  df_key <- read_excel(...,
                       col_names = TRUE, col_types = "text", 
                       range = cell_limits(c(min(rows), 1), 
                                           c(max(rows), number_of_key_cols))
  )
  df_key <- df_key[-1,]
  # Read substance names (row 1)
  # End cell = c(1, NA), where NA means number of columns is not set 
  df_names <- read_excel(...,
                         col_names = FALSE, col_types = "text",
                         range = cell_limits(c(min(rows), number_of_key_cols + 1), 
                                             c(min(rows), number_of_total_cols - number_of_key_cols))
  )
  # Set 'proper' column names "Substance_1", "Substance_2" etc.
  column_names <- paste0("Substance_", 1:ncol(df_names))
  names(df_names) <- column_names
  
  # Read units (row 2)
  # Number of columns is set to be the same as df_names 
  df_units <- read_excel(...,
                         col_names = names(df_names), col_types = "text", 
                         range = cell_limits(c(min(rows) + 1, number_of_key_cols+1), 
                                             c(min(rows) + 1, number_of_total_cols - number_of_key_cols))
  )
  
  # Read measurement data
  # All data are read as text (strings) and later converted, due to the "<" signs
  # Number of rows is set to be the same as df_keys 
  # Number of columns is set to be the same as df_names 
  df_chem <- read_excel(...,
                        col_names = names(df_names), col_types = "text", 
                        range = cell_limits(c(min(rows) + 2, number_of_key_cols + 1), 
                                            c(max(rows), number_of_total_cols - number_of_key_cols))
  )
  
  # Combine key variables and data, and put them on long format  
  data <- bind_cols(df_key, df_chem) %>%
    tidyr::pivot_longer(-seq(1,ncol(df_key)), names_to = "Temporary_name", values_to = "Value_chr")
  # Make look-up table for substance names
  X <- as.data.frame(df_names)
  lookup_names <- data.frame(
    Temporary_name = names(df_names),
    Substance = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  X <- as.data.frame(df_units)
  lookup_units <- data.frame(
    Temporary_name = names(df_units),
    Unit = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  # Add columns "Substance" and "Unit" to the data using the look-up tables
  data <- data %>% 
    filter(!is.na(Value_chr)) %>%
    left_join(lookup_names, by = "Temporary_name") %>%
    left_join(lookup_units, by = "Temporary_name")
  
  # Make numeric data values
  x <- sub(",", ".", data$Value_chr, fixed = TRUE)
  x <- sub(",", ".", x, fixed = TRUE)
  data$Value <- as.numeric(sub("<", "", x))
  
  # Make less-than flag
  data$Flag <- ifelse(grepl("<", data$Value_chr, fixed = TRUE), "<", NA)
  
  # Return the data, without the 'Temporary_name' column  
  data %>% select(-Temporary_name)
  
  # Combine key variables and data, and put them on long format  
  data <- bind_cols(df_key, df_chem) %>%
    tidyr::pivot_longer(-seq(1, ncol(df_key)), names_to = "Temporary_name", values_to = "Value_chr")
  
  # Make look-up table for substance names and units
  X <- as.data.frame(df_names)
  lookup_names <- data.frame(
    Temporary_name = names(df_names),
    Substance = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  X <- as.data.frame(df_units)
  lookup_units <- data.frame(
    Temporary_name = names(df_units),
    Unit = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  
  # Add columns "Substance" and "Unit" to the data using the look-up tables
  data <- data %>% 
    filter(!is.na(Value_chr)) %>%
    left_join(lookup_names, by = "Temporary_name") %>%
    left_join(lookup_units, by = "Temporary_name")
  
  # Make numeric data values
  x <- sub(",", ".", data$Value_chr, fixed = TRUE)
  x <- sub(",", ".", x, fixed = TRUE)
  data$Value <- as.numeric(sub("<", "", x))
  
  # Make less-than flag
  data$Flag <- ifelse(grepl("<", data$Value_chr, fixed = TRUE), "<", NA)
  
  # Return the data, without the 'Temporary_name' column  
  data %>% select(-Temporary_name)
  
}

# For testing
if (FALSE){
  df1 <- read_broad_chemistry_table(
    path = "Data_Urbanfjord/ElectronicAppendix2019.xlsx", 
    sheet = "Electronic Appendix",
    rows = 3:22)
}


old <- function(){  
  if (sheetname == "WaterChemistry"){ 
    number_of_key_cols <- 8
    numeric_vars <- c("ProjectId", "StationId", "Depth1", "Depth2")
    time_vars <- "SampleDate"
  } else if (sheetname == "SedimentChemistry"){ 
    number_of_key_cols <- 9
    numeric_vars <- c("ProjectId", "StationId", "SampleTag", "Depth1", "Depth2")
    time_vars <- "SampleDate"
  } else if (sheetname == "BiotaChemistry"){
    number_of_key_cols <- 12
    numeric_vars <- c("ProjectId", "StationId", "SampleId", "ReplicateNo")
    time_vars <- c("CatchDateFirst", "CatchDateLast", "SampleDate")
  }
  # Read key columns (leftmost columns)
  # Read column names as well (col_names = TRUE)
  df_key <- read_excel(
    fn, sheet = sheetname, col_names = TRUE, col_types = "text", 
    range = cell_limits(c(2,NA), c(NA, number_of_key_cols))
  )
  # Read substance names (row 1)
  # End cell = c(1, NA), where NA means number of columns is not set 
  df_names <- read_excel(
    fn, sheet = sheetname, col_names = FALSE, col_types = "text", 
    range = cell_limits(c(1, number_of_key_cols+1), c(1, NA))
  )
  # Set 'proper' column names "Substance_1", "Substance_2" etc.
  column_names <- paste0("Substance_", 1:ncol(df_names))
  names(df_names) <- column_names
  # Column names 
  # Read units (row 2)
  # Number of columns is set to be the same as df_names 
  df_units <- read_excel(
    fn, sheet = sheetname, col_names = names(df_names), col_types = "text", 
    range = cell_limits(c(2, number_of_key_cols+1), c(2, number_of_key_cols + ncol(df_names)))
  )
  # Read measurement data
  # All data are read as text (strings) and later converted, due to the "<" signs
  # Number of rows is set to be the same as df_keys 
  # Number of columns is set to be the same as df_names 
  df_chem <- read_excel(
    fn, sheet = sheetname, col_names = names(df_names), col_types = "text", 
    range = cell_limits(c(3, number_of_key_cols+1), c(nrow(df_key) + 2, number_of_key_cols + ncol(df_names)))
  )
  for (col in numeric_vars){
    df_key[[col]] <- as.numeric(df_key[[col]])
  }
  # df_key: convert these variables to time
  for (col in time_vars){
    df_key[[col]] <- lubridate::dmy_hms(df_key[[col]])
  }
  # Combine key variables and data, and put them on long format  
  data <- bind_cols(df_key, df_chem) %>%
    tidyr::pivot_longer(-seq(1,ncol(df_key)), names_to = "Temporary_name", values_to = "Value_chr")
  # Make look-up table for substance names
  X <- as.data.frame(df_names)
  lookup_names <- data.frame(
    Temporary_name = names(df_names),
    Substance = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  X <- as.data.frame(df_units)
  lookup_units <- data.frame(
    Temporary_name = names(df_units),
    Unit = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  # Add columns "Substance" and "Unit" to the data using the look-up tables
  data <- data %>% 
    filter(!is.na(Value_chr)) %>%
    left_join(lookup_names, by = "Temporary_name") %>%
    left_join(lookup_units, by = "Temporary_name")
  
  # Make numeric data values
  x <- sub(",", ".", data$Value_chr, fixed = TRUE)
  x <- sub(",", ".", x, fixed = TRUE)
  data$Value <- as.numeric(sub("<", "", x))
  
  # Make less-than flag
  data$Flag <- ifelse(grepl("<", data$Value_chr, fixed = TRUE), "<", NA)
  
  # Return the data, without the 'Temporary_name' column  
  data %>% select(-Temporary_name)
}


convert_param_to_nivabase <- function(Substance){
  case_when(
    nchar(Substance) == 2 ~ toupper(Substance),   # Metals
    substr(Substance,1,4) == "PCB-" ~ sub("PCB-", "CB", Substance, fixed = TRUE),
    substr(Substance,1,4) == "BDE-" ~ sub("BDE-", "BDE", Substance, fixed = TRUE),
    substr(Substance,1,4) == "4-n-" ~ toupper(Substance),
    substr(Substance,1,4) == "4-t-" ~ toupper(Substance),
    Substance == "TTS" ~ "DRYWT%",
    Substance == "BD183" ~ "BDE183",
    Substance == "a-HBCD" ~ "HBCDA",
    Substance == "b-HBCD" ~ "HBCDB",
    Substance == "g-HBCD" ~ "HBCDG",
    Substance == "OCS" ~ "Oktaklorstyren (OCS)",
    Substance == "MBT" ~ "Monobutyltinn (MBT)-Sn",
    Substance == "DBT" ~ "Dibutyltinn-Sn (DBT-Sn)",
    Substance == "TPhT" ~ "Trifenyltinn (TPhT)-Sn",
    Substance == "TTBT" ~ "Tetrabutyltinn (TTBT)-Sn",
    Substance == "4-nonylfenol" ~ "4-N-NP",            # see plot in Appendix ('Check nonylphenols')
    Substance == "PFBA" ~ "Perfluorbutansyre (PFBA)",
    Substance == "PFDoA" ~ "Perfluordodekansyre (PFDoA)",
    Substance == "PFDS" ~ "Perfluordekansulfonat (PFDS)",
    Substance == "PFPeA" ~ "Perfluorpentansyre (PFPeA)",
    Substance == "PFTA" ~ "Perfluortetradekansyre (PFTA)",
    Substance == "PFTrDA" ~ "Perfluortridekansyre (PFTrA)",
    Substance == "PFHpS" ~ "Perfluorheptansulfonat (PFHpS)",
    Substance == "Sum PFC inkl. LOQ" ~ "Sum PFC forbindelser inkl. LOQ",
    Substance == "6:2 FTS" ~ "6:2 Fluortelomersulfonat (FTS, H4PFOS)",
    Substance == "HPFHpA" ~ "7H-dodekafluorheptansyre (HPFHpA)",
    Substance == "PF37DMOA" ~ "Perfluor-3,7-dimetyloktansyre (PF37DMOA)",
    Substance == "Sum PFOS/PFOA inkl. LOQ" ~ "Total PFOS/PFOA inkl. LOQ",
    Substance == "PFDCA" ~ "PFDcA",
    Substance == "PFHXS" ~ "PFHxS",
    Substance == "Trifenylfosfat (TPhP)" ~ "TPhP",
    TRUE ~ Substance
  )
}
convert_param_to_nivabase("PCB-118")
convert_param_to_nivabase("BDE-196")
