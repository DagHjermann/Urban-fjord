
# 110_Data_from_excel_2020-2021_functions.R  

# Copied from 
# https://raw.githubusercontent.com/NIVANorge/Milkys2_pc/master/812_Import_til_NIVAbasen_NILU_functions.R
# Except that 'Generic data tables' are commented out  

library(dplyr)
library(tidyr)
library(purrr)
library(safejoin)   # https://github.com/moodymudskipper/safejoin

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Generic data tables ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# df_tissue <- readxl::read_excel("Files_to_Jupyterhub_2020/Lookup_table_Nivabasen_tissue.xlsx") 
# df_species <- readxl::read_excel("Files_to_Jupyterhub_2020/Lookup_table_Nivabasen_species.xlsx") 
# df_taxon <- readxl::read_excel("Files_to_Jupyterhub_2020/Lookup_table_Nivabasen_taxon.xlsx") 
# df_taxoncodes <- readxl::read_excel("Files_to_Jupyterhub_2020/Lookup_table_Nivabasen_taxoncodes.xlsx") 

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Species functions ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Creates a one-frame lookup table 
# Meant for only a single species input!
latin_to_taxid <- function(latin_name){
  df <- df_taxon %>% filter(LATIN_NAME %in% latin_name)
  x <- df_taxoncodes %>% filter(NIVA_TAXON_ID %in% df$NIVA_TAXON_ID) %>% pull(TAXONOMY_CODE_ID)
  tibble(LATIN_NAME = latin_name, TAXONOMY_CODE_ID = x)
}

# Creates a one-frame lookup table 
# Meant for only a single-number input!
taxid_to_latin <- function(taxonomy_code_id){
  df <- df_taxoncodes %>% filter(TAXONOMY_CODE_ID %in% taxonomy_code_id)
  x <- df_taxon %>% filter(NIVA_TAXON_ID %in% df$NIVA_TAXON_ID) %>% pull(LATIN_NAME)
  tibble(TAXONOMY_CODE_ID = taxonomy_code_id, LATIN_NAME = x)
}

# Tests
# latin_to_taxid("Gadus morhua")
# c("Gadus morhua", "Clupea harengus") %>% map_df(latin_to_taxid)
# taxid_to_latin(8850)
# c(8849,8850) %>% map_df(taxid_to_latin)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for creating SQL ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


make_sql_single_specimen <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SINGLE_SPECIMENS ",
                "(DATE_CAUGHT, STATION_ID, TAXONOMY_CODE_ID, SPECIMEN_NO)\n",  # \n for line shift
                "values (",
                "TO_DATE(", sQuote(df[i, 'DATE_CAUGHT']), ", 'YYYY-MM-DD'), ",
                df[i, 'STATION_ID'], ", ",
                df[i, 'TAXONOMY_CODE_ID'], ", ",
                df[i, 'SPECIMEN_NO'],
                ")"
  )
  options(original_options)
  txt
}

# make_sql_single_specimen(1, biota_single_specimens_eider)
# make_sql_single_specimen(2, biota_single_specimens_eider)



make_sql_sample <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SAMPLES ",
                "(STATION_ID, TISSUE_ID, REPNO, TAXONOMY_CODE_ID, SAMPLE_DATE, SAMPLE_NO)\n",  # \n for line shift
                "values (",
                df[i, 'STATION_ID'], ", ",
                df[i, 'TISSUE_ID'], ", ",
                df[i, 'REPNO'], ", ",
                df[i, 'TAXONOMY_CODE_ID'], ", ",
                "TO_DATE(", sQuote(df[i, 'SAMPLE_DATE']), ", 'YYYY-MM-DD'), ",
                df[i, 'SAMPLE_NO'],
                ")"
  )
  options(original_options)
  txt
}


make_sql_samples_specimens <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SAMPLES_SPECIMENS ",
                "(SAMPLE_ID, SPECIMEN_ID)\n",  # \n for line shift
                "values (",
                df[i, 'SAMPLE_ID'], ", ",
                df[i, 'SPECIMEN_ID'],
                ")"
  )
  options(original_options)
  txt
}

# Test
# make_sql_samples_specimens(1, biota_sample_specimens_eider)


#
# BIOTA_CHEMISTRY_VALUES
#

# "VALUE_ID"              - Let the database decide
# "SAMPLE_ID"             - From the database, after BIOTA_SAMPLES have been inserted
# "METHOD_ID"             - Lookup based on NAME and UNIT
# "VALUE"                 - From data
# "FLAG1"                 - From data
# "FLAG2"                 - NA
# "ENTERED_BY"            - DHJ
# "ENTERED_DATE"          - date, see above
# "REMARK"                - NA
# "DETECTION_LIMIT"       - NA
# "UNCERTAINTY"           - NA
# "QUANTIFICATION_LIMIT"  - NA
# "APPROVED"              - NA?


make_sql_chemistry_values <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  flag <- df[i, 'FLAG1']
  txt <- paste0("insert into NIVADATABASE.BIOTA_CHEMISTRY_VALUES ",
                "(SAMPLE_ID, METHOD_ID, VALUE, FLAG1, APPROVED)\n",  # \n for line shift
                "values (",
                df[i, 'SAMPLE_ID'], ", ",
                df[i, 'METHOD_ID'], ", ",
                round(df[i, 'VALUE'], 6), ", ",
                ifelse(is.na(flag), "NULL", sQuote(flag)), ", ",
                1,
                ")"
  )
  options(original_options)
  txt
}
# Test
# make_sql_chemistry_values(1, biota_chemistry_values_eider)

#
# For "select all" - NOT FINISHED!
#
# make_sql_chemistry_values_intoall <- function(lines, data){
#   
#   df <- as.data.frame(data)
#   data_section <- make_sql_chemistry_values_single <- function( data)
#   
#   original_options <- options(useFancyQuotes = FALSE)
#   txt <- paste0("insert all\n",
#                 data_section,
#                 "select 1 from dual"
#   )
#   options(original_options)
#   txt
# }

#
# For "select all" - NOT FINISHED!
#

# make_sql_chemistry_values_single <- function(i, data){
#   
#   df <- as.data.frame(data)
#   
#   original_options <- options(useFancyQuotes = FALSE)
#   
#   flag <- round(df[i, 'FLAG1'], 6)
#   txt <- paste0("    into NIVADATABASE.BIOTA_CHEMISTRY_VALUES ",
#                 "    (SAMPLE_ID, METHOD_ID, VALUE, FLAG1,APPROVED)\n",  # \n for line shift
#                 "    values (",
#                 df[i, 'SAMPLE_ID'], ", ",
#                 df[i, 'METHOD_ID'], ", ",
#                 round(df[i, 'VALUE'], 6), ", ",
#                 ifelse(is.na(flag), "NULL", sQuote(flag)),
#                 1,
#                 ")"
#   )
#   options(original_options)
#   txt
# }



make_sql_methods <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  name <- df[i, 'NAME']
  unit <- df[i, 'UNIT']
  lab <- df[i, 'LABORATORY']
  method_ref <- df[i, 'METHOD_REF']
  matrix <- df[i, 'MATRIX']
  cas <- df[i, 'CAS']
  txt <- paste0(
    "insert into NIVADATABASE.METHOD_DEFINITIONS ",
    "(NAME, UNIT, LABORATORY, METHOD_REF, MATRIX, CAS, MATRIX_ID)\n",  # \n for line shift
    "values (",
    ifelse(is.na(name), "NULL", sQuote(name)), ", ",
    ifelse(is.na(unit), "NULL", sQuote(unit)), ", ",
    ifelse(is.na(lab), "NULL", sQuote(lab)), ", ",
    ifelse(is.na(method_ref), "NULL", sQuote(method_ref)), ", ",
    ifelse(is.na(matrix), "NULL", sQuote(matrix)), ", ",
    ifelse(is.na(cas), "NULL", sQuote(cas)), ", ",
    df[i, 'MATRIX_ID'],
    ")"
  )
  options(original_options)
  txt
}

# See script 75:
# make_sql_methods(1, new_methods)


#
# Helper functions for making SQL parts
#
# Takes the unique values of a variable and puts them in a bracket (sql style)
# 
#

make_sql_ids <- function(data, variable){
  values <- data[[variable]] %>% unique()
  if (class(data[[variable]]) == "character"){
    original_options <- options(useFancyQuotes = FALSE)
    values <- sQuote(values)
    options(original_options)
  }
  paste0("(",
         values %>% paste(collapse = ","),
         ")")
}

# make_sql_ids(biota_samples, "STATION_ID")      
# "(46980,47221,50478,67807,69711)"
#
# make_sql_ids(biota_chemistry_values, "FLAG1")
# "('<','NA')"


#
# BIOTA_CHEMISTRY_VALUES
#

# "VALUE_ID"              - Let the database decide
# "SAMPLE_ID"             - From the database, after BIOTA_SAMPLES have been inserted
# "METHOD_ID"             - Lookup based on NAME and UNIT
# "VALUE"                 - From data
# "FLAG1"                 - From data
# "FLAG2"                 - NA
# "ENTERED_BY"            - DHJ
# "ENTERED_DATE"          - date, see above
# "REMARK"                - NA
# "DETECTION_LIMIT"       - NA
# "UNCERTAINTY"           - NA
# "QUANTIFICATION_LIMIT"  - NA
# "APPROVED"              - NA?

# Table
# WATER_CHEMISTRY_VALUES
#
# Columns
# WATER_SAMPLE_ID
# METHOD_ID
# VALUE
# FLAG1
# DETECTION_LIMIT QUANTIFICATION_LIMIT
# UNCERTAINTY


make_sql_waterchemistry_values <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  
  flag <- df[i, 'FLAG1']
  txt <- paste0("insert into NIVADATABASE.WATER_CHEMISTRY_VALUES ",
                "(WATER_SAMPLE_ID, METHOD_ID, VALUE, FLAG1, APPROVED)\n",  # \n for line shift
                "values (",
                df[i, 'WATER_SAMPLE_ID'], ", ",
                df[i, 'METHOD_ID'], ", ",
                round(df[i, 'VALUE'], 6), ", ",
                ifelse(is.na(flag), "NULL", sQuote(flag)), ", ",
                1,
                ")"
  )
  options(original_options)
  txt
}
# Test
# make_sql_waterchemistry_values(1, biota_chemistry_values_eider)



# Table
# SEDIMENT_CHEMISTRY_VALUES
#
# Columns
# SLICE_ID
# METHOD_ID
# MATRIX - e.g. NS
# FRACTION_SIZE - typically empty for surface grab chemistry
# VALUE
# FLAG1
# DETECTION_LIMIT 
# QUANTIFICATION_LIMIT
# UNCERTAINTY

make_sql_sedimentchemistry_values <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  flag <- df[i, 'FLAG1']
  txt <- paste0("insert into NIVADATABASE.SEDIMENT_CHEMISTRY_VALUES ",
                "(SLICE_ID, METHOD_ID, MATRIX, VALUE, FLAG1, APPROVED)\n",  # \n for line shift
                "values (",
                df[i, 'SLICE_ID'], ", ",
                df[i, 'METHOD_ID'], ", ",
                sQuote(df[i, 'MATRIX']), ", ",
                round(df[i, 'VALUE'], 6), ", ",
                ifelse(is.na(flag), "NULL", sQuote(flag)), ", ",
                1,
                ")"
  )
  options(original_options)
  txt
}
# Test
# make_sql_sedimentchemistry_values(1, df_nilu_03_sed_02)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for creating BIOTA_SAMPLES_SPECIMENS ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# NOTE:  
#  * Works only for a single station/species!! (but for several tissues)  
#    For several stations, you can use map() and map2(); see script 73 part 11d
#  * Assumes that data for BIOTA_SINGLE_SPECIMENS and BIOTA_SAMPLES has already been made 
#  * ...and also that we have entered these into the database and retrieved the correct SPECIMEN_ID and SAMPLE_ID  
#  * Input data for add_ids is 'sampletable', which is a table with one row per fish 
#    (with a uniquely numbered column named "SPECIMEN_NO" included)  
# * Furthermore, 'sampletable' must have has one column per tissue, names equal to tissue names or defined in 'columnnames'
# * I.e., in add_ids(), argument 'columnnames' is optional. If column names = TISSUE_NAME names (e.g. "Muskel"), you 
#      don't have to specify this lookup table. See script 70 for use of 'columnnames' and script 71 for an example where it is not used.

#
# Example of data set defining column names used in data 
#
# NOTE: 'data' in Must have column names "TISSUE_NAME", "Colname"
# UNCOMMENT NEXT LINES TO USE:
# columnnames_tissue <- read.table(textConnection("
# TISSUE_NAME        Colname
#      Muskel  Sample_muscle
#       Lever   Sample_liver
# "), header = TRUE, stringsAsFactors = FALSE)


# Input:
#   tissue name: one TISSUE_NAME    
#   biota_samples: one line per physical sample, 
#     columns SPECIMEN_NO, SAMPLE_NO, TISSUE_NAME
# Output:
#   Data frame with one line per physical sample for that tissue
#   2 columns: 1) SAMPLE_NO,  2) column named after tissue
get_tissue_samples <- function(tissue_name, biota_samples){
  result <- biota_samples %>% 
    filter(TISSUE_NAME == tissue_name) %>% 
    select(SAMPLE_NO, SAMPLE_ID) %>% 
    as.data.frame()
  colnames(result)[2] <- tissue_name
  result
}


# Test 1
# get_tissue_samples(23, ex_BIOTA_SAMPLES)
# Test 2
# tissuesamples_list <- c("Muskel", "Lever) %>% map(get_tissue_samples, ex_BIOTA_SAMPLES)


# Inputs:
#    'sampletable': a table with one line per specimen and one column per tissue  
#       The columns must either have names identical to TISSUE_NAME, or one
#       must supply a lookup table 'columnnames' (see example above)
#    single_specimens: has one line per specimen, columns SPECIMEN_ID, SPECIMEN_NO
#    tissuesamples: is a list of data frames, one per tissue type
#    tissues: character vector with the names of the tissues(in same order as tissuesamples)
#    columnnames: lookup table for column names
add_ids <- function(sampletable, 
                    single_specimens, 
                    tissuesamples, 
                    tissues,
                    columnnames = NULL){
  if (is.null(columnnames)){
    result <- sampletable[c("SPECIMEN_NO", tissues)]
  } else {
    colnames <- tibble(TISSUE_NAME = tissues) %>% 
      safe_left_join(columnnames) %>%
      pull(Colname)
    result <- sampletable[c("SPECIMEN_NO", colnames)]
  }
  result <- result %>%
    safe_left_join(single_specimens[c("SPECIMEN_ID", "SPECIMEN_NO")], by = "SPECIMEN_NO", check = "v")
  for (tissuesample in tissuesamples){
    tissue <- colnames(tissuesample)[2]
    if (is.null(columnnames)){
      colname <- tissue
    } else {
      colname <- subset(columnnames, TISSUE_NAME == tissue)$Colname
    }
    colnames(result)[colnames(result) == colname] <- "SAMPLE_NO"
    result <- result %>%
      left_join(tissuesample, by = "SAMPLE_NO") %>%
      select(-SAMPLE_NO)    # remove SAMPLE_NO, otherwise there will be confusion when the loop is in round 2
  }
  result
}
# Test
# id_table <- add_ids(ex_data, ex_BIOTA_SINGLE_SPECIMENS, tissuesamples_list)

gather_ids <- function(fish_to_id_table, tissues){
  df <- fish_to_id_table[c("SPECIMEN_ID", tissues)] 
  df %>% tidyr::gather("TISSUE_NAME", "SAMPLE_ID", -SPECIMEN_ID) %>%
    filter(!is.na(SAMPLE_ID)) %>%
    arrange(SPECIMEN_ID, SAMPLE_ID) 
}

# Test
# gather_ids(id_table)


#
# Functions for reading from database
#

# TEST
sql_test <- "
select s.STATION_CODE, a.STATION_ID, a.SPECIMEN_ID, a.SPECIMEN_NO, a.DATE_CAUGHT, a.TAXONOMY_CODE_ID, 
b.SAMPLE_ID, c.TISSUE_ID, 
d.METHOD_ID, d.VALUE, d.FLAG1,
e.NAME, e.UNIT, e.LABORATORY
from NIVADATABASE.PROJECTS_STATIONS s
join NIVADATABASE.BIOTA_SINGLE_SPECIMENS a on s.STATION_ID = a.STATION_ID
join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS b on a.SPECIMEN_ID = b.SPECIMEN_ID
join NIVADATABASE.BIOTA_SAMPLES c on b.SAMPLE_ID = c.SAMPLE_ID
join NIVADATABASE.BIOTA_CHEMISTRY_VALUES d on c.SAMPLE_ID = d.SAMPLE_ID
left join NIVADATABASE.METHOD_DEFINITIONS e on d.METHOD_ID = e.METHOD_ID
where s.STATION_CODE = '19N'
and DATE_CAUGHT > TO_DATE('2017-03-01', 'YYYY-MM-DD')
and DATE_CAUGHT < TO_DATE('2018-03-01', 'YYYY-MM-DD')"

sql_get_biota_chemistry_station <- "
select s.STATION_CODE, a.STATION_ID, a.SPECIMEN_ID, a.SPECIMEN_NO, a.DATE_CAUGHT, a.TAXONOMY_CODE_ID, 
b.SAMPLE_ID, c.TISSUE_ID, 
d.METHOD_ID, d.VALUE, d.FLAG1,
e.NAME, e.UNIT, e.LABORATORY
from NIVADATABASE.PROJECTS_STATIONS s
join NIVADATABASE.BIOTA_SINGLE_SPECIMENS a on s.STATION_ID = a.STATION_ID
join NIVADATABASE.BIOTA_SAMPLES_SPECIMENS b on a.SPECIMEN_ID = b.SPECIMEN_ID
join NIVADATABASE.BIOTA_SAMPLES c on b.SAMPLE_ID = c.SAMPLE_ID
join NIVADATABASE.BIOTA_CHEMISTRY_VALUES d on c.SAMPLE_ID = d.SAMPLE_ID
left join NIVADATABASE.METHOD_DEFINITIONS e on d.METHOD_ID = e.METHOD_ID
where s.STATION_CODE = '%s'
and DATE_CAUGHT > TO_DATE('%i-03-01', 'YYYY-MM-DD')
and DATE_CAUGHT < TO_DATE('%i-03-01', 'YYYY-MM-DD')"

get_biota_chemistry_station <- function(station_code, myear){
  sql <- sprintf(sql_get_biota_chemistry_station, 
                 station_code, myear, myear + 1)
  df <- get_nivabase_data(sql) %>%
    rename(PARAM = NAME)
  # Get table with latin names
  df_latin <- df$TAXONOMY_CODE_ID %>% unique() %>% map_df(taxid_to_latin)
  df <- df %>%
    left_join(df_latin, by = "TAXONOMY_CODE_ID") %>%
    left_join(df_tissue, by = "TISSUE_ID") %>%
    select(STATION_CODE, DATE_CAUGHT, LATIN_NAME, SPECIMEN_NO, TISSUE_NAME, PARAM, VALUE, FLAG1, UNIT, 
           STATION_ID, SPECIMEN_ID, TAXONOMY_CODE_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, LABORATORY)
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for getting lookup data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Get rows from METHOD_DEFINITIONS (including 'METHOD_ID') fitting NAME, UNIT, LABORATORY, and MATRIX
# - NAME, UNIT are from a data frame
# - LABORATORY, MATRIX must be the same for all (if they are in the data frame, they will be ignored)

get_lookup_methodid <- function(df_parameters, lab, matrix){
  # First get all rows fitting NAME
  result_allunits <- get_nivabase_selection(
    "NAME, UNIT, LABORATORY, METHOD_ID, MATRIX",
    "METHOD_DEFINITIONS",
    "NAME",
    df_parameters$NAME, values_are_text = TRUE, 
    extra_sql = paste0("AND LABORATORY = ", sQuote(lab), "AND MATRIX = ", sQuote(matrix))
  )
  # result_allunits
  # Then remove rows not fitting UNIT (for each NAME)
  input_data <- df_parameters[c("NAME", "UNIT")]
  # Replace NA with an empty string ("")
  result_allunits$UNIT[is.na(result_allunits$UNIT)] <- ""
  input_data$UNIT[is.na(input_data$UNIT)] <- ""
  result <- result_allunits %>% 
    semi_join(input_data, by = join_by(NAME, UNIT))
  result
}

if (FALSE){
  # TEST
  df_params <- structure(list(
    NAME = c("HCBD", "PeCB", "HCB"), 
    UNIT = c("ng/g w.w.", "ng/g w.w.", "ng/g w.w.")
  ), row.names = c(NA, -3L), class = c("data.frame"))
  debugonce(get_lookup_methodid)
  test <- get_lookup_methodid(df_params, lab = 'NILU', matrix = 'BIOTA')
}

#
# Search for a given NAME, LABORATORY, MATRIX (returns all values regardless of UNIT)
# - if exact = TRUE, NAME can be several values  
# - if exact = FALSE, NAME can be only one value (searches within string, but is case sensitive)  
# - one or both of LABORATORY, MATRIX can be NA; if so, they are ignored  
# - possible improvement: case-insensitive search
#
search_lookup_methodid <- function(names, lab, matrix, exact = TRUE){
  extra_sql <- ""
  if (!is.na(lab)){
    extra_sql <- paste(extra_sql, "AND LABORATORY = ", sQuote(lab))
  }
  if (!is.na(matrix)){
    extra_sql <- paste(extra_sql, "AND MATRIX = ", sQuote(matrix))
  }
  if (exact){
  result_allunits <- get_nivabase_selection(
    "NAME, UNIT, LABORATORY, METHOD_ID, MATRIX",
    "METHOD_DEFINITIONS",
    "NAME",
    names, values_are_text = TRUE, 
    extra_sql = extra_sql
  )
  } else {
    if (length(names) > 1)
      stop("When exact = FALSE, 'names' must only be a single word/string")
    sql <- paste(
      "SELECT NAME, UNIT, LABORATORY, METHOD_ID, MATRIX",
      "FROM NIVADATABASE.METHOD_DEFINITIONS",
      "WHERE NAME like",
      sQuote(paste0("%", names, "%"))
    )
    sql <- paste(sql, extra_sql)
    result_allunits <- get_nivabase_data(sql)
  }
  result_allunits
}

if (FALSE){
  search_lookup_methodid("d13CVPDP", lab = 'IFE', matrix = 'BIOTA')
  search_lookup_methodid("d13C", lab = 'IFE', matrix = 'BIOTA', exact = FALSE)
  search_lookup_methodid("TOC", lab = 'NIVA', matrix = 'SEDIMENT', exact = FALSE)
  search_lookup_methodid("TOC", lab = NA, matrix = 'SEDIMENT', exact = FALSE)
  search_lookup_methodid("TOC", lab = 'NIVA', matrix = NA, exact = FALSE)
  search_lookup_methodid("TOC", lab = NA, matrix = NA, exact = FALSE)
  
  search_lookup_methodid("d15N", lab = 'IFE', matrix = 'BIOTA', exact = FALSE)
  test <- search_lookup_methodid("TOC", lab = NA, matrix = NA, exact = FALSE)
}

get_species_from_id <- function(taxoncode_id){
  taxoncode_id <- taxoncode_id[!is.na(taxoncode_id)]
  taxoncode_id <- unique(taxoncode_id)
  if (length(taxoncode_id) > 0) {
    result <- get_nivabase_data(paste("select a.TAXONOMY_CODE_ID, b.LATIN_NAME", 
                                          "from NIVADATABASE.TAXONOMY_CODES a LEFT JOIN NIVADATABASE.TAXONOMY b ON a.NIVA_TAXON_ID = b.NIVA_TAXON_ID", 
                                          "where TAXONOMY_CODE_ID in", "(", paste(taxoncode_id, 
                                                                                  collapse = ","), ");"))
  } else {
    result <- NULL
  }
  result
}

if (FALSE){
  # debugonce(get_species_from_id)
  get_species_from_id(8927)
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for checking samples in LIMS ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


check_lims_samples <- function(lims_samples){
  
  check <- list(
    check_lims_samples_type(lims_samples, type = "all"),
    check_lims_samples_type(lims_samples, type = "biota")
  )
  
  warning_message <- c(
    "Some info necessary for all samples is lacking!",
    "Some info necessary for biota samples is lacking!"
  )
  
  # 1. For all rows (i=1) and biota rows (i=2), check that there are no NA values     
  for (i in 1:2){
    if (sum(check[[i]]$coltable) > 0){
      
      samples <- check[[i]]$samples
      coltable <- check[[i]]$coltable
      
      cat("=============================================================\n")
      cat(warning_message[i], "\n=============================================================\n")
      
      message("\nColumns lacking info:")
      columns_lacking_info <- coltable > 0
      print(names(coltable)[columns_lacking_info])  
      
      message("\nRows lacking info:")
      samples_lacking_info <- apply(is.na(samples), 1, sum) > 0
      print(
        samples[samples_lacking_info, "DESCRIPTION"])  
      cat("\n")
      
    }
  }
  
  # 2. For biota rows, check if there are no BIOTA_SAMPLENO = 0  
  i <- 2
  samples <- check[[i]]$samples
  sampleno_is_zero <- samples$BIOTA_SAMPLENO %in% 0
  if (sum(sampleno_is_zero) > 0){
    cat("=============================================================\n")
    cat("Some BIOTA_SAMPLENO are zero!\n=============================================================\n")
    message("\nRows with BIOTA_SAMPLENO = 0:")
    print(
      samples[sampleno_is_zero, "DESCRIPTION"])  
    cat("\n")
  }

  
  invisible(
    list(
      all = check[[1]],
      biota = check[[2]]
    )
  )
  
}

# Helper function for 'check_lims_samples'  

check_lims_samples_type <- function(lims_samples, type = "all"){
  
  if (type == "all"){
    check <- lims_samples %>%
      select(ANALYSEOPPDRAG, TEXT_ID, SAMPLE_TYPE, SAMPLED_DATE, DESCRIPTION, 
             AQUAMONITOR_CODE)
  } else if (type == "biota"){
    check <- lims_samples %>%
      filter(SAMPLE_TYPE == "BIOTA") %>%
      select(DESCRIPTION, SPECIES, TISSUE, BIOTA_SAMPLENO)
  } else {
    stop("Type must be 'all' or 'biota")
  }
  check_tab <- apply(is.na(check), 2, sum)  
  
  list(samples = check, coltable = check_tab)
  
}

if (FALSE){
  
  df_samples_all <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT like '%urban%'")
  
  df_samples_all <- df_samples_all %>%
    mutate(
      year = year(SAMPLED_DATE),
      year_from_textid = substr(df_samples_all$TEXT_ID, 4, 7))
  
  df_samples_2022 <- df_samples_all %>%
    filter(year_from_textid == 2022) %>%
    filter(!ANALYSEOPPDRAG %in% c("1150-11687", "1150-11698","1150-11699", "1150-11700", "1150-11701", "1150-11690")) %>%
    arrange(TEXT_ID)
  
  df_samples_2022 %>%
    select(ANALYSEOPPDRAG, TEXT_ID, SAMPLE_TYPE, SAMPLED_DATE, DESCRIPTION, 
           AQUAMONITOR_CODE, SPECIES, TISSUE) %>% dput()
  
  # or get sample data from "APPENDIX 1: LIMS sample data example", below!
  
  check_lims_samples_type(df_samples_2022, type = "all")
  check_lims_samples_type(df_samples_2022, type = "biota")
  
  # debugonce(check_lims_samples)
  check_lims_samples(df_samples_2022)
  
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for testing ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Checks if there are any NAs in 'variablename' of 'data'
# If there are any NAs:
#    error = TRUE, stops with an error
#    error = FALSE, gives waening and returns the rows with NA values  

check_na <- function(data, variablename, error = TRUE){
  sel <- is.na(data[[variablename]])
  df_with_na <- data[sel,]
  if (nrow(df_with_na) > 0){
    if (error){
      stop("Some rows lack ", variablename)
    } else {
      warning("Some rows lack ", variablename)
      result <- df_with_na
    }
  } else {
    result <- NULL
  }
  invisible(result)
}


# APPENDIX 1: LIMS sample data example ----  
# contains lacking AQUAMONITOR_CODE

if (FALSE){
  
  
  # source("C:/Data/seksjon 212/Urban-fjord/0001_Functions_sql_and_more.R")
  #   df_samples_2022 %>%
  #  +     select(ANALYSEOPPDRAG, TEXT_ID, SAMPLE_TYPE, SAMPLED_DATE, DESCRIPTION, 
  #               +            AQUAMONITOR_CODE, SPECIES, TISSUE) %>% dput()
  df_samples_2022 <- structure(list(ANALYSEOPPDRAG = c("1150-11649", "1150-11649", 
                                    "1150-11649", "1150-11649", "1150-11649", "1150-11649", "1150-11649", 
                                    "1150-11649", "1150-11649", "1150-11649", "1150-11688", "1150-11688", 
                                    "1150-11688", "1150-11688", "1150-11688", "1150-11688", "1150-11688", 
                                    "1150-11688", "1150-11688", "1150-11688", "1150-11688", "1150-11688", 
                                    "1150-11688", "1150-11688", "1150-11688", "1150-11689", "1150-11689", 
                                    "1150-11689", "1150-11691", "1150-11691", "1150-11691", "1150-11691", 
                                    "1150-11691", "1150-11691", "1150-11703", "1150-11703", "1150-11703", 
                                    "1150-11703", "1150-11703", "1150-11703", "1150-11703", "1150-11703", 
                                    "1150-11703", "1150-11703", "1150-11703", "1150-11703", "1150-11737", 
                                    "1150-11737", "1150-11737", "1150-11737", "1150-11737", "1150-11737", 
                                    "1150-11737", "1150-11737", "1150-11737", "1150-11737"), 
                 TEXT_ID = c("NR-2022-10332", 
                             "NR-2022-10333", "NR-2022-10334", "NR-2022-10335", "NR-2022-10336", 
                             "NR-2022-10337", "NR-2022-10338", "NR-2022-10339", "NR-2022-10340", 
                             "NR-2022-10342", "NR-2022-10791", "NR-2022-10792", "NR-2022-10793", 
                             "NR-2022-10794", "NR-2022-10795", "NR-2022-10796", "NR-2022-10797", 
                             "NR-2022-10798", "NR-2022-10799", "NR-2022-10800", "NR-2022-10801", 
                             "NR-2022-10802", "NR-2022-10803", "NR-2022-10804", "NR-2022-10805", 
                             "NR-2022-10806", "NR-2022-10807", "NR-2022-10808", "NR-2022-10824", 
                             "NR-2022-10825", "NR-2022-10826", "NR-2022-10827", "NR-2022-10828", 
                             "NR-2022-10829", "NR-2022-11003", "NR-2022-11004", "NR-2022-11005", 
                             "NR-2022-11006", "NR-2022-11007", "NR-2022-11008", "NR-2022-11009", 
                             "NR-2022-11010", "NR-2022-11011", "NR-2022-11012", "NR-2022-11013", 
                             "NR-2022-11014", "NR-2022-11599", "NR-2022-11600", "NR-2022-11601", 
                             "NR-2022-11603", "NR-2022-11604", "NR-2022-11605", "NR-2022-11606", 
                             "NR-2022-11607", "NR-2022-11608", "NR-2022-11609"), 
                 SAMPLE_TYPE = c("BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "SEDIMENT", "SEDIMENT", "SEDIMENT", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", 
                                 "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA", "BIOTA"
                 ), 
                 SAMPLED_DATE = structure(c(1610150400, 1610150400, 1610150400, 
                                            1610150400, 1610150400, 1610150400, 1610236800, 1610236800, 1610236800, 
                                            1610150400, 1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 
                                            1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 
                                            1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 
                                            1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 1660694400, 
                                            1660694400, 1661040000, 1661040000, 1661040000, 1661040000, 1661040000, 
                                            1661040000, 1661040000, 1661040000, 1661040000, 1661040000, 1661040000, 
                                            1661040000, 1610150400, 1610150400, 1610150400, 1610150400, 1610150400, 
                                            1610150400, 1610150400, 1610150400, 1610150400, 1610150400), tzone = "UTC", 
                                          class = c("POSIXct", 
                                                    "POSIXt")), 
                 DESCRIPTION = c("Selspekk1", "Selspekk2", "Selspekk3", 
                                 "Selspekk4", "Selspekk5", "Selspekk6", "Selspekk7", "Selspekk8", 
                                 "Selspekk9", "Selspekk11", "Børstemark Cm21, Blandprøve", "Børstemark Bekkelaget, Blandprøve", 
                                 "Børstemark Alna, Blandprøve", "Zoopl./Krill Blandprøve 1", 
                                 "Zoopl./Krill Blandprøve 2", "Zoopl./Krill Blandprøve 3", "Reker, Blandprøve 1", 
                                 "Reker, Blandprøve 2", "Reker, Blandprøve 3", "Blåskjell Blandprøve 1", 
                                 "Blåskjell Blandprøve 2", "Blåskjell Blandprøve 3", "Sild Blandprøve 1", 
                                 "Sild Blandprøve 2", "Sild Blandprøve 3", "Cm21 Cm21", "Bq41 Bekkelaget", 
                                 "AL1 Alnas utløp", "Torsk Lever Blandprøve 1", "Torsk Lever Blandprøve 2", 
                                 "Torsk Lever Blandprøve 3", "Torsk Muskel Blandprøve 1", "Torsk Muskel Blandprøve 2", 
                                 "Torsk Muskel Blandprøve 3", "Ærfugl Blod blandpr. 1", "Ærfugl Blod blandpr. 2", 
                                 "Ærfugl Blod blandpr. 3", "Ærfugl Egg blandpr. 1", "Ærfugl Egg blandpr. 2", 
                                 "Ærfugl Egg blandpr. 3", "Gråmåke Blod blandpr. 1", "Gråmåke Blod blandpr. 2", 
                                 "Gråmåke Blod blandpr. 3", "Gråmåke Egg blandpr. 1", "Gråmåke Egg blandpr. 2", 
                                 "Gråmåke Egg blandpr. 3", "Selmuskel1", "Selmuskel2", "Selmuskel3", 
                                 "Selmuskel5", "Selmuskel6", "Selmuskel7", "Selmuskel8", "Selmuskel9", 
                                 "Selmuskel10", "Selmuskel11"), 
                 AQUAMONITOR_CODE = c("Sel Torbj.sk.", 
                                      "Sel Torbj.sk.", "Sel Torbj.sk.", "Sel Torbj.sk.", "Sel Torbj.sk.", 
                                      "Sel S.Missing.", "Sel Singleøy", "Sel Singleøy", "Sel Singleøy", 
                                      "Sel Garnh.", "Cm21", "Bq41", "AL1", "IO", "IO", "IO", "IO", 
                                      "IO", "IO", "IO Blåskjell", "IO Blåskjell", "IO Blåskjell", 
                                      "IO", "IO", "IO", "Cm21", "Bq41", "AL1", "IO", "IO", "IO", "IO", 
                                      "IO", "IO", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Sel Torbj.sk.", 
                                      "Sel Torbj.sk.", "Sel Torbj.sk.", "Sel Torbj.sk.", "Sel S.Missing.", 
                                      "Sel Singleøy", "Sel Singleøy", "Sel Singleøy", "Sel Singleøy", 
                                      "Sel Garnh."), 
                 SPECIES = c("Phoca vitulina", "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", "Phoca vitulina",
                             "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", 
                             "Polychaeta", "Polychaeta", "Polychaeta", "Euphausiacea indet", 
                             "Euphausiacea indet", "Euphausiacea indet", "Pandalus borealis", 
                             "Pandalus borealis", "Pandalus borealis", "Mytilus edulis", "Mytilus edulis", 
                             "Mytilus edulis", "Clupea harengus", "Clupea harengus", "Clupea harengus", 
                             NA, NA, NA, "Gadus morhua", "Gadus morhua", "Gadus morhua", "Gadus morhua", 
                             "Gadus morhua", "Gadus morhua", "Somateria mollissima", "Somateria mollissima", 
                             "Somateria mollissima", "Somateria mollissima", "Somateria mollissima", 
                             "Somateria mollissima", "Larus argentatus", "Larus argentatus", 
                             "Larus argentatus", "Larus argentatus", "Larus argentatus", "Larus argentatus", 
                             "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", 
                             "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", "Phoca vitulina", 
                             "Phoca vitulina", "Phoca vitulina"), 
                 TISSUE = c("SK-Spekk", "SK-Spekk", 
                            "SK-Spekk", "SK-Spekk", "SK-Spekk", "SK-Spekk", "SK-Spekk", "SK-Spekk", 
                            "SK-Spekk", "SK-Spekk", "WO-Hel organisme", "WO-Hel organisme", 
                            "WO-Hel organisme", "WO-Hel organisme", "WO-Hel organisme", "WO-Hel organisme", 
                            "SB-Whole soft body", "SB-Whole soft body", "SB-Whole soft body", 
                            "SB-Whole soft body", "SB-Whole soft body", "SB-Whole soft body", 
                            "MU-Muskel", "MU-Muskel", "MU-Muskel", NA, NA, NA, "LI-Lever", 
                            "LI-Lever", "LI-Lever", "MU-Muskel", "MU-Muskel", "MU-Muskel", 
                            "BL-Blod", "BL-Blod", "BL-Blod", "EH-Egg", "EH-Egg", "EH-Egg", 
                            "BL-Blod", "BL-Blod", "BL-Blod", "EH-Egg", "EH-Egg", "EH-Egg", 
                            "MU-Muskel", "MU-Muskel", "MU-Muskel", "MU-Muskel", "MU-Muskel", 
                            "MU-Muskel", "MU-Muskel", "MU-Muskel", "MU-Muskel", "MU-Muskel"
                 )), class = "data.frame", row.names = c(NA, -56L))
  
}
