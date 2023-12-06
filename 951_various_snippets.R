
library(niRvana)
library(dplyr)
library(purrr)
library(lubridate)
library(readxl)
library(ggplot2)
library(tidyr)  
library(knitr)     # for kable
library(glue)      # alternative to paste()  
# library(fuzzyjoin)   # regex_full_join

source("../Milkys/991_Vannmiljo_snippets_functions.R")
source("../Milkys/992_Vannmiljo_Urban_fjord_functions.R")
source("../Milkys/994_Industry_data_functions.R")         # for add_coordinates()

#
# Get Labware samples for a given account ---- 
#

get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where PROSJEKT = 'O 210135;ANA - Miljøgifter i en urban fjord (2021-2026)'") 

df_check <- get_nivabase_data("select * from NIVADATABASE.LABWARE_CHECK_SAMPLE where ACCOUNT_NUMBER = '1155'") 

writexl::write_xlsx(df_check, "Data/951_labware_check_sample_account1155.xlsx")


