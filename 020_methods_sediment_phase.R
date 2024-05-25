
#
# Create lookup table for particle-phase parameters and their METHOD_ID  
#

library(dplyr)
library(stringr)
library(readr)

# Start with 2021 data  
dat_nb_water2021 <- readRDS("Data/200_dat_nb_water_Milfersk_2013-2022_2023-10-09.rds")
table(dat_nb_water2021$Year)

# All parameters, with method ID
df_params_all <- dat_nb_water2021 %>%
  distinct(NAME, METHOD_ID)

# Select 'particle phase' parameters (ending with '-P')  
sel_partic <- stringr::str_sub(df_params_all$NAME, start = -2) == "-P"

# Create lookup table
df_params_both_phases <- df_params_all[sel_partic,] %>%
  rename(
    NAME_partic = NAME,
    METHOD_ID_partic = METHOD_ID) %>%
  mutate(NAME_water = stringr::str_sub(NAME_partic, end = -3)) %>%
  left_join(df_params_all, by = c("NAME_water" = "NAME")) %>%
  rename(
    METHOD_ID_water = METHOD_ID) %>%
  # reorder columns:
  select(
    NAME_water, METHOD_ID_water, NAME_partic, METHOD_ID_partic
  )

# Save result  
readr::write_csv(df_params_both_phases, "Input_data/Lookup_files/lookup_particlephase_2021.csv")
