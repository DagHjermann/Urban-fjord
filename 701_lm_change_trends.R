
library(dplyr)
library(purrr)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggeasy)
library(ggdist)
library(tidyr)
library(forcats)     # fct_rev()
library(broom)       # for tidy()
library(cowplot)     # for plot_grid()
library(NADA2)       # for regression with data under LOQ 


lm_result <- list()

for (name in name_order){
  
  data_for_analysis <- dat_sel %>% 
    filter(NAME == name & is.na(FLAG1) & Year %in% 2015:2022) %>%
    mutate(logVALUE = log(VALUE))
  
  lm_result_onevar <- try(
    get_manipulated_lm_oneseries2(
      data = data_for_analysis,
      df_changevalues = df_change, 
      no_samples = 3, 
      no_draws = 50))  
  
  saveRDS(lm_result_onevar, paste0("Data/701_lmresult2/result_", name, ".rds"))
  
  lm_result[[name]] <- lm_result_onevar
  
}

# lm_result[["D4"]]  # error  
# lm_result <- lm_result[!names(lm_result) %in% "D4"]

saveRDS(lm_result, "Data/701_lmresult2/lm_result.rds")
