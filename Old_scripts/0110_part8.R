
## 8. Add bile samples  

*NOTE: If bile samples already have been added, skip to 9*
  
  - rows with no LIMS number
- These are gall samples from cod 2020 
- must have special treatment (add gall samples to Nivabasen, "paralell" to muscle samples)  

### Check rows  
```{r}

cat("Number of rows with no LIMS number: \n")
sum(!sel_bio & !sel_wat & !sel_sed) 

# Make data for checking
check_no_lims <- df_nilu_02[!sel_bio & !sel_wat & !sel_sed,]  

```

### Check sample description  

- in this case, they are samples of bile (galle) from 16 fish  
- 1-15 

```{r}

check_no_lims %>%
  pull(LIMS.NR) %>%
  unique()

```


### Get muscle samples 2020    

- Because bile samples correspond to muscle samples  
- E.g. muscle no 4 and bile number 4 are from the same fish  

```{r}

if (FALSE) {
  
  df_orig <- df_nilu_02 %>%
    filter(MYEAR == 2020 & grepl("Hg", NAME_orig) & !is.na(BIOTA_SAMPLE_ID))
  
  df_samples_biota <- get_nivabase_selection(
    columns = "*", 
    table = "BIOTA_SAMPLES", 
    selection_column = "SAMPLE_ID", 
    selection_values = unique(df_orig$BIOTA_SAMPLE_ID)) %>%
    left_join(lookup_tissues)
  
  write.csv(df_samples_biota, "Data/0110_df_samples_biota_2020-21.csv", row.names = FALSE)  
  
}

df_samples_biota <- read.csv("Data/0110_df_samples_biota_2020-21.csv")

df_samples_muscle <- df_samples_biota %>%
  filter(TISSUE_NAME %in% "Muskel")

```

### Link samples to individuals  

```{r}

if (FALSE){
  
  library(niRvana)
  library(networkD3)
  
  sqltext <- "NIVADATABASE.BIOTA_SAMPLES_SPECIMENS a full join NIVADATABASE.BIOTA_SINGLE_SPECIMENS b on a.SPECIMEN_ID = b.SPECIMEN_ID"
  
  df_samples_muscle_ind <- get_nivabase_selection(
    "a.SAMPLE_ID, b.STATION_ID, b.SPECIMEN_ID, b.TAXONOMY_CODE_ID, b.SPECIMEN_NO, b.DATE_CAUGHT", 
    sqltext, 
    "SAMPLE_ID", 
    df_samples_muscle$SAMPLE_ID, 
    table_literal = TRUE)
  
  df_samples_muscle_ind <- df_samples_muscle_ind %>%
    left_join(df_samples_muscle %>% select(SAMPLE_ID, SAMPLE_NO))
  
  write.csv(df_samples_muscle_ind, "0110_df_samples_muscle_before_insert.csv", row.names = FALSE)
  
}

df_samples_muscle_ind <- read.csv("0110_df_samples_muscle_before_insert.csv")

# Plot
# install.packages("networkD3")
networkD3::simpleNetwork(df_samples_muscle_ind %>% select(SPECIMEN_NO, SAMPLE_ID), charge = -4)


```




### Add samples      

- Add to BIOTA_SAMPLES (get STATION_ID from df_samples_muscle)    
- Add to BIOTA_SAMPLES_SPECIMENS (get SPECIMEN_ID from df_samples_muscle) 

- Columns
- "SAMPLE_ID"            - Let the database decide
- "TISSUE_ID"            - Lookup value from TISSUE_NAME
- "STATION_ID"           - Use value in 'data_for_input'
- "REMARK"               - NA
- "SAMPLE_NO"            - e.g. 1-15 for each tissue, corresponding to SPECIMEN_NO
- "SAMPLE_DATE"          - corresponding to DATE_CAUGHT
- "REPNO"                - usually 1  
- "TAXONOMY_CODE_ID"     - Lookup value

#### Set 1: Data for gall samples 1-15   

- Based on existing liver samples, just change BIOTA_ID   

```{r}

id_samples_muscle <- df_samples_muscle_ind %>%
  filter(SPECIMEN_ID %in% 333305:333319) %>%
  pull(SAMPLE_ID)

df_samples_bile_set1 <- df_samples_muscle %>%
  filter(SAMPLE_ID %in% id_samples_muscle) %>%
  select(STATION_ID, SAMPLE_NO, REPNO, SAMPLE_DATE, TAXONOMY_CODE_ID)

df_samples_bile_set1$TISSUE_ID <- subset(lookup_tissues, TISSUE_NAME == "Galle")$TISSUE_ID

```

#### Set 2: Data for gall sample 16   

- Afterwards, need to add a row in NIVADATABASE.BIOTA_SINGLE_SPECIMENS  
- and then a row in BIOTA_SAMPLES_SPECIMENS  

```{r}

# Sample 16: Just use one of the samples made, and change SAMPLE_NO + REPNO
df_samples_bile_set2 <- df_samples_bile_set1[1,]

df_samples_bile_set2$SAMPLE_NO <- 16  
df_samples_bile_set2$REPNO <- 16  

```

#### Samples 1-16

```{r}

df_samples_bile <- bind_rows(
  df_samples_bile_set1, 
  df_samples_bile_set2)

```

####  Make SQLs  
PASTE INTO SQL DEVELOPER TO ADD THE RECORDS   

Note to DHJ: use "SQL developer (latest version)" from desktop. Don't use the start menu.  
- remember `commit;` after running the insert sentences   
- See `milkys_2020_erod-alad_biota_samples.sql` in `C:\Users\DHJ\AppData\Roaming\SQL Developer`  

```{r}

# Check columns 
# get_nivabase_data("select * from NIVADATABASE.BIOTA_SAMPLES where rownum <= 5")

# Test functions
# make_sql_sample(1, biota_samples_eider)

sql_list <- 1:nrow(df_samples_bile) %>% 
  map_chr(make_sql_sample, data = df_samples_bile)
sql <- paste(sql_list, collapse = ";\n")
sql <- paste0(sql, ";\n")
writeLines(sql, "clipboard")  # copies SQLs to clipboard - go to SQL Developer and paste

n_records1 <- length(sql_list)  # 92
cat("Number of SQLs: \n")
n_records1

cat("\nSample of SQLs: \n")
sql_list[1:3]

n_records1 <- length(sql_list)

```



#### Get the records we just added   

- Also checks that the number of new records is the same as no. of SQLs  
- Remember to **commit** in SQL developer first  

```{r}

# day_for_adding_records <- substr(lubridate::now(tzone = "UTC"), 1, 10)
day_for_adding_records <- "2023-01-24"

biota_samples_fromdatabase <- niRvana::get_nivabase_selection(
  "*",
  "BIOTA_SAMPLES",
  "STATION_ID",
  df_samples_muscle$STATION_ID[1], 
  extra_sql = paste0(
    "and TRUNC(ENTERED_DATE) = TO_DATE(", sQuote(day_for_adding_records), ", 'yyyy-mm-dd')", 
    " and ENTERED_BY = 'DHJ'")
)

cat("Number of retrieved records: ")
n_records2 <- nrow(biota_samples_fromdatabase)
n_records2

# Save for later use  
# write.csv(biota_samples_fromdatabase, "Input_data/2020/df_bile_samples.csv", row.names = FALSE)

if (n_records2 != n_records1){
  stop("Number of added records is not the same as number of SQLs in the 'Make SQLs' section!")
} else {
  cat("The number of added records is the same as number of SQLs in the 'Make SQLs' section \n")
}


```

### Add specimen, gall sample 16  

- need DATE_CAUGHT, STATION_ID, TAXONOMY_CODE_ID, SPECIMEN_NO

```{r}

# Gets one existing row from BIOTA_SINGLE_SPECIMENS and change it   

df_specimens_to_add <- df_samples_muscle_ind[1,] %>%
  select(DATE_CAUGHT, STATION_ID, TAXONOMY_CODE_ID, SPECIMEN_NO)

# chenge number
df_specimens_to_add$SPECIMEN_NO <- 16  

```

#### Make SQLs  
PASTE INTO SQL DEVELOPER TO ADD THE RECORDS   
Note to DHJ: use "SQL developer (latest version)" from desktop. Don't use the start menu.  
- remember `commit;` after running the insert sentences  
- NOT NEEDED in this case  
```{r}

sql_list <- 1:nrow(df_specimens_to_add) %>% 
  map_chr(make_sql_single_specimen, data = df_specimens_to_add)
sql <- paste(sql_list, collapse = ";\n")
sql <- paste0(sql, ";\n")  
writeLines(sql, "clipboard")  # copies SQLs to clipboard - go to SQL Developer and paste


cat("Number of SQLs: \n")
length(sql_list)  # 9


cat("\nSample of SQLs: \n")
sql_list[1:3]

```


#### Get existing records in Nivabase

```{r}

biota_specimens_fromdatabase <- niRvana::get_nivabase_selection(
  "*",
  "BIOTA_SINGLE_SPECIMENS",
  "STATION_ID",
  df_samples_muscle$STATION_ID[1], 
  extra_sql = paste0(
    "and TRUNC(ENTERED_DATE) = TO_DATE(", sQuote(day_for_adding_records), ", 'yyyy-mm-dd')", 
    " and ENTERED_BY = 'DHJ'")
)

```


### Add sample-specimen link, gall sample 16  

- need SAMPLE_ID, SPECIMEN_ID  

```{r}

# Gets one existing row from BIOTA_SINGLE_SPECIMENS and change it   

df_samples_specimens_to_add <- data.frame(
  SAMPLE_ID = biota_samples_fromdatabase$SAMPLE_ID[16],
  SPECIMEN_ID = biota_specimens_fromdatabase$SPECIMEN_ID[1]
)

```

#### Make SQLs  
PASTE INTO SQL DEVELOPER TO ADD THE RECORDS   
Note to DHJ: use "SQL developer (latest version)" from desktop. Don't use the start menu.  
- remember `commit;` after running the insert sentences  
- NOT NEEDED in this case  
```{r}

sql_list <- 1:nrow(df_samples_specimens_to_add) %>% 
  map_chr(make_sql_samples_specimens, data = df_samples_specimens_to_add)
sql <- paste(sql_list, collapse = ";\n")
sql <- paste0(sql, ";\n")  
writeLines(sql, "clipboard")  # copies SQLs to clipboard - go to SQL Developer and paste


cat("Number of SQLs: \n")
length(sql_list)  # 9


cat("\nSample of SQLs: \n")
sql_list[1:3]

```



#### Get tissue ID for gall  
```{r}

tissue_id_galle <- lookup_tissues %>%
  filter(TISSUE_NAME == "Galle") %>%
  pull(TISSUE_ID)  

```