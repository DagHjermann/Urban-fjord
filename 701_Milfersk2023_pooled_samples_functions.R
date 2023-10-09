

#
# Get list of SAMPLE_IDs for a given species, station and tissue    
#

draw_samples <- function(species, station, tissue, year, no_samples, data){
  samples <- data %>%
    filter(LATIN_NAME %in% species, 
           STATION_NAME %in% station, 
           TISSUE_NAME %in% tissue,
           Year %in% year) %>%
    distinct(LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, SAMPLE_ID) %>%
    pull(SAMPLE_ID)
  n <- length(samples)
  samples_shuffled <- sample(samples, size = n, replace = FALSE)
  # 
  # Pick start and stop index for each pooled sample
  #
  # i1 <- seq(1, n, by = round(n/no_samples))
  ix <- seq(1, n+1, length = no_samples + 1)
  i1 <- round(head(ix, -1))
  i2 <- c(tail(i1,-1)-1, n)
  size_pool <- i2 - i1 + 1
  #
  # Draw samples
  #
  # Draw sample_id in original order (for testing)
  # sample_list <- map2(i1, i2, \(x,y) samples[x:y])
  # Draw sample_id in random order
  sample_list <- map2(i1, i2, \(x,y) samples_shuffled[x:y])
  
  # For testing:
  # df_split <- matrix(c(i1, i2, size_pool), length(i1), 3)
  
  sample_list
  
}

if (FALSE){
  # TEST
  # debugonce(draw_samples)
  draw_samples("Salmo trutta", "Femunden", "Muskel", 2020, 4, data = dat)  
  draw_samples("Salmo trutta", "Femunden", "Muskel", 2020, 5, data = dat)  
  draw_samples("Salmo trutta", "Femunden", "Muskel", 2020, 6, data = dat)  
}

#
# Get list of data frames with concentration of given parameters  
#

draw_concentrations <- function(params, ..., data = dat){
  samples <- draw_samples(..., data = dat)  
  names(samples) <- seq_along(samples)
  data <- ungroup(data)
  map_dfr(samples, 
          \(x) data %>% 
            filter(SAMPLE_ID %in% x & NAME %in% params) %>% 
            select(LATIN_NAME, STATION_NAME, TISSUE_NAME, SAMPLE_ID, NAME, VALUE, FLAG1),
          .id = "Pooled_sample")
}

if (FALSE){
  # TEST
  unique(dat$NAME)
  dat %>% count(NAME, Year) %>% View()
  # debugonce(draw_concentrations)
  draw_concentrations("PFOSA", 
                      "Salmo trutta", "Mjøsa", "Muskel", 2018, 4, data = dat)  
  draw_concentrations(c("PFOSA", "PFOS"), 
                      "Salmo trutta", "Mjøsa", "Muskel", 2018, 4, data = dat)  

}



#
# Get means per pooled sample, that is, the expected concentration if the physically pooled sample,
#   assuming that equally much tissue (in grams) is taken from each individual
#

draw_pooled_means_single <- function(..., under_loq_treatement = "random between LOQ/2 and LOQ"){
  data <- draw_concentrations(...)
  data$Conc_for_mean <- data$VALUE
  # sel = the cases where concentration is < LOQ
  sel <- data$FLAG1 %in% "<"
  if (sum(sel) > 0){
    LOQ <- mean(data$VALUE[sel])
    if (under_loq_treatement == "random between LOQ/2 and LOQ"){
      data$Conc_for_mean[sel] <- runif(sum(sel), data$VALUE[sel]/2, data$VALUE[sel])
    } else if (under_loq_treatement == "LOQ/2"){
      data$Conc_for_mean[sel] <- data$VALUE[sel]/2
    }
  } else {
    LOQ <- min()
  }
  result <- data %>%
    group_by(Pooled_sample, LATIN_NAME, STATION_NAME, TISSUE_NAME, NAME) %>%
    summarise(
      N_in_pool = n(),
      N_over_LOQ = sum(is.na(FLAG1)),
      Conc = mean(Conc_for_mean), 
      .groups = "drop"
    )
  # Set FLAG1 values, and adjust Conc up to LOQ if the value is below it
  result$FLAG1 <- NA
  if (sum(sel) > 0){
    sel2 <- result$Conc < LOQ
    result$Conc[sel2] <- LOQ
    result$FLAG1[sel2] <- "<"
  }
  result
}


if (FALSE){
  
  # Check fraction of conc. under LOQ 
  dat %>%
    filter(TISSUE_NAME == "Muskel") %>%
    group_by(NAME) %>%
    summarise(P_under_LOQ = mean(FLAG1 %in% "<"))
  
  draw_pooled_means_single("PFOSA", 
                  "Salmo trutta", "Femunden", "Muskel", 2018, 4, data = dat)  
  draw_pooled_means_single(c("PFOSA", "PFOS"), 
                           "Salmo trutta", "Femunden", "Muskel", 2018, 4, data = dat)  


}

#
# 
# 

draw_pooled_means <- function(..., no_draws){
  map_dfr(
    1:no_draws,
    \(x)
    draw_pooled_means_single(...),
    .id = "Draw"
  )
}


if (FALSE){

  draw_pooled_means(c("PFOSA", "PFOS"), 
                    "Salmo trutta", "Femunden", "Muskel", 2018, 4, data = dat, no_draws = 2)  
  
  
}  

#
# Get original data (Strategy = "Single") plus data from 'draw_pooled_means' (Strategy = "Pooled")
#

get_combined_data <- function(params, species, station, tissue, year, no_samples, no_draws,
                              data, under_loq_treatement = "random between LOQ/2 and LOQ"){
  data_orig <- data %>% 
    filter(NAME %in% params,
           LATIN_NAME %in% species, 
           STATION_NAME %in% station, 
           TISSUE_NAME %in% tissue,
           Year %in% year) %>% 
    select(LATIN_NAME, STATION_NAME, TISSUE_NAME, SAMPLE_ID, NAME, VALUE, FLAG1) %>%
    rename(Conc = VALUE) %>%
    mutate(Strategy = "Single", Draw = NA, Pooled_sample = NA, .before = everything())
  
  data_pooled <- draw_pooled_means(params=params, species=species, station=station, tissue=tissue, 
                                   year = year, no_samples=no_samples, data=data, no_draws=no_draws,
                                 under_loq_treatement=under_loq_treatement) %>%
    mutate(Strategy = "Pooled", .before = everything())
  
  bind_rows(data_orig, data_pooled)
  
}

if (FALSE){
  # params, species, station, tissue, year, no_samples, no_draws
  test <- get_combined_data("PFOS", 
                    "Salmo trutta", "Femunden", "Muskel", 2018, 4, 2, data = dat)  

  test
  ggplot(test, aes(paste(Strategy, Draw), Conc)) +
    geom_point(aes(shape = is.na(FLAG1)))
}


#
# Get statistical summary for original data plus each single draw  
#

get_summary_each_draw <- function(...){
  get_combined_data(...) %>%
    group_by(NAME, LATIN_NAME, STATION_NAME, TISSUE_NAME, Strategy, Draw) %>%
    summarise(
      Median = median(Conc),
      Min = min(Conc),
      Max = max(Conc),
      N = n(),
      N_over_LOQ = sum(is.na(FLAG1))
    )
  
}



if (FALSE){  
  get_summary_each_draw("PFOSA", 
                    "Salmo trutta", "Femunden", "Muskel", 2018, data = dat, no_samples = 4, no_draws = 2)
  get_summary_each_draw(c("PFOSA","PFOS"), 
                        "Salmo trutta", "Femunden", "Muskel", 2018, data = dat, no_samples = 4, no_draws = 2)
  test <- get_summary_each_draw(c("PFOS","PFOSA"), 
                                 "Salmo trutta", "Femunden", "Muskel", 2018, data = dat, no_samples = 4, no_draws = 2)
  
  test <- get_combined_data("PFOS", 
                            "Salmo trutta", "Femunden", "Muskel", 2018, 4, 2, data = dat)  
  
  test
  ggplot(test, aes(paste(Strategy, Draw), Median)) +
    geom_pointrange(aes(min = Min, max = Max)) +
    facet_wrap(vars(NAME))
}

get_summary_aggregating_draws <- function(...){
  get_summary_each_draw(...) %>%
    group_by(NAME, LATIN_NAME, STATION_NAME, TISSUE_NAME, Strategy) %>%
    summarise(
      Median_mean = mean(Median),
      Median_sd = sd(Median),
      Median_min = min(Median),
      Median_max = max(Median),
      Min_mean = mean(Min),
      Max_mean = mean(Max),
      N = mean(N),
      N_over_LOQ = mean(N_over_LOQ)
    )
}

if (FALSE){
  test <- get_summary_aggregating_draws("Perfluoroktansulfonamid (PFOSA)", 
                                "Salmo trutta", "Femunden", "Muskel", 2020, data = dat, no_samples = 4, no_draws = 50)
  test
  ggplot(test, aes(Strategy, Median_mean)) +
    geom_pointrange(aes(min = Median_mean - Median_sd, max = Median_mean + Median_sd))
  
  test <- get_summary_aggregating_draws(c("Perfluoroktansulfonamid (PFOSA)", "Perfluoroktylsulfonat (PFOS)", "Perfluordekansulfonat (PFDS)"),
                                "Salmo trutta", "Femunden", "Muskel", 2020, data = dat, no_samples = 4, no_draws = 50)
  test
  ggplot(test, aes(Strategy, Median_mean)) +
    geom_pointrange(aes(min = Median_mean - Median_sd, max = Median_mean + Median_sd)) +
    facet_wrap(vars(NAME))

}
