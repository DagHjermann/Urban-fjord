

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
  # draw_samples("Salmo trutta", "Femunden", "Muskel", 2020, 4, data = dat)  

  draw_samples("Brown trout", "Mjøsa", "Muscle", 2018, 3, data = dat)  
  draw_samples("Brown trout", "Mjøsa", "Muscle", 2018, 4, data = dat)  
  draw_samples("Brown trout", "Mjøsa", "Muscle", 2018, 5, data = dat)  
  draw_samples("Brown trout", "Mjøsa", "Muscle", 2018, 6, data = dat)  
  
}

#
# Get list of data frames with concentration of given parameters  
#

draw_concentrations <- function(params, ..., data = dat){
  samples <- draw_samples(..., data = data)  
  names(samples) <- seq_along(samples)
  data <- ungroup(data)
  map_dfr(samples, 
          \(x) data %>% 
            filter(SAMPLE_ID %in% x & NAME %in% params) %>% 
            select(LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, SAMPLE_ID, NAME, VALUE, FLAG1),
          .id = "Pooled_sample")
}

if (FALSE){
  # TEST
  unique(dat$NAME)
  dat %>% count(NAME, Year) %>% View()
  # debugonce(draw_concentrations)
  # draw_concentrations(c("PFOSA", "PFOS"), 
  #                     "Salmo trutta", "Mjøsa", "Muskel", 2018, 4, data = dat)  
  
  draw_concentrations(c("PFOSA", "PFOS"), "Brown trout", "Mjøsa", "Muscle", 2014, 4, data = dat)  
  

}



#
# Get means per pooled sample, that is, the expected concentration if the physically pooled sample,
#   assuming that equally much tissue (in grams) is taken from each individual
#

draw_pooled_means_single <- function(..., under_loq_treatement = "random between LOQ/2 and LOQ"){
  data <- draw_concentrations(...)
  data$VALUE_for_mean <- data$VALUE
  # sel = the cases where concentration is < LOQ
  sel <- data$FLAG1 %in% "<"
  if (sum(sel) > 0){
    LOQ <- mean(data$VALUE[sel])
    if (under_loq_treatement == "random between LOQ/2 and LOQ"){
      data$VALUE_for_mean[sel] <- runif(sum(sel), data$VALUE[sel]/2, data$VALUE[sel])
    } else if (under_loq_treatement == "LOQ/2"){
      data$VALUE_for_mean[sel] <- data$VALUE[sel]/2
    }
  } else {
    LOQ <- NA
  }
  result <- data %>%
    group_by(Pooled_sample, LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, NAME) %>%
    summarise(
      N_in_pool = n(),
      N_over_LOQ = sum(is.na(FLAG1)),
      VALUE = mean(VALUE_for_mean), 
      .groups = "drop"
    )
  # Set FLAG1 values, and adjust VALUE up to LOQ if the value is below it
  result$FLAG1 <- NA
  if (sum(sel) > 0){
    sel2 <- result$VALUE < LOQ
    result$VALUE[sel2] <- LOQ
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
  
  # draw_pooled_means_single(c("PFOSA", "PFOS"), 
  #                          "Salmo trutta", "Femunden", "Muskel", 2018, 4, data = dat)  
  
  draw_pooled_means_single(c("PFOSA", "PFOS"), 
                           "Brown trout", "Mjøsa", "Muscle", 2018, 4, data = dat)  
  


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

  # draw_pooled_means(c("PFOSA", "PFOS"), 
  #                   "Salmo trutta", "Femunden", "Muskel", 2018, 4, data = dat, no_draws = 2)  
  draw_pooled_means(c("PFOSA", "PFOS"), 
                    "Brown trout", "Mjøsa", "Muscle", 2018, 4, data = dat, no_draws = 2)  
  
  
}  

#
# Get original data (Strategy = "Single") plus data from 'draw_pooled_means' (Strategy = "Pooled")
#

get_combined_data <- function(params, species, station, tissue, year, no_samples, no_draws,
                              data, 
                              under_loq_treatement = "random between LOQ/2 and LOQ",
                              detrended = TRUE){
  data_orig <- data %>% 
    mutate(Draw = as.character(NA), 
           Pooled_sample = as.character(NA)) %>%
    filter(NAME %in% params,
           LATIN_NAME %in% species, 
           STATION_NAME %in% station, 
           TISSUE_NAME %in% tissue,
           Year %in% year) %>% 
    select(LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, SAMPLE_ID, NAME, VALUE, FLAG1) %>%
    rename(Conc = VALUE)
  
  if (detrended){
    data_orig <- get_detrended(data_orig) %>%
      mutate(
        Strategy = "Single", Sampling = "Single", .before = everything())
  }

  data_pooled <- draw_pooled_means(params=params, species=species, station=station, tissue=tissue, 
                                   year = year, no_samples=no_samples, data=data, no_draws=no_draws,
                                 under_loq_treatement=under_loq_treatement) %>%
    mutate(Strategy = "Pooled",  Sampling = paste(Strategy, Draw), .before = everything())
  
  if (nrow(data_pooled) > 0) {
    result <- bind_rows(data_orig, data_pooled)
  } else {
    result <- data_orig 
  }
  
  result <- result %>%
    mutate(
      `LOQ status` = case_when(
        is.na(FLAG1) ~ "Over LOQ",
        !is.na(FLAG1) ~ "Under LOQ"))
  
  result

}

if (FALSE){
  # params, species, station, tissue, year, no_samples, no_draws
  # test <- get_combined_data("PFOS", 
  #                   "Salmo trutta", "Femunden", "Muskel", 2018, 4, 2, data = dat)  
  debugonce(get_combined_data)
  test <- get_combined_data(c("PFOSA", "PFOS"), 
                    "Brown trout", "Mjøsa", "Muscle", 2019, 4, 2, data = dat)  
  test
  ggplot(test, aes(paste(Strategy, Draw), Conc)) +
    geom_point(aes(shape = is.na(FLAG1))) +
    facet_wrap(vars(NAME), scales = "free_y")
  
}

get_cv_one_series <- function(data, cv_analytic){
  
}



get_detrended_one_series <- function(params, species, station, tissue, data){
  if (length(params) > 1)
    stop("Max. one parameter!")
  result <- data %>% 
    filter(NAME %in% params,
           LATIN_NAME %in% species, 
           STATION_NAME %in% station, 
           TISSUE_NAME %in% tissue,
           # Year %in% year,
           !is.na(VALUE)
    ) %>% 
    select(LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, SAMPLE_ID, NAME, VALUE, FLAG1)
    #  select(LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, SAMPLE_ID, NAME, Conc, FLAG1)
  mod <- lm(VALUE ~ Year, result)
  # data_orig$VALUE_orig <- data_orig$VALUE
  # data_orig$VALUE <- data_orig$VALUE_orig - predict(mod) + mean(data_orig$VALUE_orig)
  # data_orig
  mean_VALUE <- result %>% pull(VALUE) %>% mean()
  fitted_VALUE <- predict(mod)
  result %>%
    mutate(
      VALUE_orig = VALUE,
      VALUE = VALUE_orig - fitted_VALUE + mean_VALUE
    )
}

if (FALSE){
  # debugonce(get_detrended_one_series)
  test <- get_detrended_one_series("PFOS", "Brown trout", "Mjøsa", "Muscle", data = dat %>% rename(Conc = VALUE))  
  test <- get_detrended_one_series("BDE99", "Brown trout", "Mjøsa", "Muscle", data = dat %>% rename(Conc = VALUE))  
  # test
  ggplot(test, aes(Year)) +
    geom_point(aes(y = Conc_orig, shape = is.na(FLAG1))) +
    geom_point(aes(y = Conc, shape = is.na(FLAG1)), color = "red")
}


get_detrended <- function(data){
  data_orig <- data %>% 
    filter(!is.na(VALUE))
  df_series <- data_orig %>%
    distinct(NAME, LATIN_NAME, STATION_NAME, TISSUE_NAME)  
  n <- nrow(df_series)
  result <- map_dfr(
    1:n,
    \(i) get_detrended_one_series(
      params = df_series$NAME[i],
      species = df_series$LATIN_NAME[i],
      station = df_series$STATION_NAME[i],
      tissue = df_series$TISSUE_NAME[i],
      data = data_orig)  
  )
  result
}

if (FALSE){
  # debugonce(get_detrended)
  test_data <- dat %>% 
    filter(
      NAME %in% c("BDE99", "PFOS"),
      LATIN_NAME %in% "Brown trout", 
      STATION_NAME %in% "Mjøsa", 
      TISSUE_NAME %in% "Muscle")
  test <- get_detrended(data = test_data)  
  # test
  table(test$NAME)
  ggplot(test, aes(Year)) +
    geom_jitter(aes(y = Conc_orig, shape = is.na(FLAG1))) +
    geom_jitter(aes(y = Conc, shape = is.na(FLAG1)), color = "red") +
    facet_wrap(vars(NAME))
  test %>% filter(NAME == "BDE99") %>% lm(Conc ~ Year, .) %>% summary()  # no remaining time trend
  test %>% filter(NAME == "PFOS") %>% lm(Conc ~ Year, .) %>% summary()
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


sim_get_means_OLD <- function(n_years, change, mean){
  x <- 1:n_years
  y_1 <- change*(x-x[1])
  y <- y_1 - mean(y_1) + mean
  data.frame(x, y)
}

sim_get_means <- function(years, change, mean){
  x <- years - min(years) + 1 
  y_1 <- change*(x-x[1])
  y <- y_1 - mean(y_1) + mean
  data.frame(Year = years, logVALUE = y)
}

if (FALSE){
  sim_get_means(10, change = 0.2, mean = 5)
}

# NOTE: year must be called "Year"  
sim_get_rawdata_oneyear <- function(data, year, x, yvar, adjusted_mean = 0, extravars = NULL){
  result <- data %>% filter(Year %in% year) %>% mutate(x = x, .before = 1)
  result$y_sim <- result[[yvar]] - mean(result[[yvar]]) + adjusted_mean
  result[c("x", "Year", yvar, "y_sim", extravars)]
}

if (FALSE){
  test1 <- data.frame(Year = 1991:2020, variable = 1:30) 
  test2 <- data.frame(Year = rep(1991:2020, each = 3), variable = rep(1:30, each = 3) + rep(c(-0.1,0,0.1), 30))
  sim_get_rawdata_oneyear(test2, year = 2000, x = 1, yvar = "variable", adjusted_mean = 4)
  # real data
  dat %>% filter(LATIN_NAME %in% "Brown trout" & TISSUE_NAME %in% "Muscle") %>%
    mutate(over_loq = is.na(FLAG1)) %>%
    count(NAME, over_loq)
  test3 <- dat %>% filter(LATIN_NAME %in% "Brown trout" & TISSUE_NAME %in% "Muscle" & NAME %in% "BDE126")
  ggplot(test3, aes(Year, VALUE, color = is.na(FLAG1))) +
    geom_point()
  sim_get_rawdata_oneyear(test3, year = 2021, x = 1, yvar = "VALUE", adjusted_mean = 4, extravars = "FLAG1")
}

sim_get_rawdata <- function(data, yvar = "VALUE", n_years, change, mean = 2, extravars = NULL){
  means <- sim_get_means(n_years=n_years, change=change, mean=mean)
  n <- nrow(means)
  yrs_pick <- data %>%
    pull(Year) %>%
    unique() %>%
    sample(size = n, replace = TRUE)
  1:n %>% map_dfr(
    \(i) {
      sim_get_rawdata_oneyear(data, year = yrs_pick[i], x = i, yvar = yvar, adjusted_mean = means$y[i], extravars = extravars)
    }
  )
}

if (FALSE){
  # test data
  test1 <- data.frame(Year = 1991:2020, variable = 1:30)
  test2 <- data.frame(Year = rep(1991:2020, each = 3), variable = rep(1:30, each = 3))
  # debugonce(sim_get_rawdata)
  # debugonce(sim_get_rawdata_oneyear)
  sim_get_rawdata(test2, yvar = "variable", n_years = 10, change = 0.2, mean = 7)
  # real data
  test3 <- dat %>% filter(LATIN_NAME %in% "Brown trout" & TISSUE_NAME %in% "Muscle" & NAME %in% "BDE126")
  result <- sim_get_rawdata(test3, yvar = "VALUE", n_years = 3, change = 0.2, mean = 5, extravars = "FLAG1")
  ggplot(result, aes(x = Year, y = VALUE, color = FLAG1)) + geom_point()
  ggplot(result, aes(x = x, y = y_sim, color = FLAG1)) + geom_point()
}

#
# Get regression results (on log scale)\
# - using oridnary regression (on all data, ignoring the FLAG1 ciolumn) or NADA::cencorreg
# - note that cencorreg uses log-transformed data by default
#

sim_get_regression_results <- function(data, xvar = "Year", 
                                       yvar = "VALUE", 
                                       flagvar = "FLAG1", 
                                       method = "lm",
                                       LOG = TRUE){
  data$y <- data[[yvar]]
  data$x <- data[[xvar]]
  if (method == "lm"){
    if (LOG){
      data$y <- log(data$y)
    }
    mod <- lm(y~x, data)
    result <- summary(mod)$coef[2,]
    names(result)[4] <- "pvalue"
  } else if (method == "NADA"){
    data$censored <- !is.na(data[[flagvar]])
    X <- cencorreg(data$y, data$censored, data$x, LOG = LOG, verbose = 0)
    Estimate <- summary(X)$table[2, 1]
    Std_error <- summary(X)$table[2, 2]
    loglik1 <- X$loglik[1]
    loglik2 <- X$loglik[1]
    chisq <- 2*diff(X$loglik)
    pvalue <- pchisq(chisq, df = 1, lower.tail = FALSE)
    result <- data.frame(Estimate, Std_error,  loglik1, loglik2, chisq, pvalue)
  }
  result
}

if (FALSE){

  test3 <- dat %>% 
    # filter(LATIN_NAME %in% "Brown trout" & TISSUE_NAME %in% "Muscle" & NAME %in% "BDE99") %>%
    filter(LATIN_NAME %in% "Brown trout" & TISSUE_NAME %in% "Muscle" & NAME %in% "BDE126") %>%
    mutate(log_conc = log(VALUE))
  ggplot(test3, aes(Year, log_conc, color = is.na(FLAG1))) +
    geom_point()
  # debugonce(sim_get_regression_results)
  sim_get_regression_results(test3)
  sim_get_regression_results(test3, method = "NADA")

}




#
# Get list of data frames with concentration of given parameters  
#

# as 'draw_concentrations' but with Conc

draw_concentrations2 <- function(params, ..., data = dat){
  samples <- draw_samples(..., data = dat)  
  names(samples) <- seq_along(samples)
  data <- ungroup(data)
  map_dfr(samples, 
          \(x) data %>% 
            filter(SAMPLE_ID %in% x & NAME %in% params) %>% 
            select(LATIN_NAME, STATION_NAME, TISSUE_NAME, Year, NAME, Conc, FLAG1),
          .id = "Pooled_sample")
}

if (FALSE){
  # TEST
  unique(dat$NAME)
  dat %>% count(NAME, Year) %>% View()
  # debugonce(draw_concentrations)
  # draw_concentrations(c("PFOSA", "PFOS"), 
  #                     "Salmo trutta", "Mjøsa", "Muskel", 2018, 4, data = dat)  
  
  draw_concentrations2(c("PFUnDA"), "Brown trout", "Mjøsa", "Muscle", 2018, 3, data = dat_sel)  
  
  
}

get_manipulated_data_oneseries <- function(data, given_change){
  
  # Annual expected value, as observed
  mod <- lm(logVALUE ~ Year, data)
  df_yr <- data %>% distinct(Year)
  df_means_orig <- data.frame(
    Year = df_yr$Year, 
    logVALUE = predict(mod, df_yr))
  
  # Annual expected value, mean
  mean <- mean(data$logVALUE)
  df_means_new <- sim_get_means(df_yr$Year, change = given_change, mean = mean)
  
  data_manip <- data %>%
    left_join(df_means_orig %>% rename(mean_orig = logVALUE),
              by = join_by(Year)) %>%
    left_join(df_means_new %>% rename(mean_new = logVALUE),
              by = join_by(Year)) %>%
    mutate(
      logVALUE_orig = logVALUE,
      logVALUE = logVALUE_orig - mean_orig + mean_new,
      VALUE = exp(logVALUE)
    )
  
  data_manip
  
}

if (FALSE){
  
  dat_test <- data.frame(Year = rep(c(2001,2002,2005), each = 3),
                         logVALUE = rep(c(20,15,5), each = 3) + rnorm(9),
                         SAMPLE_ID = 1:9)
  
  dat_test2 <- dat_test %>% get_manipulated_data_oneseries(given_change = 5)
  
  cowplot::plot_grid(
    ggplot(dat_test, 
           aes(Year, logVALUE)) + 
      geom_point(),
    ggplot(dat_test2, 
           aes(Year, logVALUE)) + 
      geom_point()
  )

}


get_manipulated_lm_oneseries_onechange <- function(data, given_change,
                                         no_samples, no_draws){
  
  yrs <- data %>% pull(Year) %>% unique()  
  
  data_pooled <- draw_pooled_means(name_order, "Brown trout", "Mjøsa", "Muscle", yrs, 
                                       no_samples = no_samples, no_draws = no_draws, 
                                       data = get_manipulated_data_oneseries(
                                         data, 
                                         given_change = 0)) %>%
    mutate(logVALUE = log(VALUE))
  
  ### Test 4 - Rrgression of pooled data  
  # ?reframe
  data_pooled %>%
    nest_by(NAME, Draw) %>%
    mutate(model = list(lm(logVALUE ~ Year, data = data))) %>%
    summarise(tidy(model), .groups = "drop")%>%
    filter(term == "Year")
  
}

if (FALSE){
  
  dat_test <- data.frame(Year = rep(c(2001,2002,2005), each = 3),
                         logVALUE = rep(c(20,15,5), each = 3) + rnorm(9),
                         FLAG1 = NA,
                         SAMPLE_ID = 1:9,
                         NAME = "x", LATIN_NAME = "Brown trout", 
                         STATION_NAME = "Mjøsa", TISSUE_NAME = "Muscle")
  
  debugonce(get_manipulated_lm_oneseries_onechange)
  debugonce(draw_pooled_means_single)
  debugonce(draw_concentrations)
  debugonce(draw_samples)
  get_manipulated_lm_oneseries_onechange(dat_test, given_change = 5, no_samples = 3, no_draws = 5)
  
  
}

