# ------ Load packages -----
library(tidyverse)
library(lubridate)
library(slider)
library(duckdbfs)
library(scoringRules)
#--------------------------#

# model ID
my_model_id <- "Ye_0415_reforecast"

#------- Read data --------
# Read in the targets data
targets <- read_csv(
  "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"
)

# Only keep BARC and temperature
focal_sites <- "BARC"

targets <- targets %>%
  filter(site_id %in% focal_sites,
         variable == "temperature")
#--------------------------#


# ------ Weather data ------
met_variables <- c("air_temperature")

# Past stacked weather -----
weather_past_s3 <- neon4cast::noaa_stage3()

weather_past <- weather_past_s3 |>
  dplyr::filter(site_id %in% focal_sites,
                datetime >= ymd("2017-01-01"),
                variable %in% met_variables) |>
  dplyr::collect()

# Aggregate the past to mean daily values
weather_past_daily <- weather_past |>
  mutate(datetime = as_date(datetime)) |>
  group_by(datetime, site_id, variable) |>
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
  mutate(prediction = ifelse(variable == "air_temperature",
                             prediction - 273.15,
                             prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  mutate(air_temp_sq = air_temperature^2)
#--------------------------#


# ------ Reforecast settings ------
# Use one reforecast start date every day for one year
all_dates <- seq(ymd("2024-01-01"), ymd("2025-01-01"), by = "1 day")

# Forecast horizon and ensemble size
forecast_horizon <- 35
n_members <- 31

all_forecast_df <- NULL
#--------------------------#


# ----- Fit model & generate reforecasts -----
for (i in seq_along(all_dates)) {
  
  forecast_date <- all_dates[i]
  noaa_date <- forecast_date - days(1)
  
  forecasted_dates <- seq(from = forecast_date,
                          to = forecast_date + forecast_horizon,
                          by = "day")
  
  # Future weather forecast
  weather_future_s3 <- neon4cast::noaa_stage2(start_date = as.character(noaa_date))
  
  weather_future <- weather_future_s3 |>
    dplyr::filter(datetime >= forecast_date,
                  site_id %in% focal_sites,
                  variable %in% met_variables) |>
    collect()
  
  weather_future_daily <- weather_future |>
    mutate(datetime = as_date(datetime)) |>
    group_by(datetime, site_id, parameter, variable) |>
    summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |>
    mutate(prediction = ifelse(variable == "air_temperature",
                               prediction - 273.15,
                               prediction)) |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    mutate(air_temp_sq = air_temperature^2) |>
    select(any_of(c("datetime", "site_id", "air_temperature", "air_temp_sq", "parameter")))
  
  # Generate a dataframe to fit the model to
  targets_lm <- targets |>
    pivot_wider(names_from = "variable", values_from = "observation") |>
    left_join(weather_past_daily, by = c("datetime", "site_id")) |>
    arrange(site_id, datetime) |>
    group_by(site_id) |>
    mutate(temp_lag = lag(temperature)) |>
    ungroup() |>
    filter(!is.na(temp_lag), !is.na(air_temp_sq))
  
  forecast_df <- NULL
  
  for (j in 1:length(focal_sites)) {
    
    curr_site <- focal_sites[j]
    
    # Only use data available before the reforecast start date
    site_target <- targets_lm |>
      filter(site_id == curr_site,
             datetime < forecast_date)
    
    # Initial condition from the most recent available observation
    recent_obs <- site_target |>
      filter(!is.na(temperature)) |>
      arrange(desc(datetime)) |>
      slice(1)
    
    curr_wt <- recent_obs$temperature
    ic_sd <- 0.5
    ic_uc <- rnorm(n = n_members, mean = curr_wt, sd = ic_sd)
    
    # Fit linear model
    fit <- lm(temperature ~ air_temperature + temp_lag + air_temp_sq,
              data = site_target)
    
    coef_est <- coef(fit)
    coef_se  <- summary(fit)$coefficients[, 2]
    model_sigma <- summary(fit)$sigma
    
    # Use the 31 NOAA ensemble members once each
    met_members <- sort(unique(weather_future_daily$parameter))
    
    for (ens in 1:n_members) {
      
      met_ens <- met_members[ens]
      last_obs_temp <- ic_uc[ens]
      
      beta0 <- rnorm(1, coef_est["(Intercept)"], coef_se["(Intercept)"])
      beta1 <- rnorm(1, coef_est["air_temperature"], coef_se["air_temperature"])
      beta2 <- rnorm(1, coef_est["temp_lag"], coef_se["temp_lag"])
      beta3 <- rnorm(1, coef_est["air_temp_sq"], coef_se["air_temp_sq"])
      
      for (t in 1:length(forecasted_dates)) {
        
        temp_driv <- weather_future_daily %>%
          filter(datetime == forecasted_dates[t],
                 site_id == curr_site,
                 parameter == met_ens)
        
        if (nrow(temp_driv) == 0 || is.na(temp_driv$air_temperature[1])) next
        
        forecasted_temperature <- beta0 +
          beta1 * temp_driv$air_temperature[1] +
          beta2 * last_obs_temp +
          beta3 * temp_driv$air_temp_sq[1] +
          rnorm(1, mean = 0, sd = model_sigma)
        
        curr_site_df <- tibble(
          datetime = forecasted_dates[t],
          site_id = curr_site,
          parameter = as.character(ens),
          prediction = forecasted_temperature,
          variable = "temperature"
        )
        
        forecast_df <- dplyr::bind_rows(forecast_df, curr_site_df)
        
        last_obs_temp <- forecasted_temperature
      }
    }
    
    message(curr_site, " forecast run for ", forecast_date)
  }
  
  forecast_df <- forecast_df %>%
    mutate(reference_datetime = forecast_date)
  
  all_forecast_df <- bind_rows(all_forecast_df, forecast_df)
}
#--------------------------#


#---- Convert to EFI standard ----
forecast_df_EFI <- all_forecast_df %>%
  filter(datetime > reference_datetime) %>%
  mutate(model_id = my_model_id,
         family = "ensemble",
         duration = "P1D",
         parameter = as.character(parameter),
         project_id = "neon4cast") %>%
  select(datetime, reference_datetime, duration, site_id, family, parameter,
         variable, prediction, model_id, project_id)
#---------------------------#


# ----- Read baseline forecasts -----
all_results <- open_dataset(
  "s3://bio230014-bucket01/challenges/forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature",
  s3_endpoint = "sdsc.osn.xsede.org",
  anonymous = TRUE
)

baseline_forecasts <- all_results |>
  filter(
    model_id %in% c("climatology", "persistenceRW"),
    site_id == "BARC",
    reference_datetime >= as_datetime("2024-01-01 00:00:00"),
    reference_datetime <= as_datetime("2025-01-01 00:00:00")
  ) |>
  collect() |>
  mutate(
    datetime = as.Date(datetime),
    reference_datetime = as.Date(reference_datetime)
  )
#---------------------------#


# ----- Prepare target data for evaluation -----
targets_eval <- targets |>
  mutate(datetime = as.Date(datetime)) |>
  select(datetime, site_id, variable, observation)
#---------------------------#


# ----- CRPS functions -----
crps_tidy_ensemble <- function(prediction, observation) {
  pred <- prediction[!is.na(prediction)]
  obs <- observation[1]
  if (length(pred) == 0 || is.na(obs)) return(NA_real_)
  crps_sample(y = obs, dat = pred)
}
#---------------------------#


# ----- Evaluate your model -----
my_forecasts <- forecast_df_EFI |>
  mutate(
    datetime = as.Date(datetime),
    reference_datetime = as.Date(reference_datetime),
    model_id = my_model_id
  )

my_crps <- my_forecasts |>
  left_join(targets_eval, by = c("datetime", "site_id", "variable")) |>
  drop_na(observation) |>
  mutate(lead_time = as.numeric(datetime - reference_datetime)) |>
  group_by(model_id, reference_datetime, site_id, datetime, lead_time) |>
  summarize(crps = crps_tidy_ensemble(prediction, observation), .groups = "drop")
#---------------------------#


# ----- Evaluate persistenceRW -----
persistence_crps <- baseline_forecasts |>
  filter(model_id == "persistenceRW") |>
  left_join(targets_eval, by = c("datetime", "site_id", "variable")) |>
  drop_na(observation) |>
  mutate(lead_time = as.numeric(datetime - reference_datetime)) |>
  group_by(model_id, reference_datetime, site_id, datetime, lead_time) |>
  summarize(crps = crps_tidy_ensemble(prediction, observation), .groups = "drop")
#---------------------------#


# ----- Evaluate climatology -----
climatology_crps <- baseline_forecasts |>
  filter(model_id == "climatology") |>
  left_join(targets_eval, by = c("datetime", "site_id", "variable")) |>
  drop_na(observation) |>
  pivot_wider(names_from = parameter, values_from = prediction) |>
  mutate(lead_time = as.numeric(datetime - reference_datetime)) |>
  group_by(model_id, reference_datetime, site_id, datetime, lead_time) |>
  summarize(
    crps = crps_norm(y = observation[1], mean = mu[1], sd = sigma[1]),
    .groups = "drop"
  )
#---------------------------#


# ----- Combine CRPS from all models -----
all_crps <- bind_rows(
  my_crps,
  persistence_crps,
  climatology_crps
)

crps_summary <- all_crps |>
  group_by(model_id, lead_time) |>
  summarize(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")
#---------------------------#


# ----- Main comparison plot -----
p <- ggplot(crps_summary,
            aes(x = lead_time, y = mean_crps, color = model_id)) +
  geom_line(linewidth = 1) +
  labs(x = "Forecast horizon (days)",
       y = "Mean CRPS",
       color = "Model",
       title = "Reforecast performance comparison at BARC") +
  theme_bw()

print(p)

ggsave("crps_comparison_BARC.png", p, width = 7, height = 5)
#---------------------------#