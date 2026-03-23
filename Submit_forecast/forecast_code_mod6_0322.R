# install.packages('remotes')
# install.packages('tidyverse') # collection of R packages for data manipulation, analysis, and visualisation
# install.packages('lubridate') # working with dates and times
# remotes::install_github('eco4cast/neon4cast') # package from NEON4cast challenge organisers to assist with forecast building and submission

# ------ Load packages -----
library(tidyverse)
library(lubridate)
library(slider)
#--------------------------#

# Change this for your model ID
# Include the word "example" in my_model_id for a test submission
# Don't include the word "example" in my_model_id for a forecast that you have registered (see neon4cast.org for the registration form)
my_model_id <- 'Ye_0322_forecast'

# --Model description--- #

# The model uses a linear regression model to predict daily water temperature using air temperature, the previous day’s water temperature, and the square of the air temperature. The model is fitted using historical observations and weather data, and then used to generate forecasts for the next 30 days.

# -- Uncertainty representation -- #

# Three sources of uncertainty (driver uncertainty, parameter uncertainty, and process uncertainty) are included in the forecast. Driver uncertainty is represented by the 31-member NOAA weather forecast ensemble. Parameter uncertainty is represented by drawing regression coefficients from normal distributions based on their estimated standard errors. Process uncertainty is represented by adding random noise based on the residual standard deviation of the fitted model.

#------- Read data --------
# read in the targets data
targets <- read_csv("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz")

# read in the sites data
aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/refs/heads/main/neon4cast_field_site_metadata.csv") |>
  dplyr::filter(aquatics == 1)

focal_sites <- aquatic_sites |> 
  filter(field_site_subtype == 'Lake') |> 
  pull(field_site_id)

# Filter the targets
targets <- targets %>%
  filter(site_id %in% focal_sites,
         variable == 'temperature')
#--------------------------#



# ------ Weather data ------
met_variables <- c("air_temperature")

# Past stacked weather -----
weather_past_s3 <- neon4cast::noaa_stage3()

weather_past <- weather_past_s3  |> 
  dplyr::filter(site_id %in% focal_sites,
                datetime >= ymd('2017-01-01'),
                variable %in% met_variables) |> 
  dplyr::collect()

# aggregate the past to mean values
weather_past_daily <- weather_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  # convert air temperature to Celsius if it is included in the weather data
  mutate(prediction = ifelse(variable == "air_temperature", prediction - 273.15, prediction)) |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  # add air_temp_sq
  mutate(air_temp_sq = air_temperature^2)

# Future weather forecast --------
# New forecast only available at 5am UTC the next day
forecast_date <- Sys.Date() 
noaa_date <- forecast_date - days(1)

weather_future_s3 <- neon4cast::noaa_stage2(start_date = as.character(noaa_date))

weather_future <- weather_future_s3 |> 
  dplyr::filter(datetime >= forecast_date,
                site_id %in% focal_sites,
                variable %in% met_variables) |> 
  collect()

weather_future_daily <- weather_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  # convert air temperature to Celsius if it is included in the weather data
  mutate(prediction = ifelse(variable == "air_temperature", prediction - 273.15, prediction)) |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  mutate(air_temp_sq = air_temperature^2) |> 
  select(any_of(c('datetime', 'site_id', 'air_temperature', 'air_temp_sq', 'parameter')))

weather_future_daily <- weather_future_daily |>
  mutate(air_temp_sq = air_temperature^2)
#--------------------------#

forecast_horizon <- 30
forecasted_dates <- seq(from = ymd(forecast_date), to = ymd(forecast_date) + forecast_horizon, by = "day")
n_members <- 310

# ----- Fit model & generate forecast----

# Generate a dataframe to fit the model to 
targets_lm <- targets |> 
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(weather_past_daily, 
            by = c("datetime","site_id")) |>
  arrange(site_id, datetime) |> 
  group_by(site_id) |> 
  mutate(temp_lag = lag(temperature)) |> 
  ungroup() |> 
  filter(!is.na(temp_lag), !is.na(air_temp_sq))

# Loop through each site to fit the model
forecast_df <- NULL

for(i in 1:length(focal_sites)) {  
  
  curr_site <- focal_sites[i]
  
  site_target <- targets_lm |>
    filter(site_id == curr_site)
  
  # initial uncertainty
  ic_sd <- 0.1
  curr_wt <- tail(site_target$temperature, 1)
  ic_uc <- rnorm(n = n_members, mean = curr_wt, sd = ic_sd)
  
  noaa_future_site <- weather_future_daily |> 
    filter(site_id == curr_site)
  
  weather_ensemble_names <- unique(noaa_future_site$parameter)
  
  #Fit linear model based on past data: water temperature = m * air temperature + b
  #you will need to change the variable on the left side of the ~ if you are forecasting oxygen or chla
  fit <- lm(temperature ~ air_temperature + temp_lag + air_temp_sq, data = site_target)
  
  coef_est <- coef(fit)
  coef_se  <- summary(fit)$coefficients[,2]
  model_sigma <- summary(fit)$sigma
  
  met_members <- unique(weather_future_daily$parameter)
  n_met <- length(met_members)
  
  # Loop over each ensemble member
  for(ens in 1:n_members){
    
    met_ens <- met_members[((ens - 1) %% n_met) + 1]
    last_obs_temp <- ic_uc[ens]
    
    beta0 <- rnorm(1, coef_est["(Intercept)"], coef_se["(Intercept)"])
    beta1 <- rnorm(1, coef_est["air_temperature"], coef_se["air_temperature"])
    beta2 <- rnorm(1, coef_est["temp_lag"], coef_se["temp_lag"])
    beta3 <- rnorm(1, coef_est["air_temp_sq"], coef_se["air_temp_sq"])
    
    # Loop through all forecast dates
    for (t in 1:length(forecasted_dates)) {
      
      # use linear regression to forecast water temperature for each ensemble member
      # You will need to modify this line of code if you add additional weather variables or change the form of the model
      # The model here needs to match the model used in the lm function above (or what model you used in the fit)
        
      #pull driver ensemble for the relevant date; here we are using all 31 NOAA ensemble members
      temp_driv <- weather_future_daily %>%
        filter(datetime == forecasted_dates[t],
               site_id == curr_site,
               parameter == met_ens)
      
      forecasted_temperature <- beta0 + beta1 * temp_driv$air_temperature + beta2 * last_obs_temp + beta3 * temp_driv$air_temp_sq + rnorm(1, mean = 0, sd = model_sigma)
      
      # put all the relevant information into a tibble that we can bind together
      curr_site_df <- tibble(datetime = forecasted_dates[t],
                             site_id = curr_site,
                             parameter = as.character(ens),
                             prediction = forecasted_temperature,
                             variable = "temperature") #Change this if you are forecasting a different variable
     
      forecast_df <- dplyr::bind_rows(forecast_df, curr_site_df)
      
      last_obs_temp <- forecasted_temperature
    }
  }
  
  message(curr_site, ' forecast run')
  
}


#---- Covert to EFI standard ----

# Make forecast fit the EFI standards
forecast_df_EFI <- forecast_df %>%
  filter(datetime > forecast_date) %>%
  mutate(model_id = my_model_id,
         reference_datetime = forecast_date,
         family = 'ensemble',
         duration = 'P1D',
         parameter = as.character(parameter),
         project_id = 'neon4cast') %>%
  select(datetime, reference_datetime, duration, site_id, family, parameter, variable, prediction, model_id, project_id)
#---------------------------#



# ----- Submit forecast -----
# Write the forecast to file
theme <- 'aquatics'
date <- forecast_df_EFI$reference_datetime[1]
forecast_name <- paste0(forecast_df_EFI$model_id[1], ".csv")
forecast_file <- paste(theme, date, forecast_name, sep = '-')

write_csv(forecast_df_EFI, forecast_file)

neon4cast::forecast_output_validator(forecast_file)


neon4cast::submit(forecast_file =  forecast_file, ask = FALSE) # if ask = T (default), it will produce a pop-up box asking if you want to submit

#--------------------------#

forecast_df_EFI |> 
  ggplot(aes(x=datetime, y=prediction, group = parameter)) +
  geom_line() +
  facet_wrap(~site_id, scales = "free_y") +
  labs(title = paste0('Forecast generated for ', forecast_df_EFI$variable[1], ' on ', forecast_df_EFI$reference_datetime[1]))

plot_file_name <- paste0("Submit_forecast/", forecast_df_EFI$variable[1], '-', forecast_df_EFI$reference_datetime[1], ".png")
ggsave(plot_file_name)
