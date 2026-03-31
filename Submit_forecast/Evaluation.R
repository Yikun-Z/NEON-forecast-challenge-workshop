install.packages("duckdbfs")
install.packages("scoringRules")

library(tidyverse)
library(scoringRules)
library(duckdbfs)

#Get this link from https://radiantearth.github.io/stac-browser/#/external/raw.githubusercontent.com/eco4cast/neon4cast-catalog/main/forecasts/Aquatics/Daily_Water_temperature/collection.json
# You will need to modify the link to remove the anonymous@ and the ?endpoint_override... part
all_results <- open_dataset("s3://bio230014-bucket01/challenges/forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature", 
                            s3_endpoint = "sdsc.osn.xsede.org", 
                            anonymous = TRUE)

# Get today's forecast from three models for a particular site and reference_datetime
my_forecasts <- all_results |> 
  filter(reference_datetime == as_datetime("2026-03-24 00:00:00"),
         site_id == "BARC",
         model_id %in% c("climatology", "persistenceRW", "flareGLM")) |> 
  collect()


# Plot a parametric forecast
my_forecasts |> 
  filter(model_id == "climatology") |> 
  pivot_wider(names_from = parameter, values_from = prediction) |> 
  mutate(lower_2.5 = mu - 1.96*sigma,
         upper_97.5 = mu + 1.96*sigma) |> 
  ggplot(aes(x = datetime)) +
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_97.5), fill = "lightblue") +
  geom_line(aes(y = mu))

# Plot an ensemble forecast
my_forecasts |> 
  filter(model_id == "persistenceRW") |> 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line()

# Plot the third model
my_forecasts |> 
  filter(model_id == "flareGLM") |> 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line(alpha = 0.5, color = "orange")

# Get a past forecast from THREE models for a particular site and reference_datetime
my_forecasts <- all_results |> 
  filter(reference_datetime == as_datetime("2025-03-24 00:00:00"),
         site_id == "BARC",
         model_id %in% c("climatology", "persistenceRW", "flareGLM")) |> 
  collect()

# Single forecast
single_forecast_ensemble <- my_forecasts |> 
  filter(model_id == "persistenceRW",
         datetime == as_datetime("2025-03-27 00:00:00"))

# Get the target data and read in
url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"
targets <- read_csv(url)

# Filter to only the datetime, variable, and site_id that match our single forecast
targets_single_forecast <- targets |> 
  filter(datetime == as_datetime("2025-03-27 00:00:00"),
         variable == "temperature",
         site_id == "BARC")

# Calculate CRPS
crps_sample(y = targets_single_forecast$observation,
            dat = single_forecast_ensemble$prediction)

# Calculate Log Score
logs_sample(y = targets_single_forecast$observation,
            dat = single_forecast_ensemble$prediction)

# Get a single parameteric forecast
single_forecast_parametric <- my_forecasts |> 
  filter(model_id == "climatology",
         datetime == as_datetime("2025-03-27 00:00:00")) |> 
  pivot_wider(names_from = parameter, values_from = prediction) 

# Calculate CRPS for the parametric distribution
crps_norm(y = targets_single_forecast$observation, 
          mean = single_forecast_parametric$mu,
          sd = single_forecast_parametric$sigma)


# Score multiple forecasts
# Added "flareGLM" into this filter so both ensemble models get scored together
multi_forecast_ensemble <- my_forecasts |> 
  filter(model_id %in% c("persistenceRW", "flareGLM")) |> 
  left_join(targets, by = c("datetime", "site_id", "variable")) |> 
  select(model_id, datetime, prediction, observation, parameter) |>
  drop_na(observation) # Small fix to avoid CRPS errors if observation is NA

multi_forecast_parametric <- my_forecasts |> 
  filter(model_id == "climatology") |> 
  left_join(targets, by = c("datetime", "site_id", "variable")) |> 
  select(model_id, datetime, prediction, observation, parameter) |> 
  pivot_wider(names_from = parameter, values_from = prediction) |>
  drop_na(observation)

normal_crps <- multi_forecast_parametric |> 
  group_by(datetime, model_id) |> 
  summarize(crps = crps_norm(observation, mean = mu, sd = sigma), .groups = "drop")

# Scoring multiple forecaasts using crps_sample in a group_by summarize
crps_tidy_ensemble <- function(prediction, observation) {
  obs <- observation[1] 
  crps_sample(obs, prediction)
}

# Use the function
ensemble_crps <- multi_forecast_ensemble |> 
  group_by(datetime, model_id) |> 
  summarize(crps = crps_tidy_ensemble(prediction, observation), .groups = "drop") 

# Plot the histogram of the scores (Added basic transparency so you can see all 3 colors)
bind_rows(normal_crps, ensemble_crps) |> 
  ggplot(aes(x = crps, fill = model_id)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30)