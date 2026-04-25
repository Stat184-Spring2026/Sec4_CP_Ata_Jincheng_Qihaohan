# Weather Analysis for State College, PA ----
# Style Guide: tidyverse style guide (https://style.tidyverse.org/)
# Project: STAT 184 Weather Patterns in State College, PA
# Data source: NOAA Climate Data Online, station GHCND:USC00368449

# Load Packages ----
library(tidyverse)
library(lubridate)
library(knitr)
library(scales)

# Read Data ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

weather_raw <- read_csv("SC_weather_10.csv", show_col_types = FALSE)

# Clean and Tidy Data ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

weather_clean <- weather_raw |>
  janitor::clean_names() |>
  mutate(
    date = as_date(date),
    year = year(date),
    month_num = month(date),
    month = month(date, label = TRUE, abbr = TRUE),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA_character_
    ),
    season = factor(
      season,
      levels = c("Winter", "Spring", "Summer", "Fall")
    ),
    temp_range = tmax - tmin,
    rain_day = prcp > 0,
    snow_day = snow > 0,
    heavy_prcp_day = prcp >= 1,
    freezing_day = tmin <= 32
  ) |>
  select(
    station,
    name,
    latitude,
    longitude,
    elevation,
    date,
    year,
    month_num,
    month,
    season,
    tmax,
    tmin,
    temp_range,
    prcp,
    snow,
    snwd,
    rain_day,
    snow_day,
    heavy_prcp_day,
    freezing_day
  )

# Export Cleaned Data ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

write_csv(weather_clean, "weather_clean.csv")

# Summary Tables ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

annual_weather_summary <- weather_clean |>
  group_by(year) |>
  summarize(
    days_observed = n(),
    avg_tmax = mean(tmax, na.rm = TRUE),
    avg_tmin = mean(tmin, na.rm = TRUE),
    avg_temp_range = mean(temp_range, na.rm = TRUE),
    total_prcp = sum(prcp, na.rm = TRUE),
    total_snow = sum(snow, na.rm = TRUE),
    max_snow_depth = max(snwd, na.rm = TRUE),
    rain_days = sum(rain_day, na.rm = TRUE),
    snow_days = sum(snow_day, na.rm = TRUE),
    freezing_days = sum(freezing_day, na.rm = TRUE),
    .groups = "drop"
  )

monthly_weather_summary <- weather_clean |>
  group_by(month_num, month) |>
  summarize(
    avg_tmax = mean(tmax, na.rm = TRUE),
    avg_tmin = mean(tmin, na.rm = TRUE),
    median_prcp = median(prcp, na.rm = TRUE),
    total_prcp = sum(prcp, na.rm = TRUE),
    total_snow = sum(snow, na.rm = TRUE),
    rain_days = sum(rain_day, na.rm = TRUE),
    snow_days = sum(snow_day, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(month_num)

seasonal_weather_summary <- weather_clean |>
  group_by(season) |>
  summarize(
    avg_tmax = mean(tmax, na.rm = TRUE),
    avg_tmin = mean(tmin, na.rm = TRUE),
    avg_temp_range = mean(temp_range, na.rm = TRUE),
    total_prcp = sum(prcp, na.rm = TRUE),
    total_snow = sum(snow, na.rm = TRUE),
    rain_days = sum(rain_day, na.rm = TRUE),
    snow_days = sum(snow_day, na.rm = TRUE),
    .groups = "drop"
  )

# Professional Tables for Report ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

annual_weather_table <- annual_weather_summary |>
  mutate(
    avg_tmax = round(avg_tmax, 1),
    avg_tmin = round(avg_tmin, 1),
    avg_temp_range = round(avg_temp_range, 1),
    total_prcp = round(total_prcp, 2),
    total_snow = round(total_snow, 1),
    max_snow_depth = round(max_snow_depth, 1)
  ) |>
  rename(
    Year = year,
    `Days Observed` = days_observed,
    `Avg. Maximum Temp` = avg_tmax,
    `Avg. Minimum Temp` = avg_tmin,
    `Avg. Daily Temp Range` = avg_temp_range,
    `Total Precipitation` = total_prcp,
    `Total Snowfall` = total_snow,
    `Max Snow Depth` = max_snow_depth,
    `Rain Days` = rain_days,
    `Snow Days` = snow_days,
    `Freezing Days` = freezing_days
  )

monthly_weather_table <- monthly_weather_summary |>
  mutate(
    avg_tmax = round(avg_tmax, 1),
    avg_tmin = round(avg_tmin, 1),
    median_prcp = round(median_prcp, 2),
    total_prcp = round(total_prcp, 2),
    total_snow = round(total_snow, 1)
  ) |>
  select(-month_num) |>
  rename(
    Month = month,
    `Avg. Maximum Temp` = avg_tmax,
    `Avg. Minimum Temp` = avg_tmin,
    `Median Daily Precipitation` = median_prcp,
    `Total Precipitation` = total_prcp,
    `Total Snowfall` = total_snow,
    `Rain Days` = rain_days,
    `Snow Days` = snow_days
  )

# Exploratory Data Visualizations ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

fig_annual_temperature <- ggplot(annual_weather_summary, aes(x = year)) +
  geom_line(aes(y = avg_tmax, linetype = "Average maximum"), linewidth = 1) +
  geom_point(aes(y = avg_tmax, shape = "Average maximum"), size = 2) +
  geom_line(aes(y = avg_tmin, linetype = "Average minimum"), linewidth = 1) +
  geom_point(aes(y = avg_tmin, shape = "Average minimum"), size = 2) +
  scale_x_continuous(breaks = annual_weather_summary$year) +
  labs(
    title = "Average Daily Temperature by Year in State College, PA",
    subtitle = "NOAA daily weather station data",
    x = "Year",
    y = "Temperature (°F)",
    linetype = "Measure",
    shape = "Measure",
    caption = "Data source: NOAA Climate Data Online, GHCND:USC00368449"
  ) +
  theme_minimal()

fig_monthly_precipitation <- ggplot(
  monthly_weather_summary,
  aes(x = month, y = total_prcp)
) +
  geom_col() +
  labs(
    title = "Total Precipitation by Month in State College, PA",
    subtitle = "Monthly totals aggregated across the ten-year dataset",
    x = "Month",
    y = "Total precipitation",
    caption = "Data source: NOAA Climate Data Online, GHCND:USC00368449"
  ) +
  theme_minimal()

fig_seasonal_temperature_boxplot <- ggplot(
  weather_clean,
  aes(x = season, y = tmax)
) +
  geom_boxplot() +
  labs(
    title = "Distribution of Daily Maximum Temperature by Season",
    subtitle = "Boxplots show seasonal differences in daily high temperatures",
    x = "Season",
    y = "Daily maximum temperature (°F)",
    caption = "Data source: NOAA Climate Data Online, GHCND:USC00368449"
  ) +
  theme_minimal()

fig_annual_snowfall <- ggplot(
  annual_weather_summary,
  aes(x = year, y = total_snow)
) +
  geom_col() +
  scale_x_continuous(breaks = annual_weather_summary$year) +
  labs(
    title = "Total Snowfall by Year in State College, PA",
    subtitle = "Annual snowfall totals from daily NOAA records",
    x = "Year",
    y = "Total snowfall",
    caption = "Data source: NOAA Climate Data Online, GHCND:USC00368449"
  ) +
  theme_minimal()

fig_precipitation_distribution <- ggplot(
  weather_clean,
  aes(x = prcp)
) +
  geom_histogram(binwidth = 0.1, boundary = 0) +
  labs(
    title = "Distribution of Daily Precipitation",
    subtitle = "Most days have little or no measured precipitation",
    x = "Daily precipitation",
    y = "Number of days",
    caption = "Data source: NOAA Climate Data Online, GHCND:USC00368449"
  ) +
  theme_minimal()

# Save Figures ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

if (!dir.exists("figures")) {
  dir.create("figures")
}

ggsave(
  filename = "figures/annual_temperature.png",
  plot = fig_annual_temperature,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "figures/monthly_precipitation.png",
  plot = fig_monthly_precipitation,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "figures/seasonal_temperature_boxplot.png",
  plot = fig_seasonal_temperature_boxplot,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "figures/annual_snowfall.png",
  plot = fig_annual_snowfall,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "figures/precipitation_distribution.png",
  plot = fig_precipitation_distribution,
  width = 7,
  height = 4.5,
  dpi = 300
)

# Optional: Display Tables in Console ----
# Code Header:
# Primary author: Qihaohan
# Reviewer: Ata / Jincheng

kable(
  annual_weather_table,
  caption = "Annual weather summary for State College, PA."
)

kable(
  monthly_weather_table,
  caption = "Monthly weather summary for State College, PA."
)
