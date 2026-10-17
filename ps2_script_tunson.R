# Tesahe Tunson, Problem Set 2, EC 422

library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(region = case_when(
    symbol == "UNRATE" ~ "United States",
    symbol == "ORUR"   ~ "Oregon",
    symbol == "CAUR"   ~ "California",
    symbol == "TXUR"   ~ "Texas",
    symbol == "NYUR"   ~ "New York"
  ))


unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_summary

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary

unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n = 5)

unemp_top_increases

unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n = 5)

unemp_top_decreases

# 4.4
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(order_by = unemployment_rate_change, n = 5)

unemp_top_increases_no_covid

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(order_by = unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid

unemp_no_covid |> filter(year(date) == 2020)

#Part 5
# 5.1 

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rate, 1976-Present", 
    x = "Date", 
    y = "Unemployment rate (%, seasonally adjusted)",
    color = "Region"
  ) +
  theme_minimal()

# 5.2

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate by Region, 1976-Present",
    x = "Date",
    y = "Unemployment rate (%, seasonally adjusted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 5.3

ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col(fill = "blue") +
  labs(
    title = "Average Unemployment Rate by Region, 1976-Present",
    x = "Region",
    y = "Mean unemployment rate (%)"
  ) +
  theme_minimal()

# 5.4
unemp |>
  filter(date >= as.Date("2019-01-01"), date <= as.Date("2022-12-01")) |>
  ggplot(aes(x = date, y = unemployment_rate, color = region)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") +
  labs(
    title = "Unemployment Rates Through the COVID-19 Shock, 2019–2022",
    subtitle = "Dashed line marks March 2020 (start of pandemic shutdowns)",
    x = "Date",
    y = "Unemployment rate (%, seasonally adjusted)",
    color = "Region"
  ) +
  theme_minimal()

# I plotted a specific spike of unemployment rates in March 2020 following the beginning of the
# COVID-19 pandemic. Compared to the national average in pink, the rates of New York and California
# were much higher than Oregon and Texas, possibly due to having more densly populated urban areas.
