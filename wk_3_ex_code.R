library(tsibble)
library(fable)
library(tidyverse)
library(knitr)
library(tsibbledata)

# Maximum number of forecast horizons
H <- 4
# First origin: 20 years of monthly data
INIT <- 240

# Filter for the specific series: Victoria restaurant sales
cafes <- aus_retail |>
  filter(
    State == "Victoria",
    Industry == "Cafes, restaurants and catering services"
  )

origins_expand <- cafes |>
  stretch_tsibble(.init = INIT, .step = 1)

tscv_expand_fc <- origins_expand |>
  model(mean = MEAN(Turnover),
        snaive = SNAIVE(Turnover),
        drift = RW(Turnover ~ drift()),
        tslm = TSLM(Turnover ~ trend() + season())) |>
  forecast(h = H) |>
  filter(Month <= max(cafes$Month)) |>
  group_by(.id, .model) |> mutate(h = row_number()) |> ungroup() |>
  as_fable(response = "Turnover", distribution = Turnover)


acc_expand <- tscv_expand_fc |>
  accuracy(cafes, by = c("h", ".model")) |>
  select(h, .model, MAE) |>
  pivot_wider(names_from = .model, values_from = c(MAE)) |>
  kable(digits = 2, caption = "MAE by Model and Horizon")

head(acc_expand)
