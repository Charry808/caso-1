library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(moments)
library(lubridate)
install.packages("moments")
library(moments)
setwd("/Users/manuelagranadoshernandez/Documents/GitHub/caso-1/")
cost_path <- "hoja1caso.csv"
read.csv(caso_path)
# Instalar y cargar paquetes necesarios
packages <- c("tidyverse","lubridate","janitor","readr")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

parse_week <- function(x) lubridate::mdy(x)
weekly <- weekly |>
  mutate(week = parse_week(week_2008_2009)) |>
  rename(
    visits = visits,
    unique_visits = unique_visits,
    pageviews = pageviews,
    pages_per_visit = pages_visit,
    avg_time_secs = avg_time_on_site_secs,
    bounce_rate = bounce_rate,
    pct_new_visits = x_new_visits
  )
  financials <- financials |>
  mutate(week = parse_week(week_2008_2009)) |>
  select(week, revenue, profit, lbs_sold, inquiries)
  df <- weekly |>
  inner_join(financials, by = "week") |>
  arrange(week)
  bp <- strucchange::breakpoints(visits ~ 1, data = df, breaks = 3) 
cuts_idx <- bp$breakpoints
if (sum(!is.na(cuts_idx)) == 3) {
  brks <- c(0, cuts_idx, nrow(df))
  df <- df |>
    mutate(period = cut(row_number(), brks,
                        labels = c("Initial","Pre-Promo","Promotion","Post-Promo"),
                        include.lowest = TRUE))
} else  else 
  df <- df |>
    mutate(period = case_when(
      week <= as.Date("2008-08-02") ~ "Initial",
      week <= as.Date("2008-12-20") ~ "Pre-Promo",
      week <= as.Date("2009-03-07") ~ "Promotion",
      TRUE ~ "Post-Promo"
    ))
  df$period <- factor(df$period, levels = c("Initial","Pre-Promo","Promotion","Post-Promo"))

