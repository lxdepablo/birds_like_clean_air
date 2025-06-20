# ---- Script Metadata ----
# author: Luis X. de Pablo
# contact: luis.depablo@colorado.edu
# last modified: 5/29/25

# load libraries and set working directory ----
library(mgcv) # for GAMS
library(segmented) # for segmented model
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in bird data -----
birds_raw <- read_csv("bird_haboob.csv")
# read in environmental data ----
env_raw <- read_csv("weather_haboob.csv")

# wrangle bird data ----
birds_clean <- birds_raw %>%
  filter(value > 0.5) %>%  # drop rows with low confidence
  mutate(species = substr(name, 21, nchar(name)),
         timestamp = ymd_hms(timestamp)) %>%
  dplyr::select(c(timestamp, value, meta.vsn, species))

# get summary stats
birds_summary <- birds_clean %>%
  mutate(year = year(timestamp),
         month = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp)) %>%
  group_by(meta.vsn, year, month, day, hour) %>%
  summarize(richness = length(unique(species)), .groups = "drop") %>%
  mutate(date = ymd_h(paste(year, month, day, hour)))

# backfill zeroes for missing hours
birds_backfilled <- bind_rows(lapply(unique(birds_summary$meta.vsn), function(x){
  curr_node <- filter(birds_summary, meta.vsn == x)
  curr_backfilled <- data.frame(date = seq(min(curr_node$date),
                                           max(curr_node$date),
                                           by = "hour")) %>%
    left_join(curr_node, by = "date") %>%
    mutate(richness = ifelse(is.na(richness), 0, richness),
           meta.vsn = x) %>%
    ungroup() %>%
    dplyr::select(-c(year, month, day, hour))
}))

# get data just around haboob
birds_haboob <- birds_backfilled %>%
  filter(date %in% seq(ymd_h("2025-05-15 16"), ymd_h("2025-05-18 22"), by = "hour")) %>%
  mutate(tod = hour(date),
         tod_sin = sin(2 * pi * tod / 24),
         tod_cos = cos(2 * pi * tod / 24))

# wrangle env data ----
particles <- env_raw %>%
  filter(name %in% c("aqt.particle.pm1", "aqt.particle.pm2.5", "aqt.particle.pm10"))
pm10 <- env_raw %>%
  filter(name == "aqt.particle.pm10")

# visualize particles ----
ggplot(data = pm10, aes(x = timestamp, y = value)) +
  geom_line() +
  geom_vline(xintercept = ymd_h("2025-05-16 18"), col = "red", linetype = "dashed") +
  facet_wrap(~meta.vsn) +
  labs(x = "Date", y = "PM10 Concentration (ug/m^3)", title = "Particulate Matter Aligns with Haboob Onset") +
  theme_minimal()

# visualize bird biodiversity ----
ggplot(data = birds_haboob, aes(x = date, y = richness)) +
  geom_smooth() +
  geom_point() +
  geom_vline(xintercept = ymd_h("2025-05-16 18"), col = "red", linetype = "dashed") +
  facet_wrap(~meta.vsn) +
  theme_minimal() +
  labs(x = "Date", y = "Bird Species Richness")

ggplot(data = birds_haboob, aes(x = date, y = richness, col = meta.vsn)) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = ymd_h("2025-05-16 18"), col = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Date", y = "Daily Bird Species Richness")

# fit models ----
lm_base <- lm(richness ~ date, data = birds_haboob)
seg_model <- segmented(lm_base, seg.Z = ~date, psi = list(date = ymd_h("2025-05-16 18")))
summary(seg_model)

# convert date to numeric (days since start)
birds_haboob <- birds_haboob %>%
  mutate(date_numeric = as.numeric(difftime(date, min(date), units = "days"))) %>%
  filter(!is.na(meta.vsn)) %>%
  mutate(meta.vsn = as.factor(meta.vsn))

# fit GAM with cyclic time-of-day effect and random site effect
gam_model <- gam(
  richness ~ s(date_numeric) + tod_sin + tod_cos + s(meta.vsn, bs = "re"),
  data = birds_haboob
)
summary(gam_model)

# visualize smooth effect of time ----
newdata <- birds_haboob %>%
  dplyr::select(date_numeric) %>%
  distinct() %>%
  arrange(date_numeric) %>%
  mutate(
    tod_sin = 0,
    tod_cos = 0,
    meta.vsn = NA
  )

all_newdata <- bind_rows(lapply(levels(birds_haboob$meta.vsn), function(vsn) {
  birds_haboob %>%
    dplyr::select(date, date_numeric) %>%
    distinct() %>%
    arrange(date) %>%
    mutate(
      tod_sin = 0,
      tod_cos = 0,
      meta.vsn = factor(vsn, levels = levels(birds_haboob$meta.vsn))
    )
}), .id = "vsn_index")

all_preds <- predict(gam_model, newdata = all_newdata, se.fit = TRUE)

plot_df_all <- all_newdata %>%
  mutate(
    fit = all_preds$fit,
    lower = fit - 1.96 * all_preds$se.fit,
    upper = fit + 1.96 * all_preds$se.fit
  )

haboob_time <- ymd_h("2025-05-16 18")

ggplot(plot_df_all, aes(x = date, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = haboob_time, linetype = "dashed", color = "red") +
  facet_wrap(~ meta.vsn) +
  labs(
    title = "GAM-predicted Bird Richness Over Time by Site",
    x = "Date",
    y = "Predicted Richness"
  ) +
  theme_minimal()

# visualize time-of-day effect ----
beta_sin <- coef(gam_model)["tod_sin"]
beta_cos <- coef(gam_model)["tod_cos"]

tod_vals <- seq(0, 24, by = 0.1)
tod_effect_df <- tibble(
  tod = tod_vals,
  effect = beta_sin * sin(2 * pi * tod / 24) +
    beta_cos * cos(2 * pi * tod / 24)
)

ggplot(tod_effect_df, aes(x = tod, y = effect)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Estimated Effect of Time of Day on Richness",
    x = "Hour of Day",
    y = "Effect (on model scale)"
  ) +
  theme_minimal()
