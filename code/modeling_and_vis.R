# Metadata ----
# Author: Luis X. de Pablo
# Contact: luis.depablo@colorado.edu

# set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries and source helper functions ----
library(tidyverse)
library(mgcv)
library(ggrepel)

# read in clean data ----
# bird richness
birds_haboob <- read_csv("../data/birds_haboob.csv") %>%
  mutate(meta.vsn = as.factor(meta.vsn))
# haboob onset
haboob_time <- read_csv("../data/haboob_time.csv")
# air particulates
pm10 <- read_csv("../data/pm10.csv")
# airport visibility
visibility <- read_csv("../data/visibility.csv")

# fit GAMs ----
# fit GAM random site effect
gam_model <- gam(
  richness ~ s(time_since_haboob) +
    #s(tod, bs = "cc") +
    s(meta.vsn, bs = "re"),
  data = birds_haboob,
  method = "REML"
)
summary(gam_model)

AIC(gam_model)

# fit GAM without random effect of site
gam_model_global <- gam(
  richness ~ s(time_since_haboob),
  data = birds_haboob
)

ggplot(data = birds_haboob, aes(x = time_since_haboob, y = richness)) + 
  geom_smooth(col = "black") +
  geom_smooth(aes(col = meta.vsn), alpha = 0.5, se=F) +
  scale_color_viridis_d()

# visualize smooth effect of time ----
smooth_data <- birds_haboob %>%
  dplyr::select(meta.vsn, date, time_since_haboob, tod)
global_smooth_data <- smooth_data %>%
  filter(meta.vsn == "W08B") %>%
  select(-meta.vsn)

all_preds <- predict(gam_model, newdata = smooth_data, se.fit = TRUE)
global_preds <- predict(gam_model_global, newdata = global_smooth_data, se.fit = TRUE)

plot_df_all <- smooth_data %>%
  mutate(
    fit = all_preds$fit,
    lower = fit - 1.96 * all_preds$se.fit,
    upper = fit + 1.96 * all_preds$se.fit
  ) %>%
  # cut node with missing pm10 data
  filter(meta.vsn != "W09A")
plot_df_global <- global_smooth_data %>%
  mutate(
    fit = global_preds$fit,
    lower = fit - 1.96 * global_preds$se.fit,
    upper = fit + 1.96 * global_preds$se.fit
  )

# plot separate lines for each node
ggplot(plot_df_all, aes(x = date, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red") +
  #geom_point(data = birds_haboob, aes(x = date, y = richness)) +
  facet_wrap(~ meta.vsn) +
  labs(
    title = "Bird Richness Recovery After Haboob",
    x = "Date",
    y = "Hourly Richness"
  ) +
  theme_minimal()

# plot global trend
# get mean time of haboob
mean_haboob_time <- mean(haboob_time$haboob_peak)
ggplot(plot_df_global, aes(x = date, y = fit)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = mean_haboob_time, linetype = "dashed", color = "red") +
  #facet_wrap(~ meta.vsn) +
  labs(
    title = "Bird Richness Recovers After Haboob",
    x = "Date",
    y = "Hourly Richness"
  ) +
  theme_minimal(base_size = 20)

# map of node locations ----
# list node coordinates
node_coords <- data.frame(
  name = c("W0A4",
           "W0A1",
           "W0A0",
           "W09E",
           "W09D",
           "W09A",
           "W099",
           "W098",
           "W096",
           "W095",
           "W08E",
           "W08D",
           "W08B"),
  lat = c(41.701483642,
          41.905550029,
          41.777039645,
          41.868044352,
          41.795216999,
          41.701454052,
          42.051426431,
          41.823356575,
          41.869581577,
          41.884884633495616,
          41.719851873,
          41.98053299237866,
          41.822954831),
  lon = c(-87.995254402,
          -87.703508453,
          -87.609767906,
          -87.613360413,
          -88.006116273,
          -87.99523958,
          -87.677612626,
          -87.609374087,
          -87.645648819,
          -87.97871741056426,
          -87.612807435,
          -87.71662374616044,
          -87.609539707)
)

# get shapefile for illinois
il_map <- map_data("county", region = "illinois")

ggplot() +
  geom_polygon(data = il_map, aes(x = long, y = lat, group = group),
               fill = "lightgray", col = "white") +
  geom_point(data = node_coords, aes(x = lon, y = lat), col = "red", size = 3) +
  geom_label_repel(data = node_coords, aes(x = lon, y = lat, label = name),
                   col = "black", segment.color = "black", size = 8) +
  coord_fixed(
    ratio = 1.3,
    xlim = c(-88.1, -87.55),  # Longitude bounds
    ylim = c(41.65, 42.10)    # Latitude bounds
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid = element_blank()
  ) +
  labs(title = "Sensor Locations in Chicago Area",
       x = "Longitude", y = "Latitude")

# PM10 timeseries with haboob time ----
# subset data around haboob
haboob_pm10 <- pm10 %>%
  filter(timestamp > ymd_hms("2025-05-16 08:00:00") &
           timestamp < ymd_hms("2025-05-20 08:00:00"))

ggplot(data = haboob_pm10, aes(x = timestamp, y = value, col = meta.vsn)) +
  geom_line() +
  #geom_vline(xintercept = ymd_hm("2025-05-16 18:23"), col = "red", linetype = "dashed") +
  theme_minimal(base_size = 20) +
  labs(x = "Date",
       y = "PM10 Concentration (ug/m^3)",
       col = "CROCUS Node",
       title = "Particulate Matter Spikes During Haboob") +
  scale_color_viridis_d()
  
# airport visibility ----
ggplot(data = visibility, aes(x = timestamp, y = vsby, col = station)) +
  geom_line(linewidth = 2) +
  theme_minimal(base_size = 20) +
  labs(x = "Date",
       y = "Visibility (mi)",
       col = "Station",
       title = "Visibility Drops During Haboob") +
  scale_color_viridis_d()










  
  
