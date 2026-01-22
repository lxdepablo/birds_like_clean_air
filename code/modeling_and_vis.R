# Metadata ----
# Author: Luis X. de Pablo
# Contact: luis.depablo@colorado.edu

# set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries and source helper functions ----
library(tidyverse)
library(mgcv)
library(ggrepel)
library(sf)

# read in clean data ----
# bird richness
birds_haboob <- read_csv("../data/birds_haboob.csv") %>%
  mutate(meta.vsn = as.factor(meta.vsn))
birds_3h_haboob <- read_csv("../data/birds_3h_haboob.csv") %>%
  mutate(meta.vsn = as.factor(meta.vsn))
# haboob onset
haboob_time <- read_csv("../data/haboob_time.csv")
# air particulates
pm10 <- read_csv("../data/pm10.csv")
# airport visibility
visibility <- read_csv("../data/visibility.csv")
# wind
wind <- read_csv("../data/wind.csv")

# get max pm10 concentration for each node during haboob ----
max_pm10 <- pm10 %>%
  filter(timestamp > ymd_hms("2025-05-16 08:00:00"), timestamp < ymd_hms("2025-05-17 08:00:00")) %>%
  group_by(meta.vsn) %>%
  summarize(max_val = max(value))

# fit GAMs ----
# fit GAM random site effect
gam_model <- gam(
  richness ~ s(time_since_haboob) +
    #s(tod, bs = "cc") +
    s(meta.vsn, bs = "re"),
  data = birds_3h_haboob,
  method = "REML"
)
summary(gam_model)

AIC(gam_model)

# fit GAM without random effect of site
gam_model_global <- gam(
  richness ~ s(time_since_haboob),
  data = birds_3h_haboob
)

ggplot(data = birds_3h_haboob, aes(x = time_since_haboob, y = richness)) +
  geom_point(col = "blue") +
  geom_smooth(col = "black") +
  #geom_smooth(aes(col = meta.vsn), alpha = 0.5, se=F) +
  facet_wrap(~meta.vsn) +
  scale_color_viridis_d()

ggplot(data = filter(birds_haboob, meta.vsn == "W08E"), aes(x = time_since_haboob, y = richness)) +
  geom_point()

# visualize smooth effect of time ----
smooth_data <- birds_3h_haboob %>%
  dplyr::select(meta.vsn, date, time_since_haboob, tod)
global_smooth_data <- smooth_data %>%
  select(-meta.vsn)

all_preds <- predict(gam_model, newdata = smooth_data, se.fit = TRUE)
global_preds <- predict(gam_model_global, newdata = global_smooth_data, se.fit = TRUE)

plot_df_all <- smooth_data %>%
  mutate(
    fit = all_preds$fit,
    lower = fit - 1.96 * all_preds$se.fit,
    upper = fit + 1.96 * all_preds$se.fit
  )
plot_df_global <- global_smooth_data %>%
  mutate(
    fit = global_preds$fit,
    lower = fit - 1.96 * global_preds$se.fit,
    upper = fit + 1.96 * global_preds$se.fit
  )

# get minimum predicted species richness timing
min_richness_timing <- plot_df_global[which(plot_df_global$fit == min(plot_df_global$fit)),]$date

# plot GAM with separate lines for each node
ggplot(plot_df_all, aes(x = date, y = fit)) +
  geom_point(data = birds_3h_haboob, aes(x = date, y = richness), size = 1, alpha = 0.3) +
  geom_smooth(data = birds_3h_haboob, aes(x = date, y = richness), linetype = "dashed", col = "red", alpha = 0.3) +
  geom_line(color = "blue", linewidth = 1.5, alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red") +
  facet_wrap(~ meta.vsn) +
  labs(
    title = "Bird Richness Recovery After Haboob",
    x = "Date",
    y = "Bird Species Richness"
  ) +
  theme_minimal(base_size = 20)

# plot loess
ggplot(data = filter(birds_3h_haboob, meta.vsn != "W09A")) +
  geom_point(aes(x = time_since_haboob, y = richness), size = 1, alpha = 0.3) +
  geom_smooth(aes(x = time_since_haboob, y = richness), color = "blue") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red") +
  #geom_point(data = birds_haboob, aes(x = date, y = richness)) +
  facet_wrap(~meta.vsn) +
  labs(
    title = "Bird Richness Recovery After Haboob",
    x = "Days Since Haboob",
    y = "Hourly Richness"
  ) +
  theme_minimal(base_size = 20)

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
#il_map <- map_data("county", region = "illinois")
il_map <- st_read("../data/illinois_map/IL_BNDY_State_Py.shp")
chicago_map <- st_read("../data/chicago_map/geo_export_163a5206-da52-4e0c-bf5e-3a62023dcd97.shp") %>%
  st_transform(crs = 4326)

plot(chicago_map)

ggplot() +
  geom_sf(data = il_map,
               fill = "lightgray", col = "white") +
  geom_sf(data = chicago_map,
               fill = "darkgray", col = "darkgray", inherit.aes = F) +
  geom_point(data = node_coords, aes(x = lon, y = lat), col = "red", size = 4) +
  geom_label_repel(data = node_coords, aes(x = lon, y = lat, label = name),
                   col = "black", segment.color = "black", size = 8) +
  coord_sf(
    xlim = c(-88.1, -87.55),  # longitude bounds
    ylim = c(41.65, 42.10),   # latitude bounds
    expand = FALSE
  ) +
  theme_void(base_size = 20) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid = element_blank()
  )

# PM10 timeseries with haboob time ----
# subset data around haboob
haboob_pm10 <- pm10 %>%
  filter(timestamp > ymd_hms("2025-05-16 08:00:00") &
           timestamp < ymd_hms("2025-05-20 08:00:00")) %>%
  # drop malfunctioning nodes
  filter(meta.vsn != "W0A0",
         meta.vsn != "W098")

ggplot(data = haboob_pm10, aes(x = timestamp, y = value, col = meta.vsn)) +
  geom_line() +
  #geom_vline(xintercept = ymd_hm("2025-05-16 18:23"), col = "red", linetype = "dashed") +
  theme_minimal(base_size = 20) +
  labs(x = "Date",
       y = "PM10 Concentration (ug/m^3)",
       col = "Node ID",
       title = "Particulate Matter Spikes During Haboob") +
  scale_color_viridis_d()
  
# airport visibility ----
ggplot(data = visibility, aes(x = timestamp, y = vsby, col = station)) +
  geom_line() +
  theme_minimal(base_size = 20) +
  labs(x = "Date",
       y = "Visibility (mi)",
       col = "Station",
       title = "Visibility Drops During Haboob") +
  scale_color_viridis_d()

# wind speed and direction ----
wind_speed <- filter(wind, name == "wxt.wind.speed")
wind_direction <- filter(wind, name == "wxt.wind.direction")

ggplot(data = wind_speed, aes(x = timestamp, y = value, col = meta.vsn)) +
  geom_smooth() +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red")

ggplot(data = wind_direction, aes(x = timestamp, y = value, col = meta.vsn)) +
  geom_smooth() +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red")








  
  
