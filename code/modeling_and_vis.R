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
# fit GAM with doy fixed effect
gam_model <- gam(
  richness ~ s(time_since_haboob) + s(meta.vsn, bs = "re"),
  data = birds_3h_haboob,
  family = nb(link = "log"),
  method = "REML"
)
summary(gam_model)

# fit GAM without fixed effect
gam_model_global <- gam(
  richness ~ s(time_since_haboob),
  data = birds_3h_haboob
)
summary(gam_model_global)

ggplot(data = birds_3h_haboob, aes(x = time_since_haboob, y = richness)) +
  geom_point(col = "blue") +
  geom_smooth(col = "black") +
  #geom_smooth(aes(col = meta.vsn), alpha = 0.5, se=F) +
  facet_wrap(~meta.vsn) +
  scale_color_viridis_d()

ggplot(data = filter(birds_haboob, meta.vsn == "W08E"), aes(x = time_since_haboob, y = richness)) +
  geom_point()

# visualize smooth effect of time ----

# smooth_data <- birds_3h_haboob %>%
#   dplyr::select(meta.vsn, date, time_since_haboob, doy, haboob_yr)
# global_smooth_data <- smooth_data %>%
#   select(-meta.vsn)
# 
# all_preds <- predict(gam_model, newdata = smooth_data, se.fit = TRUE, type="response")
# global_preds <- predict(gam_model_global, newdata = global_smooth_data, se.fit = TRUE)
# 
# plot_df_all <- smooth_data %>%
#   mutate(
#     fit = all_preds$fit,
#     lower = fit - 1.96 * all_preds$se.fit,
#     upper = fit + 1.96 * all_preds$se.fit
#   )
# plot_df_global <- global_smooth_data %>%
#   mutate(
#     fit = global_preds$fit,
#     lower = fit - 1.96 * global_preds$se.fit,
#     upper = fit + 1.96 * global_preds$se.fit
#   )

plot_df <- birds_3h_haboob %>%
  filter(!is.na(time_since_haboob)) %>%
  left_join(haboob_time %>% dplyr::select(meta.vsn, haboob_peak), by = "meta.vsn") %>%
  mutate(year = factor(year(date)),
         days_since_haboob = as.numeric(difftime(date, haboob_peak, units = "days")))

pred <- predict(gam_model, newdata = plot_df, se.fit = TRUE) # link scale

plot_df <- plot_df %>%
  mutate(fit = exp(pred$fit),
         lower = exp(pred$fit - 1.96 * pred$se.fit),
         upper = exp(pred$fit + 1.96 * pred$se.fit))

ggplot(plot_df, aes(x = date)) +
  geom_point(aes(y = richness), size = 1.5, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  geom_smooth(aes(y = richness), col = "red", linetype = "dashed", se = FALSE) +
  geom_line(aes(y = fit), linewidth = 1.5, alpha = 0.7, col = "blue") +
  geom_vline(data = distinct(plot_df, meta.vsn, haboob_peak),
             aes(xintercept = haboob_peak),
             linetype = "dashed", colour = "red") +
  facet_wrap(~ meta.vsn) +
  scale_x_datetime(date_labels = "%b %d") +
  labs(x = "Date", y = "Bird species richness") +
  theme_minimal(base_size = 20)

# get min and max mean predicted richness across nodes for each 3 hour bin
mean_richness <- plot_df %>%
  mutate(fit = as.numeric(fit)) %>%
  group_by(date) %>%
  summarize(mean_fit = mean(fit, na.rm = TRUE),
            n_nodes = n(), .groups = "drop")

richness_summary <- mean_richness %>%
  summarize(
    min_richness = min(mean_fit),
    min_time     = date[which.min(mean_fit)],
    max_richness = max(mean_fit),
    max_time     = date[which.max(mean_fit)],
    increase     = max_richness - min_richness,
    pct_change   = (min_richness / max_richness - 1) * 100
  )
richness_summary

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
  labs(#title = "Particulate Matter Spikes During Haboob",
       x = "Date",
       y = "PM10 Concentration (ug/m^3)",
       col = "Node ID") +
  scale_color_viridis_d()
  
# airport visibility ----
ggplot(data = visibility, aes(x = timestamp, y = vsby, col = station)) +
  geom_line() +
  theme_minimal(base_size = 20) +
  labs(#title = "Visibility Drops During Haboob",
       x = "Date",
       y = "Visibility (mi)",
       col = "Station") +
  scale_color_viridis_d()

# zoomed haboob timing vs richness windows (Fig. S3) ----
zoom <- c(ymd_hms("2025-05-16 12:00:00"), ymd_hms("2025-05-17 12:00:00"))

# elevated dust: PM10 above half of each node's peak
dust <- haboob_pm10 %>%
  filter(timestamp > zoom[1], timestamp < zoom[2]) %>%
  group_by(meta.vsn) %>%
  filter(value > max(value) / 2) %>%
  summarize(start = min(timestamp), end = max(timestamp))

ggplot() +
  geom_rect(data = dust, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.4) +
  geom_vline(xintercept = seq(zoom[1], zoom[2], by = "3 hours"), col = "grey80") +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), col = "red", linetype = "dashed") +
  geom_line(data = filter(haboob_pm10, timestamp > zoom[1], timestamp < zoom[2]),
            aes(x = timestamp, y = value)) +
  geom_step(data = filter(birds_3h_haboob, date >= zoom[1], date <= zoom[2]),
            aes(x = date, y = richness * 200), col = "blue") +
  facet_wrap(~meta.vsn) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 200, name = "Bird Species Richness")) +
  scale_x_datetime(date_labels = "%H:%M") +
  coord_cartesian(xlim = zoom) +
  theme_minimal(base_size = 20) +
  labs(x = "Time (UTC)",
       y = "PM10 Concentration (ug/m^3)")

# wind speed and direction ----
wind_speed <- filter(wind, name == "wxt.wind.speed")
wind_direction <- filter(wind, name == "wxt.wind.direction")

ggplot(data = wind_speed, aes(x = timestamp, y = value, col = meta.vsn)) +
  geom_smooth() +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red")

ggplot(data = wind_direction, aes(x = timestamp, y = value, col = meta.vsn)) +
  geom_smooth() +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red")








  
  
