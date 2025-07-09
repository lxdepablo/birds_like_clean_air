# Metadata ----
# Author: Luis X. de Pablo
# Contact: luis.depablo@colorado.edu

# set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries ----
library(tidyverse)

# read in data pulled from API ----
## birds data -----
birds_raw <- read_csv("../data/birds_richness.csv")
## environmental data ----
env_raw <- read_csv("../data/air_quality.csv")

# read in ASOS data downloaded from https://mesonet.agron.iastate.edu/request/download.phtml?network=IL_ASOS ----
asos_raw <- read_csv("../data/asos.csv")

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
  filter(date %in% seq(ymd_h("2025-05-15 16"), ymd_h("2025-05-30 22"), by = "hour")) %>%
  mutate(tod = hour(date),
         tod_sin = sin(2 * pi * tod / 24),
         tod_cos = cos(2 * pi * tod / 24))

# wrangle env data ----
pm10 <- env_raw %>%
  filter(name == "aqt.particle.pm10")
pm2.5 <- env_raw %>%
  filter(name == "aqt.particle.pm2.5")
pm1 <- env_raw %>%
  filter(name == "aqt.particle.pm1")

# visualize particles
ggplot(data = pm10, aes(x = timestamp, y = value)) +
  geom_line() +
  geom_vline(xintercept = ymd_h("2025-05-16 18"), col = "red", linetype = "dashed") +
  facet_wrap(~meta.vsn) +
  labs(x = "Date", y = "PM10 Concentration (ug/m^3)", title = "Particulate Matter Aligns with Haboob Onset") +
  theme_minimal()
ggplot() +
  geom_line(data = pm2.5, aes(x = timestamp, y = value), col = "cornsilk") +
  geom_vline(xintercept = ymd_h("2025-06-05 12"), col = "red", linetype = "dashed") +
  facet_wrap(~meta.vsn) +
  labs(x = "Date", y = "PM2.5 Concentration (ug/m^3)") +
  theme_minimal()
ggplot(data = env_raw, aes(x = timestamp, y = value, col = name)) +
  geom_line() +
  geom_vline(xintercept = ymd_h("2025-06-05 12"), col = "red", linetype = "dashed") +
  facet_wrap(~meta.vsn) +
  labs(x = "Date", y = "PM Concentration (ug/m^3)") +
  theme_minimal()

# find start of haboob for each site
haboob_time <- bind_rows(lapply(unique(pm10$meta.vsn), function(node){
  curr_node <- pm10 %>%
    filter(meta.vsn == node) %>%
    # zero out data outside of expected haboob time range
    mutate(value = ifelse(timestamp > ymd_hms("2025-05-16 08:00:00") &
                            timestamp < ymd_hms("2025-05-17 08:00:00"),
                          value, 0))
  
  peak <- curr_node$timestamp[which.max(curr_node$value)]
  # format return df
  data.frame(meta.vsn = node,
             haboob_peak = peak,
             peak_pm10 = max(curr_node$value))
  
})) %>%
  # drop W0A0 since it didnt record data during the haboob
  filter(meta.vsn != "W0A0")

# add time since haboob column to birds data
birds_haboob <- birds_haboob %>%
  # drop nodes with incomplete data
  filter(!(meta.vsn %in% c("W0A0"))) %>%
  left_join(haboob_time, by = "meta.vsn") %>%
  mutate(time_since_haboob = as.numeric(difftime(date, haboob_peak, units = "days")),
         tod = hour(date)) %>%
  dplyr::select(-haboob_peak)

# format airport visibility data
asos_clean <- asos_raw %>%
  mutate(timestamp = ymd_hms(valid)) %>%
  select(-valid) %>%
  filter(timestamp > ymd_hms("2025-05-16 12:00:00") &
           timestamp < ymd_hms("2025-05-17 12:00:00"))

# write data to CSVs ----
write_csv(pm10, "../data/pm10.csv")
write_csv(haboob_time, "../data/haboob_time.csv")
write_csv(birds_haboob, "../data/birds_haboob.csv")
write_csv(asos_clean, "../data/visibility.csv")





