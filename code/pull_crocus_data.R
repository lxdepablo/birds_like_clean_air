# Metadata ----
# Author: Luis X. de Pablo
# Contact: luis.depablo@colorado.edu

# set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries ----
library(sageDataR)
library(riem)
library(furrr)
library(tidyverse)

# pull bird richness data ----
# define which nodes to pull data from
crocus_vsns <- c("W0A4", 
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
                 "W08B")

# pull bird richness data from each node
# pull data in parallel
plan(multisession, workers = availableCores())

all_bird_data <- bind_rows(future_map(crocus_vsns, function(vsn){
  curr_node_birds <- query_sage_data(start = "2025-05-02",
                                     end = "2025-06-18",
                                     filter = list(name = "env.detection.avian.*",
                                                   vsn = vsn))
}))


# pull air quality data ----
all_aq_data <- bind_rows(future_map(crocus_vsns, function(vsn){
  curr_node_aq <- query_sage_data(start = "2025-05-02",
                                  end = "2025-06-18",
                                  filter = list(name = "aqt.particle.*",
                                                vsn = vsn))
}))

# pull wind speed data ----
all_wind_data <- bind_rows(future_map(crocus_vsns, function(vsn){
  curr_node_birds <- query_sage_data(start = "2025-05-16 12:00:00",
                                     end = "2025-05-17 12:00:00",
                                     filter = list(name = "wxt.wind.*",
                                                   vsn = vsn))
}))

plan(sequential)


# pull ASOS visibility data ----
# list stations to pull data from
stations <- c("MDW", "ORD", "06C", "DPA", "IGQ", "LOT", "PWK")

all_asos_data <- bind_rows(lapply(stations, function(s){
  curr_asos <- riem_measures(station = s,
                            date_start = "2025-05-16",
                            date_end = "2025-05-18",
                            data = "vsby")
}))


# write data to CSVs ----
write_csv(all_bird_data, "../data/birds_richness.csv")
write_csv(all_aq_data, "../data/air_quality.csv")
write_csv(all_asos_data, "../data/asos.csv")













