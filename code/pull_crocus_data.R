# Metadata ----
# Author: Luis X. de Pablo
# Contact: luis.depablo@colorado.edu

# set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries and source helper functions ----
library(sageDataR)
library(furrr)

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
tic()
all_aq_data <- bind_rows(future_map(crocus_vsns, function(vsn){
  curr_node_aq <- query_sage_data(start = "2025-05-02",
                                  end = "2025-06-18",
                                  filter = list(name = "aqt.particle.*",
                                                vsn = vsn))
}))

plan(sequential)
toc()

# write data to CSVs ----
write_csv(all_bird_data, "../data/birds_richness.csv")
write_csv(all_aq_data, "../data/air_quality.csv")



