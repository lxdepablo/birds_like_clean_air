# Metadata ----
# Author: Luis X. de Pablo
# Contact: luis.depablo@colorado.edu

# set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries and source helper functions ----
library(tidyverse)

# read in clean data ----
birds_haboob <- read_csv("../data/birds_haboob.csv") %>%
  mutate(meta.vsn = as.factor(meta.vsn))
haboob_time <- read_csv("../data/haboob_time.csv")
pm10 <- read_csv("../data/pm10.csv")

# fit GAMs ----
# fit GAM with cyclic time-of-day effect and random site effect
gam_model <- gam(
  richness ~ s(time_since_haboob) + s(meta.vsn, bs = "re"),
  data = birds_haboob
)
summary(gam_model)

ggplot(data = birds_haboob, aes(x = time_since_haboob, y = richness)) + 
  geom_smooth() +
  facet_wrap(~meta.vsn)

# visualize smooth effect of time ----
smooth_data <- birds_haboob %>%
  dplyr::select(meta.vsn, date, time_since_haboob)

all_preds <- predict(gam_model, newdata = smooth_data, se.fit = TRUE)

plot_df_all <- smooth_data %>%
  mutate(
    fit = all_preds$fit,
    lower = fit - 1.96 * all_preds$se.fit,
    upper = fit + 1.96 * all_preds$se.fit
  )

ggplot(plot_df_all, aes(x = date, y = fit)) +
  geom_line(color = "blue") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  geom_vline(data = haboob_time, aes(xintercept = haboob_peak), linetype = "dashed", color = "red") +
  facet_wrap(~ meta.vsn) +
  labs(
    title = "Bird Richness Recovery After Haboob",
    x = "Date",
    y = "Hourly Richness"
  ) +
  theme_minimal()
