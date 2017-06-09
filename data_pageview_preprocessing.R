library(readr)
library(dplyr)
library(ggplot2)
data <- read_delim("~/Documentos/workspace/chaordic-challenge/data/data.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)
catalog <- read_csv("~/Documentos/workspace/chaordic-challenge/data/catalog")

summary(data)


data_pageview <- data %>% filter(event_type == "pageview")
data_pageview$is_purchase <- FALSE
