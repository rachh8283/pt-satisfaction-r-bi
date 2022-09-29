if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, sqldf, rio, magrittr, tidyverse, psych, ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(magrittr)
library(sqldf)
library(ggplot2)

# Set directory and read in data
setwd("D:/R/cmsdata")
df <- import("clean_file.xlsx")

# Convert Topic to factor
df <- df %>% 
  mutate(Topic = forcats::as_factor(Topic))

# Set seed and read in structure, head, and number of cols and rows
set.seed(1234)
head(df)
str(df)

# Summarize means for ratings at proprietary facilities, group by topic.
hospital_data_summarise <- df %>% 
  filter(Ownership == "Proprietary") %>% 
  group_by(Topic) %>% 
  summarise(ave_pt_rating = mean(PtRating),
            ave_hosp_rating = mean(HospRating)) %>%
  ungroup()
hospital_data_summarise

# scatter plot with geom point
ggplot(hospital_data_summarise, aes(x = Topic, y = ave_pt_rating)) +
  geom_point(color="blue") +
  geom_abline(color="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Regression models
fit <- lm(HospRating ~ Topic, data=df)
summary(fit)

fit1 <- lm(PtRating ~ Topic, data=df)
summary(fit1)

# Contingency table of survey topics and patient rating of the facility
ct <- (table(df$Topic, df$PtRating, df$Ownership=="Proprietary"))
ct

rm(list=ls())
graphics.off()
