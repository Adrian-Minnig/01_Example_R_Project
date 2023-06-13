# PHS Course Submission
# Quarto document with descriptiv statistics (means, median, SD, etc) with one
# plot. Boxplot / Facet Plot. Then at least one inferential statistic with 
# comparisons (comparing different groups)


setwd('C:/Users/adrian/Desktop/PHS_Course/Week_1_Basic_Statistics_Projects_R/01_Example_R_Project')

library(tidyverse)
library(unibeCols)
library(usethis)
library(gitcreds)
library(here)
library(medicaldata)
library(cowplot)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)


# Report Submission
data("ToothGrowth")       # This is a dataset already preinstalled in RStudio

# The response is the length of odontoblasts (cells responsible for tooth growth) 
# in 60 guinea pigs. Each animal received one of three dose levels of vitamin C 
# (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or 
# ascorbic acid (a form of vitamin C and coded as VC).

View(ToothGrowth)
summary(ToothGrowth)

# The command "filter" is used to filter coloumns

ToothGrowth %>%             # Filter ToothGrowth to show only the supplement OJ
  filter(supp == "OJ")
data_OJ <- ToothGrowth %>%
  filter(supp == "OJ")

data_OJ %>%                    # Filter data_OJ to show only dose 0.5
  filter(dose == 0.5)
data_OJ_0.5 <- data_OJ %>%
  filter(dose == 0.5)

data_OJ %>%                   # Filter data_OJ to show only dose 1
  filter(dose == 1)
data_OJ_1 <- data_OJ %>%          
  filter(dose == 1)

data_OJ %>%                   # Filter data_OJ to show only dose 2
  filter(dose == 2)
data_OJ_2 <- data_OJ %>%          
  filter(dose == 2)


ToothGrowth %>%               # Filter ToothGrowth to show only supplement VC
  filter(supp == "VC")
data_VC <- ToothGrowth %>%
  filter(supp == "VC")

data_VC %>%                    # Filter data_VC to show only dose 0.5
  filter(dose == 0.5)
data_VC_0.5 <- data_OJ %>%
  filter(dose == 0.5)

data_VC %>%                   # Filter data_VC to show only dose 1
  filter(dose == 1)
data_VC_1 <- data_OJ %>%          
  filter(dose == 1)

data_VC %>%                   # Filter data_VC to show only dose 2
  filter(dose == 2)
data_VC_2 <- data_OJ %>%          
  filter(dose == 2)

ToothGrowth %>%               # Filter ToothGrowth to show only dose 0.5
  filter(dose == 0.5)
dose0.5 <- ToothGrowth %>%
  filter(dose == 0.5)

ToothGrowth %>%               # Filter ToothGrowth to show only dose 1
  filter(dose == 1)
dose_1 <- ToothGrowth %>%
  filter(dose == 1)

ToothGrowth %>%               # Filter ToothGrowth to show only dose 2
  filter(dose == 2)
dose_2 <- ToothGrowth %>%
  filter(dose == 2)


# Calculating descriptive values
mean(data_OJ_0.5$len)
mean(data_OJ_1$len)
mean(data_OJ_2$len)
mean(data_VC_0.5$len)
mean(data_VC_1$len)
mean(data_VC_2$len)

mean(data_OJ$len)
mean(data_VC$len)

median(data_OJ$len)
median(data_VC$len)

median(data_OJ_0.5$len)
median(data_OJ_1$len)
median(data_OJ_2$len)
median(data_VC_0.5$len)
median(data_VC_1$len)
median(data_VC_2$len)

# Mutate dose as factor for data_VC
data_VC %>%
  mutate(dose_f = as.factor(as.character(dose)))
data_VC <- data_VC %>%
  mutate(dose_f = as.factor(as.character(dose)))

# Mutate dose as factor for data_OJ and data_VC
data_OJ %>%
  mutate(dose_f = as.factor(as.character(dose)))
data_OJ <- data_OJ %>%
  mutate(dose_f = as.factor(as.character(dose)))


data_VC %>%
  mutate(dose_f = as.factor(as.character(dose)))
data_VC <- data_VC %>%
  mutate(dose_f = as.factor(as.character(dose)))



# Generating boxplots for the two different supplements
data_OJ %>%
  ggplot(aes(dose_f, len)) +
  geom_boxplot()

data_VC %>%
  ggplot(aes(dose_f, len)) +
  geom_boxplot()


# Analysis

# Normality Testing



