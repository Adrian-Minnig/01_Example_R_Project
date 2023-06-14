# PHS Course Submission
# Quarto document with descriptiv statistics (means, median, SD, etc) with one
# plot. Boxplot / Facet Plot. Then at least one inferential statistic with 
# comparisons (comparing different groups)


setwd('C:/Users/adrian/Desktop/PHS_Course/Week_1_Basic_Statistics_Projects_R/01_Example_R_Project')


library(tidyverse)
library(unibeCols)
library(usethis)
library(remotes)
library(gitcreds)
library(here)
library(medicaldata)
library(cowplot)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(rstatix)

# Report Submission
data("ToothGrowth")       # This is a dataset already preinstalled in RStudio

# The response is the length of odontoblasts (cells responsible for tooth growth) 
# in 60 guinea pigs. Each animal received one of three dose levels of vitamin C 
# (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or 
# ascorbic acid (a form of vitamin C and coded as VC).

View(ToothGrowth)
summary(ToothGrowth)

head(ToothGrowth)
str(ToothGrowth)

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
dose_0.5 <- ToothGrowth %>%
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
ToothGrowth %>%
  group_by(supp, dose) %>%
  summarize(mean(len))



# Mutate dose as factor for data_OJ and data_VC
data_OJ %>%
  mutate(dose_f = as.factor(as.character(dose)))
data_OJ <- data_OJ %>%
  mutate(dose_f = as.factor(as.character(dose)))

data_VC %>%
  mutate(dose_f = as.factor(as.character(dose)))
data_VC <- data_VC %>%
  mutate(dose_f = as.factor(as.character(dose)))

ToothGrowth %>%
  mutate(dose_f = as.factor(as.character(dose)))
ToothGrowth <- ToothGrowth %>%
  mutate(dose_f = as.factor(as.character(dose)))



# Mutate OJ and VC as factors for different dosages
dose_0.5 %>%
  mutate(supp_f = as.factor(as.character(supp)))
dose_0.5 <- dose_0.5 %>%
  mutate(supp_f = as.factor(as.character(supp)))

dose_1 %>%
  mutate(supp_f = as.factor(as.character(supp)))
dose_1 <- dose_1 %>%
  mutate(supp_f = as.factor(as.character(supp)))

dose_2 %>%
  mutate(supp_f = as.factor(as.character(supp)))
dose_2 <- dose_0.5 %>%
  mutate(supp_f = as.factor(as.character(supp)))

# Generating boxplots for the two different supplements
data_OJ %>%
  ggplot(aes(dose_f, len)) +
  geom_boxplot() +
  xlab(label = "Dose of supplement [mg/d]") +
  ylab(label = "Tooth-Length [mm]") +
  theme_bw() + theme(legend.position="bottom") 


data_VC %>%
  ggplot(aes(dose_f, len)) +
  geom_boxplot() 
  




#Generating boxplots for the different dosages
dose_0.5 %>%
  ggplot(aes(supp_f, len)) +
  geom_boxplot()

dose_1 %>%
  ggplot(aes(supp_f, len)) +
  geom_boxplot()

dose_2 %>%
  ggplot(aes(supp_f, len)) +
  geom_boxplot()

# Generating Summary Tables


Summary_Table_OJ <- 
  data_OJ %>%
  select(len, dose) %>%
  tbl_summary(
    by = dose, 
    label = list(len ~ "Tooth Length [mm]"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(len ~ c(1, 1))) %>%
  add_overall()
print(Summary_Table_OJ)

  
Summary_Table_VC <-
data_VC %>%
  select(len, dose) %>%
  tbl_summary(
    by = dose, 
    label = list(len ~ "Tooth Length [mm]"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(len ~ c(1, 1))) %>%
  add_overall()
print(Summary_Table_VC)


ToothGrowth %>%
  group_by(supp, dose) %>%
  summarize(mean(len))
  





# ------------   Analysis  ---------------------------

# Normality Testing
shapiro.test(data_OJ_0.5$len)
shapiro.test(data_OJ_1$len)
shapiro.test(data_OJ_2$len)
shapiro.test(data_VC_0.5$len)
shapiro.test(data_VC_1$len)
shapiro.test(data_VC_2$len)

shapiro.test(ToothGrowth$len)

# QQ Plot
ToothGrowth %>%
  ggplot(aes(sample = len)) + 
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(color = "steelblue", distribution = stats::qnorm) + 
  xlab("Theoretical Quantiles") +
  ylab("Tooth Length Quantiles") +
  theme_bw() +
  ggtitle(label = "QQ-Plot of Tooth Length")


# The QQ-Plot suggests normally distributed data and the shapiro Tests with 
# high p-values confirmes Normality as the Null-Hypothesis of Normality is
# confirmed
# Null-Hypothesis of Shapiro Wilk Test = sample distribution is normal
# Statistical Tests


# Analysis -------------------------------------------------
# Hypothesis 1: There is a difference in Toothgrowth between the supplements
# OJ and VC for the different dosages   
# Null Hypothesis = no difference in Toothgrowth between OJ and VC


dose_0.5 %>%
  t_test(len ~ supp)

dose_1 %>%
  t_test(len ~ supp)

dose_2 %>%
  t_test(len ~ supp) 

# Null Hypothesis can be rejected for doses 0.5 and 1 (and probably dosages
# in between tough these were not tested)
# Null Hypothesis cannot be rejected for dose 2.