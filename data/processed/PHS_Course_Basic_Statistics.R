
setwd('C:/Users/adrian/Desktop/PHS_Course/Week_1_Basic_Statistics_Projects_R/01_Example_R_Project')
getwd()

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


# Basic Statistics, PHS Course Thursday

data<-read.csv("perulung_ems.csv")
getwd()
getwd()

str(data)       # this can be used to inspect data
head(data, 10)
tail(data, 10)
summary(data)
View(data)

# So far, we worked with base R tools. In RSTudio, lots of packages have
# useful functions. tidyverse makes a lot of statistical work easier
library(tidyverse)

data %>% select(sex)%>% filter(sex==1) %>% table         
# select means selecting different coloumns
# filter means selecting different rows



# The following step converts sex, respsymptoms as factors and id is an integer
# factor = categorical variable

data<- data |> mutate(id=as.integer(id), 
                      sex = factor(sex, levels=c(0,1), labels=c("f","m")), 
                      respsymptoms=factor(respsymptoms, levels=c(0,1), labels=c("no","yes")), 
                      asthma_hist=factor(asthma_hist))
data

ggplot(data, aes(x=fev1)) +              # fex = forced exspirational volume
  geom_histogram()


# When creating histograms, we need to take care how it is displayed
# In general, we want a "smooth" histogram.
ggplot(data, aes(x=fev1)) +             # Histograms are a trade-of of bias
  geom_histogram(bins=40)               # and variance


summary(data)

# Quantil = Werte in verschiedene Teile aufteilen
# Quartile = in vier Teile aufteilen (0.25, 0.5, 0.75,1)

quantile(data$fev1, c(0.25,0.5,0.75))

ggplot(data, aes(y=fev1, color=sex)) + 
  geom_boxplot(color = "blue") +
  scale_x_discrete()

