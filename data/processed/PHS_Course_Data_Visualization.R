
setwd('C:/Users/adrian/Desktop/PHS_Course/Week_1_Basic_Statistics_Projects_R/01_Example_R_Project/data/raw/')

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


#Exercice 4 - Ebola data, creating plots
# Exercice 4A read Ebola data
data_ebola <- read.csv("ebola.csv")

# format column datum of data_ebola as date
data_ebola$Date <- as.Date(data_ebola$Date)

# sort data_ebola by date
data_ebola <- arrange(data_ebola, Date)

head(data_ebola)

# Plotting always uses the same structure:
# data + aesthetic (aes) + geometric (geom)

# We will filter the data so that the dataframe only contains data with the date
# before the 31st of March 2015. Additionally, we select 3 countries with 
# the filter function.
data_ebola_cum_cases <- data_ebola %>% 
  select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) %>% 
  filter(date <= as.Date("2015-03-31") & 
           (country == "Guinea" | country ==  "Liberia" | country == "Sierra Leone"))

#Exercice 4B --> Plot Versions 0

# create point plot
plot_ebola_point_v0 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_point()  
print(plot_ebola_point_v0)

# create line plot
plot_ebola_line_v0 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(aes(group = country))
print(plot_ebola_line_v0)

# create column plot
plot_ebola_col_v0 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(position = "stack")
print(plot_ebola_col_v0)

#Exercice 4C --> V1
# create point plot
plot_ebola_point_v1 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_point(alpha = 0.7, colour = "blue", fill = "green", 
             shape = 22, size = 1.5, stroke = 1.5) 
print(plot_ebola_point_v1)


# create line plot
plot_ebola_line_v1 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, colour = "blue", linetype = "dashed", linewidth = 1.5)
print(plot_ebola_line_v1)


# create column plot
plot_ebola_col_v1 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_col(alpha = 0.7, colour = "blue", fill = "green", 
           linetype = "solid", linewidth = 0.1, position = "stack", width = 0.7)
print(plot_ebola_col_v1)

#Exercice 4D --> V2
# create point plot
plot_ebola_point_v2 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) 
print(plot_ebola_point_v2)


# create line plot
plot_ebola_line_v2 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5)
print(plot_ebola_line_v2)


# create column plot
plot_ebola_col_v2 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7)
print(plot_ebola_col_v2)

#Exercice 4E - Labels --> V3
# create point plot
plot_ebola_point_v3 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_point_v3)


# create line plot
plot_ebola_line_v3 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_line_v3)


# create column plot
plot_ebola_col_v3 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_col_v3)

#Exercice 4F --> V4
# create point plot
plot_ebola_point_v4 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  ggtitle(label = "Confirmed Ebola") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_point_v4)


# create line plot
plot_ebola_line_v4 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  ggtitle(label = "Confirmed Ebola") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_line_v4)


# create column plot
plot_ebola_col_v4 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_col_v4)

#Exercice 4G --> V5
# create point plot
plot_ebola_point_v5 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, 
             shape = 22, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_point_v5)


# create line plot
plot_ebola_line_v5 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_line_v5)


# create column plot
plot_ebola_col_v5 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 2500),
                     limits = c(0, 15000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
print(plot_ebola_col_v5)

#Exercice 4H --> V6
# create point plot
plot_ebola_point_v6 <- ggplot(data = data_ebola_cum_cases, 
                              mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom")
print(plot_ebola_point_v6)


# create line plot
plot_ebola_line_v6 <- ggplot(data = data_ebola_cum_cases, 
                             mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom")
print(plot_ebola_line_v6)


# create column plot
plot_ebola_col_v6 <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 2500),
                     limits = c(0, 15000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom")
print(plot_ebola_col_v6)

# Exercice 4I --> facet plots
# create point plot facets
plot_ebola_point_facet <- ggplot(data = data_ebola_cum_cases, 
                                 mapping = aes(x = date, y = cum_conf_cases, colour = country,fill = country)) + 
  geom_point(alpha = 0.7,  
             shape = 22, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", 
                                  "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  facet_grid(cols = vars(country))
print(plot_ebola_point_facet)

# create line plot facets
plot_ebola_line_facet <- ggplot(data = data_ebola_cum_cases, 
                                mapping = aes(x = date, y = cum_conf_cases, colour = country)) + 
  geom_line(mapping = aes(group = country), 
            alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  facet_grid(cols = vars(country))

print(plot_ebola_line_facet)

# create column plot facets
plot_ebola_col_facet <- ggplot(data = data_ebola_cum_cases, 
                               mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_col(alpha = 0.7, linetype = "solid", 
           linewidth = 0.1, position = "stack", width = 0.7) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", 
                                  "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 2500),
                     limits = c(0, 15000)) +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  facet_grid(cols = vars(country))

print(plot_ebola_col_facet)

# Exercice 4J: Grids
plot_ebola_col_grid <- plot_grid(plotlist = list(plot_ebola_col_v1, plot_ebola_col_v2, plot_ebola_col_v3, 
                                                  plot_ebola_col_v4, plot_ebola_col_v5, plot_ebola_col_v6),
                                  labels = c("V1", "V2", "V3", "V4", "V5", "V6"), label_size = 12, nrow = 2)
print(plot_ebola_col_grid)
# Plot needs to be adjusted before export as png or other file format (e.g. size)

# Exercices with insurance data...

# Report Submission
data("ToothGrowth")
# The response is the length of odontoblasts (cells responsible for tooth growth) 
# in 60 guinea pigs. Each animal received one of three dose levels of vitamin C 
# (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or 
# ascorbic acid (a form of vitamin C and coded as VC).

