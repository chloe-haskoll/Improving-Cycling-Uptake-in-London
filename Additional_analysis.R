# Import necessary libraries.
library(ggplot2)
library(tidyverse)
library(plotly)
library(ggiraph)
library(ggiraphExtra)
install.packages("rcartocolor")
library(rcartocolor)
library(car)

# Read csv files and summary
outer <- read.csv("outer_l.csv", header = TRUE)
inner <- read.csv("inner_l.csv", header = TRUE)
central <- read.csv("central_l.csv", header = TRUE)
greater <- read.csv("greater_london_mil.csv", header = TRUE)

summary(outer)
summary(inner)
summary(central)
summary(greater)

# Start exploratory plots
ggplot(data = outer, aes(x = Survey.date, y = Total.cycles)) + 
  geom_line() +
  theme_light()

# Changing survey.date format to date time
outer$Survey.date <-  as.POSIXct(outer$Survey.date, 
                                 format = "%Y-%m-%d")
inner$Survey.date <-  as.POSIXct(inner$Survey.date, 
                                 format = "%Y-%m-%d")
central$Survey.date <-  as.POSIXct(central$Survey.date,
                                   format = "%Y-%m-%d")
greater$Survey.date <-  as.POSIXct(greater$Survey.date,
                                   format = "%Y-%m-%d")

# Filling missing values                                                                   format = "%Y-%m-%d")
outer_fill <- outer %>%
  mutate(Survey.date = Survey.date) %>%
  group_by(Site.ID) %>%
  fill(Survey.date, .direction = "downup")

inner_fill <- inner %>%
  mutate(Survey.date = Survey.date) %>%
  group_by(Site.ID) %>%
  fill(Survey.date, .direction = "downup")

central_fill <- central %>%
  mutate(Survey.date = Survey.date) %>%
  group_by(Site.ID) %>%
  fill(Survey.date, .direction = "downup")

greater_fill <- greater %>%
  mutate(Survey.date = Survey.date) %>%
  group_by(Site.ID) %>%
  fill(Survey.date, .direction = "downup")

# Creating time series to check seasonality
# Outer London:
outer_series <-ts (outer_fill$Total.cycles, start=c(2015,6), 
                   end=c(2021,6), frequency=12)

decomp_out<-decompose(outer_series) 

# Plotting decomposed timeseries:
plot(decomp_out) 
title(sub = "Outer London")

seas_outer <- outer_series - decomp_out$seasonal

# Plotting trends without seasonality
plot(seas_outer)  

# Plotting only seasonality
plot(decomp_out$seasonal)

# Inner London:
inner_series <-ts (inner_fill$Total.cycles, start=c(2015,5), 
                   end=c(2021,12), frequency=12)

decomp_in<-decompose(inner_series)

# Plotting decomposed timeseries:
plot(decomp_in) 
title(sub = "Inner London")

seas_inner <- inner_series - decomp_in$seasonal

# Plotting trends without seasonality
plot(seas_inner) 

# Plotting only seasonality
plot(decomp_in$seasonal)

# Central London:
central_series <-ts (central_fill$Total.cycles, start=c(2014,1), 
                   end=c(2021,12), frequency=12)

decomp_cen <- decompose(central_series)

# Plotting decomposed timeseries:
plot(decomp_cen) 
title(sub = "Central London")

seas_central <- central_series - decomp_cen$seasonal

# Plotting trends without seasonality 
plot(seas_central)

# Plotting only seasonality
plot(decomp_cen$seasonal)

# Greater London:
greater_series <-ts (greater_fill$Total.cycles, start=c(2015,5), 
                   end=c(2021,12), frequency=12)

decomp_great<-decompose(greater_series)

# Plotting decomposed timeseries:
plot(decomp_great) 
title(sub = "Greater London")

seas_greater <- greater_series - decomp_great$seasonal

# Plotting trends without seasonality
plot(seas_greater) 

# Plotting only seasonality
plot(decomp_great$seasonal)

# Histogram for peak hours:
ggplot(data = outer, aes(x = Period)) +
  geom_bar(stat = 'count') +
  scale_x_discrete(guide = guide_axis(angle = 45))

# Peak times
# Outer:
ggplot(outer, aes(x = Time, y = Total.cycles, fill=Total.cycles)) +
  geom_bar(stat = "sum") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_fill_continuous(high = '#011f4b', 
                        low = '#b3cde0') +
  theme(legend.position="none") +
  labs(title = "Outer London Peak Riding Hours")

# Inner: 
ggplot(inner, aes(x = Time, y = Total.cycles, fill=Total.cycles)) +
  geom_bar(stat = "sum") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_fill_continuous(high = "darkred", 
                        low = '#ffbaba') +
  theme(legend.position="none") +
  labs(title = "Inner London Peak Riding Hours")

# Central:
ggplot(central, aes(x = Time, y = Total.cycles, fill=Total.cycles)) +
  geom_bar(stat = "sum") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_fill_continuous(high = "#a98600", 
                        low = '#fff9ae')  +
  theme(legend.position="none") +
  labs(title = "Central London Peak Riding Hours")

# Greater:
ggplot(greater, aes(x = Time, y = Total.cycles, fill=Total.cycles)) +
  geom_bar(stat = "sum") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_fill_continuous(high = "#643d6e", 
                        low = '#d4c0d9')  +
  theme(legend.position="none") +
  labs(title = "Greater London Peak Riding Hours")

# Checking direction of travel:
# Outer:
outer_rides <- ggplot(outer, aes(x = Time, y = Total.cycles, fill = Direction)) +
                 geom_bar(stat = "identity") +
                 scale_x_discrete(guide = guide_axis(angle = 90),
                                  labels=c("0600 - 0615" = "6:00", 
                                           "0615 - 0630" = "6:15",
                                           "0630 - 0645" = "6:30",
                                           "0645 - 0700" = "6:45",                                                "0700 - 0715" = "7:00",
                                           "0715 - 0730" = "7:15",
                                           "0730 - 0745" = "7:30",
                                           "0745 - 0800" = "7:45",
                                           "0800 - 0815" = "8:00",
                                           "0815 - 0830" = "8:15",
                                           "0830 - 0845" = "8:30",
                                           "0845 - 0900" = "8:45",
                                           "0900 - 0915" = "9:00",
                                           "0915 - 0930" = "9:15",
                                           "0930 - 0945" = "9:30",
                                           "0945 - 1000" = "9:45",
                                           "1000 - 1015" = "10:00",
                                           "1015 - 1030" = "10:15",
                                           "1030 - 1045" = "10:30",
                                           "1045 - 1100" = "10:45",
                                           "1100 - 1115" = "11:00",
                                           "1115 - 1130" = "11:15",
                                           "1130 - 1145" = "11:30",
                                           "1145 - 1200" = "11:45",
                                           "1200 - 1215" = "12:00",
                                           "1215 - 1230" = "12:15",
                                           "1230 - 1245" = "12:30",
                                           "1245 - 1300" = "12:45",
                                           "1300 - 1315" = "13:00",
                                           "1315 - 1330" = "13:15",
                                           "1330 - 1345" = "13:30",
                                           "1345 - 1400" = "13:45",
                                           "1400 - 1415" = "14:00",
                                           "1415 - 1430" = "14:15",
                                           "1430 - 1445" = "14:30",
                                           "1445 - 1500" = "14:45",
                                           "1500 - 1515" = "15:00",
                                           "1515 - 1530" = "15:15",
                                           "1530 - 1545" = "15:30",
                                           "1545 - 1600" = "15:45",
                                           "1600 - 1615" = "16:00",
                                           "1615 - 1630" = "16:15",
                                           "1630 - 1645" = "16:30",
                                           "1645 - 1700" = "16:45",
                                           "1700 - 1715" = "17:00",
                                           "1715 - 1730" = "17:15",
                                           "1730 - 1745" = "17:30",
                                           "1745 - 1800" = "17:45",
                                           "1800 - 1815" = "18:00",
                                           "1815 - 1830" = "18:15",
                                           "1830 - 1845" = "18:30",
                                           "1845 - 1900" = "18:45",
                                           "1900 - 1915" = "19:00",
                                           "1915 - 1930" = "19:15",
                                           "1930 - 1945" = "19:30",
                                           "1945 - 2000" = "19:45",
                                           "2000 - 2015" = "20:00",
                                           "2015 - 2030" = "20:15",
                                           "2030 - 2045" = "20:30",
                                           "2045 - 2100" = "20:45",
                                           "2100 - 2115" = "21:00",
                                           "2115 - 2130" = "21:15",
                                           "2130 - 2145" = "21:30",
                                           "2145 - 2200" = "21:45")) +
                 scale_fill_brewer(palette = "Dark2") +
                 scale_color_steps() +
                 labs(title="Bike Traffic during the day: Outer London",
                      y = "Total Rides") +
                 theme_bw()

# Inner:
inner_rides <- ggplot(inner, aes(x = Time, y = Total.cycles, fill = Direction)) +
                 geom_bar(stat = "identity") +
                 scale_x_discrete(guide = guide_axis(angle = 90),
                                  labels=c("0600 - 0615" = "6:00", 
                                           "0615 - 0630" = "6:15",
                                           "0630 - 0645" = "6:30",
                                           "0645 - 0700" = "6:45",                                                "0700 - 0715" = "7:00",
                                           "0715 - 0730" = "7:15",
                                           "0730 - 0745" = "7:30",
                                           "0745 - 0800" = "7:45",
                                           "0800 - 0815" = "8:00",
                                           "0815 - 0830" = "8:15",
                                           "0830 - 0845" = "8:30",
                                           "0845 - 0900" = "8:45",
                                           "0900 - 0915" = "9:00",
                                           "0915 - 0930" = "9:15",
                                           "0930 - 0945" = "9:30",
                                           "0945 - 1000" = "9:45",
                                           "1000 - 1015" = "10:00",
                                           "1015 - 1030" = "10:15",
                                           "1030 - 1045" = "10:30",
                                           "1045 - 1100" = "10:45",
                                           "1100 - 1115" = "11:00",
                                           "1115 - 1130" = "11:15",
                                           "1130 - 1145" = "11:30",
                                           "1145 - 1200" = "11:45",
                                           "1200 - 1215" = "12:00",
                                           "1215 - 1230" = "12:15",
                                           "1230 - 1245" = "12:30",
                                           "1245 - 1300" = "12:45",
                                           "1300 - 1315" = "13:00",
                                           "1315 - 1330" = "13:15",
                                           "1330 - 1345" = "13:30",
                                           "1345 - 1400" = "13:45",
                                           "1400 - 1415" = "14:00",
                                           "1415 - 1430" = "14:15",
                                           "1430 - 1445" = "14:30",
                                           "1445 - 1500" = "14:45",
                                           "1500 - 1515" = "15:00",
                                           "1515 - 1530" = "15:15",
                                           "1530 - 1545" = "15:30",
                                           "1545 - 1600" = "15:45",
                                           "1600 - 1615" = "16:00",
                                           "1615 - 1630" = "16:15",
                                           "1630 - 1645" = "16:30",
                                           "1645 - 1700" = "16:45",
                                           "1700 - 1715" = "17:00",
                                           "1715 - 1730" = "17:15",
                                           "1730 - 1745" = "17:30",
                                           "1745 - 1800" = "17:45",
                                           "1800 - 1815" = "18:00",
                                           "1815 - 1830" = "18:15",
                                           "1830 - 1845" = "18:30",
                                           "1845 - 1900" = "18:45",
                                           "1900 - 1915" = "19:00",
                                           "1915 - 1930" = "19:15",
                                           "1930 - 1945" = "19:30",
                                           "1945 - 2000" = "19:45",
                                           "2000 - 2015" = "20:00",
                                           "2015 - 2030" = "20:15",
                                           "2030 - 2045" = "20:30",
                                           "2045 - 2100" = "20:45",
                                           "2100 - 2115" = "21:00",
                                           "2115 - 2130" = "21:15",
                                           "2130 - 2145" = "21:30",
                                           "2145 - 2200" = "21:45")) +
                 scale_fill_brewer(palette = "Dark2") +
                 labs(title="Bike Traffic during the day: Inner London",
                      y = "Total Rides") +
                 theme_bw()

# Central:
central_rides <- ggplot(central, aes(x = Time, y = Total.cycles, fill = Direction)) +
                   geom_bar(stat = "identity") +
                   scale_x_discrete(guide = guide_axis(angle = 90),
                                    labels=c("0600 - 0615" = "6:00", 
                                    "0615 - 0630" = "6:15",
                                    "0630 - 0645" = "6:30",
                                    "0645 - 0700" = "6:45",                                                "0700 - 0715" = "7:00",
                                    "0715 - 0730" = "7:15",
                                    "0730 - 0745" = "7:30",
                                    "0745 - 0800" = "7:45",
                                    "0800 - 0815" = "8:00",
                                    "0815 - 0830" = "8:15",
                                    "0830 - 0845" = "8:30",
                                    "0845 - 0900" = "8:45",
                                    "0900 - 0915" = "9:00",
                                    "0915 - 0930" = "9:15",
                                    "0930 - 0945" = "9:30",
                                    "0945 - 1000" = "9:45",
                                    "1000 - 1015" = "10:00",
                                    "1015 - 1030" = "10:15",
                                    "1030 - 1045" = "10:30",
                                    "1045 - 1100" = "10:45",
                                    "1100 - 1115" = "11:00",
                                    "1115 - 1130" = "11:15",
                                    "1130 - 1145" = "11:30",
                                    "1145 - 1200" = "11:45",
                                    "1200 - 1215" = "12:00",
                                    "1215 - 1230" = "12:15",
                                    "1230 - 1245" = "12:30",
                                    "1245 - 1300" = "12:45",
                                    "1300 - 1315" = "13:00",
                                    "1315 - 1330" = "13:15",
                                    "1330 - 1345" = "13:30",
                                    "1345 - 1400" = "13:45",
                                    "1400 - 1415" = "14:00",
                                    "1415 - 1430" = "14:15",
                                    "1430 - 1445" = "14:30",
                                    "1445 - 1500" = "14:45",
                                    "1500 - 1515" = "15:00",
                                    "1515 - 1530" = "15:15",
                                    "1530 - 1545" = "15:30",
                                    "1545 - 1600" = "15:45",
                                    "1600 - 1615" = "16:00",
                                    "1615 - 1630" = "16:15",
                                    "1630 - 1645" = "16:30",
                                    "1645 - 1700" = "16:45",
                                    "1700 - 1715" = "17:00",
                                    "1715 - 1730" = "17:15",
                                    "1730 - 1745" = "17:30",
                                    "1745 - 1800" = "17:45",
                                    "1800 - 1815" = "18:00",
                                    "1815 - 1830" = "18:15",
                                    "1830 - 1845" = "18:30",
                                    "1845 - 1900" = "18:45",
                                    "1900 - 1915" = "19:00",
                                    "1915 - 1930" = "19:15",
                                    "1930 - 1945" = "19:30",
                                    "1945 - 2000" = "19:45",
                                    "2000 - 2015" = "20:00",
                                    "2015 - 2030" = "20:15",
                                    "2030 - 2045" = "20:30",
                                    "2045 - 2100" = "20:45",
                                    "2100 - 2115" = "21:00",
                                    "2115 - 2130" = "21:15",
                                    "2130 - 2145" = "21:30",
                                    "2145 - 2200" = "21:45")) +
                    scale_fill_brewer(palette = "Dark2") +
                    labs(title="Bike Traffic during the day: Central London",
                              y = "Total Rides") +
                    theme_bw()

# Greater London:
central_rides <- ggplot(greater, aes(x = Time, 
                                     y = Total.cycles, 
                                     fill = Direction)) +
                   geom_bar(stat = "identity") +
                   scale_x_discrete(guide = guide_axis(angle = 90),
                                    labels=c("0600 - 0615" = "6:00", 
                                             "0615 - 0630" = "6:15",
                                             "0630 - 0645" = "6:30",
                                             "0645 - 0700" = "6:45",
                                             "0700 - 0715" = "7:00",
                                             "0715 - 0730" = "7:15",
                                             "0730 - 0745" = "7:30",
                                             "0745 - 0800" = "7:45",
                                             "0800 - 0815" = "8:00",
                                             "0815 - 0830" = "8:15",
                                             "0830 - 0845" = "8:30",
                                             "0845 - 0900" = "8:45",
                                             "0900 - 0915" = "9:00",
                                             "0915 - 0930" = "9:15",
                                             "0930 - 0945" = "9:30",
                                             "0945 - 1000" = "9:45",
                                             "1000 - 1015" = "10:00",
                                             "1015 - 1030" = "10:15",
                                             "1030 - 1045" = "10:30",
                                             "1045 - 1100" = "10:45",
                                             "1100 - 1115" = "11:00",
                                             "1115 - 1130" = "11:15",
                                             "1130 - 1145" = "11:30",
                                             "1145 - 1200" = "11:45",
                                             "1200 - 1215" = "12:00",
                                             "1215 - 1230" = "12:15",
                                             "1230 - 1245" = "12:30",
                                             "1245 - 1300" = "12:45",
                                             "1300 - 1315" = "13:00",
                                             "1315 - 1330" = "13:15",
                                             "1330 - 1345" = "13:30",
                                             "1345 - 1400" = "13:45",
                                             "1400 - 1415" = "14:00",
                                             "1415 - 1430" = "14:15",
                                             "1430 - 1445" = "14:30",
                                             "1445 - 1500" = "14:45",
                                             "1500 - 1515" = "15:00",
                                             "1515 - 1530" = "15:15",
                                             "1530 - 1545" = "15:30",
                                             "1545 - 1600" = "15:45",
                                             "1600 - 1615" = "16:00",
                                             "1615 - 1630" = "16:15",
                                             "1630 - 1645" = "16:30",
                                             "1645 - 1700" = "16:45",
                                             "1700 - 1715" = "17:00",
                                             "1715 - 1730" = "17:15",
                                             "1730 - 1745" = "17:30",
                                             "1745 - 1800" = "17:45",
                                             "1800 - 1815" = "18:00",
                                             "1815 - 1830" = "18:15",
                                             "1830 - 1845" = "18:30",
                                             "1845 - 1900" = "18:45",
                                             "1900 - 1915" = "19:00",
                                             "1915 - 1930" = "19:15",
                                             "1930 - 1945" = "19:30",
                                             "1945 - 2000" = "19:45",
                                             "2000 - 2015" = "20:00",
                                             "2015 - 2030" = "20:15",
                                             "2030 - 2045" = "20:30",
                                             "2045 - 2100" = "20:45",
                                             "2100 - 2115" = "21:00",
                                             "2115 - 2130" = "21:15",
                                             "2130 - 2145" = "21:30",
                                             "2145 - 2200" = "21:45")) +
                      scale_fill_brewer(palette = "Dark2") +
                      labs(title="Peak Hours of Bike Traffic by Direction",
                           y = "Total Rides") +
                      theme_bw()

# Making the plots interactive:
ggplotly(outer_rides) %>%
  layout(xaxis = list(tickangle = -90))

ggplotly(inner_rides) %>%
  layout(xaxis = list(tickangle = -90))

ggplotly(central_rides) %>%
  layout(xaxis = list(tickangle = -90))

ggplotly(greater_rides) %>%
  layout(xaxis = list(tickangle = -90))

# Checking possible correlations between total cycles and other variables:
# Keeping only numeric columns:
# 1. Preparing data
# Outer London:
outer_num <- outer[, c("Number.of.male.cycles", 
                       "Number.of.female.cycles",
                       "Number.of.unknown.cycles",
                       "Total.cycles")]

# Inner London:
inner_num <- inner[, c("Number.of.private.cycles", 
                       "Number.of.cycle.hire.bikes",
                       "Total.cycles")]

# Central London:
central_num <- central[, c("Number.of.private.cycles", 
                              "Number.of.cycle.hire.bikes",
                              "Total.cycles")]

# Greater London:
greater_num <- greater[, c("Start.hour", 
                           "Total.cycles",
                           "Start.minute",
                           "Year")]

# 2. Correlation matrices:
# Outer London:
cor(outer_num)

## high correlation .977 between male cycles and total cycles 
## and .675 correlation with female cycles

# Inner London:
cor(inner_num)

## high correlation with private hires .996 
## and moderate with hire bikes .430

# Central London:
cor(central_num)

## high correlation between private hires and total cycles .996
## fairly high with hire bikes .755

# Greater London:
cor(greater_num)

## no major correlations between the numeric variables
## Greater London dataset discarded for LR model

# 3. Linear Regression Models:
# Outer London:
model_out <- lm(Total.cycles ~ Number.of.male.cycles 
                + Number.of.female.cycles, 
                data=outer_num)
summary(model_out)
## adj R-squared of 0.9975

# Inner London:
model_in <- lm(Total.cycles ~ Number.of.private.cycles  + 
               Number.of.cycle.hire.bikes, 
               data=inner_num)
summary(model_in)
## adj R-squared 0.9996

# Central London:
model_cen <- lm(Total.cycles ~ Number.of.private.cycles  + 
                  Number.of.cycle.hire.bikes, data=central_num)
summary(model_cen)

## adj R-squared 1

# Plotting linear regressions:
ggPredict(model_out, colorAsFactor = TRUE, interactive = TRUE)

ggPredict(model_in, colorAsFactor = TRUE, interactive = TRUE)

ggPredict(model_cen, colorAsFactor = TRUE, interactive = TRUE)

# Checking MLR assumptions:
# Linearity:
plot(model_out)
plot(model_in)
plot(model_cen)

# Independence:
durbinWatsonTest(model_out)
durbinWatsonTest(model_in)
durbinWatsonTest(model_cen)

## durbin-watson tests shows there isn't independence between the variables.
## MLR disregarded as model is not strong enough.
