#'---
#' date: 2015-06-15
#' title: Lesson 5. Case Studies
#' author: Jonas Schöley
#' license: MIT
#' output:
#'   md_document:
#'     variant: markdown_github
#'     preserve_yaml: TRUE
#' ---

library(ggplot2)   # our plotting package
library(tidyr)     # for reshaping between wide and long format
library(dplyr)     # for general data transformation
library(ggmap)     # using googlemaps (and other) in plots
library(lubridate) # working with dates

#'## Case Study 1. The Fukushima Nuclear Disaster.

#' During the Fukushima nuclear disaster in 2011 private individuals streamed
#' their Geiger counter readings via webcam to the internet. This dataset
#' contains readings from some of these web-streams over time. Note that these
#' are not official readings but readings provided by private individuals.
fukushima <- read.csv("../../data/fukushima.csv",
                      skip = 8, stringsAsFactors = FALSE)

head(fukushima)

#' Convert the Time variable to R-date-time format.
fukushima %>%
  mutate(Time = parse_date_time(Time,
                                orders = "%Y%m%d %H%M",
                                tz = "CET")) -> fukushima

#' Plot smoothed timelines of radiation levels.
ggplot(fukushima, aes(x = Time, y = Value, group = Source, colour = Source)) +
  # The raw data is displayed as points.
  geom_point(alpha = 0.5, size = 1.2) +
  # We apply smoothing to each timeline.
  geom_smooth(size = 1) +
  # The mean radiation level for each timeline is added.
  geom_line(stat = "hline", yintercept = "mean", lty = 2) +
  # We have separate panels for each unit of measurement.
  facet_wrap(~ Unit, scales = "free_y")

#'## Guardian: The Counted

#' Source: <http://www.theguardian.com/thecounted>

counted <- read.csv("../../data/the_counted.csv",
                    na.strings = "Unknown",
                    stringsAsFactors = FALSE)

uspop <- read.csv("../../data/uspop_2014.csv",
                  skip = 8, stringsAsFactors = FALSE)

RecodeArmed <- function (x) {
  x <- ifelse(x == "Disputed", NA, x)
  x <- ifelse(x == "Firearm", "Yes - Firearm", x)
  x <- ifelse(x != "No" & x != "Yes - Firearm", "Yes - Other", x)
  x
}

counted %>%
  mutate(citystate = paste(city, state),
         armed_simple = RecodeArmed(armed),
         time = paste(month, day, year),
         time = parse_date_time(time, orders = "%B %d! %Y!")) %>%
  bind_cols(., geocode(.$citystate, messaging = FALSE)) %>%
  inner_join(., uspop, by = c("state" = "Short")) -> killed

usmap <- get_map(location = c(-130, 20, -60, 50), maptype = "toner")

ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat),
             colour = "red")

ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red")

plot_killed +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red") +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5)

plot_killed +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red") +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5) +
  geom_polygon(data = killed,
               aes(x = lon, y = lat, fill = ..level..),
               stat = "density2d",
               bins = 5,
               alpha = 0.2)

plot_killed +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red") +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5) +
  geom_polygon(data = killed,
               aes(x = lon, y = lat, fill = ..level..),
               stat = "density2d",
               bins = 5,
               alpha = 0.2) +
  facet_wrap(~ armed_simple) +
  guides(fill = FALSE, size = FALSE)

killed %>%
  group_by(State, armed_simple) %>%
  summarise(deaths = n(),
            population = unique(Population)) %>%
  ungroup() %>%
  mutate(rate = deaths / population * 1E6,
         state = tolower(State)) %>%
  select(-State) -> killed_aggr

us_states_border <- map_data("state")

left_join(x = us_states_border, y = killed_aggr,
          by = c("region" = "state")) -> killed_aggr_map

ggmap(usmap) +
  geom_polygon(data = killed_aggr_map,
               aes(x = long, y = lat, group = group,
                   fill = cut(rate, breaks = seq(0, 3, 1))),
               alpha = 0.5) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             colour = "red", alpha = 0.4,
             stat = "sum") +
  facet_wrap(~ armed_simple) +
  scale_fill_brewer(name = "People Killed\nper Million",
                    type = "seq", palette = 9) +
  scale_size(name = "Cases of\nPeople Killed", range = c(2, 10))
