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

#' The British newspaper "The Guardian" assembled a database of people killed
#' by police in the US in 2015.
#' Source: <http://www.theguardian.com/thecounted>

#' Read the data. Note that the strings should not be automatically converted
#' to factor variables.
counted <- read.csv("../../data/the_counted.csv",
                    na.strings = "Unknown",
                    stringsAsFactors = FALSE)
head(counted)

#' Use Google Maps API to get latitude and longitude for each of the places
#' in the dataset. This is completely automated, but takes some time and is
#' limited by Google to 2500 map requests per day per IP. The `geocode` function
#' is part of the `ggmap` package.
counted %>%
  mutate(citystate = paste(city, state)) %>%
  bind_cols(., geocode(.$citystate)) -> killed

#' Download a map of the US along with geographical coordinates. Note that this
#' is not a shapefile but a raster image in Mercator map projection with
#' longitude and latitude information. It is readily formatted for usage with
#' `ggmap`.
usmap <- get_map(location = c(-130, 20, -60, 50),
                 maptype = "toner")

#' A dot-density map of killed persons located by longitude and latitude.
#' Instead of `ggplot` + `data` we use the `ggmap` command and supply it with
#' our mapdata produced by `get_map`. The rest standard `ggplot` and we
#' can use add ggplot geoms as usual. The only difference is that we must pass
#' the data we want to display on the map as an extra `data` argument to the
#' individual geoms.
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat),
             colour = "red")

#' We can sum up cases at the same position and map the summed value to
#' the size aestetic. `..n..` is a variable produced by the `sum` statistic
#' and gives the number of cases at each `lon` and `lat`.
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", alpha = 0.7)

#' We overlay 2d density contours...
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", alpha = 0.7) +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5)

# ...and shade them according to level.
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", alpha = 0.7) +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5) +
  geom_polygon(data = killed,
               aes(x = lon, y = lat, fill = ..level..),
               stat = "density2d",
               bins = 5,
               alpha = 0.2)

#' For further analysis we recode the `armed` variable into 4 categories:
#' NA, No, Yes - Firearm, Yes - Other. This is a small function doing just that.
RecodeArmed <- function (x) {
  x <- ifelse(x == "Disputed", NA, x)
  x <- ifelse(x == "Firearm", "Yes - Firearm", x)
  x <- ifelse(x != "No" & x != "Yes - Firearm", "Yes - Other", x)
  x
}

killed %>%
  mutate(armed_simple = RecodeArmed(armed)) -> killed

#' Now let's produce our density map separate by armament status.
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", alpha = 0.7) +
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

#' The problem with these density maps is that they don't adjust for different
#' populations sizes in the different regions and therefore just show the
#' geographical population-distribution in the United States. A strategy to show
#' the number of killed standardized by population size is to calculate
#' population-specific event-proportions and show the result as a
#' shaded map region. In order to do this we need the US population by state
#' and geographic information about the borders of the US state (a shapefile).

#' This is a dataset of the 2014 US population by state.
uspop <- read.csv("../../data/uspop_2014.csv",
                  skip = 8, stringsAsFactors = FALSE)
head(uspop)

#' So let's get our state-specific proportions by armament status.
killed %>%
  # Merge Guardian data with population by state data.
  inner_join(., uspop, by = c("state" = "Short")) %>%
  # Count the number of killed by state and armament status.
  group_by(State, armed_simple) %>%
  summarise(deaths = n(),
            population = unique(Population)) %>%
  ungroup() %>%
  # Cases per 1,000,000 population in state.
  mutate(prop = deaths / population * 1E6,
         state = tolower(State)) %>%
  select(-State) -> killed_aggr

head(killed_aggr)

#' This is the shape data of the US states.
us_states_border <- map_data("state")

#' We merge it with the data we want to plot.
left_join(us_states_border, killed_aggr,
          by = c("region" = "state")) -> killed_aggr_map

#' Let's do it!
ggmap(usmap) +
  geom_polygon(data = killed_aggr_map,
               aes(x = long, y = lat, group = group,
                   fill = cut(prop, breaks = seq(0, 3, 1))),
               alpha = 0.6) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             colour = "red", alpha = 0.4,
             stat = "sum") +
  facet_wrap(~ armed_simple) +
  scale_fill_brewer(name = "People Killed\nper Million",
                    type = "seq", palette = 9) +
  scale_size(name = "Cases of\nPeople Killed", range = c(2, 10))
