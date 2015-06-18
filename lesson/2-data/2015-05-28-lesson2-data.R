#'---
#' date: 2015-05-28
#' title: Lesson 2. Data
#' author: Jonas Schöley
#' license: MIT
#' output:
#'   md_document:
#'     variant: markdown_github
#'     preserve_yaml: TRUE
#'---

library(ggplot2) # our plotting package
library(tidyr)   # for reshaping between wide and long format
library(dplyr)   # for general data transformation

#'## Revisiting the Name Data Set

#' The data set is stored as an `Rdata` file. We can load it from the disk and
#' it immediately appears as an object in our workspace. In contrast to other
#' data sources (e.g. csv, dat) there is no need to assign it to an object
#' first.

load("../../data/name.Rdata")
head(name)

#' Connect the letter pairs with a line segment and colour the segments by sex.
ggplot(name) +
  geom_segment(aes(x = First_n, y = 0,
                   xend = 0, yend = Last_n,
                   colour = Sex)) +
  scale_x_continuous(breaks = 1:26, labels = LETTERS, limits = c(0, 26)) +
  scale_y_continuous(breaks = 1:26, labels = LETTERS, limits = c(0, 26)) +
  coord_fixed() # Units on x and y are represented by the same distance.

#'## Reshaping Data: World Phones

head(WorldPhones)

#' We use `dplyr` and `tidyr` to reshape our data.

WorldPhones %>%
  # Convert to data frame.
  as.data.frame %>%
  # Add a year column containing rownames of "WorldPhones".
  mutate(Year = as.numeric(rownames(.))) %>%
  # Reshape into long-format.
  gather(key = Region, value = N, -Year) -> W2

head(W2)

#' A line plot of telephones in use over time by region.
ggplot(W2) +
  geom_line(aes(x = Year, y = N, group = Region, colour = Region))

#'# Reshaping Data: USArrests

head(USArrests)

USArrests %>%
  # Add a state column containing rownames of "USArrests".
  mutate(State = rownames(.)) %>%
  # Reshape into long format.
  gather(key = Crime, value = p, c(1, 2, 4)) -> USA

head(USA)

#' Select the murder cases.
USA %>% filter(Crime == "Murder") -> murder

#' Barchart of murder rate per state.
ggplot(murder) +
  geom_bar(aes(x = reorder(State, p), y = p),
           stat = "identity") + # Take the y values form the data,
                                # don't do any additional transformations.
  coord_flip() # Interchange x and y-scales to do horizontal bars.

#' Barchart of all crimes per state.
ggplot(USA) +
  geom_bar(aes(x = reorder(State, p), y = p, fill = Crime),
           stat = "identity") +
  coord_flip()

#'## Reshaping Data: Paolos Example

#' Swiss data on economic indicators.
head(swiss)

swiss %>%
  # Add a city column containing the rownames of "swiss".
  mutate(City = rownames(.)) -> swi

head(swi)

#' Plot the percentage of catholic population in each city.
ggplot(swi, aes(x = reorder(City, Catholic))) +
  geom_bar(aes(y = Catholic),
           stat = "identity") +
  coord_flip()

#' Plot the percentage of catholic population in each city and
#' add points for Infant mortality.
ggplot(swi, aes(x = reorder(City, Catholic))) +
  geom_bar(aes(y = Catholic),
           stat = "identity") +
  geom_point(aes(y = Infant.Mortality),
             colour = "red") +
  coord_flip()

#' Instead of points, we can also add a line. But because we got a discrete axis
#' the ggplot line geom gets a bit confused and we have to specify that all our
#' Infant Mortality values belong to the same line, i.e. the same group.
ggplot(swi, aes(x = reorder(City, Catholic))) +
  geom_bar(aes(y = Catholic),
           stat = "identity") +
  geom_line(aes(y = Infant.Mortality, group = 1),
             colour = "#FF0092", size = 1) +
  coord_flip()

#'## Reshaping Data: Steffies Example

#' Steffie decided to reshape all the economic indicator variables into an
#' indicator- and a value column.
head(swi) %>%
  gather(key = Indicator, value = p, 1:6) -> swi

head(swi)

#' With this data structure she's able to "facet" over all indicators.
ggplot(swi) +
  geom_bar(aes(x = City, y = p),
           stat = "identity") +
  facet_wrap(~ Indicator) +
  coord_flip()
