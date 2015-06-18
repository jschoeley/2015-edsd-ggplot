#'---
#' date: 2015-05-27
#' title: Lesson 1. Basic Chart Types in ggplot
#' author: Jonas Schöley
#' license: MIT
#' output:
#'   md_document:
#'     variant: markdown_github
#'     preserve_yaml: TRUE
#'---

#' In order to start we need the following packages.

library(ggplot2) # our plotting package
library(tidyr)   # for reshaping between wide and long format
library(dplyr)   # for general data transformation

#'## The Name Example

#' The data contains the first and last letter of some peoples given names.
name <- data.frame(
  First = c("M", "L", "A", "K", "F",
            "P", "A", "S", "J", "E",
            "M", "D", "J", "J", "F",
            "C", "S"),
  Last  = c("L", "A", "O", "A", "O",
            "O", "B", "A", "Y", "E",
            "A", "S", "S", "N", "A",
            "A", "E"),
  Sex   = c("M", "F", "M", "F", "M",
            "M", "M", "F", "F", "M",
            "F", "M", "M", "M", "F",
            "F", "F")
)

#' We reshape the data a bit.
name %>% # Take the name data frame...
  mutate(
    # ...and add columns with the numerical alphabet positions
    # of the first and last letters.
    First_n = match(First, LETTERS),
    Last_n  = match(Last, LETTERS),
    # Also add a column with combined first and last letters.
    Comb    = paste0(First, Last)) -> name


#' This is a scatterplot of the alphabetical letter positions.
ggplot(name) +
  geom_point(aes(x = First_n, y = Last_n, label = Comb))

#' We can label the numerical scales with the corresponding letters.
ggplot(name) +
  geom_point(aes(x = First_n, y = Last_n, label = Comb)) +
  scale_x_continuous(breaks = 1:26, labels = LETTERS) +
  scale_y_continuous(breaks = 1:26, labels = LETTERS)

#' Instead of points, we might as well draw letter pairs.
ggplot(name) +
  geom_text(aes(x = First_n, y = Last_n, label = Comb)) +
  scale_x_continuous(breaks = 1:26, labels = LETTERS) +
  scale_y_continuous(breaks = 1:26, labels = LETTERS)

#'## Other Examples

#' A data set of weight over time for chickens on different diets.
head(ChickWeight)

#' For each chicken we draw a red line of weight over time.
ggplot(ChickWeight) +
  geom_line(aes(x = Time, y = weight, group = Chick),
            colour = "red")

#' A data set of environmental indicators.
head(airquality)

#' We plot a histogram of ozone levels. Each bin has a width of 50.
ggplot(airquality) +
  geom_histogram(aes(x = Ozone),
                 binwidth = 50)

#' From the same data we make a boxplot. Note that we have to tell ggplot where
#' to put the boxplot on the x-axis.
ggplot(airquality) +
  geom_boxplot(aes(x = 0, y = Ozone))
