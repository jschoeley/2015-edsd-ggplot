#'---
#' date: 2015-06-11
#' title: Lesson 3. Scales 1 - Scale Attributes
#' author: Jonas Schöley
#' license: MIT
#' output:
#'   md_document:
#'     variant: markdown_github
#'     preserve_yaml: TRUE
#'---

library(ggplot2) # our plotting package
library(dplyr)   # for general data transformation

#'## Gapminder

#' This package provides us with data from the Gapminder webpage
#' <http://www.gapminder.org/>.
library(gapminder)

head(gapminder)

#' Subset to year 2007.
gapminder %>% filter(year == 2007) -> gappy

head(gappy)

#' "Bubblecharts" are just scatterplots with pointsize mapped to some variable.

#' Note that the mapping of visual dimension to the data dimension takes place
#' inside the `aes` command. These are "aesthetics". On the other hand, we don't
#' treat the general transparency of our points (alpha) as an aesthetic. It is a
#' fixed value independent of the data.
bubble <-
  ggplot(gappy) +
  geom_point(aes(x      = gdpPercap,
                 y      = lifeExp,
                 size   = pop,
                 colour = continent),
             alpha = 0.5)
bubble

#' Things we can do with scales...

#' We can rename our scales:
bubble +
  scale_x_continuous(name = "GDP per capita") +
  scale_y_continuous(name = "Life Expectancy") +
  scale_size(name = "Population") +
  scale_color_discrete(name = "Continent")

#' We can transform our scales:
bubble +
  scale_x_continuous(trans = "log10") +
  scale_size(trans = "log10")

#' We can change the limits of our scales:
bubble +
  scale_x_continuous(limits = c(200, 100000)) +
  scale_y_continuous(limits = c(25, 85))

#' We can change the breaks (markings) of our scales:
bubble +
  scale_x_continuous(breaks = c(200, 400, 1000, 2000, 4000, 10000, 20000, 40000)) +
  scale_y_continuous(breaks = seq(25, 85, 5)) +
  scale_size(breaks = c(10^(6:9)))

#' We can change the labels (text at breaks) of our scales:
bubble +
  scale_size(breaks = c(10^(7:9)),
             labels = c("10 Million", "100 Million", "1 Billion"))

#' There are also specialized scales with unique attributes, like:

#' The maximum area of a "bubble"...
bubble +
  scale_size_area(max_size = 30)

#' ...or the colours used in a colour scale.
bubble +
  scale_color_brewer(type = "qual", palette = 6)

#' This is how the plot looks if we apply all these scale operations at once:
bubble +
  scale_x_continuous(name = "GDP per capita",
                     trans = "log10",
                     limits = c(200, 100000),
                     breaks = c(200, 400, 1000, 2000, 4000, 10000, 20000, 40000)) +
  scale_y_continuous(name = "Life Expectancy",
                     limits = c(25, 85),
                     seq(25, 85, 5)) +
  scale_size_area(name = "Population",
                  breaks = c(10^(7:9)),
                  labels = c("10 Million", "100 Million", "1 Billion"),
                  max_size = 30) +
  scale_color_brewer(name = "Continent",
                     type = "qual", palette = 6)
