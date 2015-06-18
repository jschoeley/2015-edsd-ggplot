#'---
#' date: 2015-06-12
#' title: Lesson 4. Scales 2 - Colour Scales
#' author: Jonas Schöley
#' license: MIT
#' output:
#'   md_document:
#'     variant: markdown_github
#'     preserve_yaml: TRUE
#' ---

library(ggplot2) # our plotting package
library(tidyr)   # for reshaping between wide and long format
library(dplyr)   # for general data transformation

#'## Sequential Colour Scale: Plotting Magnitudes

#' Danish and Swedish Female Period Death and Population Counts.
#' Taken from the Human Mortality Database.
load(file = "../../data/dnk_swe_fem_hmd.Rdata")

head(dnk_swe_fem_hmd)

#' Add mortality rates and quantile-cut mortality rates to data frame.
dnk_swe_fem_hmd %>%
  mutate(mx = Dx / Nx,
         mx_d = cut(mx, breaks = quantile(mx, na.rm = TRUE))) -> mort

head(mort)

#' A heatmap of mortality rates for Swedish and Danish females over period and
#' age (Period Lexis-surface). The mortality rates are cut into quantiles.
ggplot(mort) +
  geom_tile(aes(x = Year, y = Age, fill = mx_d)) +
  # Use a sequential scale created by colour expert Cynthia Brewer
  scale_fill_brewer(type = "seq") +
  # Create small multiples by country.
  facet_wrap(~ Country)

#'## Divergent Colour Scale: Plotting Differences and Proportions

#' We compute the proportions of Danish and Swedish mortality rates at each
#' period-age crossing.
mort %>%
  # Remove Dx, Nx and mx_d, otherwise `spread` gets confused. It only accepts a
  # single value column (mx in our case)
  select(-Dx, -Nx, -mx_d) %>%
  # Convert to "wide" format in order to make it easier to compute proportions.
  spread(key = Country, value = mx) %>%
  # Compute continuous and discrete proportions of mortality rates.
  mutate(prop = DNK / SWE) -> diffmort

diffmort

#' Plot the mortality proportions using a continuous divergent colour scale.
ggplot(diffmort) +
  geom_tile(aes(x = Year, y = Age, fill = prop)) +
  scale_fill_gradient2(
    # Lower limit of the colour scale should be half-mortality.
    # Upper limit should be double-mortality.
    limits = c(0.5, 2),
    breaks = c(0.5, 1, 2),
    # Note that we set the midpoint of our divergent colour scale to 1.
    midpoint = 1)

#' We face the problem that our colour scale is not symmetric on a
#' multaplicative level (where half and double are equally distanced from 1). We
#' can fix that with a logarithmic scale transformation. Not that we have to
#' reset the midpoint when we use a log-transform on our colour scale (log2(1) =
#' 0).
ggplot(diffmort) +
  geom_tile(aes(x = Year, y = Age, fill = prop)) +
  scale_fill_gradient2(
    limits = c(0.5, 2),
    breaks = c(0.5, 1, 2),
    midpoint = 0, trans = "log2")

#' A good alternative is to build the symmetric scale yourself and use discrete
#' value categories. These are the breaks we will use to cut our data.
breaks <- c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf)
diffmort %>%
  mutate(prop_d = cut(prop, breaks = breaks)) -> diffmort

ggplot(diffmort) +
  geom_tile(aes(x = Year, y = Age, fill = prop_d)) +
  scale_fill_brewer(type = "div")

#' Now let's get all out insane and add all the niceties to our plot to make it
#' "publication ready".
ggplot(diffmort) +
  geom_tile(aes(x = Year, y = Age,
                fill = prop_d)) +
  # add age, period, and cohort guidelines for every 10 years
  geom_vline(xintercept = seq(1910, 2000, 10),
             colour = "black", size = 0.5, lty = 3) +
  geom_hline(yintercept = seq(10, 100, 10),
             colour = "black", size = 0.5, lty = 3) +
  geom_abline(intercept = seq(-100, 110, 10) - 1910,
              colour = "black", size = 0.5, lty = 3) +
  # Add one of Brewers "divergent" colour scales.
  scale_fill_brewer(
    # Use math annotation in scale title.
    # See `help(plotmath)` for more information.
    name = expression(frac(m(x)[DNK], m(x)[SWE])),
    type = "div", palette = 4) +
  # Reverse the order of the legend.
  guides(fill = guide_legend(reverse = TRUE)) +
  # Change the labels on the x and y scales to
  # correspond to the Lexis grid and reduce the
  # extra space ggplot draws around the plot.
  scale_x_continuous(breaks = seq(1900, 2010, 10), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 110, 10), expand = c(0.01, 0)) +
  # Scale the x and the y axis equally so that a
  # cohort line is at a 45° angle.
  coord_fixed() +
  # Add a title
  ggtitle("Comparing Danish and Swedish Mortality Rates") +
  # Use a very minimal theme.
  theme_minimal()
