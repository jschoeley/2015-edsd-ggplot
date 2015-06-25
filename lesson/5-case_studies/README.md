---
date: 2015-06-15
title: Lesson 5. Case Studies
author: Jonas Schöley
license: MIT
output:
  md_document:
    variant: markdown_github
    preserve_yaml: TRUE
---

Case Study 1. The Fukushima Nuclear Disaster.
---------------------------------------------

During the Fukushima nuclear disaster in 2011 private individuals streamed their Geiger counter readings via webcam to the internet. This dataset contains readings from some of these web-streams over time. Note that these are not official readings but readings provided by private individuals.

``` r
fukushima <- read.csv("../../data/fukushima.csv",
                      skip = 8, stringsAsFactors = FALSE)

head(fukushima)
```

    ##   Source                 Time Value Unit
    ## 1 tokyo1 2011-03-16 00:44 CET  12.2  cpm
    ## 2 tokyo1 2011-03-16 01:46 CET  13.3  cpm
    ## 3 tokyo1 2011-03-16 12:42 CET  31.0  cpm
    ## 4 tokyo1 2011-03-16 14:18 CET  17.2  cpm
    ## 5 tokyo1 2011-03-16 15:16 CET  16.7  cpm
    ## 6 tokyo1 2011-03-16 16:06 CET  16.3  cpm

Convert the Time variable to R-date-time format.

``` r
fukushima %>%
  mutate(Time = parse_date_time(Time,
                                orders = "%Y%m%d %H%M",
                                tz = "CET")) -> fukushima
```

Plot smoothed timelines of radiation levels.

``` r
ggplot(fukushima, aes(x = Time, y = Value, group = Source, colour = Source)) +
  # The raw data is displayed as points.
  geom_point(alpha = 0.5, size = 1.2) +
  # We apply smoothing to each timeline.
  geom_smooth(size = 1) +
  # The mean radiation level for each timeline is added.
  geom_line(stat = "hline", yintercept = "mean", lty = 2) +
  # We have separate panels for each unit of measurement.
  facet_wrap(~ Unit, scales = "free_y")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

Guardian: The Counted
---------------------

The British newspaper "The Guardian" assembled a database of people killed by police in the US in 2015. Source: <http://www.theguardian.com/thecounted> Read the data. Note that the strings should not be automatically converted to factor variables.

``` r
counted <- read.csv("../../data/the_counted.csv",
                    na.strings = "Unknown",
                    stringsAsFactors = FALSE)
head(counted)
```

    ##                name age gender   raceethnicity   month day year
    ## 1   Roberto Ornelas  18   Male Hispanic/Latino January   1 2015
    ## 2   Matthew Ajibade  22   Male           Black January   1 2015
    ## 3     Garrett Gagne  22   Male           White January   1 2015
    ## 4      Lewis Lembke  47   Male           White January   2 2015
    ## 5 Michael Kocher Jr  19   Male           White January   3 2015
    ## 6     John Quintero  23   Male Hispanic/Latino January   3 2015
    ##               streetaddress      city state             cause
    ## 1           39 N Marlin Ave Key Largo    FL              <NA>
    ## 2      1050 Carl Griffin Dr  Savannah    GA  Death in custody
    ## 3 Crowell Rd and Tipcart Dr   Chatham    MA Struck by vehicle
    ## 4      4505 SW Masters Loop     Aloha    OR           Gunshot
    ## 5        2600 Kaumualii Hwy Kaumakani    HI Struck by vehicle
    ## 6      500 North Oliver Ave   Wichita    KS           Gunshot
    ##                 lawenforcementagency   armed
    ## 1     Monroe County Sheriff's Office      No
    ## 2    Chatham County Sheriff's Office      No
    ## 3          Chatham Police Department      No
    ## 4 Washington County Sheriff's Office Firearm
    ## 5            Kauai Police Department      No
    ## 6          Wichita Police Department      No

Use Google Maps API to get latitude and longitude for each of the places in the dataset. This is completely automated, but takes some time and is limited by Google to 2500 map requests per day per IP. The `geocode` function is part of the `ggmap` package.

``` r
counted %>%
  mutate(citystate = paste(city, state)) %>%
  bind_cols(., geocode(.$citystate)) -> killed
```

Download a map of the US along with geographical coordinates. Note that this is not a shapefile but a raster image in Mercator map projection with longitude and latitude information. It is readily formatted for usage with `ggmap`.

``` r
usmap <- get_map(location = c(-130, 20, -60, 50),
                 maptype = "toner")
```

A dot-density map of killed persons located by longitude and latitude. Instead of `ggplot` + `data` we use the `ggmap` command and supply it with our mapdata produced by `get_map`. The rest standard `ggplot` and we can use add ggplot geoms as usual. The only difference is that we must pass the data we want to display on the map as an extra `data` argument to the individual geoms.

``` r
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat),
             colour = "red")
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

We can sum up cases at the same position and map the summed value to the size aestetic. `..n..` is a variable produced by the `sum` statistic and gives the number of cases at each `lon` and `lat`.

``` r
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", alpha = 0.7)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

We overlay 2d density contours...

``` r
ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", alpha = 0.7) +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
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
```

![](README_files/figure-markdown_github/unnamed-chunk-9-2.png)

For further analysis we recode the `armed` variable into 4 categories: NA, No, Yes - Firearm, Yes - Other. This is a small function doing just that.

``` r
RecodeArmed <- function (x) {
  x <- ifelse(x == "Disputed", NA, x)
  x <- ifelse(x == "Firearm", "Yes - Firearm", x)
  x <- ifelse(x != "No" & x != "Yes - Firearm", "Yes - Other", x)
  x
}

killed %>%
  mutate(armed_simple = RecodeArmed(armed)) -> killed
```

Now let's produce our density map separate by armament status.

``` r
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
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

The problem with these density maps is that they don't adjust for different populations sizes in the different regions and therefore just show the geographical population-distribution in the United States. A strategy to show the number of killed standardized by population size is to calculate population-specific event-proportions and show the result as a shaded map region. In order to do this we need the US population by state and geographic information about the borders of the US state (a shapefile). This is a dataset of the 2014 US population by state.

``` r
uspop <- read.csv("../../data/uspop_2014.csv",
                  skip = 8, stringsAsFactors = FALSE)
head(uspop)
```

    ##        State Short Population
    ## 1    Alabama    AL    4849377
    ## 2     Alaska    AK     736732
    ## 3    Arizona    AZ    6731484
    ## 4   Arkansas    AR    2966369
    ## 5 California    CA   38802500
    ## 6   Colorado    CO    5355866

So let's get our state-specific proportions by armament status.

``` r
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
```

    ## Source: local data frame [6 x 5]
    ## 
    ##    armed_simple deaths population      prop   state
    ## 1 Yes - Firearm      6    4849377 1.2372723 alabama
    ## 2   Yes - Other      2    4849377 0.4124241 alabama
    ## 3            No      1     736732 1.3573457  alaska
    ## 4 Yes - Firearm      1     736732 1.3573457  alaska
    ## 5            No      2    6731484 0.2971113 arizona
    ## 6 Yes - Firearm     15    6731484 2.2283348 arizona

This is the shape data of the US states.

``` r
us_states_border <- map_data("state")
```

We merge it with the data we want to plot.

``` r
left_join(us_states_border, killed_aggr,
          by = c("region" = "state")) -> killed_aggr_map
```

Let's do it!

``` r
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
```

![](README_files/figure-markdown_github/unnamed-chunk-16-1.png)
