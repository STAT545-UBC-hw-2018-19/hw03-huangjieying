Use dply/ggplot2 to manipulate and explore data
================

## Load the data and packages

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
```

## Task1: Get the maximum and minimum of GDP per capita for all continents.

``` r
mm=gapminder %>% 
  group_by(continent) %>% 
  summarise(max.gdp=max(gdpPercap), min.gdp=min(gdpPercap))
knitr::kable(mm)
```

| continent |   max.gdp |    min.gdp |
| :-------- | --------: | ---------: |
| Africa    |  21951.21 |   241.1659 |
| Americas  |  42951.65 |  1201.6372 |
| Asia      | 113523.13 |   331.0000 |
| Europe    |  49357.19 |   973.5332 |
| Oceania   |  34435.37 | 10039.5956 |

Let’s visualize this data. Here I will use bar chart to plot maximum and
minimum values for each continent seperately.

``` r
gapminder %>% 
  group_by(continent) %>% 
  summarise(max=max(gdpPercap), min=min(gdpPercap)) %>%
  gather(key=type, value=GDP.per.capita, 2:3) %>% 
  ggplot(aes(x=continent, y=GDP.per.capita, fill=continent)) + 
  geom_bar(stat = "identity")+
  facet_wrap(.~type, scale="free_y") +
  guides(fill=FALSE)
```

![](hw03_use_dplyr_ggplot2_to_manipulate_and_explore_data_files/figure-gfm/minimum%20gdp-1.png)<!-- -->

## Task2: Look at the spread of GDP per capita within the continents.

Let’s look at the standard deviation of gdp on different continents
through `sd()` function

``` r
stats=gapminder %>% 
  group_by(continent) %>% 
  summarise(spread=sd(gdpPercap),
            min=min(gdpPercap),
            Qu.1st=quantile(gdpPercap, probs= 0.25),
            median=median(gdpPercap),
            mean=mean(gdpPercap),
            Qu.3st=quantile(gdpPercap, probs= 0.25),
            max=max(gdpPercap))
            
knitr::kable(stats)
```

| continent |    spread |        min |    Qu.1st |    median |      mean |    Qu.3st |       max |
| :-------- | --------: | ---------: | --------: | --------: | --------: | --------: | --------: |
| Africa    |  2827.930 |   241.1659 |   761.247 |  1192.138 |  2193.755 |   761.247 |  21951.21 |
| Americas  |  6396.764 |  1201.6372 |  3427.779 |  5465.510 |  7136.110 |  3427.779 |  42951.65 |
| Asia      | 14045.373 |   331.0000 |  1056.993 |  2646.787 |  7902.150 |  1056.993 | 113523.13 |
| Europe    |  9355.213 |   973.5332 |  7213.085 | 12081.749 | 14469.476 |  7213.085 |  49357.19 |
| Oceania   |  6358.983 | 10039.5956 | 14141.859 | 17983.304 | 18621.609 | 14141.859 |  34435.37 |

One way to visualize the spread of data is through histogram

``` r
gapminder %>% 
  ggplot(aes(gdpPercap))+
  geom_histogram(aes(y=..density.., fill=continent), bins=20)+
  geom_density()+
  facet_wrap(~continent)+
  guides(fill=FALSE)
```

![](hw03_use_dplyr_ggplot2_to_manipulate_and_explore_data_files/figure-gfm/spread%20of%20GDP%20per%20capita%20within%20the%20continents-1.png)<!-- -->

When we want to check the spread of data, using violin along with jitter
plot is a good option

``` r
gapminder%>%
  ggplot(aes(continent, gdpPercap, color = continent))+
  geom_jitter(aes(alpha = 0.1))+
  geom_violin() 
```

![](hw03_use_dplyr_ggplot2_to_manipulate_and_explore_data_files/figure-gfm/spread%20with%20violin%20and%20jitter%20plot-1.png)<!-- -->

## Task3: Compute a trimmed mean or a weighted mean of life expectancy for different years.

Here I will explore the mean weighting by population

``` r
df_m=gapminder %>% 
  group_by(year) %>% 
  summarise(weighted_lifeExp=weighted.mean(lifeExp, pop))
knitr::kable(df_m)
```

| year | weighted\_lifeExp |
| ---: | ----------------: |
| 1952 |          48.94424 |
| 1957 |          52.12189 |
| 1962 |          52.32438 |
| 1967 |          56.98431 |
| 1972 |          59.51478 |
| 1977 |          61.23726 |
| 1982 |          62.88176 |
| 1987 |          64.41635 |
| 1992 |          65.64590 |
| 1997 |          66.84934 |
| 2002 |          67.83904 |
| 2007 |          68.91909 |

Let’s visualize this data

``` r
gapminder %>% 
  group_by(year) %>% 
  summarise(weighted_mean=weighted.mean(lifeExp, pop)) %>% 
  ggplot(aes(year, weighted_mean)) + 
  geom_point()+
  geom_smooth(method = "lm")
```

![](hw03_use_dplyr_ggplot2_to_manipulate_and_explore_data_files/figure-gfm/weighted%20mean%20lifeExp%20over%20time-1.png)<!-- -->

## Task4: How is life expectancy changing over time on different continents?

Here, I will use weighted mean of lifeExp to illustrate its change over
time

``` r
life=gapminder %>%
  group_by(continent, year) %>%
  summarise(weighted_lifeExp=weighted.mean(lifeExp, pop))
knitr::kable(life)
```

| continent | year | weighted\_lifeExp |
| :-------- | ---: | ----------------: |
| Africa    | 1952 |          38.79973 |
| Africa    | 1957 |          40.94031 |
| Africa    | 1962 |          43.09925 |
| Africa    | 1967 |          45.17721 |
| Africa    | 1972 |          47.21229 |
| Africa    | 1977 |          49.20883 |
| Africa    | 1982 |          51.01744 |
| Africa    | 1987 |          52.82479 |
| Africa    | 1992 |          53.37292 |
| Africa    | 1997 |          53.28327 |
| Africa    | 2002 |          53.30314 |
| Africa    | 2007 |          54.56441 |
| Americas  | 1952 |          60.23599 |
| Americas  | 1957 |          62.01806 |
| Americas  | 1962 |          63.43706 |
| Americas  | 1967 |          64.50630 |
| Americas  | 1972 |          65.70490 |
| Americas  | 1977 |          67.60591 |
| Americas  | 1982 |          69.19264 |
| Americas  | 1987 |          70.35814 |
| Americas  | 1992 |          71.72177 |
| Americas  | 1997 |          73.19154 |
| Americas  | 2002 |          74.24736 |
| Americas  | 2007 |          75.35668 |
| Asia      | 1952 |          42.94114 |
| Asia      | 1957 |          47.28835 |
| Asia      | 1962 |          46.57369 |
| Asia      | 1967 |          53.88261 |
| Asia      | 1972 |          57.52159 |
| Asia      | 1977 |          59.55648 |
| Asia      | 1982 |          61.57472 |
| Asia      | 1987 |          63.53710 |
| Asia      | 1992 |          65.14874 |
| Asia      | 1997 |          66.77092 |
| Asia      | 2002 |          68.13732 |
| Asia      | 2007 |          69.44386 |
| Europe    | 1952 |          64.90540 |
| Europe    | 1957 |          66.89364 |
| Europe    | 1962 |          68.45957 |
| Europe    | 1967 |          69.54963 |
| Europe    | 1972 |          70.46884 |
| Europe    | 1977 |          71.53989 |
| Europe    | 1982 |          72.56247 |
| Europe    | 1987 |          73.44717 |
| Europe    | 1992 |          74.44273 |
| Europe    | 1997 |          75.70849 |
| Europe    | 2002 |          77.02232 |
| Europe    | 2007 |          77.89057 |
| Oceania   | 1952 |          69.17040 |
| Oceania   | 1957 |          70.31693 |
| Oceania   | 1962 |          70.98808 |
| Oceania   | 1967 |          71.17848 |
| Oceania   | 1972 |          71.92273 |
| Oceania   | 1977 |          73.25684 |
| Oceania   | 1982 |          74.58291 |
| Oceania   | 1987 |          75.98107 |
| Oceania   | 1992 |          77.35788 |
| Oceania   | 1997 |          78.61843 |
| Oceania   | 2002 |          80.16006 |
| Oceania   | 2007 |          81.06215 |

``` r
life %>% 
  ggplot(aes(year, weighted_lifeExp, color=continent)) + 
  geom_point()+
  geom_smooth(method = "lm")
```

![](hw03_use_dplyr_ggplot2_to_manipulate_and_explore_data_files/figure-gfm/lifeExp%20vs.%20time%20on%20different%20continents-1.png)<!-- -->

## Task5: Report the absolute and/or relative abundance of countries with low life expectancy over time by continent:

Here, I will first compute the mean value of worldwide life expectancy,
then use this value as a benchmark to determine how many countries on
each continent have a life expectancy less than this value, for each
year.

``` r
mean=summarise(gapminder,mean(lifeExp)) ## compute mean worldwide life expectancy
## knowing the mean value of worldwide life expectancy is 59.5, I will count how many countries within each continent have a ife expectancy less than this value
df_c=gapminder %>% 
  group_by(continent) %>% 
  filter(lifeExp<59.5) %>% 
  tally()
knitr::kable(df_c)
```

| continent |   n |
| :-------- | --: |
| Africa    | 544 |
| Americas  |  82 |
| Asia      | 173 |
| Europe    |  11 |

From this table we can see that Africa and Asia countries have more
population have lower life expectancy compared to countries on other
continents.

Now let’s plot the data. I will first plot lifeExp of all range for all
continents but use mean value as a benchmark to differentiate life
expectancy above and below such value.

``` r
gapminder %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_point(aes(color=lifeExp>=59.5))+ # use mean lifeExp as a benchmark 
  facet_wrap(~continent) +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](hw03_use_dplyr_ggplot2_to_manipulate_and_explore_data_files/figure-gfm/lifeExp%20greater%20than%2059.5-1.png)<!-- -->
