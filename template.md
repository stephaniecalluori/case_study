Simple document
================

I’m an R Markdown document!

``` r
devtools::install_github("p8105/p8105.datasets")
```

    ## Skipping install of 'p8105.datasets' from a github remote, the SHA1 (412759e3) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(p8105.datasets)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
data(nyc_airbnb)
```

``` r
nyc_airbnb = 
  nyc_airbnb |> 
  janitor::clean_names() |> 
  rename(borough = neighbourhood_group) |> 
  mutate(stars = review_scores_location / 2)

view(nyc_airbnb)
```

``` r
avg_price_borough <- nyc_airbnb |> 
  group_by(borough) |> 
  summarize(avg = mean(price))
```

``` r
avg_price_neighbourhood <- nyc_airbnb |> 
  group_by(borough, neighbourhood) |> 
  summarize(avg = mean(price))
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

``` r
str(nyc_airbnb)
```

    ## tibble [40,753 × 18] (S3: tbl_df/tbl/data.frame)
    ##  $ id                            : num [1:40753] 7949480 16042478 1886820 6627449 5557381 ...
    ##  $ review_scores_location        : num [1:40753] 10 NA NA 10 10 10 10 9 10 9 ...
    ##  $ name                          : chr [1:40753] "City Island Sanctuary relaxing BR & Bath w Parking" "WATERFRONT STUDIO APARTMENT" "Quaint City Island Community." "Large 1 BDRM in Great location" ...
    ##  $ host_id                       : num [1:40753] 119445 9117975 9815788 13886510 28811542 ...
    ##  $ host_name                     : chr [1:40753] "Linda & Didier" "Collins" "Steve" "Arlene" ...
    ##  $ borough                       : chr [1:40753] "Bronx" "Bronx" "Bronx" "Bronx" ...
    ##  $ neighbourhood                 : chr [1:40753] "City Island" "City Island" "City Island" "City Island" ...
    ##  $ lat                           : num [1:40753] -73.8 -73.8 -73.8 -73.8 -73.8 ...
    ##  $ long                          : num [1:40753] 40.9 40.9 40.8 40.8 40.9 ...
    ##  $ room_type                     : chr [1:40753] "Private room" "Private room" "Entire home/apt" "Entire home/apt" ...
    ##  $ price                         : num [1:40753] 99 200 300 125 69 125 85 39 95 125 ...
    ##  $ minimum_nights                : num [1:40753] 1 7 7 3 3 2 1 2 3 2 ...
    ##  $ number_of_reviews             : num [1:40753] 25 0 0 12 86 41 74 114 5 206 ...
    ##  $ last_review                   : Date[1:40753], format: "2017-04-23" NA ...
    ##  $ reviews_per_month             : num [1:40753] 1.59 NA NA 0.54 3.63 2.48 5.43 2.06 5 2.98 ...
    ##  $ calculated_host_listings_count: num [1:40753] 1 1 1 1 1 1 1 4 3 4 ...
    ##  $ availability_365              : num [1:40753] 170 180 365 335 352 129 306 306 144 106 ...
    ##  $ stars                         : num [1:40753] 5 NA NA 5 5 5 5 4.5 5 4.5 ...

bronx, brooklyn, manhattan, queens, staten island

``` r
avg_price_bronx_n <- nyc_airbnb |> 
  filter(borough == "Bronx") |> 
  group_by(neighbourhood) |> 
  summarize(avg = mean(price))
```

\##Brainstorm questions where are Airbnbs most expensive? which units
have the most availabilty? how is review score impacted by location?
where are airbnbs? bourgh? neighborhood? lag and long?

\#Do somne EDA

``` r
nyc_airbnb |> 
  count(borough)
```

    ## # A tibble: 5 × 2
    ##   borough           n
    ##   <chr>         <int>
    ## 1 Bronx           649
    ## 2 Brooklyn      16810
    ## 3 Manhattan     19212
    ## 4 Queens         3821
    ## 5 Staten Island   261

``` r
nyc_airbnb |> 
  group_by(borough) |> 
  summarize(mean_price = mean(price))
```

    ## # A tibble: 5 × 2
    ##   borough       mean_price
    ##   <chr>              <dbl>
    ## 1 Bronx               82.8
    ## 2 Brooklyn           120. 
    ## 3 Manhattan          180. 
    ## 4 Queens              94.7
    ## 5 Staten Island      128.

check mean

``` r
nyc_airbnb |> 
  group_by(borough, room_type) |> 
  summarize(mean_price = mean(price)) |> 
  pivot_wider(
    names_from = room_type,
    values_from = mean_price
  )
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   borough [5]
    ##   borough       `Entire home/apt` `Private room` `Shared room`
    ##   <chr>                     <dbl>          <dbl>         <dbl>
    ## 1 Bronx                      125.           65.5          57.5
    ## 2 Brooklyn                   175.           76.7          59.6
    ## 3 Manhattan                  238.          107.           84.7
    ## 4 Queens                     140.           70.6          49.1
    ## 5 Staten Island              207.           65.4          25

check median bc it might be skewed distribution

``` r
nyc_airbnb |> 
  group_by(borough, room_type) |> 
  summarize(median_price = median(price)) |> 
  pivot_wider(
    names_from = room_type,
    values_from = median_price
  )
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   borough [5]
    ##   borough       `Entire home/apt` `Private room` `Shared room`
    ##   <chr>                     <dbl>          <dbl>         <dbl>
    ## 1 Bronx                      100              55            43
    ## 2 Brooklyn                   145              65            40
    ## 3 Manhattan                  190              90            65
    ## 4 Queens                     119              60            39
    ## 5 Staten Island              112.             55            25

``` r
nyc_airbnb |> 
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_grid(borough ~ room_type)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](template_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
nyc_airbnb |> 
  filter(price >= 9500)
```

    ## # A tibble: 6 × 18
    ##         id review_scores_location name   host_id host_name borough neighbourhood
    ##      <dbl>                  <dbl> <chr>    <dbl> <chr>     <chr>   <chr>        
    ## 1  3103784                     10 A Pri…  9.83e6 Michael   Brookl… Brooklyn Hei…
    ## 2  4737930                      8 Spani…  1.24e6 Olson     Manhat… East Harlem  
    ## 3   187529                     NA $3200…  9.02e5 Georgia   Manhat… Lower East S…
    ## 4  9528920                      9 Quiet…  3.91e6 Amy       Manhat… Lower East S…
    ## 5 16429718                     NA Charm…  1.36e7 Lena      Brookl… Sheepshead B…
    ## 6 12955683                      8 Great…  3.57e7 Duan      Manhat… Upper West S…
    ## # ℹ 11 more variables: lat <dbl>, long <dbl>, room_type <chr>, price <dbl>,
    ## #   minimum_nights <dbl>, number_of_reviews <dbl>, last_review <date>,
    ## #   reviews_per_month <dbl>, calculated_host_listings_count <dbl>,
    ## #   availability_365 <dbl>, stars <dbl>

``` r
nyc_airbnb |>  
  filter(price < 1000, room_type == "Entire home/apt") |> 
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_grid(. ~ borough)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](template_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
nyc_airbnb |> 
  filter(
    price < 1000,
    room_type == "Entire home/apt",
    borough == "Manhattan"
  ) |> 
  group_by(neighbourhood) |> 
  summarize(
    n_apt = n(),
    mean_price = mean(price)
  ) |> 
  arrange(desc(mean_price))
```

    ## # A tibble: 32 × 3
    ##    neighbourhood      n_apt mean_price
    ##    <chr>              <int>      <dbl>
    ##  1 Tribeca              100       358.
    ##  2 NoHo                  61       312.
    ##  3 Flatiron District     75       307.
    ##  4 SoHo                 234       296.
    ##  5 Theater District      93       282.
    ##  6 Midtown              655       276.
    ##  7 Battery Park City     44       271.
    ##  8 Greenwich Village    282       256.
    ##  9 Chelsea              765       255.
    ## 10 Financial District   228       250.
    ## # ℹ 22 more rows
