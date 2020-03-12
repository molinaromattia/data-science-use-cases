COVID-19 trend analysis
================
Mattia Molinaro

In this notebook I will inspect the evolution of the number of people infected by the COVID-19.

``` r
## Loading the necessary libraries
library( tidyverse)
library( data.table)

## Data loading and pre-processing
confirmed = as.tbl( fread( "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")) # Downloading the data
print( confirmed) # The data has wide format, whereby each date is represented by a column
```

    ## # A tibble: 404 x 54
    ##    `Province/State` `Country/Region`    Lat   Long `1/22/20` `1/23/20`
    ##    <chr>            <chr>             <dbl>  <dbl>     <int>     <int>
    ##  1 ""               Thailand          15     101           2         3
    ##  2 ""               Japan             36     138           2         1
    ##  3 ""               Singapore          1.28  104.          0         1
    ##  4 ""               Nepal             28.2    84.2         0         0
    ##  5 ""               Malaysia           2.5   112.          0         0
    ##  6 British Columbia Canada            49.3  -123.          0         0
    ##  7 New South Wales  Australia        -33.9   151.          0         0
    ##  8 Victoria         Australia        -37.8   145.          0         0
    ##  9 Queensland       Australia        -28.0   153.          0         0
    ## 10 ""               Cambodia          11.6   105.          0         0
    ## # … with 394 more rows, and 48 more variables: `1/24/20` <int>,
    ## #   `1/25/20` <int>, `1/26/20` <int>, `1/27/20` <int>, `1/28/20` <int>,
    ## #   `1/29/20` <int>, `1/30/20` <int>, `1/31/20` <int>, `2/1/20` <int>,
    ## #   `2/2/20` <int>, `2/3/20` <int>, `2/4/20` <int>, `2/5/20` <int>,
    ## #   `2/6/20` <int>, `2/7/20` <int>, `2/8/20` <int>, `2/9/20` <int>,
    ## #   `2/10/20` <int>, `2/11/20` <int>, `2/12/20` <int>, `2/13/20` <int>,
    ## #   `2/14/20` <int>, `2/15/20` <int>, `2/16/20` <int>, `2/17/20` <int>,
    ## #   `2/18/20` <int>, `2/19/20` <int>, `2/20/20` <int>, `2/21/20` <int>,
    ## #   `2/22/20` <int>, `2/23/20` <int>, `2/24/20` <int>, `2/25/20` <int>,
    ## #   `2/26/20` <int>, `2/27/20` <int>, `2/28/20` <int>, `2/29/20` <int>,
    ## #   `3/1/20` <int>, `3/2/20` <int>, `3/3/20` <int>, `3/4/20` <int>,
    ## #   `3/5/20` <int>, `3/6/20` <int>, `3/7/20` <int>, `3/8/20` <int>,
    ## #   `3/9/20` <int>, `3/10/20` <int>, `3/11/20` <int>

``` r
confirmed = confirmed %>% gather( Date, Confirmed_Cases, -(1:4)) # Melting the data, such that each row corresponds to one region/province in one day
print( confirmed)
```

    ## # A tibble: 20,200 x 6
    ##    `Province/State` `Country/Region`    Lat   Long Date    Confirmed_Cases
    ##    <chr>            <chr>             <dbl>  <dbl> <chr>             <int>
    ##  1 ""               Thailand          15     101   1/22/20               2
    ##  2 ""               Japan             36     138   1/22/20               2
    ##  3 ""               Singapore          1.28  104.  1/22/20               0
    ##  4 ""               Nepal             28.2    84.2 1/22/20               0
    ##  5 ""               Malaysia           2.5   112.  1/22/20               0
    ##  6 British Columbia Canada            49.3  -123.  1/22/20               0
    ##  7 New South Wales  Australia        -33.9   151.  1/22/20               0
    ##  8 Victoria         Australia        -37.8   145.  1/22/20               0
    ##  9 Queensland       Australia        -28.0   153.  1/22/20               0
    ## 10 ""               Cambodia          11.6   105.  1/22/20               0
    ## # … with 20,190 more rows

``` r
confirmed = confirmed %>% mutate( Date = as.Date( Date, format = "%m/%d/%y")) # Casting strings to dates
confirmed = confirmed %>% group_by( `Country/Region`, Date) %>% summarise( Confirmed_Cases = sum( Confirmed_Cases)) %>% ungroup() # Number of diseased people grouped by region and date

confirmed = confirmed %>% rename( Country = `Country/Region`) %>% filter( Country != 'Others')
temp = confirmed %>% filter( Date == max( Date)) %>% mutate( Position = dense_rank( desc( Confirmed_Cases))) %>% select( Country, Position)
confirmed = confirmed %>% inner_join( temp, "Country") %>% filter( Position <= 5)
confirmed = confirmed %>% mutate( Confirmed_Cases = ifelse( Confirmed_Cases == 0, 0.9, Confirmed_Cases)) %>% select( -Position)
head( confirmed)
```

    ## # A tibble: 6 x 3
    ##   Country Date       Confirmed_Cases
    ##   <chr>   <date>               <dbl>
    ## 1 China   2020-01-22             548
    ## 2 China   2020-01-23             643
    ## 3 China   2020-01-24             920
    ## 4 China   2020-01-25            1406
    ## 5 China   2020-01-26            2075
    ## 6 China   2020-01-27            2877
