Practice
================
ndmawpu
2023-02-19

## Libraries

``` r
library(tidyverse)
library(tidyquant)
library(dplyr)
library(readxl)
# Explore Data
library(DataExplorer)

# Visualization
library(ggplot2)
library(gridExtra)
library(gtable)
library(GGally)
```

## Data Preparation

``` r
data <- read_excel("Data/Price-Data-W2.xlsx", sheet = "Chung khoan")
head(data,5)
```

    ## # A tibble: 5 × 6
    ##      ID Timestamp           ACB.HM.Close ACB.HM.Volume BAB.HN.Close BAB.HN.Vol…¹
    ##   <dbl> <dttm>                     <dbl>         <dbl>        <dbl>        <dbl>
    ## 1     1 2022-08-22 00:00:00        24650       2239300        17100        19800
    ## 2     2 2022-08-19 00:00:00        24850       1909000        17300        10800
    ## 3     3 2022-08-18 00:00:00        25150       5981500        17400        70287
    ## 4     4 2022-08-17 00:00:00        24950       3350600        16700        15103
    ## 5     5 2022-08-16 00:00:00        24850       3234500        16900        11783
    ## # … with abbreviated variable name ¹​BAB.HN.Volume

## EDA

``` r
glimpse(data)
```

    ## Rows: 2,655
    ## Columns: 6
    ## $ ID            <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    ## $ Timestamp     <dttm> 2022-08-22, 2022-08-19, 2022-08-18, 2022-08-17, 2022-08…
    ## $ ACB.HM.Close  <dbl> 24650, 24850, 25150, 24950, 24850, 24950, 24800, 24750, …
    ## $ ACB.HM.Volume <dbl> 2239300, 1909000, 5981500, 3350600, 3234500, 3154600, 14…
    ## $ BAB.HN.Close  <dbl> 17100, 17300, 17400, 16700, 16900, 16800, 16800, 16900, …
    ## $ BAB.HN.Volume <dbl> 19800, 10800, 70287, 15103, 11783, 11200, 20208, 10006, …

the function show data size and structure - there are 2,655 observations
and 6 features

|     | Features      | Description |
|-----|---------------|-------------|
|     | ID            |             |
|     | Timestamp     |             |
|     | ACB.HM.Close  |             |
|     | ACB.HM.Volume |             |
|     | BAB.HN.Close  |             |
|     | BAB.HN.Volume |             |

``` r
introduce(data)
```

    ## # A tibble: 1 × 9
    ##    rows columns discrete_columns conti…¹ all_m…² total…³ compl…⁴ total…⁵ memor…⁶
    ##   <int>   <int>            <int>   <int>   <int>   <int>   <int>   <int>   <dbl>
    ## 1  2655       6                1       5       0    3076    1116   15930  130104
    ## # … with abbreviated variable names ¹​continuous_columns, ²​all_missing_columns,
    ## #   ³​total_missing_values, ⁴​complete_rows, ⁵​total_observations, ⁶​memory_usage

``` r
plot_intro(data)
```

![](Practice_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot_missing(data,
             title = "Percent Missing Values by Features")
```

![](Practice_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# summary statistics
summary(data)
```

    ##        ID           Timestamp                       ACB.HM.Close  
    ##  Min.   :   1.0   Min.   :2012-01-03 00:00:00.00   Min.   : 3463  
    ##  1st Qu.: 664.5   1st Qu.:2014-09-06 12:00:00.00   1st Qu.: 4491  
    ##  Median :1328.0   Median :2017-05-08 00:00:00.00   Median : 7349  
    ##  Mean   :1328.0   Mean   :2017-05-01 15:38:50.84   Mean   :10248  
    ##  3rd Qu.:1991.5   3rd Qu.:2019-12-24 12:00:00.00   3rd Qu.:12299  
    ##  Max.   :2655.0   Max.   :2022-08-22 00:00:00.00   Max.   :30360  
    ##                                                    NA's   :5      
    ##  ACB.HM.Volume       BAB.HN.Close   BAB.HN.Volume      
    ##  Min.   :   14661   Min.   :13727   Min.   :    114.8  
    ##  1st Qu.:  732692   1st Qu.:13997   1st Qu.:   4129.8  
    ##  Median : 2732540   Median :14944   Median :  10500.1  
    ##  Mean   : 5093336   Mean   :16650   Mean   :  32616.7  
    ##  3rd Qu.: 7464587   3rd Qu.:18989   3rd Qu.:  39251.0  
    ##  Max.   :48725312   Max.   :29616   Max.   :1021100.9  
    ##  NA's   :5          NA's   :1529    NA's   :1537

calculated mean,median,25th and 75th quartiles,min,max

``` r
boxplot(data)
```

![](Practice_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Deleting rows where value is missing
data <- data %>% 
  na.omit(c(data$BAB.HN.Close, data$BAB.HN.Volume))
```

``` r
plot_missing(data,
             title = "Percent Missing Values by Features")
```

![](Practice_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Data Visualization

``` r
p1 <- ggplot(data, aes(x = Timestamp, y=ACB.HM.Close)) +
  geom_line() +
  theme_minimal()
p2 <- ggplot(data, aes(x = Timestamp, y=BAB.HN.Close)) +
  geom_line() +
  theme_minimal()

grid.arrange(
  p1,
  p2,
  nrow = 2,
  top = "Close price by Timestamp"
  )
```

![](Practice_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
p3 <- ggplot(data, aes(x = Timestamp, y=ACB.HM.Volume)) +
  geom_bar(stat="identity") +
  coord_flip()
p4 <- ggplot(data, aes(x = Timestamp, y=BAB.HN.Volume)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal()
grid.arrange(
  p3,
  p4,
  nrow = 1,
  top = "Sum of Volumn by Timestamp"
)
```

![](Practice_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggpairs(data)
```

![](Practice_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggcorr(data)
```

![](Practice_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
