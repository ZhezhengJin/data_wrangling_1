data manipulation
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

## Load in the FAS Litters Data

``` r
litters_df = read_csv("./data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df = janitor :: clean_names(litters_df)
```

## ‘select’

choose some columns and not others

``` r
select(litters_df, litter_number, pups_survive)
```

    ## # A tibble: 49 × 2
    ##    litter_number   pups_survive
    ##    <chr>                  <dbl>
    ##  1 #85                        3
    ##  2 #1/2/95/2                  7
    ##  3 #5/5/3/83/3-3              5
    ##  4 #5/4/2/95/2                4
    ##  5 #4/2/95/3-3                6
    ##  6 #2/2/95/3-2                4
    ##  7 #1/5/3/83/3-3/2            9
    ##  8 #3/83/3-3                  8
    ##  9 #2/95/3                    8
    ## 10 #3/5/2/2/95                8
    ## # … with 39 more rows

Renaming columns

``` r
select(litters_df, team = group, Litter_number = litter_number)
```

    ## # A tibble: 49 × 2
    ##    team  Litter_number  
    ##    <chr> <chr>          
    ##  1 Con7  #85            
    ##  2 Con7  #1/2/95/2      
    ##  3 Con7  #5/5/3/83/3-3  
    ##  4 Con7  #5/4/2/95/2    
    ##  5 Con7  #4/2/95/3-3    
    ##  6 Con7  #2/2/95/3-2    
    ##  7 Con7  #1/5/3/83/3-3/2
    ##  8 Con8  #3/83/3-3      
    ##  9 Con8  #2/95/3        
    ## 10 Con8  #3/5/2/2/95    
    ## # … with 39 more rows

``` r
rename(litters_df, team = group, Litter_number = litter_number)
```

    ## # A tibble: 49 × 8
    ##    team  Litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

select helper

``` r
select(litters_df, starts_with("pups"))
```

    ## # A tibble: 49 × 3
    ##    pups_born_alive pups_dead_birth pups_survive
    ##              <dbl>           <dbl>        <dbl>
    ##  1               3               4            3
    ##  2               8               0            7
    ##  3               6               0            5
    ##  4               5               1            4
    ##  5               6               0            6
    ##  6               6               0            4
    ##  7               9               0            9
    ##  8               9               1            8
    ##  9               8               0            8
    ## 10               8               0            8
    ## # … with 39 more rows

``` r
select(litters_df, litter_number, everything())
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr>           <chr>      <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 #85             Con7        19.7        34.7       20       3       4       3
    ##  2 #1/2/95/2       Con7        27          42         19       8       0       7
    ##  3 #5/5/3/83/3-3   Con7        26          41.4       19       6       0       5
    ##  4 #5/4/2/95/2     Con7        28.5        44.1       19       5       1       4
    ##  5 #4/2/95/3-3     Con7        NA          NA         20       6       0       6
    ##  6 #2/2/95/3-2     Con7        NA          NA         20       6       0       4
    ##  7 #1/5/3/83/3-3/2 Con7        NA          NA         20       9       0       9
    ##  8 #3/83/3-3       Con8        NA          NA         20       9       1       8
    ##  9 #2/95/3         Con8        NA          NA         20       8       0       8
    ## 10 #3/5/2/2/95     Con8        28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
relocate(litters_df, litter_number)
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr>           <chr>      <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 #85             Con7        19.7        34.7       20       3       4       3
    ##  2 #1/2/95/2       Con7        27          42         19       8       0       7
    ##  3 #5/5/3/83/3-3   Con7        26          41.4       19       6       0       5
    ##  4 #5/4/2/95/2     Con7        28.5        44.1       19       5       1       4
    ##  5 #4/2/95/3-3     Con7        NA          NA         20       6       0       6
    ##  6 #2/2/95/3-2     Con7        NA          NA         20       6       0       4
    ##  7 #1/5/3/83/3-3/2 Con7        NA          NA         20       9       0       9
    ##  8 #3/83/3-3       Con8        NA          NA         20       9       1       8
    ##  9 #2/95/3         Con8        NA          NA         20       8       0       8
    ## 10 #3/5/2/2/95     Con8        28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive
