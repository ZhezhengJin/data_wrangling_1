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

## ‘filter’

``` r
filter(litters_df, pups_survive > 4)
```

    ## # A tibble: 39 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  2 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  3 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  4 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  5 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  6 Con8  #2/95/3               NA          NA         20       8       0       8
    ##  7 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ##  8 Con8  #5/4/3/83/3           28          NA         19       9       0       8
    ##  9 Con8  #1/6/2/2/95-2         NA          NA         20       7       0       6
    ## 10 Con8  #3/5/3/83/3-3-2       NA          NA         20       8       0       8
    ## # … with 29 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, pups_survive == 8)
```

    ## # A tibble: 11 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  2 Con8  #2/95/3               NA          NA         20       8       0       8
    ##  3 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ##  4 Con8  #5/4/3/83/3           28          NA         19       9       0       8
    ##  5 Con8  #3/5/3/83/3-3-2       NA          NA         20       8       0       8
    ##  6 Mod7  #3/83/3-2             NA          NA         19       8       0       8
    ##  7 Low7  #84/2                 24.3        40.8       20       8       0       8
    ##  8 Low7  #107                  22.6        42.4       20       9       0       8
    ##  9 Mod8  #97                   24.5        42.8       20       8       1       8
    ## 10 Mod8  #5/93/2               NA          NA         19       8       0       8
    ## 11 Mod8  #7/110/3-2            27.5        46         19       8       1       8
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, pups_survive != 8)
```

    ## # A tibble: 38 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #1/6/2/2/95-2         NA          NA         20       7       0       6
    ##  9 Con8  #2/2/95/2             NA          NA         19       5       0       4
    ## 10 Con8  #3/6/2/2/95-3         NA          NA         20       7       0       7
    ## # … with 28 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, group == "Con7")
```

    ## # A tibble: 7 × 8
    ##   group litter_number   gd0_weight gd18_weight gd_of_b…¹ pups_…² pups_…³ pups_…⁴
    ##   <chr> <chr>                <dbl>       <dbl>     <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Con7  #85                   19.7        34.7        20       3       4       3
    ## 2 Con7  #1/2/95/2             27          42          19       8       0       7
    ## 3 Con7  #5/5/3/83/3-3         26          41.4        19       6       0       5
    ## 4 Con7  #5/4/2/95/2           28.5        44.1        19       5       1       4
    ## 5 Con7  #4/2/95/3-3           NA          NA          20       6       0       6
    ## 6 Con7  #2/2/95/3-2           NA          NA          20       6       0       4
    ## 7 Con7  #1/5/3/83/3-3/2       NA          NA          20       9       0       9
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive

``` r
filter(litters_df, group %in% c("Con7", "Low7"))
```

    ## # A tibble: 15 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Low7  #84/2                 24.3        40.8       20       8       0       8
    ##  9 Low7  #107                  22.6        42.4       20       9       0       8
    ## 10 Low7  #85/2                 22.2        38.5       20       8       0       6
    ## 11 Low7  #98                   23.8        43.8       20       9       0       9
    ## 12 Low7  #102                  22.6        43.3       20      11       0       7
    ## 13 Low7  #101                  23.8        42.7       20       9       0       9
    ## 14 Low7  #111                  25.5        44.6       20       3       2       3
    ## 15 Low7  #112                  23.9        40.5       19       6       1       1
    ## # … with abbreviated variable names ¹​gd_of_birth, ²​pups_born_alive,
    ## #   ³​pups_dead_birth, ⁴​pups_survive
