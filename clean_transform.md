Cleaning And Transforming Dutch Horse Racing Data from ndr.nl
================

#### Set directory of this notebook as working directory

``` r
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
```

# Load packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.1     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Load Data

``` r
csv_paths <- list.files("./data_wo_header", full.names = TRUE)
results_csvs <- lapply(
  csv_paths, read.csv, header = FALSE, encoding = "utf-8", 
  colClasses = c("integer", rep("character", 20))
)
races_nl <- bind_rows(results_csvs)
c_names <- c(
  "ndr_id", "date_track", "time", "race_number", "title", 
  "description1", "description2", "description3", "race_infos",
  "position", "horse", "driver", "distance", "startnummer", "startnr", "draw", 
  "red_km_str", "dist_btn", "Hcap", "prize_money", "odds"
)
colnames(races_nl) <- c_names
```

# Columns

Column descriptions:

- ndr_id: an ID which is used by ndr.nl internally to identify the horse
  racing events

- date_track: Date and racecourse on which the race took place

- time: Time of day at which the race took place

- race_number

- title

- description1

- description2

- description3

- race_infos

- position

- horse: Name of the horse

- driver: Name of the driver (or jockey for flat races)

- distance

- startnummer and startnr: two different column names used by ndr.nl.
  Both have the same meaning

- draw: Only used in flat races to indicate the stall number of the
  starting gate for the horse in this race.

- red_km_str: [Réduction
  kilométrique](https://www.zone-turf.fr/definition/reduction-kilometrique-99.html)
  as a string which is just the average pace per km of the horse in this
  race. The string looks like this for example: ‘1.15,6’ (so the length
  of this string should normally be 6 characters)

- dist_btn

- Hcap

- prize_money

- odds

# Columns

## “ndr_id”

We can just leave that one alone.

## “race_infos” and new column discipline

We are only interested in the harness racing results. So the first thing
we could do is filter for the discipline “Drafsport”.

``` r
# Adding new column "discipline"
races_nl <- races_nl %>% 
  mutate(discipline = gsub(" -.*", "", race_infos))
unique(races_nl$discipline)
```

    ## [1] "Drafsport" "Rensport"

``` r
races_nl <- races_nl %>% 
  filter(discipline == "Drafsport")
```

## “date_track”

The date_track column contains two types of information and we should
separate them. Before that we should check that all values for
“date_track” follow the same pattern.

``` r
date_track_na <- races_nl %>% 
  filter(!grepl("\\d{2}-\\d{2}-\\d{2}, .*$", date_track))
```

``` r
races_nl <- races_nl %>% 
  separate(col = date_track, into = c("date", "racecourse"), sep = ", ")
```

``` r
unique(races_nl$racecourse)
```

    ##  [1] "Wolvega"         "Nootdorp"        "Hilversum"       "Duindigt"       
    ##  [5] "Alkmaar"         "Drachten"        "Groningen"       "Assendelft"     
    ##  [9] "Wognum"          "Venhuizen"       "Hoogkarspel"     "Warmond"        
    ## [13] "Uitgeest"        "Emmeloord"       "Zandvoort"       "Stompwijk"      
    ## [17] "Voorschoten"     "Santpoort"       "Beverwijk"       "Noordwijk"      
    ## [21] "de Lier"         "Hoorn"           "Schagen"         "Heemskerk"      
    ## [25] "Purmerend"       "Hillegom"        "Medemblik"       "Enkhuizen"      
    ## [29] "Roden"           "Lisse"           "Middenbeemster"  "Joure"          
    ## [33] "t Zand"          "Amsterdam-Noord" "IJmuiden"        "Aduard"         
    ## [37] "Sassenheim"      "Eenrum"          "Zwanenburg"      "Utrecht"        
    ## [41] "Bennebroek"      "Leek"            "Egmond"          "Bemmel"         
    ## [45] "Helmond"         "Wateringen"      "Schiermonnikoog" "Hattemerbroek"  
    ## [49] "Slovenië"        "Heino"           "Hoofddorp"       "Rotterdam"

## “race_number”

``` r
unique(races_nl$race_number)
```

    ##  [1] "KW" "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "13" "14" "12"
    ## [16] "0"  "20" "21" "15" "16" "22" "23" "24" "25" "26" "27" "28" "29"

Those are some crazy numbers. I have never been to the races and have
watched a 21st race on that day. Actually those are codes for canceled
races.

## Dropping columns used for flat racing results

After a glimpse at the data we can safely assume that “description2” and
“description3” are only used for flat racing results.

``` r
races_nl %>% 
  summarise(
    #vars("description1", "description2", "description3"), mean(str_length)
    mean_strlen_desc1 = mean(str_length(description1)),
    mean_strlen_desc2 = mean(str_length(description2)),
    mean_strlen_desc3 = mean(str_length(description3))
  )
```

    ##   mean_strlen_desc1 mean_strlen_desc2 mean_strlen_desc3
    ## 1          47.83433                 0                 0

``` r
races_nl <- races_nl %>% 
  select(ndr_id:description1, race_infos:startnr, red_km_str, prize_money, odds)
```

## “prize_money”

We think that the right way to store currency values in an R data frame
is as an integer and in our case Euro-Cents. At first we should check if
there are values for prize money which don’t follow the two expected
patterns: “” (empty string) and “€ .\*”

``` r
races_nl %>% 
  filter(!grepl("^€ .*$|^$", prize_money)) %>% 
  select(ndr_id, race_number, horse, prize_money)
```

    ##    ndr_id race_number            horse    prize_money
    ## 1    8538           8       Ramay "HT"      B. Thomas
    ## 2    9036          KW   Agar's Saroech L.E.M. Beckman
    ## 3    9036          KW Waasama's Makala  B.R.A. Afonso
    ## 4    9089           1           Marsha      B. Thomas
    ## 5   11620           4            Kabon      B. Thomas
    ## 6   11909           1     Liberty Moon    M.J. Watlow
    ## 7   13331           4           Djambo       R. Chang
    ## 8   21657          KW Select Lady (GB)      B. Hadjeb
    ## 9   21657          KW       Penny Lane    M.J. Watlow
    ## 10  14255           6     Maduka (IRE)       R. Chang
    ## 11  14255           8          Pebbles       R. Chang

So for 11 rows the expected patterns in “prize_money” don’t show up.
After checking what went wrong it has been found out that in these cases
there have been problems with the tables on the ndr.nl website and we
can simply eliminate the rows. These are just parts of the flat racing
results. Before we eliminate them we can just build our new column
“prize_eurcents”. There should be NAs introduced to our new column in
the defective rows.

``` r
races_nl <- races_nl %>% 
  mutate(
    # replace empty strings with "0"
    prize_eurcents = if_else(prize_money == "", "0", prize_money),
    # convert to euro cents
    prize_eurcents = as.integer(gsub("€ |\\.|,", "", prize_eurcents))
  )
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `prize_eurcents = as.integer(gsub("€ |\\.|,", "",
    ##   prize_eurcents))`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
sum(is.na(races_nl$prize_eurcents))
```

    ## [1] 11

``` r
races_nl <- races_nl %>% 
  filter(!is.na(prize_eurcents))
```

``` r
#sort(unique(races_nl$prize_money))
```

## Inspecting the column ‘red_km_str’

Since the length of the string should be 6 and follow the pattern
described above, we are also interested in strings of length 6 which do
not match the pattern

``` r
races_nl %>% 
  filter(
    !grepl("^\\d\\.\\d{2},\\d$", red_km_str) & str_length(red_km_str) == 6
  ) %>% 
  arrange(red_km_str) %>% 
  .$red_km_str
```

    ##  [1] "1,20.9" "1.16.9" "1.18.9" "1.19.3" "1.19.8" "1.19.9" "1.20.3" "1.20.5"
    ##  [9] "1.20.6" "1.20.6" "1.20.8" "1.22.9" "1.22.9" "1.23.2" "1.23.3" "1.23.3"
    ## [17] "1.23.7" "1.23.8" "1.24.0"

``` r
# red_km_str_not6 <- races_nl %>% 
#   filter(str_length(red_km_str) != 6)
# red_km_str_equal6 <- races_nl %>% 
#   filter(str_length(red_km_str) == 6)
```

Add columns

``` r
# races_nl <- races_nl %>% 
#   mutate(
#     bahn = gsub("^.*, ", "", date_track),
#     disziplin = gsub(" -.*", "", race_infos),
#     quote = as.numeric(gsub(",", ".", odds)),
#     datum = as.Date(gsub(", .*", "", date_track), format = "%d-%m-%y"),
#     red_km_sec = as.numeric(gsub("\\..*", "", red_km_str)) * 60 +
#       as.numeric(gsub(",", ".", gsub("^.\\.", "", red_km_str)))
#   )
```

``` r
# red_km_sec_na <- races_nl %>% 
#   filter(is.na(red_km_sec))
```

``` r
# summary(races_nl)
```
