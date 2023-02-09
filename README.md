Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(leaflet)
```

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
fn <- "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz"
if (!file.exists("met_all.gz"))
  download.file(fn, destfile = "met_all.gz")
met <- data.table::fread("met_all.gz")
```

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

``` r
met <- merge(
  # Data
  x = met,
  y = stations,
  # List of variables to match 
  by.x = "USAFID",
  by.y = "USAF",
  # Which obs to keep?
  all.x = TRUE,
  all.y = FALSE
)
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
met_avg <- met %>% 
  group_by(USAFID) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm=TRUE),
    atm.press = mean(atm.press, na.rm=TRUE)
  ) %>% collect()
```

``` r
met_med <- met_avg %>% 
  summarise(across(
    c(temp, wind.sp, atm.press), 
    function(x) quantile(x, prob = 0.5, na.rm = TRUE) 
    )
  ) %>% collect()
```

``` r
met_avg %>% 
  filter(
    temp == met_med %>%  pull(temp) | 
    wind.sp == met_med %>%  pull(wind.sp) |
    atm.press == met_med %>%  pull(atm.press)
  ) %>% collect()
```

    ## # A tibble: 1 × 4
    ##   USAFID  temp wind.sp atm.press
    ##    <int> <dbl>   <dbl>     <dbl>
    ## 1 720929  17.4    2.46       NaN

``` r
met_avg %>% 
  filter(
    between(temp, met_med %>%  pull(temp) - 0.003, met_med %>%  pull(temp) + 0.002) | 
    wind.sp == met_med %>%  pull(wind.sp) |
    between(atm.press, met_med %>%  pull(atm.press) - 0.0005, met_med %>%  pull(atm.press) + 0.001) 
  ) %>% collect()
```

    ## # A tibble: 3 × 4
    ##   USAFID  temp wind.sp atm.press
    ##    <int> <dbl>   <dbl>     <dbl>
    ## 1 720458  23.7    1.21      NaN 
    ## 2 720929  17.4    2.46      NaN 
    ## 3 723200  25.8    1.54     1015.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
distance <- function(x, y, z, a, b, c) {
    sqrt((x - a)^2 + (y - b)^2 + (z - c)^2)
}

met_state <- met %>% 
  group_by(USAFID) %>% 
  summarize(
    lat = mean(lat, na.rm=TRUE),
    lon = mean(lon, na.rm=TRUE),
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm=TRUE),
    atm.press = mean(atm.press, na.rm=TRUE),
    state = STATE
  )  %>% distinct() %>%  collect()
```

``` r
state_median <-  met_state %>% 
  group_by(state) %>% 
  summarize(
    state = state,
    state_temp = median(temp, na.rm=TRUE),
    state_wind.sp = median(wind.sp, na.rm=TRUE),
    state_atm.press = median(atm.press, na.rm=TRUE)
  ) %>% 
  distinct() %>% 
  collect()
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
state_avg_med <- merge(
  x = met_state,
  y = state_median,
  by.x = "state",
  by.y = "state",
  all.x = TRUE,
  all.y = FALSE
)

state_avg_med[is.na(state_avg_med)] <- 0
diff <- state_avg_med %>% 
  mutate(distance = distance(temp, wind.sp, atm.press, state_temp, state_wind.sp, state_atm.press))

min_diff <- diff %>% group_by(state) %>% summarise(min_diff = min(distance))
med_close <- merge(
  x = min_diff,
  y = diff,
  by.x = c("min_diff", "state"),
  by.y = c("distance", "state"),
  all.x = FALSE,
  all.y = FALSE)
med_close %>% select(state, USAFID)
```

    ##    state USAFID
    ## 1     DE 724180
    ## 2     WA 720254
    ## 3     FL 722106
    ## 4     AL 722286
    ## 5     MA 725064
    ## 6     TX 722416
    ## 7     OK 723545
    ## 8     ND 720911
    ## 9     IA 725480
    ## 10    MS 722358
    ## 11    WV 724176
    ## 12    AZ 722745
    ## 13    VA 724016
    ## 14    KS 724580
    ## 15    NE 725560
    ## 16    NC 723174
    ## 17    IN 725327
    ## 18    NJ 724090
    ## 19    RI 725079
    ## 20    ID 725867
    ## 21    LA 722486
    ## 22    CA 722970
    ## 23    TN 723346
    ## 24    GA 723160
    ## 25    ME 726077
    ## 26    OH 724298
    ## 27    MI 725395
    ## 28    CT 725087
    ## 29    UT 725755
    ## 30    PA 725130
    ## 31    VT 726115
    ## 32    MD 724057
    ## 33    NH 726050
    ## 34    SD 726590
    ## 35    MO 723495
    ## 36    MT 726798
    ## 37    AR 723407
    ## 38    NY 725194
    ## 39    WY 726650
    ## 40    IL 725440
    ## 41    KY 724240
    ## 42    WI 726452
    ## 43    SC 723190
    ## 44    MN 726550
    ## 45    NV 725805
    ## 46    CO 724767
    ## 47    OR 725895
    ## 48    NM 722686

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.
