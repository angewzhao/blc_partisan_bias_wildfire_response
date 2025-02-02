2020-06-27

  - [Reading in Zipcode Data](#reading-in-zipcode-data)
  - [Cleaning and Wrangling zipcode
    data](#cleaning-and-wrangling-zipcode-data)
  - [Next steps: plotting zipcode centroids. Try to get that on a CA map
    first.](#next-steps-plotting-zipcode-centroids.-try-to-get-that-on-a-ca-map-first.)
  - [Plotting CA on a map](#plotting-ca-on-a-map)
  - [Plotting zipcodes onto the map](#plotting-zipcodes-onto-the-map)

``` r
# Libraries
library(tidyverse)
library(sf)
library(haven)
library(ussf)
library(muRL)
library(zipcode)

# Parameters
file_raw <- here::here("data/STAN0128_main_OUTPUT.rds")

# PROJ string for CA
CA_ALBERS <- 
  "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

file_zipcodes_ca_geometry <-
  here::here("data/ca_zipcodes/ca_zipcodes.shp")

CA_ALBERS <- 
  "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
```

## Reading in Zipcode Data

``` r
zipcodes <-
  file_raw %>% 
  read_dta() %>% 
  mutate_if(is.labelled, as_factor)
```

## Cleaning and Wrangling zipcode data

``` r
zipcodes_wanted <-
  zipcodes %>% 
  filter(str_detect(consent, "Yes")) %>% 
  distinct(inputzip) %>% 
  rename(zip = inputzip)
```

## Next steps: plotting zipcode centroids. Try to get that on a CA map first.

## Plotting CA on a map

``` r
ca <- 
  boundaries(geography = "state", resolution = "500k", projection = "albers") %>% 
  filter(NAME == "California") %>% 
  st_transform(crs = CA_ALBERS) 
```

## Plotting zipcodes onto the map

``` r
data(zipcode)

zipcodes_ca <-
  zipcode %>% 
  filter(state == "CA")

zipcodes_ca_geometry <-
  file_zipcodes_ca_geometry %>% 
  read_sf() %>% 
  rename(zipcode = name) %>% 
  filter(zipcode %in% zipcodes_ca$zip) %>% 
  st_transform(crs = CA_ALBERS) %>% 
  mutate(
    zipcode_centroid = st_centroid(geometry)
  )

ca %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = zipcodes_ca_geometry) +
  theme_void()
```

![](STAN_0128_zipcodes_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
