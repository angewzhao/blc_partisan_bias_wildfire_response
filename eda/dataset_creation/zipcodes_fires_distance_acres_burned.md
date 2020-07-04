Zipcodes’ Distance to Fires and Acres Burned of Fires
================
Angela Zhao
2020-07-03

  - [Notes:](#notes)
      - [Troubleshooting st\_intersects and st\_crs w/
        buffer](#troubleshooting-st_intersects-and-st_crs-w-buffer)
  - [Reading in wildfires data](#reading-in-wildfires-data)
  - [Data Wrangling](#data-wrangling)
      - [Reading in Zipcode Data](#reading-in-zipcode-data)
      - [Plotting zipcodes onto the
        map](#plotting-zipcodes-onto-the-map)
  - [Looking at overlaps between zip code buffers of 3, 5, 10 miles from
    geometry of
    fires](#looking-at-overlaps-between-zip-code-buffers-of-3-5-10-miles-from-geometry-of-fires)
      - [Created a tidy tibble of zipcodes, geometry, centroid,
        buffer\_radius, and buffer
        geometry](#created-a-tidy-tibble-of-zipcodes-geometry-centroid-buffer_radius-and-buffer-geometry)
      - [Running fire\_intersect using map\_dfr and
        functions](#running-fire_intersect-using-map_dfr-and-functions)
      - [Adding distance from zipcode and fire centroid, and acres
        burned
        variables](#adding-distance-from-zipcode-and-fire-centroid-and-acres-burned-variables)
          - [Functions created](#functions-created)
          - [Creating new tibbles to hold mean\_distance of fires
            intersected with zipcode, min\_distance to fire, and
            min\_acres\_burned and
            avg\_acres\_burned](#creating-new-tibbles-to-hold-mean_distance-of-fires-intersected-with-zipcode-min_distance-to-fire-and-min_acres_burned-and-avg_acres_burned)
      - [Reading out files](#reading-out-files)

``` r
# Libraries
library(tidyverse)
library(sf)
library(ussf)
library(zipcode)
library(lubridate)
library(haven)


# Parameters
file_raw_survey <- here::here("data/STAN0128_main_OUTPUT.rds")

file_zipcodes_ca_geometry <-
  here::here("data/ca_zipcodes/ca_zipcodes.shp")

file_raw_fire_perim <- 
  here::here("data/frap_fire_perim_firep19_1_shp/firep19_1.shp")

file_out_num_fires_intersect <-
  here::here(
    "data/num_fires_intersect_zipcodes/num_fires_intersect_zipcodes.shp"
  )

file_out_distance_acres_2017_2019 <-
  here::here(
    "data/zipcodes_fires_distance_acres_burned/distance_acres_burned_2017_2019.csv"
  )

file_out_distance_acres_2018_2019 <-
  here::here(
    "data/zipcodes_fires_distance_acres_burned/distance_acres_burned_2018_2019.csv"
  )

# PROJ string for CA
CA_ALBERS <- 
  "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

CA_ALBERS_EPSG <- 3310
```

# Notes:

zipcodes\_fires\_ca: final tibble with all the good stuff in it.
Contains zipcodes, geoid, geometry of the zipcodes, and number of fires
3, 5, 10 miles away from 2017-2019 and 2018-2019. Comes in tidy and
untidy versions.

zipcodes\_fires\_ca\_smoke: final tibble that’s zipcodes\_fires\_ca but
filtered for the dates with data for wildfire smoke, which is from
01-2018 to 03-2019. Only comes in tidy version.

## Troubleshooting st\_intersects and st\_crs w/ buffer

Used CA\_ALBERS\_ESPG as opposed to CA\_ALBERS because the projection
wasn’t sticking to the buffer otherwise.

So gdb files contain multisurface and is a limitation not of sf but of
GEOS. This means that st\_intersects refuses to work with multisurface
and gives off the error: Error in CPL\_geos\_binop(st\_geometry(x),
st\_geometry(y), op, par, pattern, : Evaluation error: ParseException:
Unknown WKB type 12.

Code given: st\_intersects(test\(buffer_shape, test\)fire\_geometry) Two
paths: either turn it into a multipolygon and hope that the multisurface
goes away, or import the layer of interest in the gdb as a shapefile.
Consider “test” as a df. How to find if multisurface:
unique(st\_geometry\_type(st\_geometry(test\(fire_geometry or foo)))  To cast to a different shape: st_cast(test\)fire\_geometry,
“POLYGON”) To change st\_intersect result, an sgbp, into vector: a \<-
st\_intersects(test\(buffer_shape, test\)fire\_geometry) sel\_logical =
lengths(a) \> 0

# Reading in wildfires data

``` r
# Function for reading in wildfires

read_wildfires <- function() {
  file_raw_fire_perim %>% 
    read_sf() %>% 
    rename_all(tolower) %>% 
    rename(
      geometry_length = shape_leng,
      geometry_area = shape_area,
      year = year_
    ) %>% 
    mutate(
      alarm_date = as.Date(alarm_date),
      cont_date = as.Date(cont_date), 
      id = row_number()
    ) %>%
    select(
      fire_name, 
      alarm_date, 
      cont_date, 
      gis_acres,
      geometry_length, 
      geometry_area, 
      geometry
    )
}

wildfires_2017_19 <-
  read_wildfires() %>% 
  filter(alarm_date >= "2017/08/15" & alarm_date <= "2019/08/15") 
 

wildfires_2018_19 <-
  read_wildfires() %>% 
  filter(alarm_date >= "2018/08/15" & alarm_date <= "2019/08/15")
```

# Data Wrangling

## Reading in Zipcode Data

``` r
data(zipcode)

zipcodes_ca <-
  zipcode %>% 
  filter(state == "CA") %>% 
  distinct(zip)

zipcodes_survey_ca <-
  file_raw_survey %>% 
  read_dta() %>% 
  mutate_if(is.labelled, as_factor) %>%
  rename(zip = inputzip) %>% 
  filter(str_detect(consent, "Yes"), zip %in% zipcodes_ca$zip) %>% 
  distinct(zip)
```

## Plotting zipcodes onto the map

``` r
# From the zipcode package

create_buffer <- function(data, radius) {
  data %>% 
  st_buffer(units::set_units(radius, "miles")) %>% 
    st_transform(crs = CA_ALBERS) %>% 
    st_transform(CA_ALBERS_EPSG)
}

# These are the zipcodes that are in CA and found in the survey. They also have centroid, general zipcode polygon, and buffer data. 
zipcodes_ca_geometry <-
  file_zipcodes_ca_geometry %>% 
  read_sf() %>% 
  rename(zipcode = name) %>% 
  filter(zipcode %in% zipcodes_survey_ca$zip) %>% 
  st_transform(crs = CA_ALBERS) %>% 
  rename(
    zipcode_geometry = geometry
  ) %>% 
  mutate(
    zipcode_centroid = st_centroid(zipcode_geometry),
    "3_mile_buffer" = create_buffer(zipcode_centroid, 3), 
    "5_mile_buffer" = create_buffer(zipcode_centroid, 5),
    "10_mile_buffer" = create_buffer(zipcode_centroid, 10)
  ) %>% 
  select(-c(B00001001, B00001001e))

# st_crs(zipcodes_ca_geometry$`10_mile_buffer`)
```

# Looking at overlaps between zip code buffers of 3, 5, 10 miles from geometry of fires

## Created a tidy tibble of zipcodes, geometry, centroid, buffer\_radius, and buffer geometry

``` r
zipcodes_fires_ca <-
  zipcodes_ca_geometry %>% 
  pivot_longer(
    cols = c("3_mile_buffer":"10_mile_buffer"), 
    names_to = "buffer_radius(miles)", 
    values_to = "buffer_shape"
  ) %>% 
  mutate(
    "buffer_radius(miles)" = 
      str_extract(`buffer_radius(miles)`, "\\d+") %>% as.double(), 
    zipcode_centroid = st_transform(zipcode_centroid, CA_ALBERS)
  )
```

    ## Warning in val_cols[col_id] <- unname(as.list(data[cols])): number of items to
    ## replace is not a multiple of replacement length

``` r
st_crs(zipcodes_fires_ca$zipcode_centroid) <- CA_ALBERS_EPSG
```

    ## Warning: st_crs<- : replacing crs does not reproject data; use st_transform for
    ## that

## Running fire\_intersect using map\_dfr and functions

``` r
fires_intersect_buffer_2017_2019 <- function(buffer_shape) {
    st_intersects(buffer_shape, wildfires_2017_19$geometry, sparse = FALSE) %>% 
    sum()
}

fires_intersect_buffer_2018_2019 <- function(buffer_shape) {
    st_intersects(buffer_shape, wildfires_2018_19$geometry, sparse = FALSE) %>% 
    sum()
}

## The function safely() returns a nested tibble with one column results and the other error. Due to reliance on unnesting, safely() is not used. 

zipcodes_fires_ca <-
  zipcodes_fires_ca %>% 
  mutate(
    num_fires_2017_2019 = map(buffer_shape, fires_intersect_buffer_2017_2019), 
    num_fires_2018_2019 = map(buffer_shape, fires_intersect_buffer_2018_2019)
  )  %>% 
  unnest(cols = c(num_fires_2017_2019, num_fires_2018_2019))
```

## Adding distance from zipcode and fire centroid, and acres burned variables

### Functions created

``` r
id_fires_intersect_buffer_2017_2019 <- function(buffer_shape) {
  st_intersects(buffer_shape, wildfires_2017_19$geometry, sparse = FALSE) %>% 
    as_tibble() %>% 
    pivot_longer(
      cols = everything(), 
      names_to = "id", 
      values_to = "intersects"
    ) %>% 
    mutate(
      id = row_number()
    ) %>% 
    filter(intersects == TRUE) %>% 
    pull(id)
}

id_fires_intersect_buffer_2018_2019 <- function(buffer_shape) {
  st_intersects(buffer_shape, wildfires_2018_19$geometry, sparse = FALSE) %>% 
    as_tibble() %>% 
    pivot_longer(
      cols = everything(), 
      names_to = "id", 
      values_to = "intersects"
    ) %>% 
    mutate(
      id = row_number()
    ) %>% 
    filter(intersects == TRUE) %>% 
    pull(id)
}

## Correct row is the wildfire_id number. 


get_value <- function(data, row_num, value) {
  data %>% 
    slice(row_num) %>% 
    pull({{value}})
}

add_distance_acres_burned <- function(data, num_fires) {
  data %>% 
    group_by(zipcode) %>%
    mutate(
      avg_acres = mean(wildfire_area),
      min_acres = min(wildfire_area),
      avg_distance_m = mean(distance_zipcode_wildfire) %>% as.double(),
      min_distance_m = min(distance_zipcode_wildfire) %>% as.double()
    ) %>%
    select(
      zipcode,
      {{num_fires}},
      avg_acres:min_distance_m
    ) %>% 
    distinct()
}
```

### Creating new tibbles to hold mean\_distance of fires intersected with zipcode, min\_distance to fire, and min\_acres\_burned and avg\_acres\_burned

Distance is calculated using the zipcode and wildfire centroid

``` r
zipcodes_fires_ca_2018_2019 <-
  zipcodes_fires_ca %>%
  filter(num_fires_2018_2019 != 0, `buffer_radius(miles)` == 10) %>% 
  distinct(zipcode, zipcode_centroid, buffer_shape, num_fires_2018_2019) %>%
  mutate(
    wildfire_id = map(buffer_shape, id_fires_intersect_buffer_2018_2019)
  ) %>% 
  unnest(cols = wildfire_id) %>% 
  mutate(
    wildfire_geometry = get_value(wildfires_2018_19, wildfire_id, geometry), 
    wildfire_area = get_value(wildfires_2018_19, wildfire_id, gis_acres), 
    wildfire_centroid = st_centroid(wildfire_geometry), 
    distance_zipcode_wildfire = st_distance(zipcode_centroid, wildfire_centroid)
  ) %>% 
  add_distance_acres_burned(num_fires_2018_2019)
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
zipcodes_fires_ca_2017_2019 <- 
  zipcodes_fires_ca %>%
  filter(num_fires_2017_2019 != 0, `buffer_radius(miles)` == 10) %>% 
  distinct(zipcode, zipcode_centroid, buffer_shape, num_fires_2017_2019) %>%
  mutate(
    wildfire_id = map(buffer_shape, id_fires_intersect_buffer_2017_2019)
  ) %>% 
  unnest(cols = wildfire_id) %>% 
  mutate(
    wildfire_geometry = get_value(wildfires_2017_19, wildfire_id, geometry), 
    wildfire_area = get_value(wildfires_2017_19, wildfire_id, gis_acres),
    wildfire_centroid = st_centroid(wildfire_geometry), 
    distance_zipcode_wildfire = st_distance(zipcode_centroid, wildfire_centroid)
  ) %>% 
  add_distance_acres_burned(num_fires_2017_2019)
```

## Reading out files

``` r
zipcodes_fires_ca %>% 
  rename(
    "2018-2019" = "num_fires_2018_2019", 
    "2017-2019" = "num_fires_2017_2019"
  ) %>% 
  rename(
    'zipcode_radius(miles)' = `buffer_radius(miles)`, 
    zipcode_buffer_shape = buffer_shape
  ) %>% 
  pivot_longer(
    names_to = "years", 
    values_to = "num_fires_intersect", 
    cols = c("2018-2019", "2017-2019")
  ) %>% 
  write_sf(file_out_num_fires_intersect)
```

    ## Warning in abbreviate_shapefile_names(obj): Field names abbreviated for ESRI
    ## Shapefile driver

    ## Warning in clean_columns(as.data.frame(obj), factorsAsCharacter): Dropping
    ## column(s) zpcd_cn,zpcd_b_ of class(es) sfc_POINT;sfc,sfc_POLYGON;sfc

``` r
zipcodes_fires_ca_2017_2019 %>% 
  write_csv(file_out_distance_acres_2017_2019)

zipcodes_fires_ca_2018_2019 %>% 
   write_csv(file_out_distance_acres_2018_2019)
```
