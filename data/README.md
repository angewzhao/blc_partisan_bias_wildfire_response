# Processed data

## zipcodes_fire_ca:
This is the tidy version. Contains shapefiles. Created a new dataset that stretches from August 15, 2017- August 15, 2019 and from August 15, 2018-August 15, 2019. 

Contains: geoid, zipcode, zpcd_.. = zipcode_buffer_radius in miles, years, nm_frs_ = num_fires_intersect_zipcode_buffer, geometry = zipcode_geometry. 

For geometry, it is missing crs. Use this code: st_crs(zipcodes_fire_smoke_survey$geometry) <- CA_ALBERS_EPSG, where CA_ALBERS_EPSG <- 3310. 



## zipcodes_fire_ca_smoke:
Only has tidy version. Confains shapefiles. Created a new dataset that stretches from Jan 1, 2018-March 31, 2019. The dates were chosen to intersect with the mzip_month_smoke in data raw. 

Contains: geoid, zipcode, zpcd_.. = zipcode_buffer_radius in miles, years, nm_frs_ = num_fires_intersect_zipcode_buffer, geometry = zipcode_geometry. 

For geometry, it is missing crs. Use this code: st_crs(zipcodes_fire_smoke_survey$geometry) <- CA_ALBERS_EPSG, where CA_ALBERS_EPSG <- 3310. 



## zipcodes_ca_smoke_survey:
Only has tidy version. Confains shapefiles. Created a new dataset that stretches from Jan 1, 2018-March 31, 2019. The dates were chosen to intersect with the mzip_month_smoke.Rmd in data raw. 

Contains all information from zipcodes_fire_ca_smoke, mzip_month_smoke, and STAN0128 survey.  

Contains: geoid, zipcode, zpcd_.. = zipcode_buffer_radius in miles, years, nm_frs_ = num_fires_intersect_zipcode_buffer, geometry = zipcode_geometry.

For geometry, it is missing crs. Use this code: st_crs(zipcodes_fire_smoke_survey$geometry) <- CA_ALBERS_EPSG, where CA_ALBERS_EPSG <- 3310. 
