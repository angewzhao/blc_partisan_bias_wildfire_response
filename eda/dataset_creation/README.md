# EDA

## Key:

This is for the number of fires within zipcode radius of 3, 5, 10 miles. Each zipcode must be in CA and also in the STAN0128 survey. This folder contains cleaning the wildfire data, zipcodes in CA data, as well as creating a new dataset for zipcodes, num_fires, radii in 3, 5, 10 miles, and from fall 2017-2019 and fall 2018-2019. 

#### frap_fire_perimeter

Wrangling and filtering fire perimeter data. 

#### STAN_0128_zipcodes

Filtering for and mapping wanted zipcode geometries. 

#### zipcodes_over_fires

Looking at how many number of fires are within zipcode radius of 3, 5, 10 miles of zipcode centroids. Mapped zipcodes and fires. 

Created a new dataset that stretches from August 15, 2017- August 15, 2019 and from August 15, 2018-August 15, 2019.  

### Data used:
#### STAN0128_main_OUTPUT.sav

This data refers to the survey about major events and public opinion done in Fall 2019. Refer to the codebook in docs. 


#### frap_fire_perimiter_gis
This is a multi-agency statewide database of fire history. For CAL FIRE, timber fires 10 acres or greater, brush fires 30 acres and greater, and grass fires 300 acres or greater are included. For the USFS, there is a 10 acre minimum for fires since 1950.

This dataset contains wildfire history, prescribed burns and other fuel modification projects.

Includes separate groups for just large fires (5000+ acres) vs all fires, and a separate layer for Prescribed Burns. The All Fires group is scale dependent and starts displaying at 1:500,000 scale. Prescribed fires likewise begin displaying at 1:500,000 scale. Full labels included only for 2010+ Large Fires, Year-only labels for other large fires, and those all display starting at 1:500,000 scale. Only 2000s and 2010s large fires are on by default.

The data is updated annually with fire perimeters from the previous fire season. This service represents the latest official release, and is updated annually when a new version is released. As of June, 2019 it represents fire19_1.

Find the key contained in docs of data-raw. 
