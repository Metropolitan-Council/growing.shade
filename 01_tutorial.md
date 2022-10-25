Growing Shade methods
================
05 October 2022

-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#set-parameters" id="toc-set-parameters">Set parameters</a>
-   <a href="#process-demographic-data"
    id="toc-process-demographic-data">Process demographic data</a>
    -   <a href="#decennial-census" id="toc-decennial-census">Decennial
        census</a>
    -   <a href="#american-community-survey"
        id="toc-american-community-survey">American Community Survey</a>
    -   <a href="#places-health-data" id="toc-places-health-data">PLACES health
        data</a>
-   <a href="#process-geographies" id="toc-process-geographies">Process
    geographies</a>
    -   <a href="#census-block-groups" id="toc-census-block-groups">Census block
        groups</a>
    -   <a href="#neighborhoods-and-city-levels"
        id="toc-neighborhoods-and-city-levels">Neighborhoods and city levels</a>
-   <a href="#remote-sensing-data" id="toc-remote-sensing-data">Remote
    sensing data</a>
-   <a href="#environmental-data" id="toc-environmental-data">Environmental
    data</a>
    -   <a href="#lifetime-cancer-risk-from-air-toxins"
        id="toc-lifetime-cancer-risk-from-air-toxins">Lifetime cancer risk from
        air toxins</a>
    -   <a href="#climate-vulnerability---temperature--flooding"
        id="toc-climate-vulnerability---temperature--flooding">Climate
        vulnerability - temperature &amp; flooding</a>
-   <a href="#other--geographic-overlay-files"
    id="toc-other--geographic-overlay-files">Other &amp; geographic overlay
    files</a>
    -   <a href="#redlining" id="toc-redlining">Redlining</a>
    -   <a href="#mpca-area-of-environmental-justice-concern"
        id="toc-mpca-area-of-environmental-justice-concern">MPCA area of
        environmental justice concern</a>
-   <a href="#create-final-data-pieces"
    id="toc-create-final-data-pieces">Create final data pieces</a>
    -   <a href="#merge-data-variables" id="toc-merge-data-variables">Merge data
        variables</a>
    -   <a href="#create-regional-averages"
        id="toc-create-regional-averages">Create regional averages</a>
    -   <a href="#highest-priority" id="toc-highest-priority">Highest
        priority</a>
    -   <a href="#core-mapping-data" id="toc-core-mapping-data">Core mapping
        data</a>
    -   <a href="#simplify-data-for-speed"
        id="toc-simplify-data-for-speed">Simplify data for speed</a>
-   <a href="#running-application" id="toc-running-application">Running
    application</a>
    -   <a href="#set-user-interface-parameters"
        id="toc-set-user-interface-parameters">Set user interface parameters</a>
    -   <a href="#edit-and-add-any-region-specific-language"
        id="toc-edit-and-add-any-region-specific-language">Edit and add any
        region-specific language</a>
    -   <a href="#using-arcgis-for-processing"
        id="toc-using-arcgis-for-processing">Using ArcGIS for processing</a>
    -   <a href="#troubleshooting-application"
        id="toc-troubleshooting-application">Troubleshooting application</a>

# Introduction

Growing Shade is a prioritization tool for tree canopy enhancement and
preservation. Growing Shade was conceptualized by the Metropolitan
Council, Tree Trust, and The Nature Conservancy with a focus on the Twin
Cities (Minnesota) area. All analyses and code were developed by Ellen
Esch. The Twin Cities version of Growing Shade can be accessed at:
www.growingshade.com or
<https://metrotransitmn.shinyapps.io/growing-shade/>.

The components of Growing Shade relevant to population demographics
(i.e., determining environmental justice priority areas) can be scaled
to any area within the United States. The components relevant to the
tree canopy have been parameterized to the Twin Cities region, but
should work pretty well in other temperate areas with deciduous and
coniferous trees.

This tutorial walks through how to grab and synthesize various data
pieces which go into making Growing Shade. This document should be
useful when doing data updates update, or trying to scale this workflow
to other regions/areas.

The first time you run this code, it will be helpful to walk
step-by-step through the various pieces. There are several places where
you’ll get instructions to request and save API keys, or other manual
steps which can’t be automated. However, after you walk thorough it
initially, in future runs (if needed) or for small data/geography
adjustments, you can probably be successful running everything in one
go!

# Set parameters

These global parameters dictate what state/county combinations will be
used, as well as from what year various data pieces come from. Since the
data pieces are updated on different schedules, it’s unfortunately not
as simple as setting a “global” rule to use data corresponding to the
current year

This code is set up to use/process 2020-era census geographies at the
block group level (even though some data sources are still using
2010-era geographies…but we’ll deal with those later).

# Process demographic data

The demographic information is fetched using the APIs/app tokens. Follow
these instructions:

You will need an API key from Census:

-   [Request an api key](https://api.census.gov/data/key_signup.html)
-   Enter in the console: `usethis::edit_r_environ()`
-   When the `.Renviron` file comes up in the editor, type:
    `CENSUS_KEY="KEY GOES HERE, INSIDE QUOTES"`
-   Save and close the `.Renviron` file.
-   Restart R.

You will need an app token from CDC:

-   [Request an app
    token](https://chronicdata.cdc.gov/profile/edit/developer_settings)
-   You may have to create an account with Tyler Data & Insights ID
    -   More information about “app tokens” and metadata are on the
        [PLACES data
        overview](https://dev.socrata.com/foundry/chronicdata.cdc.gov/cwsq-ngmh)
    -   Note that CDC has APIs and app tokens. It’s a bit confusing, but
        be sure you are looking at the app token for this.
-   Enter in the console: `usethis::edit_r_environ()`
-   When the `.Renviron` file comes up in the editor, type: -
    `CDC_KEY="APP TOKEN GOES HERE, INSIDE QUOTES"` -
    `CDC_EMAIL="email you signed up with goes here, inside quotes"`
-   `CDC_PASSWORD="password you used to signup with goes here, inside quotes"`
-   Save and close the `.Renviron` file.
-   Restart R.

## Decennial census

Race variables come from the decennial 2020 census. Currently, the
decennial census data is preferred over ACS data because it is a
population count rather than a sample and because ACS data uses the
average of 5 years (so starting with the ACS 2021-2025 data, it may be
logical to switch over to using the ACS data instead).

## American Community Survey

Other demographic variables come from the American Community Survey
5-year data. ACS numbers come from a surveyed population sample.

Check what is the most [recent year of ACS data
here](https://api.census.gov/data.html). Search for “acs5” and see what
is the most recent year of data available.

Of course, more variables from ACS can be added. The easiest way is to
add variables which exist at the block group level. If you’re adding
variables where tract-level data is the most detailed geography
reported, follow the steps in the code below which processes the
disability information. If more variables are being added from ACS,
please be sure to update the metadata.csv file (this is outlined later
in the “combine all variables” step).

## PLACES health data

Health metrics come from [PLACES: Local Data for Better Health, Census
Tract
Data](https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh).
I am not aware that there is a way to use a “health_year” parameter to
get a specific timestamp of the data, so please just double check that
the most recent data is being used! CDC has indicated that they *might*
soon be switching over to using 2020-vintage census geographies rather
than the old 2010 geographies, so the processing will get simplified at
that point.

Also, because this CDC data currently uses old census geographies, you
will need to download a crosswalk - Download the state-specific
crosswalk of [“Block Groups –\> Census Tracts” for year “2010 –\> 2020”
from
NHGIS](https://www.nhgis.org/geographic-crosswalks#download-from-block-groups) -
Place the resultant file in the data-raw folder

# Process geographies

Growing Shade is set up to have data at the block group, neighborhood,
and city/township (ctu) levels.

### Census block groups

Getting these geography files is easy - just downloaded directly.

### Neighborhoods and city levels

Since we’re going to be making a map which shows census tracts, cities,
or neighborhoods depending on the user input, a crosswalk needs to be
made which relates block groups to the city and neighborhood levels.

This section doesn’t seem to be easily scalable to other regions. As
such, the code for this section will likely need to be customized, and
processing will be done within the `02_geographies.Rmd` script. Further,
if this section doesn’t apply for other regions, it should be easy
enough to remove elements in the user-interface of the application.

In any case, the following files should exist in the `data` folder
(again, the `02_geographies.Rmd` script shows an example of creating
these files for the Twin Cities region).

-   **ctu_crosswalk** which assign every block group in the region into
    a core city. Column names should be as follows:
    -   `GEO_NAME` (city name, e.g. “Minneapolis” or “Stillwater”)
    -   `bg_id` (block group id)
-   **nhood_crosswalk** which assigns block group in the region into a
    neighborhood. Column names should be as follows:
    -   `GEO_NAME` (neighborhood name, e.g. “Powderhorn” or “Downtown”)
    -   `city` (core city in which neighborhood lies,
        e.g. “Minneapolis”)
    -   `bg_id` (block group id)
-   **wide_ctu_crosswalk** indicates which city(ies) each block group
    belongs to. Column names:
    -   `bg_id` (block group id)
    -   `jurisdiction` (city or cities which the block group belongs
        to - where applicable cities should be separated by a “,”;
        i.e. “St. Francis, Bethel”)

# Remote sensing data

This section *may* scale to other regions, but currently requires some
initial processing steps done *outside* of R, and hence the code has
been moved into the `02_remotesensing.Rmd` script.

These are the resulting files which should exist:

-   **bg_canopy** which has the tree canopy cover of every block group.
    Columns should be as follows:
    -   `bg_id` (block group identifier)
    -   `canopy_percent` (the percent tree canopy cover in the block
        group)
    -   `avgcanopy` (the average block group tree canopy cover; note,
        for the best comparative value, this avgcanopy variable should
        *not* be the regional average tree canopy (which would require
        incorporating the fact that all block groups are not the same
        area))
-   **ctu_list_raw** which has information about the tree canopy at the
    city level. Columns:
    -   `GEO_NAME` (the name of the city/township)
    -   `canopy_percent` (the percent tree canopy cover in the city)
    -   `avgcanopy` (the average canopy cover of block groups within the
        city)
    -   `min` (the canopy cover value from the block group with the
        lowest tree cover in the city)
    -   `max` (the canopy cover value from the block group with the
        highest tree cover in the city)
    -   `n_blockgroups` (the number of block groups which fall inside
        the city)
    -   `geometry` (the spatial coordinates of the city polygon)
-   **nhood_list_raw** which has information about the tree canopy at
    the neighborhood level. Columns:
    -   `GEO_NAME` (the name of the neighborhood)
    -   `city` (the city in which the neighborhood is located)
    -   `canopy_percent` (the percent tree canopy cover in the
        neighborhood)
    -   `avgcanopy` (the average canopy cover of block groups within the
        neighborhood)
    -   `min` (the canopy cover value from the block group with the
        lowest tree cover in the neighborhood)
    -   `max` (the canopy cover value from the block group with the
        highest tree cover in the neighborhood)
    -   `n_blockgroups` (the number of block groups which fall inside
        the neighborhood)
    -   `geometry` (the spatial coordinates of the neighborhood polygon)
-   **bg_ndvi** which has information about the greenness (as measured
    via NDVI, or normalized difference vegetation index) of block groups
    -   `bg_id` (block group identifier)
    -   `ndvi_uncultivated` (average NDVI over uncultivated land)
    -   `ndvi_land` (average NDVI over all land, including cropland)

# Environmental data

## Lifetime cancer risk from air toxins

The EPA’s 2021 EJSCREEN data release can be downloaded automatically and
filtered for the region of interest. Interestingly, the 2021 EJSCREEN
data uses 2017 AirToxScreen data, even though there does appear to be a
more recent, [2018 AirToxScreen
file](https://www.epa.gov/AirToxScreen/2018-airtoxscreen-assessment-results)
as of October 3, 2022.

It is my recommendation to use the EJSCREEN data because the process can
be automated, and then just [check back to see when this data gets
updated for future years](https://gaftp.epa.gov/EJSCREEN). Hopefully the
2018 AirToxScreen data will be incorporated soon.

Additionally, please note that the 2021 EJSCREEN data is using old
(2010-vintage) Census geographies (at the tract level). Fortunately, the
crosswalk downloaded to process the PLACES health data will additionally
work here.

## Climate vulnerability - temperature & flooding

This section relies on processing remote sensing data outside of R, and
may require some specific processing to work for other regions. Here,
the files created are as follow:

-   **env_data** Column names, as applicable, should be as follows:
    -   `bg_id` (block group geo-identifier)
    -   `avg_temp` (average land surface temperature during a heatwave)
    -   `prim_flood` (amount of land susceptible to primary flooding)

# Other & geographic overlay files

This may not be relevant for other regions, but here is the code.

## Redlining

This data describes the amount of block groups which were redlined.

## MPCA area of environmental justice concern

This data was used initially, but it seems as if MPCA has stopped
updating the data, thus it no longer seems relevant.

# Create final data pieces

These steps outline how to finally create the final files which are
needed to run the application. All of these files will go into the
`data` folder.

## Merge data variables

Merge demographic and environmental data. Additionally, standardize and
re-scale variables so we can create equally weighted priority scores. Do
*not* include spatial data at this point, it should be joined after
summarizing to save computational time.

Additionally, please check (and double check if problems arise) the
`data-raw/metadata.csv` file. If you wish to exclude variables (which
have been processed in steps above), simply remove the variable row from
the metadata file. If you have added variables in the steps above,
please make sure that those new variables appear in the metadata (or
else the variables still will not appear in the final data).

## Create regional averages

For comparative purposes, regional averages need to be computed. While
the average values for the block groups are needed to create z-scores in
the scaling and standardizing steps, those averages are *not* the
regional averages. For instance, the average median income of block
groups might be 60,000 but the regional average might be lower if there
are more people living in ares with lower income.

The regional average data mostly comes from data which has already been
fetched from the census (either decennial or ACS). In other cases, you
may wish to supplement with regional averages for other data pieces.

## Highest priority

Inside the final data base, the highest priority (among public health,
environmental justice, climate change, and conservation priorities) in
each block group is identified.

## Core mapping data

The block group geographies need some final adjustments before they are
ready to go into the mapping application.

## Simplify data for speed

Because all of the files inside the `data` folder need to be pushed up
to the cloud in order to deploy the full version of Growing Shade, it is
important to make the files as small as possible. This is very important
to help loading speed of the application. In essence, most geographic
files (polygons) can be simplified to some degree. All data files should
only have the necessary columns for running the application, and
extraneous columns should be removed.

# Running application

If the follow steps have been followed, you should be able to navigate
to the `./dev/run_dev.R` file, select all the code, and run! This should
initiate a local version of Growing Shade for the customized region!

## Set user interface parameters

Creating a `ui_params.rda` data set is the easiest solutions that I can
(currently!) think of which allows for site-specific parameters to be
set *outside* of the main application code. For instance, other regions
are unlikely to have a city called “Oakdale,” but regardless a starting
city needs to be identified in order for the map to render correctly.

## Edit and add any region-specific language

There will likely be region-specific information that should be
displayed alongside the data within the interactive application. Users
should edit files within the “R” folder as prudent.

## Using ArcGIS for processing

If platforms other than R are used for processing data, there shouldn’t
be any problem! However, you may need to replace sections of the R
processing code with code that reads in .shp files and saves them as
.rda files. You’ll also need to just ensure that the spatial projection
is correct for the leaflet package, which is used to map everything. I
would recommend placing the raw shapefiles inside the data-raw folder.
Here is an example of some code:

## Troubleshooting application

The “report” section is quite detailed, and various sections may not
work if all the data isn’t there. For instance, if there isn’t data on
NDVI, the figure relating how greenness (trees) contribute to lower land
surface temperatures won’t work.
