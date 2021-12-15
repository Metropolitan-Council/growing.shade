Methods
================

-   [Methods](#methods)
    -   [Identifying tree canopy](#identifying-tree-canopy)
    -   [Priority variables](#priority-variables)

## Methods

<br>

### Identifying tree canopy

The existing tree canopy was identified using Sentinel-2 satellite data
using Google Earth Engine. We prefer using this data over other sources
(although all have their merits) so as to have a “real-time” inventory
of the region’s tree canopy.

<br> Plant phenological patterns were leveraged in order to identify
tree canopy. Five distinct phenological time periods were used (see
below). For each time period, a composite image was made showing the
maximum NDVI observed. Then different thresholds of NDVI were used to
separate trees from grasses and crops. All areas that were classified as
water or cultivated cropland were removed. Finally, the tree canopy as
identified with Sentinel-2 data was calibrated to the tree canopy
identified in 2015 using lidar data from 2010 across the Twin Cities
metro. With 1000 equal-area regions across the 7-county area, a scaling
factor of 0.5325208 was used to bring the Sentinel data in line with
on-the-ground tree canopy. This scaling factor is appropriate for our
methods of using 10 m x 10 m resolution data, which is often much larger
than tree canopies. This scaling factor makes our data align very
closely with other reports while still leveraging the scalability and
temporal accuracy of our method. <br>

-   Winter (1 January 2020 - 15 March 2020): pixel classified as a
    conifer tree if winter NDVI is above 0.3 (identify trees which are
    green in the winter) OR
-   Spring (15 March 2020 - 30 April 2020): pixel classified as a
    deciduous tree if spring NDVI is less than 0.5 (remove cool season
    grass) AND
-   Early summer (1 May 2020 - 15 June 2020): early summer NDVI is
    greater than 0.55 (remove warm season crops) AND
-   Summer (1 July 2020 - 15 September 2020): summer NDVI is greater
    than 0.55 (identify trees which are are green in the summer) AND
-   Fall (15 September 2020 - 30 October 2020): fall NDVI is greater
    than 0.4 (remove early senescing crops)

<br>

### Priority variables

Priority variables were sourced from several locations including:

<br>

-   Equity Considerations Dataset published by the Met Council. Data is
    given at the Census tract level. More information about those
    variables can be found on the
    (<a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-society-equity-considerations" target="_blank">Minnesota
    Geospatial Commons</a>).
-   PLACES data published by the Center of Disease Control and
    Prevention. Data is at the tract level.
    (<a href="https://www.cdc.gov/places/index.html" target="_blank">Learn
    more at their website.</a>)
-   Tree canopy information obtained from Sentinel-2 and processed in
    GEE.
-   Census tract average greenness was calculated from Sentinel-2
    satellite data processed on Google Earth Engine. Briefly, the
    Normalized Difference Vegetation Index (NDVI) was used as a measure
    of greenness. A composite image of the year 2020 was made where each
    pixel contained the maximum NDVI observed within the calendar year.
    Sentinel-2 collects measurements approximately 2-3 times a week,
    with a pixel resolution of 10 meters x 10 meters. Then, the
    tract-average NDVI value from this ‘maximum NDVI’ composite image
    was taken. NDVI over water bodies (rivers or lakes) was not
    included.

<br><br> Priority variables were standardized and scaled so that the
z-score was normally distributed on a 0-10 scale (by multiplying the
normal distribution of the z-score for each variable by 10).

<br><br> Finally, based on user-defined selection of priority variables,
standard scorces are averages to create a single, integrated priority
value.

<br>

<!-- https://browser.creodias.eu/#lat=45.15999&lng=-92.79540&zoom=15&time=2020-07-05&preset=3_NDVI&datasource=Sentinel-2%20L1C -->
<br>
<hr>

<br>
