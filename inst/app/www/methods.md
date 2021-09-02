
# Methods

<br>

## Priority variables

Priority variables with the exclusion of ‘average greenness’ were
sourced from the ‘Equity Considerations Dataset’ published by the Met
Council. Data is given at the Census tract level. More information about
those variables can be found on the
(<a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-society-equity-considerations" target="_blank">Minnesota
Geospatial Commons</a>). <br><br> Census tract average greenness was
calculated from Sentinel-2 satellite data processed on Google Earth
Engine. Briefly, the Normalized Difference Vegetation Index (NDVI) was
used as a measure of greenness. A composite image of the year 2020 was
made where each pixel contained the maximum NDVI observed within the
calendar year. Sentinel-2 collects measurements approximately 2-3 times
a week, with a pixel resolution of 10 meters x 10 meters. Then, the
tract-average NDVI value from this ‘maximum NDVI’ composite image was
taken. NDVI over water bodies (rivers or lakes) was not included nor was
NDVI over cultivated fields. <br><br> Based on user-defined selection of
priority variables, the data are scaled and standardized in order to
create a single, integrated priority value. Download the pdf below for
more information on that.

<br>

## Priority areas

Detailed priority areas where tree planting could be suitable were
obtained using NDVI calculated from Sentinel-2 satellite data with all
processing done using Google Earth Engine. First, we removed all pixels
identified as having tree canopy in 2020 or classified as having a land
use of either water, highways, railroads, airports, or agricultural
crops in 2016 (see below). Then we created a composite image of the year
2020 where each pixel contained the maximum NDVI observed within the
calendar year. Land use in 2016 was assigned to each pixel for mapping
purposes.

### Identifying tree canopy in 2020

Plant phenological patterns were leveraged in order to identify tree
canopy gaps rather than identify gaps in overall ‘greenness.’ Five
distinct phenological time periods were used. For each time period, a
composite image was made showing the maximum NDVI observed. Then
different thresholds of NDVI were used to separate trees from grasses
and crops. All areas that were classified as water or cultivated
cropland were removed. Finally, we then divided our calculated tree
canopy by 2. This scaling factor is appropriate for our methods of using
10 m x 10 m resolution data, which is often much larger than tree
canopies. This scaling factor makes our data align very closely with
other reports while still leveraging the scalability and temporal
accuracy of our method.

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

<!-- https://browser.creodias.eu/#lat=45.15999&lng=-92.79540&zoom=15&time=2020-07-05&preset=3_NDVI&datasource=Sentinel-2%20L1C -->
<br>
<hr>

<br>
