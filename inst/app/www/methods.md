Methods
================

Growing Shade developed out of a collaboration between the
<a href="https://metrocouncil.org/" target="_blank">Metropolitan
Council</a>,
<a href="https://www.nature.org/en-us/about-us/where-we-work/united-states/minnesota/" target="_blank">The
Nature Conservancy</a>, and
<a href="https://treetrust.org/" target="_blank">Tree Trust</a>. We
thank members of the advisory group for initial consultations and thank
all individuals who provided feedback during the development phase of
this project.

Methods and data sources for the analyses presented within the Growing
Shade are detailed below. Please
<a href = "mailto:ellen.esch@metc.state.mn.us?subject=growing%shade%20tool&cc=eric.wojchik@metc.state.mn.us">contact
us</a> if you have questions or feedback.

<h2>
<span style="font-size:16pt">Prioritization layer</span>
</h2>

Priority variables were sourced from several locations including:

-   Demographic and socioeconomic information comes from the
    <a href = 'https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census2020population' target = '_blank'>2020
    decennial census</a>,
    <a href = 'https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs' target = '_blank'>American
    Community Survey 5-Year Summary File</a> (2016-2020), and the
    <a href = 'https://gisdata.mn.gov/dataset/us-mn-state-metc-society-equity-considerations' target = '_blank'>Equity
    Considerations data</a> published by the Metropolitan Council.
    Census block group data is used.
-   Health information comes from the
    <a href="https://www.cdc.gov/places/index.html" target="_blank">PLACES
    dataset</a> published by the Center of Disease Control and
    Prevention. PLACES data is reported by census tract.
-   Tree canopy and green space information is obtained from the
    <a href = 'https://www.esa.int/Applications/Observing_the_Earth/Copernicus/Sentinel-2' target = "_blank">Copernicus
    Sentinel-2 satellite mission</a>.
-   Climate data including temperatures and flood risk was processed
    using the
    <a href = 'https://metrocouncil.org/Communities/Planning/Local-Planning-Assistance/CVA.aspx' target = '_blank'>Climate
    Vulnerability Assessment</a> published by the Metropolitan Council.

<br> Priority variables were standardized and scaled so that the z-score
was normally distributed on a 0-10 scale (by multiplying the normal
distribution of the z-score for each variable by 10).

Based on user-defined selection of priority variables, standardized
scores are averaged to create a single, integrated priority value.

<h2>
<span style="font-size:16pt">Tree canopy</span>
</h2>

Growing Shade uses and shows a tree canopy layer from 2021. A machine
learning method was created in
<a href = 'https://earthengine.google.com/' target = "_blank">Google
Earth Engine</a> and used to detect tree cover from other land cover
types using
<a href = 'https://www.esa.int/Applications/Observing_the_Earth/Copernicus/Sentinel-2' target = "_blank">Sentinel-2
satellite imagery</a>. Any areas identified as
<a href = 'https://gisdata.mn.gov/dataset/us-mn-state-metc-water-lakes-rivers' target = "_blank">open
water</a> or
<a href = 'https://developers.google.com/earth-engine/datasets/catalog/USDA_NASS_CDL?hl=en' target = '_blank'>cultivated
cropland</a> were removed.

Next, the tree canopy as identified with Sentinel-2 data was calibrated
to the tree canopy identified in 2015 using LiDAR data from 2011
(<a href="https://gisdata.mn.gov/dataset/base-landcover-twincities" target="_blank">Twin
Cities Metropolitan area 1-meter land cover classification</a>). With
1000 equal-area regions across the 7-county area, a scaling factor of
0.885 was used to bring the Sentinel data in line with on-the-ground
tree canopy. This scaling factor is appropriate for our methods of using
10 m x 10 m resolution data, which is often larger than tree canopies.
This scaling factor makes our data align very closely with other reports
(r^2 = 0.96) while still leveraging the scalability and temporal
accuracy of our method.

<br> <br><br><br><br>
