README
================
Ellen Esch
28 May 2021

-   [Overview](#overview)

## Overview

Planting shade concept is a merger of:

-   Conversations with advisory group for Planting Shade Project
-   The Economic Values Atlas project in collaboration with Oregon Metro
    and Brookings Institution.
-   NDVI analyses

An interactive version of this analysis currently lives at:
<https://metrotransitmn.shinyapps.io/growing-shade/>

<!-- ## MetCouncil's contributions -->
<!-- We have tried to make an RShiny application that is highly portable, well documented, and requires minimal coding in order to lower the bar for other regions who might like to implement this type of analysis.  -->
<!-- At the most basic level, users may upload an Excel document containing 2 sheets. Users can also leverage R scripts to aggregate disparate data sources.  -->
<!-- To understand what you will be creating, please view [this example for the Twin Cities region](https://metrotransitmn.shinyapps.io/eva_app_v1/) or [this example for the Portland region](https://metrotransitmn.shinyapps.io/eva_app_pdx/). -->
<!-- ### Set user parameters -->
<!-- First, set the following parameters which indicate the state/s and county/ies from which tract data comes from. Right now, this code is set to handle up to 2 states although this can easily be expanded if there is a need. Also set the metro name, and the format of the data inputs. This should be the only section of code that needs editing.  -->
<!-- ### Edit and add any region-specific langauge -->
<!-- There will likely be region-specific information that should be displayed alongside the data within the interactive application. A general shell is created here. But users should edit the `./R/mod_home.R` script for any introductory information which should be displayed. And the `./R/app_ui/R` script can be edited as well. -->
<!-- Text and pictures may also be updated, and the interface can be styled with css. I'm not sure of the best way to make that portable. My inclination is to create a "Brookings" style for the generic app.  -->
<!-- ### Launch the app -->
<!-- The following code will launch your region's app (!!). Please run all the code chunks (two) prior to this section in order to see proper performance. To deploy it on an R server, you can click the blue button on the top right hand side of the app that will launch locally. -->
<!-- ## Future plans -->
<!-- What *are* our future plans?!?!  -->
<!-- - How can we update the app (particularly the data inputs) to suit our region? -->
<!-- - Are there national sources of supplemental data which might be nice to include (transit for instance?). If not, is this a thing that some code would still be helpful? -->
<!-- - Is z-score the best variable here? What are the other tabs in the Portland data? -->
