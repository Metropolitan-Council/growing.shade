README
================
Ellen Esch
03 October 2022

-   <a href="#overview" id="toc-overview">Overview</a>
-   <a href="#getting-started" id="toc-getting-started">Getting started</a>

## Overview

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

**This repository walks through the creation and aggregation of data
used in interactive mapping tool.**

## Getting started

The `01_tutorial.Rmd` script walks through the steps to get a working
versions for different areas. Depending on what data is available for
various regions, the `02_geographies.Rmd` and `03_treecanopy.Rmd`
scripts may also be needed (although this data can be created through
other methods).

Most packages are available on CRAN, so can be downloaded through
standard procedures. There are two specific packages which should be
downloaded from GitHub versions.

``` r
devtools::install_github("eclarke/ggbeeswarm")
```

    ## Skipping install of 'ggbeeswarm' from a github remote, the SHA1 (a7e38d33) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
devtools::install_github("Metropolitan-Council/councilR")
```

    ## Skipping install of 'councilR' from a github remote, the SHA1 (059f15b5) has not changed since last install.
    ##   Use `force = TRUE` to force installation
