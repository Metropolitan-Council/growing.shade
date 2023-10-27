# restart your session to clean out any extra namespaces
# rstudioapi::restartSession()

# If you built councilR from source on your machine,
# re-install it with GitHub so RStudio records the appropriate source
# otherwise it tries to find councilR on CRAN 
pkgload::load_all()
if(
  grepl(
    "Github", 
    package_info("councilR") %>% 
    filter(package == "councilR") %>% 
    magrittr::extract2("source")) == FALSE){
  remotes::install_github("Metropolitan-Council/councilR")
}

# Delete src files
# these will be re-built when you next build the package locally
# but will cause errors if they are floating around when you deploy

library(rsconnect)
options(rsconnect.packrat = TRUE)
# Deploy app to testing location on shinyapps.io
rsconnect::deployApp(
  appDir = ".",
  account = "metrotransitmn",
  server = "shinyapps.io",
  appName = "growing-shade-test",
  forceUpdate = TRUE,
  lint = FALSE,
  # appId = 7971327,
  launch.browser = TRUE,
  logLevel = "verbose"
)


# deploy app to production location on shinyapps.io
# rsconnect::deployApp(
#   appDir = ".",
#   account = "metrotransitmn",
#   server = "shinyapps.io",
#   appName = "growing-shade",
#   forceUpdate = TRUE,
#   lint = FALSE,
#   appId = 4198227,
#   launch.browser = TRUE,
#   logLevel = "verbose"
# )

