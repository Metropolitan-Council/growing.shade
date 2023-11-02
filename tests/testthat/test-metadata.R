testthat::test_that("bg_growingshade_main data is valid", {

  metadata %>% 
    filter(variable == "hhincome") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(87845,
                           tolerance = 0.01)

  metadata %>% 
    filter(variable == "ppov185") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.182,
                           tolerance = 0.01)
      
  metadata %>% 
    filter(variable == "pbipoc") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.312,
                           tolerance = 0.01)
  
  metadata %>% 
    filter(variable == "pd_any") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.0996,
                           tolerance = 0.01)
  
  
  metadata %>% 
    filter(variable == "pownhome") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.691,
                           tolerance = 0.01)
  
  
  metadata %>% 
    filter(variable == "CASTHMA") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.0945,
                           tolerance = 0.01)
  
  
  metadata %>% 
    filter(variable == "MHLTH") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.14,
                           tolerance = 0.01)
  
  metadata %>% 
    filter(variable == "COPD") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.429,
                           tolerance = 0.01)
  
  
  metadata %>% 
    filter(variable == "canopy_percent") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.423,
                           tolerance = 0.01)
  
  
  metadata %>% 
    filter(variable == "pownhome") %>% 
    magrittr::extract2("MEANRAW") %>% 
    testthat::expect_equal(0.691,
                           tolerance = 0.01)
  
  metadata %>% 
    magrittr::extract2("n") %>% 
    unique() %>% 
    testthat::expect_equal(2586)
  
  
  nhood_list %>% 
    nrow() %>% 
    testthat::expect_equal(28)
  
  
  ctu_list %>% 
    nrow() %>% 
    testthat::expect_equal(186)
})
