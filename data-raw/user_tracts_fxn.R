#` script to download user tracts from tigris. works for 2 state/county combinations

user_tracts_fxn <- function(user_state_1, user_county_1, user_state_2, user_county_2) {
  if (!is.na(user_state_1))
    a <- tigris::tracts(#state="MN", county = "Washington", class = "sf") %>% select(GEOID)
      state = user_state_1,
      county = user_county_1,
      class = "sf"
    ) %>%
      select(GEOID)
  
  # if(missing(user_state_2)) {
  if(is.na(user_state_2)) {
    b <- tribble(~GEOID)} else {
      b <- tigris::tracts(
        state = user_state_2,
        county = user_county_2,
        class = "sf"
      ) %>%
        select(GEOID)}
  
  if(is.na(user_county_2)) {tribble(~GEOID)}
  # if(missing(user_county_2)) {tibble()}
  
  user_tracts <- if(is.na(user_state_2)) {
    # if(missing(user_state_2)) {
    return(a %>% 
             st_transform(4326))
  } else {
    return(rbind(a, b) %>% 
             st_transform(4326))}
}

tribble(~GEOID)
