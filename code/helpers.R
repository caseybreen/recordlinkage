## LIBRARY packages 
pacman::p_load(
  tidyverse,
  data.table,
  cowplot,
  broom,
  vroom
)



## custom color schemes
cudb <- c("#49b7fc", "#ff7b00", "#17d898", "#ff0083", "#0015ff", "#e5d200", "#999999")
cud <- c("#D55E00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#E69F00", "#F0E442", "#999999")



## function for recoding education
recode_education <- function(df, educ_var) {
  
  df <- df  %>%
    mutate(educ_variable = !!sym(educ_var)) %>%
    mutate(educ_yrs = case_when(
      educ_variable == 2 ~ 0,
      educ_variable == 12 ~ 0,
      educ_variable == 14 ~ 1,
      educ_variable == 15 ~ 2,
      educ_variable == 16 ~ 3,
      educ_variable == 17 ~ 4,
      educ_variable == 22 ~ 5,
      educ_variable == 23 ~ 6,
      educ_variable == 25 ~ 7,
      educ_variable == 26 ~ 8,
      educ_variable == 30 ~ 9,
      educ_variable == 40 ~ 10,
      educ_variable == 50 ~ 11,
      educ_variable == 60 ~ 12,
      educ_variable == 70 ~ 13,
      educ_variable == 80 ~ 14,
      educ_variable == 90 ~ 15,
      educ_variable == 100 ~ 16,
      educ_variable == 110 ~ 17,
      educ_variable == 111 ~ 17,
      educ_variable == 112 ~ 17,
      educ_variable == 113 ~ 17
    )) %>%
    select(-educ_variable)
  
  return(df)
  
}

## custom color schemes
cudb <- c("#49b7fc", "#ff7b00", "#17d898", "#ff0083", "#0015ff", "#e5d200", "#999999")
cud <- c("#D55E00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#E69F00", "#F0E442", "#999999")


get_first_letter_lower <- function(x) {
  sapply(x, function(word) {
    if (!is.na(word) && nchar(word) > 0) {
      tolower(substr(word, 1, 1))
    } else {
      NA
    }
  })
}