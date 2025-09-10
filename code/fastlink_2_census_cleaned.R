#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-09-10    #
#########################

# Load libraries
library(here)
library(stringi)
library(tidyverse)
library(data.table)

# Paths
source_path      <- here("data/fastlink/source/")
in_census_path   <- here("data/fastlink/split/")
out_census_path  <- here("data/fastlink/cleaned/")

dir.create(out_census_path, showWarnings=FALSE)

# Source functions
source(paste0(source_path, "clean-names.R"))

# Read titles and nicknames
titles <- fread(paste0(source_path, "titles.csv"))
male_nicknames <- fread(paste0(source_path, "male_nicknames.csv"))
female_nicknames <- fread(paste0(source_path, "female_nicknames.csv"))
nicknames <- data.table(rbind(male_nicknames, female_nicknames))
old_nicknames <- fread(paste0(source_path, "old_nicknames.csv"))

# Clean census names
lname_col_census <- "namelast"
fname_col_census <- "namefrst"
sex_col_census <- "sex"
other_cols_census <- c("birthyr", "bpl", "serialp", "pernum", "histid", "marst", "age", "race")
middle_census <- ""
for (file_name in list.files(in_census_path)) {
  clean_names(in_census_path, file_name, lname_col_census, fname_col_census,
              sex_col_census, other_cols_census, out_census_path, middle_census)
}

# Clean up after yourself
rm(list = ls())

