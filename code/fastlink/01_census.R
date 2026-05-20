#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-09-10    #
#########################

# 1940 Census data ("data/fastlink/raw/census_1940.csv") include the following variables:
# ("serialp", "pernum", "age", "sex", "race", "marst", "bpl", "birthyr", "namelast", "namefrst", "histid")

# Load libraries
library(here)
library(data.table)

# Paths
data_path         <- here("data/fastlink/raw/")
out_census_path   <- here("data/fastlink/split/")

dir.create(out_census_path, showWarnings=FALSE)

# load data
data <- fread(paste0(data_path, "census_1940.csv"), na.strings=c(""))

#split by bpl
data[is.na(bpl), bpl:=99999]

bpl <- unique(data$bpl)

for (i in bpl){
  temp <- data[bpl==i & sex==1]
  if (nrow(temp)>0){
    fwrite(temp, file = paste0(out_census_path, "1940_name_", i, "_1.csv"))
  }
  rm(temp)

  temp <- data[bpl==i & sex==2 & marst<6]
  if (nrow(temp)>0){
    fwrite(temp, file = paste0(out_census_path, "1940_name_", i, "_2.csv"))
  }
  rm(temp)

  temp <- data[bpl==i & sex==2 & marst==6]
  if (nrow(temp)>0){
    fwrite(temp, file = paste0(out_census_path, "1940_name_", i, "_3.csv"))
  }
  rm(temp)
}

# Clean up after yourself
rm(list = ls())

