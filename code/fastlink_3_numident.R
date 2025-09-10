#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-09-10    #
#########################

# BUNMD v2 and BUNMD clean names V1 are located in the data path

# Load libraries
library(here)
library(data.table)

# Paths
data_path <- here("data/fastlink/raw/")

bunmd <- fread(paste0(data_path, "bunmd_v2.csv"),
               select=c("ssn", "byear", "bmonth", "sex",
                        "race_first", "race_last",
                        "bpl", "weight", "ccweight"),
               na.strings=c(""))
bunmd[bmonth<4, census_age:=1940-byear]
bunmd[bmonth>=4, census_age:=1940-byear-1]
bunmd[, bmonth:=NULL]

name <- fread(paste0(data_path, "bunmd_clean_names_v1.csv"),
               select=c("ssn", "fname_clean", "mname_clean", "lname_clean", "father_lname_clean"),
               na.strings=c(""))

bunmd <- merge(bunmd, name, by="ssn", all.x=TRUE)
fwrite(bunmd, paste0(data_path, "bunmd_name.csv"))

########## end
rm(list=ls())

