#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-08-10    #
#########################

# Load libraries
library(here)
library(data.table)
library(fastLink)

# Paths
census_path  <- here("fastlink/cleaned")
data_path    <- here("fastlink/data")
out_path     <- here("fastlink/flink")

dir.create(out_path, showWarnings=FALSE)

# bunmd
bunmd <- fread(paste0(data_path, "bunmd_name.csv"), na.strings=c(""))
setnames(bunmd,
         c("census_age", "fname_clean", "lname_clean", "father_lname_clean"),
         c("age", "fname", "lname", "father_lname"))

bunmd <- bunmd[age>=19 & age<=31 & !is.na(bpl)]
bpl <- unique(bunmd$bpl)
bpl <- bpl[order(bpl)]
bpl <- bpl[bpl<=6000]

# census file list
census_list <- list.files(census_path)

# match
for (i in bpl){
  
  dfB <- bunmd[bpl==i & sex==1]
  if (nrow(dfB)>0 & sum(grepl(paste0("1940_name_", i, "_1.csv"), census_list))==1){
    dfB[, age:=as.numeric(age)]
    dfA <- fread(paste0(census_path, "1940_name_", i, "_1.csv"))
    dfA[, age:=as.numeric(age)]
    dfA <- dfA[age>=20 & age<=30]
    if (nrow(dfA)>0){
      flink <- fastLink(dfA,
                        dfB,
                        varnames=c("fname", "lname", "age"),
                        numeric.match=c("age"),
                        stringdist.match=c("fname", "lname"),
                        partial.match=c("fname", "lname", "age"),
                        cut.a=1, cut.p=0.94,
                        cut.a.num=0.5, cut.p.num=1,
                        threshold.match=0.8,
                        n.cores=ncore)
      colnames(dfA) <- paste0("A_", colnames(dfA))
      colnames(dfB) <- paste0("B_", colnames(dfB))
      data <- data.table(dfA[flink$matches$inds.a], dfB[flink$matches$inds.b], prob=flink$posterior, flink$patterns)
      data <- data[gamma.1!=0 & gamma.2!=0 & gamma.3!=0]
      if (nrow(data)>0){
        fwrite(data, paste0(out_path, "flink_", i, "_1.csv"))
      }
      rm(flink, data)
    }
    rm(dfA)
  }
  rm(dfB)
  
  dfB <- bunmd[bpl==i & sex==2]
  if (nrow(dfB)>0 & sum(grepl(paste0("1940_name_", i, "_2.csv"), census_list))==1){
    dfB[, age:=as.numeric(age)]
    dfA <- fread(paste0(census_path, "1940_name_", i, "_2.csv"))
    dfA[, age:=as.numeric(age)]
    dfA <- dfA[age>=20 & age<=30]
    if (nrow(dfA)>0){
      flink <- fastLink(dfA,
                        dfB,
                        varnames=c("fname", "lname", "age"),
                        numeric.match=c("age"),
                        stringdist.match=c("fname", "lname"),
                        partial.match=c("fname", "lname", "age"),
                        cut.a=1, cut.p=0.94,
                        cut.a.num=0.5, cut.p.num=1,
                        threshold.match=0.8,
                        n.cores=ncore)
      colnames(dfA) <- paste0("A_", colnames(dfA))
      colnames(dfB) <- paste0("B_", colnames(dfB))
      data <- data.table(dfA[flink$matches$inds.a], dfB[flink$matches$inds.b], prob=flink$posterior, flink$patterns)
      data <- data[gamma.1!=0 & gamma.2!=0 & gamma.3!=0]
      if (nrow(data)>0){
        fwrite(data, paste0(out_path, "flink_", i, "_2.csv"))
      }
      ids <- data[, "B_ssn"]
      colnames(ids) <- "ssn"
      ids[, link:=1]
      rm(flink, data)
    }
    rm(dfA)
  }
  rm(dfB)
  
  dfB <- bunmd[bpl==i & sex==2]
  dfB <- merge(dfB, ids, all.x=TRUE)
  rm(ids)
  dfB <- dfB[is.na(link), -"link"]
  setnames(dfB,
           c("lname", "father_lname"),
           c("lname_o", "lname"))
  if (nrow(dfB)>0 & sum(grepl(paste0("1940_name_", i, "_3.csv"), census_list))==1){
    dfB[, age:=as.numeric(age)]
    dfA <- fread(paste0(census_path, "1940_name_", i, "_3.csv"))
    dfA[, age:=as.numeric(age)]
    dfA <- dfA[age>=20 & age<=30]
    if (nrow(dfA)>0){
      flink <- fastLink(dfA,
                        dfB,
                        varnames=c("fname", "lname", "age"),
                        numeric.match=c("age"),
                        stringdist.match=c("fname", "lname"),
                        partial.match=c("fname", "lname", "age"),
                        cut.a=1, cut.p=0.94,
                        cut.a.num=0.5, cut.p.num=1,
                        threshold.match=0.8,
                        n.cores=ncore)
      setnames(dfB,
               c("lname_o", "lname"),
               c("lname", "father_lname"))
      colnames(dfA) <- paste0("A_", colnames(dfA))
      colnames(dfB) <- paste0("B_", colnames(dfB))
      data <- data.table(dfA[flink$matches$inds.a], dfB[flink$matches$inds.b], prob=flink$posterior, flink$patterns)
      data <- data[gamma.1!=0 & gamma.2!=0 & gamma.3!=0]
      if (nrow(data)>0){
        fwrite(data, paste0(out_path, "flink_", i, "_3.csv"))
      }
      rm(flink, data)
    }
    rm(dfA)
  }
  rm(dfB)
}

# merge
flink_list <- list.files(out_path)
flink_list <- flink_list[grep("flink_", flink_list)]

flink <- data.table()

for (i in flink_list){
  temp <- fread(paste0(out_path, i), na.strings=c(""))
  flink <- rbind(flink, temp)
  rm(temp)
}

fwrite(flink, paste0(out_path, "merged.csv"))

# Clean up after yourself
rm(list = ls())
