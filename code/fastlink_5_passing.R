#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-09-10    #
#########################

# for those who don't have the 1940 Census with name strings and/or NUMIDENT data:
# you can skip the data preparation process and use the data in the flink path ("merged_r.csv")

# Load libraries
library(here)
library(data.table)
library(fastLink)
library(matrixStats)

# Paths
flink_path  <- here("data/fastlink/flink/")
result_path   <- here("result/fastlink/")

dir.create(result_path, showWarnings=FALSE)

# parameters
h_th <- 0.8
qt <- 4

# data
flink <- fread(paste0(flink_path, "merged.csv"), na.strings=c(""))

# data preparation
flink[, n:=.N, by="B_ssn"]
flink <- flink[n==1]
flink <- flink[, n:=NULL]

flink <- flink[A_race==200 & !is.na(B_race_first)]

flink[, A_middle:=gsub(" ", "", A_middle)]
flink[, A_middle:=substr(A_middle, 0, 1)]
flink[A_middle=="", A_middle:=NA]
flink[, B_mname_clean:=gsub(" ", "", B_mname_clean)]
flink[, B_mname_clean:=substr(B_mname_clean, 0, 1)]
flink[B_mname_clean=="", B_mname_clean:=NA]

flink[!is.na(A_middle) & !is.na(B_mname_clean), tmatch:=0]
flink[!is.na(A_middle) & A_middle==B_mname_clean, tmatch:=1]
flink[, white:=as.numeric((B_race_first==1))]

# flink <- fread(paste0(flink_path, "merged_r.csv"), na.strings=c(""))

# passing_continuous
r_h <- quantile(flink$prob, h_th)
tpass_h <- flink[prob>=r_h & tmatch==1, mean(white)]
spass_h <- flink[prob>=r_h & !is.na(tmatch), mean(white)]

passing <- data.table()
for (q in seq(0, h_th, 0.01)){
  r <- quantile(flink$prob, q)
  
  n1 <- nrow(flink[prob>=r])
  
  pass <- flink[prob>=r, mean(white)]

  n2 <- nrow(flink[prob>=r & !is.na(A_middle) & !is.na(B_mname_clean)])
  
  spass <- flink[prob>=r & !is.na(tmatch), mean(white)]
  fmr <- flink[prob>=r & !is.na(tmatch), mean(1-tmatch)]
  tpass <- flink[prob>=r & tmatch==1, mean(white)]
  fpass <- flink[prob>=r & tmatch==0, mean(white)]

  apass1 <- pass*tpass/spass
  apass2 <- pass*tpass_h/spass_h
  apass3 <- pass*tpass_h/spass
  
  passing <- rbind(passing, data.table(q=q, r=r, n1=n1, pass=pass, n2=n2, spass=spass, fmr=fmr, tpass=tpass, fpass=fpass, apass1=apass1, apass2=apass2, apass3=apass3))
  rm(n1, pass, n2, spass, fmr, tpass, fpass, apass1, apass2, apass3)
}

fwrite(passing, paste0(result_path, "passing_continuous.csv"))
rm(passing)

# passing_quantile
c <- 0
itv <- 1/qt
for (q2 in seq(itv, 1, itv)){
  c <- c + 1
  q1 <- q2 - itv
  r1 <- quantile(flink$prob, q1)
  r2 <- quantile(flink$prob, q2)
  flink[prob>=r1 & prob<r2, quant:=c]
}
tpass_h <- flink[quant==qt & tmatch==1, mean(white)]
spass_h <- flink[quant==qt & !is.na(tmatch), mean(white)]

passing <- data.table()
for (q in 1:qt){
  n1 <- nrow(flink[quant==q])
  
  pass <- flink[quant==q, mean(white)]
  
  n2 <- nrow(flink[quant==q & !is.na(A_middle) & !is.na(B_mname_clean)])
  
  spass <- flink[quant==q & !is.na(tmatch), mean(white)]
  fmr <- flink[quant==q & !is.na(tmatch), mean(1-tmatch)]
  tpass <- flink[quant==q & tmatch==1, mean(white)]
  fpass <- flink[quant==q & tmatch==0, mean(white)]
  
  apass1 <- pass*tpass/spass
  apass2 <- pass*tpass_h/spass_h
  apass3 <- pass*tpass_h/spass
  
  passing <- rbind(passing, data.table(quant=q, n1=n1, pass=pass, n2=n2, spass=spass, fmr=fmr, tpass=tpass, fpass=fpass, apass1=apass1, apass2=apass2, apass3=apass3))
  rm(n1, pass, n2, spass, fmr, tpass, fpass, apass1, apass2, apass3)
}

fwrite(passing, paste0(result_path, "passing_quantile.csv"))
rm(passing)

# passing_continuous_boot

nr <- nrow(flink)

boot_pass <- data.table(q=seq(0, h_th, 0.01))
boot_apass1 <- data.table(q=seq(0, h_th, 0.01))
boot_apass2 <- data.table(q=seq(0, h_th, 0.01))
boot_apass3 <- data.table(q=seq(0, h_th, 0.01))

for (n in 1:1000){
  
  nr <- nrow(flink)
  ind <- sample.int(nr, replace=TRUE)
  df <- flink[ind, ]
  
  r_h <- quantile(df$prob, h_th)
  tpass_h <- df[prob>=r_h & tmatch==1, mean(white)]
  spass_h <- df[prob>=r_h & !is.na(tmatch), mean(white)]
  
  boot_pass[, paste0("b", n):=as.numeric(NA)]
  boot_apass1[, paste0("b", n):=as.numeric(NA)]
  boot_apass2[, paste0("b", n):=as.numeric(NA)]
  boot_apass3[, paste0("b", n):=as.numeric(NA)]
  for (p in seq(0, h_th, 0.01)){
    r <- quantile(df$prob, p)
    pass <- df[prob>=r, mean(white)]
    spass <- df[prob>=r & !is.na(tmatch), mean(white)]
    tpass <- df[prob>=r & tmatch==1, mean(white)]
    apass1 <- pass*tpass/spass
    apass2 <- pass*tpass_h/spass_h
    apass3 <- pass*tpass_h/spass
    
    boot_pass[q==p, paste0("b", n):=pass]
    boot_apass1[q==p, paste0("b", n):=apass1]
    boot_apass2[q==p, paste0("b", n):=apass2]
    boot_apass3[q==p, paste0("b", n):=apass3]
    
    rm(r, pass, spass, tpass, apass1, apass2, apass3)
  }
}

fwrite(boot_pass, paste0(result_path, "boot_continuous_pass.csv"))
fwrite(boot_apass1, paste0(result_path, "boot_continuous_apass1.csv"))
fwrite(boot_apass2, paste0(result_path, "boot_continuous_apass2.csv"))
fwrite(boot_apass3, paste0(result_path, "boot_continuous_apass3.csv"))

passing_boot <- data.table(q=seq(0, h_th, 0.01),
                          pass_mean=rowMeans(boot_pass[, -"q"]),
                          pass_l=rowQuantiles(as.matrix(boot_pass[, -"q"]), probs=c(0.025)),
                          pass_u=rowQuantiles(as.matrix(boot_pass[, -"q"]), probs=c(0.975)),
                          apass1_mean=rowMeans(boot_apass1[, -"q"]),
                          apass1_l=rowQuantiles(as.matrix(boot_apass1[, -"q"]), probs=c(0.025)),
                          apass1_u=rowQuantiles(as.matrix(boot_apass1[, -"q"]), probs=c(0.975)),
                          apass2_mean=rowMeans(boot_apass2[, -"q"]),
                          apass2_l=rowQuantiles(as.matrix(boot_apass2[, -"q"]), probs=c(0.025)),
                          apass2_u=rowQuantiles(as.matrix(boot_apass2[, -"q"]), probs=c(0.975)),
                          apass3_mean=rowMeans(boot_apass3[, -"q"]),
                          apass3_l=rowQuantiles(as.matrix(boot_apass3[, -"q"]), probs=c(0.025)),
                          apass3_u=rowQuantiles(as.matrix(boot_apass3[, -"q"]), probs=c(0.975)))
fwrite(passing_boot, paste0(result_path, "passing_continuous_boot.csv"))
rm(passing_boot)

# passing_quantile_boot

nr <- nrow(flink)

boot_pass <- data.table(quant=1:qt)
boot_apass1 <- data.table(quant=1:qt)
boot_apass2 <- data.table(quant=1:qt)
boot_apass3 <- data.table(quant=1:qt)

for (n in 1:1000){
  
  nr <- nrow(flink)
  ind <- sample.int(nr, replace=TRUE)
  df <- flink[ind, ]
  
  c <- 0
  itv <- 1/qt
  for (q2 in seq(itv, 1, itv)){
    c <- c + 1
    q1 <- q2 - itv
    r1 <- quantile(df$prob, q1)
    r2 <- quantile(df$prob, q2)
    df[prob>=r1 & prob<r2, quant:=c]
  }
  tpass_h <- df[quant==qt & tmatch==1, mean(white)]
  spass_h <- df[quant==qt & !is.na(tmatch), mean(white)]
  
  boot_pass[, paste0("b", n):=as.numeric(NA)]
  boot_apass1[, paste0("b", n):=as.numeric(NA)]
  boot_apass2[, paste0("b", n):=as.numeric(NA)]
  boot_apass3[, paste0("b", n):=as.numeric(NA)]
  for (q in 1:qt){
    pass <- df[quant==q, mean(white)]
    spass <- df[quant==q & !is.na(tmatch), mean(white)]
    tpass <- df[quant==q & tmatch==1, mean(white)]
    apass1 <- pass*tpass/spass
    apass2 <- pass*tpass_h/spass_h
    apass3 <- pass*tpass_h/spass
    
    boot_pass[quant==q, paste0("b", n):=pass]
    boot_apass1[quant==q, paste0("b", n):=apass1]
    boot_apass2[quant==q, paste0("b", n):=apass2]
    boot_apass3[quant==q, paste0("b", n):=apass3]
    
    rm(pass, spass, tpass, apass1, apass2, apass3)
  }
}

fwrite(boot_pass, paste0(result_path, "boot_quantile_pass.csv"))
fwrite(boot_apass1, paste0(result_path, "boot_quantile_apass1.csv"))
fwrite(boot_apass2, paste0(result_path, "boot_quantile_apass2.csv"))
fwrite(boot_apass3, paste0(result_path, "boot_quantile_apass3.csv"))

passing_boot <- data.table(quant=1:qt,
                           pass_mean=rowMeans(boot_pass[, -"quant"]),
                           pass_l=rowQuantiles(as.matrix(boot_pass[, -"quant"]), probs=c(0.025)),
                           pass_u=rowQuantiles(as.matrix(boot_pass[, -"quant"]), probs=c(0.975)),
                           apass1_mean=rowMeans(boot_apass1[, -"quant"]),
                           apass1_l=rowQuantiles(as.matrix(boot_apass1[, -"quant"]), probs=c(0.025)),
                           apass1_u=rowQuantiles(as.matrix(boot_apass1[, -"quant"]), probs=c(0.975)),
                           apass2_mean=rowMeans(boot_apass2[, -"quant"]),
                           apass2_l=rowQuantiles(as.matrix(boot_apass2[, -"quant"]), probs=c(0.025)),
                           apass2_u=rowQuantiles(as.matrix(boot_apass2[, -"quant"]), probs=c(0.975)),
                           apass3_mean=rowMeans(boot_apass3[, -"quant"]),
                           apass3_l=rowQuantiles(as.matrix(boot_apass3[, -"quant"]), probs=c(0.025)),
                           apass3_u=rowQuantiles(as.matrix(boot_apass3[, -"quant"]), probs=c(0.975)))
fwrite(passing_boot, paste0(result_path, "passing_quantile_boot.csv"))
rm(passing_boot)

# Clean up after yourself
rm(list = ls())


