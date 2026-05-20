#########################
# AUTHOR: Won-tak Joo   #
# DATE:   2025-09-10    #
#########################

# Load libraries
library(here)
library(data.table)
library(foreach)
library(doParallel)
library(matrixStats)

# Paths
result_path <- here("result/simulation/reg/")
dir.create(result_path, showWarnings=FALSE)

# set seed
set.seed(1234)

# parameters
n <- 1000
sn <- 1000
rec <- seq(0.1,1,0.05)
pre <- seq(0.1,1,0.05)
merror <- c("01", "05", "10")
edirect <- 1:3
models <- 1:8

for (mr in merror){
  for (ed in edirect){
    dir.create(paste0(result_path, "mr", mr, "_", ed), showWarnings=FALSE)
  }
}

# cores
ncore <- detectCores()-1
registerDoParallel(cores=ncore)

########## we are interested in the association/effect of t with/on y
########## we link data based on l; l has different roles by model

########## error direction 1: normal (y is not biased)
########## error direction 2: absolute normal (L is matched with smaller L; beta is upward biased)
########## error direction 3: -absolute normal (L is matched with larger L; beta is downward biased)
########## case 1
##### l is independent from t
##### measurement error for l is independent from l
########## case 2
##### l is independent from t
##### measurement error for l is associated with l
########## case 3
##### l is a confounder of t
##### measurement error for l is independent from l
########## case 4
##### l is a confounder of t
##### measurement error for l is associated with l
########## case 5
##### l is a moderator
##### measurement error for l is independent from l
########## case 6
##### l is a moderator
##### measurement error for l is associated with l
########## case 7
##### l is a moderator (negative interaction)
##### measurement error for l is independent from l
########## case 8
##### l is a moderator (negative interaction)
##### measurement error for l is associated with l

for (mr in merror){
  for (ed in edirect){
    for (mo in models){
      
      result_path_r <- paste0(result_path, "mr", mr, "_", ed, "/")
      me <- as.numeric(mr)/10
      
      dt <- data.table(id=1:n)
      if (mo==1|mo==2){
        for (i in 1:1000){
          dt[, l:=rnorm(n)]
          dt[, t:=rnorm(n)]
          dt[, y:=2*t+rnorm(n)]
          lm1 <- lm(y ~ t, data=dt)
          betat1 <- lm1$coefficients[2]
          lm2 <- lm(y ~ t+l, data=dt)
          betat2 <- lm2$coefficients[2]
          if (abs(betat1-2)<0.01) break
        }
        rm(lm1, lm2)
      }
      if (mo==3|mo==4){
        for (i in 1:1000){
          dt[, l:=rnorm(n)]
          dt[, t:=l+rnorm(n)]
          dt[, y:=2*t+l+rnorm(n)]
          lm1 <- lm(y ~ t, data=dt)
          betat1 <- lm1$coefficients[2]
          lm2 <- lm(y ~ t+l, data=dt)
          betat2 <- lm2$coefficients[2]
          if (abs(betat2-2)<0.01) break
        }
        rm(lm1, lm2)
      }
      if (mo==5|mo==6){
        for (i in 1:1000){
          dt[, l:=rnorm(n)]
          dt[, t:=rnorm(n)]
          dt[, y:=2*t+l+t*l+rnorm(n)]
          lm1 <- lm(y ~ t, data=dt)
          betat1 <- lm1$coefficients[2]
          lm2 <- lm(y ~ t+l, data=dt)
          betat2 <- lm2$coefficients[2]
          if (abs(betat1-2)<0.01) break
        }
        rm(lm1, lm2)
      }
      if (mo==7|mo==8){
        for (i in 1:1000){
          dt[, l:=rnorm(n)]
          dt[, t:=rnorm(n)]
          dt[, y:=2*t+l-t*l+rnorm(n)]
          lm1 <- lm(y ~ t, data=dt)
          betat1 <- lm1$coefficients[2]
          lm2 <- lm(y ~ t+l, data=dt)
          betat2 <- lm2$coefficients[2]
          if (abs(betat1-2)<0.01) break
        }
        rm(lm1, lm2)
      }
      dt[, dl:=dnorm(l, mean=mean(l), sd=sd(l))]
      
      for (rrate in rec){
        for (prate in pre){
          if (prate < rrate){
            beta_iter <- matrix(nrow=5, ncol=sn)
          } else {
            beta_iter <- foreach(iter=1:sn, .combine=cbind) %dopar% {
              dt_1 <- dt[, c("id", "l", "t")]
              setnames(dt_1, c("id", "l"), c("id_1", "l_1"))
              dt_2 <- dt[, c("id", "l", "y", "dl")]
              if (ed==1 & (mo%%2==1)){
                dt_2 <- dt_2[, l:=l+rnorm(n, sd=me)]
              }
              if (ed==2 & (mo%%2==1)){
                dt_2 <- dt_2[, l:=l+abs(rnorm(n, sd=me))]
              }
              if (ed==3 & (mo%%2==1)){
                dt_2 <- dt_2[, l:=l-abs(rnorm(n, sd=me))]
              }
              if (ed==1 & (mo%%2==0)){
                dt_2 <- dt_2[, l:=l+2*(max(l)-l)/(max(l)-min(l))*rnorm(n, sd=me)]
              }
              if (ed==2 & (mo%%2==0)){
                dt_2 <- dt_2[, l:=l+2*(max(l)-l)/(max(l)-min(l))*abs(rnorm(n, sd=me))]
              }
              if (ed==3 & (mo%%2==0)){
                dt_2 <- dt_2[, l:=l-2*(max(l)-l)/(max(l)-min(l))*abs(rnorm(n, sd=me))]
              }
              setnames(dt_2, c("id", "l"), c("id_2", "l_2"))
              dt_s <- merge(dt_1, dt_2, by.x="id_1", by.y="id_2")
              dt_s[, id_2:=id_1]
              dt_s[, l_dist:=abs(l_1-l_2)]
              dt_s <- dt_s[order(l_dist)]
              dt_s <- dt_s[1:(.N*rrate), -"l_dist"]
              fn <- round(nrow(dt_s)*((1-prate)/prate), 0)
              for (i in (1:100)){
                if (fn>0){
                  id_1 <- dt_s[, "id_1"]
                  id_1[, check:=1]
                  dt_fp_1 <- merge(dt_1, id_1, by="id_1", all.x=TRUE)
                  dt_fp_1 <- dt_fp_1[is.na(check)]
                  id_2 <- dt_s[, "id_2"]
                  id_2[, check:=1]
                  dt_fp_2 <- merge(dt_2, id_2, by="id_2", all.x=TRUE)
                  dt_fp_2 <- dt_fp_2[is.na(check)]
                  dt_fp <- merge(dt_fp_1, dt_fp_2, by="check", allow.cartesian=TRUE)
                  dt_fp <- dt_fp[id_1!=id_2]
                  dt_fp[, check:=NULL]
                  dt_fp[, l_dist:=abs(l_1-l_2)]
                  dt_fp[, rank_1:=frank(l_dist), by="id_1"]
                  dt_fp[, rank_2:=frank(l_dist), by="id_2"]
                  dt_fp_a <- dt_fp[rank_1==1 & rank_2==1, -c("rank_1", "rank_2")]
                  if (nrow(dt_fp_a)<fn){
                    dt_s <- rbind(dt_s, dt_fp_a[, -"l_dist"])
                    fn <- fn - nrow(dt_fp_a)
                  }
                  if (nrow(dt_fp_a)>=fn){
                    dt_fp_a <- dt_fp_a[order(l_dist)]
                    dt_fp_a <- dt_fp_a[1:fn, -"l_dist"]
                    dt_s <- rbind(dt_s, dt_fp_a)
                    fn <- 0
                  }
                }
              }
              lm_s1 <- lm(y ~ t, data=dt_s)
              lm_s2 <- lm(y ~ t+l_1, data=dt_s)
              dt_s[, dls:=dnorm(l_1, mean=mean(l_1), sd=sd(l_1))]
              dt_s[, wt:=dl/dls]
              lm_w1 <- lm(y ~ t, data=dt_s, weights=wt)
              lm_w2 <- lm(y ~ t+l_1, data=dt_s, weights=wt)
              
              export <- c(lm_s1$coefficients[2], lm_s2$coefficients[2],
                          lm_w1$coefficients[2], lm_w2$coefficients[2], fn)
              return(export)
            }
          }
          if (!exists("betas1")){
            betas1 <- beta_iter[1,]
            betas2 <- beta_iter[2,]
            betaw1 <- beta_iter[3,]
            betaw2 <- beta_iter[4,]
            fns <- beta_iter[5,]
          } else {
            betas1 <- rbind(betas1, beta_iter[1,])
            betas2 <- rbind(betas2, beta_iter[2,])
            betaw1 <- rbind(betaw1, beta_iter[3,])
            betaw2 <- rbind(betaw2, beta_iter[4,])
            fns <- rbind(fns, beta_iter[5,])
          }
          rm(beta_iter)
        }
      }
      res <- data.table(rec=sort(rep(rec, length(pre))),
                        pre=rep(pre, length(rec)),
                        
                        betat1=betat1,
                        beta1=rowMeans(betas1),
                        sd1=sqrt(rowMeans((betas1-rowMeans(betas1))^2)),
                        lb1=rowQuantiles(betas1, probs=0.025),
                        ub1=rowQuantiles(betas1, probs=0.975),
                        beta1_w=rowMeans(betaw1),
                        sd1_w=sqrt(rowMeans((betaw1-rowMeans(betaw1))^2)),
                        lb1_w=rowQuantiles(betaw1, probs=0.025),
                        ub1_w=rowQuantiles(betaw1, probs=0.975),
                        
                        betat2=betat2,
                        beta2=rowMeans(betas2),
                        sd2=sqrt(rowMeans((betas2-rowMeans(betas2))^2)),
                        lb2=rowQuantiles(betas2, probs=0.025),
                        ub2=rowQuantiles(betas2, probs=0.975),
                        beta2_w=rowMeans(betaw2),
                        sd2_w=sqrt(rowMeans((betaw2-rowMeans(betaw2))^2)),
                        lb2_w=rowQuantiles(betaw2, probs=0.025),
                        ub2_w=rowQuantiles(betaw2, probs=0.975),
                        
                        fn_max=rowMaxs(fns),
                        fn_count=rowSums(fns!=0))
      res[, f:=2*pre*rec/(pre+rec)]
      rm(dt, betas1, betas2, betaw1, betaw2, fns)
      
      fwrite(res, paste0(result_path_r, mo, "_result.csv"))
      rm(res)
      
    }
  }
}

########## end

rm(list=ls())

