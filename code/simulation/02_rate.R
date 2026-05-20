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
result_path <- here("result/simulation/rate/")
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
models <- 1:4

rate <- c("05", "10", "15")
for (rt in rate){
  dir.create(paste0(result_path, "rt", rt), showWarnings=FALSE)
  for (mr in merror){
    for (ed in edirect){
      dir.create(paste0(result_path, "rt", rt, "/mr", mr, "_", ed, "/"), showWarnings=FALSE)
    }
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
##### l is associated with t
##### measurement error for l is independent from l
########## case 4
##### l is associated with t
##### measurement error for l is associated with l

for (mr in merror){
  for (ed in edirect){
    for (rt in rate){
      rp <- as.numeric(rt)/100
      result_path_r <- paste0(result_path, "rt", rt, "/mr", mr, "_", ed, "/")
      me <- as.numeric(mr)/10
      for (mo in models){
        
        dt <- data.table(id=1:n)
        if (mo==1|mo==2){
          dt[, l:=rnorm(n)]
          dt[, r1:=0]
          idx1 <- sample(n, n*0.5)
          dt[idx1, r1:=1]
          dt[, r2:=r1]
          idx2 <- sample(idx1, n*0.5*rp)
          dt[idx2, r2:=0]
          rm(idx1, idx2)
        }
        if (mo==3|mo==4){
          dt[, l:=rnorm(n)]
          cdlf <- ecdf(dt$l)
          dt[, cdl:=cdlf(l)]
          dt[, r1:=0]
          idx1 <- sample(n, n*0.5)
          dt[idx1, r1:=1]
          dt[, r2:=r1]
          idx2 <- sample(idx1, n*0.5*rp, prob=dt[r1==1]$cdl)
          dt[idx2, r2:=0]
          dt[, cdl:=NULL]
          rm(idx1, idx2, cdlf)
        }
        dt[, dl:=dnorm(l, mean=mean(l), sd=sd(l))]
        
        for (rrate in rec){
          for (prate in pre){
            if (prate < rrate){
              tr_iter <- matrix(nrow=3, ncol=sn)
            } else {
              tr_iter <- foreach(iter=1:sn, .combine=cbind) %dopar% {
                dt_1 <- dt[, c("id", "l", "r1")]
                setnames(dt_1, c("id", "l"), c("id_1", "l_1"))
                dt_2 <- dt[, c("id", "l", "r2", "dl")]
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
                dt_s[, rch:=abs(r1-r2)]
                tr_s <- mean(dt_s[r1==1]$rch)
                dt_s[, dls:=dnorm(l_1, mean=mean(l_1), sd=sd(l_1))]
                dt_s[, wt:=dl/dls]
                tr_w <- weighted.mean(dt_s[r1==1]$rch, dt_s[r1==1]$wt)
                
                export <- c(tr_s, tr_w, fn)
                return(export)
              }
            }
            if (!exists("trs")){
              trs <- tr_iter[1,]
              trw <- tr_iter[2,]
              fns <- tr_iter[3,]
            } else {
              trs <- rbind(trs, tr_iter[1,])
              trw <- rbind(trw, tr_iter[2,])
              fns <- rbind(fns, tr_iter[3,])
            }
            rm(beta_iter)
          }
        }
        res <- data.table(rec=sort(rep(rec, length(pre))),
                          pre=rep(pre, length(rec)),
                          
                          tr=rowMeans(trs),
                          sd=sqrt(rowMeans((trs-rowMeans(trs))^2)),
                          lb=rowQuantiles(trs, probs=0.025),
                          ub=rowQuantiles(trs, probs=0.975),
                          tr_w=rowMeans(trw),
                          sd_w=sqrt(rowMeans((trw-rowMeans(trw))^2)),
                          lb_w=rowQuantiles(trw, probs=0.025),
                          ub_w=rowQuantiles(trw, probs=0.975),
                          
                          fn_max=rowMaxs(fns),
                          fn_count=rowSums(fns!=0))
        res[, f:=2*pre*rec/(pre+rec)]
        rm(dt, trs, trw, fns)
        
        fwrite(res, paste0(result_path_r, mo, "_result.csv"))
        rm(res)
        
      }
    }
  }
}

########## end

rm(list=ls())
