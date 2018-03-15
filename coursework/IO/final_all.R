library(tidyverse)
library(haven)
library(knitr)
library(kableExtra)

data <- read_dta("predicted_revenue_for_exam.dta") 


data <-  data %>% select(-ls1, -ls2, -ls3, -ls4, -ls5) %>% 
  mutate_at(.vars=vars(starts_with("l")), .funs=funs(exp = exp(.))) %>%
  select(ono, year, s1, s2, s3, s4, s5, ends_with("exp"))


data1 <- data %>% filter(ono == 1) %>% select(year, s1, s2, s3, s4, s5, ends_with("1_exp")) # France  
data2 <- data %>% filter(ono == 2) %>% select(year, s1, s2, s3, s4, s5, ends_with("2_exp")) # Germany
data3 <- data %>% filter(ono == 3) %>% select(year, s1, s2, s3, s4, s5, ends_with("3_exp")) # Spain
data4 <- data %>% filter(ono == 4) %>% select(year, s1, s2, s3, s4, s5, ends_with("4_exp")) # UK
data5 <- data %>% filter(ono == 5) %>% select(year, s1, s2, s3, s4, s5, ends_with("5_exp")) # US

n1 <- nrow(data1)
n2 <- nrow(data2)
n3 <- nrow(data3)
n4 <- nrow(data4)
n5 <- nrow(data5)


nam <-c("s1", "s2", "s3", "s4", "s5")

enam1 <- c("ls1hat_1_exp", "ls2hat_1_exp", "ls3hat_1_exp", "ls4hat_1_exp", "ls5hat_1_exp")
enam2 <- c("ls1hat_2_exp", "ls2hat_2_exp", "ls3hat_2_exp", "ls4hat_2_exp", "ls5hat_2_exp")
enam3 <- c("ls1hat_3_exp", "ls2hat_3_exp", "ls3hat_3_exp", "ls4hat_3_exp", "ls5hat_3_exp")
enam4 <- c("ls1hat_4_exp", "ls2hat_4_exp", "ls3hat_4_exp", "ls4hat_4_exp", "ls5hat_4_exp")
enam5 <- c("ls1hat_5_exp", "ls2hat_5_exp", "ls3hat_5_exp", "ls4hat_5_exp", "ls5hat_5_exp")

datamat1 <- matrix(nrow = n1*5, ncol = 5)
datamat2 <- matrix(nrow = n2*5, ncol = 5)
datamat3 <- matrix(nrow = n3*5, ncol = 5)
datamat4 <- matrix(nrow = n4*5, ncol = 5)
datamat5 <- matrix(nrow = n5*5, ncol = 5)

datav <- list(data1,data2,data3,data4,data5)
datamatv <- list(datamat1,datamat2,datamat3,datamat4,datamat5)
enamv <- list(enam1,enam2,enam3,enam4,enam5)
n <- c(n1, n2, n3, n4, n5)


for (k in 1:5){
  for (j in 1:5){
    datamatv[[k]][,3]= k
    for (i in 1:n[[k]]){
      datamatv[[k]][i+(j-1)*n[[k]],4]= j
      datamatv[[k]][i+(j-1)*n[[k]],5]= as.numeric(datav[[k]][i,"year"])
      if(is.na(datav[[k]][i,nam[j]])==TRUE){
        datamatv[[k]][i+(j-1)*n[[k]],1] = 0
        datamatv[[k]][i+(j-1)*n[[k]],2] = as.numeric(datav[[k]][i,enamv[[k]][j]])
      }
      else{
        datamatv[[k]][i+(j-1)*n[[k]],1] = 1 
        datamatv[[k]][i+(j-1)*n[[k]],2] = as.numeric(datav[[k]][i,nam[j]])
      }
    }
  }
}

datamat <- rbind(datamatv[[1]],datamatv[[2]], datamatv[[3]],datamatv[[4]],datamatv[[5]])
dat <- datamat[!is.na(datamat[,2]),]
rm(n1,n2,n3,n4,n5, enam1,enam2,enam3,enam4,enam5,datamat1,datamat2,datamat3,datamat4,datamat5,data1,data2,data3,data4,data5,datamatv,datav,enamv,i,j,k,n,nam,datamat)

da <- as.tibble(dat)
names(da) <- c("export","revenue","origin","destination","year")

da <- da %>% mutate(y = 0*(year<2006) + 1*(year>=2006))

N <- 1000

rm(dat,data)
#rm(table_all, table_dest,table_origin_dest,table_dest_year,dat,data)

# PART A ------------------------------------------------------------------


bounds_all_o <- da %>% group_by(export) %>% summarize(me = sum(revenue)/n())

boot_all <- matrix(nrow = N, ncol = 3)

for(i in 1:N){
  bounds_all_b <- da %>% sample_frac(1,replace=TRUE) %>% group_by(export) %>% summarize(me = sum(revenue)/n())
  boot_all[i,1] <- as.numeric(bounds_all_b[1,2])
  boot_all[i,2] <- as.numeric(bounds_all_b[2,2])
}

ci <- 0 

kl_all <- 0
ku_all <- 0

while(ci < N*0.95){
  UB_all <- (bounds_all_o[2,2] + ku_all)
  LB_all <- (bounds_all_o[1,2] - kl_all)
  for (i in 1:N){
    boot_all[i,3] <- (boot_all[i,2]<UB_all)*(boot_all[i,1]>LB_all)
  }
  kl_all <- kl_all + sd(boot_all[,1])/20
  ku_all <- ku_all + sd(boot_all[,2])/20
  ci <- sum(boot_all[,3])
}


table_all <- matrix(nrow = 1, ncol = 4)
table_all[1,1] <- as.numeric(bounds_all_o[1,2])
table_all[1,2] <- as.numeric(bounds_all_o[2,2])
table_all[1,3] <- as.numeric(LB_all)
table_all[1,4] <- as.numeric(UB_all)
table_all <- as.data.frame(table_all)
names(table_all) <- c("LB", "UB", "LB for 95% CI", "UB for 95% CI")

rm(UB_all,LB_all,kl_all,ku_all,boot_all,bounds_all_b,bounds_all_o)

print(table_all)


# PART B ------------------------------------------------------------------

bounds_dest_o <- da %>% group_by(destination, export) %>% summarize(me = sum(revenue)/n())

boot_dest <- matrix(nrow = N, ncol = 15)

for(i in 1:N){
  bounds_dest_b <- da %>% group_by(destination) %>% sample_frac(1,replace=TRUE) %>% group_by(destination, export) %>% summarize(me = sum(revenue)/n())
  for (j in 1:10){
    boot_dest[i,j] <- as.numeric(bounds_dest_b[j,3])
  }
}

LB_dest <- matrix(nrow = 5, ncol = 1)
UB_dest <- matrix(nrow = 5, ncol = 1)


for(j in 1:5){
  ci <- 0 
  ku_dest <- 0
  kl_dest <- 0
  while(ci < N*0.95){
    for (i in 1:N){
      UB_dest[j,1] <- as.numeric(bounds_dest_o[2*j,3]) + ku_dest
      LB_dest[j,1] <- as.numeric(bounds_dest_o[(2*j-1),3]) - kl_dest
      boot_dest[i,(10+j)] <- (boot_dest[i,2*j] < UB_dest[j,1])*(boot_dest[i,(2*j-1)] > LB_dest[j,1])
    }
    ku_dest <- ku_dest + sd(boot_dest[,2*j])/20
    kl_dest <- kl_dest + sd(boot_dest[,2*j-1])/20
    ci <- sum(boot_dest[,(10+j)])
  }
}
for(j in 1:5){
  LB_dest[j,1] <- as.numeric(LB_dest[j,1])
  UB_dest[j,1] <- as.numeric(UB_dest[j,1])
}

table_dest <- matrix(nrow = 5, ncol = 5)

for(j in 1:5){
  table_dest[j,1] <- j
  table_dest[j,2] <- as.numeric(bounds_dest_o[2*j-1,3])
  table_dest[j,3] <- as.numeric(bounds_dest_o[2*j,3])
  table_dest[j,4] <- LB_dest[j,1]
  table_dest[j,5] <- UB_dest[j,1]
}

table_dest <- as.data.frame(table_dest)
names(table_dest) <- c("destination", "LB", "UB", "LB for 95% CI", "UB for 95% CI")

rm(UB_dest,LB_dest,kl_dest,ku_dest,boot_dest,bounds_dest_b,bounds_dest_o)

print(table_dest)


# PART C ------------------------------------------------------------------

bounds_origin_dest_o <- da %>% group_by(origin, destination, export) %>% summarize(me = sum(revenue)/n())

bounds_origin_dest_o_own <- bounds_origin_dest_o %>% filter((origin == destination))
bounds_origin_dest_o_cross <- bounds_origin_dest_o %>% filter((origin != destination))


boot_origin_dest <- matrix(nrow = N, ncol = 75)

for(i in 1:N){
  bounds_origin_dest_b <- da %>% group_by(origin, destination, export) %>% sample_frac(1,replace=TRUE) %>% group_by(origin, destination, export) %>% summarize(me = sum(revenue)/n())
  bounds_origin_dest_b_own <- bounds_origin_dest_b %>% filter((origin == destination))
  bounds_origin_dest_b_cross <- bounds_origin_dest_b %>% filter((origin != destination))
  
  for (j in 1:40){
    boot_origin_dest[i,j] <- as.numeric(bounds_origin_dest_b_cross[j,4])
  }

  for(j in 41:45){
    boot_origin_dest[i,j] <- 0
  }
  for(j in 46:50){
    boot_origin_dest[i,j] <- as.numeric(bounds_origin_dest_b_own[j-45,4])
  }
  
}

LB_origin_dest <- matrix(nrow = 25, ncol = 1)
UB_origin_dest <- matrix(nrow = 25, ncol = 1)


for(j in 1:20){
  ci <- 0 
  ku_origin_dest <- 0
  kl_origin_dest <- 0
  while(ci < N*0.95){
    for (i in 1:N){
      UB_origin_dest[j,1] <- as.numeric(bounds_origin_dest_o_cross[2*j,4]) + ku_origin_dest
      LB_origin_dest[j,1] <- as.numeric(bounds_origin_dest_o_cross[(2*j-1),4]) - kl_origin_dest
      boot_origin_dest[i,(50+j)] <- (boot_origin_dest[i,2*j] < UB_origin_dest[j])*(boot_origin_dest[i,(2*j-1)] > LB_origin_dest[j])
    }
    ku_origin_dest <- ku_origin_dest + sd(boot_origin_dest[,2*j])/20
    kl_origin_dest <- kl_origin_dest + sd(boot_origin_dest[,2*j-1])/20
    ci <- sum(boot_origin_dest[,(50+j)])
  }
}

for(j in 1:5){
  ci <- 0 
  ku_origin_dest <- 0
  while(ci < N*0.95){
    for (i in 1:N){
      UB_origin_dest[20+j,1] <- as.numeric(bounds_origin_dest_o_own[j,4]) + ku_origin_dest
      LB_origin_dest[20+j,1] <- 0
      boot_origin_dest[i,(70+j)] <- (boot_origin_dest[i,45+j] < UB_origin_dest[20+j])
    }
    ku_origin_dest <- ku_origin_dest + sd(boot_origin_dest[,45+j])/20
    ci <- sum(boot_origin_dest[,(70+j)])
  }
}



for(j in 1:25){
  LB_origin_dest[j,1] <- as.numeric(LB_origin_dest[j,1])
  UB_origin_dest[j,1] <- as.numeric(UB_origin_dest[j,1])
}

table_origin_dest <- matrix(nrow = 25, ncol = 6)

for(j in 1:25){
  if(j<=20)
  {
    table_origin_dest[j,1] <- as.numeric(bounds_origin_dest_o_cross[2*j-1,1])
    table_origin_dest[j,2] <- as.numeric(bounds_origin_dest_o_cross[2*j-1,2])
    table_origin_dest[j,3] <- as.numeric(bounds_origin_dest_o_cross[2*j-1,4])
    table_origin_dest[j,4] <- as.numeric(bounds_origin_dest_o_cross[2*j,4])
    table_origin_dest[j,5] <- LB_origin_dest[j,1]
    table_origin_dest[j,6] <- UB_origin_dest[j,1]
  }
  else{
    table_origin_dest[j,1] <- as.numeric(bounds_origin_dest_o_own[j-20,1])
    table_origin_dest[j,2] <- as.numeric(bounds_origin_dest_o_own[j-20,2])
    table_origin_dest[j,3] <- 0
    table_origin_dest[j,4] <- as.numeric(bounds_origin_dest_o_own[j-20,4])
    table_origin_dest[j,5] <- 0
    table_origin_dest[j,6] <- UB_origin_dest[j,1]
    
  }
}

table_origin_dest <- as.tibble(table_origin_dest)
names(table_origin_dest) <- c("origin","destination", "LB", "UB", "LB for 95% CI", "UB for 95% CI")
table_origin_dest <- arrange(table_origin_dest, origin, destination)

rm(UB_origin_dest,LB_origin_dest,kl_origin_dest,ku_origin_dest,boot_origin_dest,bounds_origin_dest_b,bounds_origin_dest_b_own,bounds_origin_dest_b_cross,bounds_origin_dest_o, bounds_origin_dest_o_own,bounds_origin_dest_o_cross)
print(table_origin_dest)


# PART D ------------------------------------------------------------------



bounds_dest_year_o <- da %>% group_by(destination, y, export) %>% summarize(me = sum(revenue)/n())


boot_dest_year <- matrix(nrow = N, ncol = 30)

for(i in 1:N){
  bounds_dest_year_b <- da %>% group_by(destination, y) %>% sample_frac(1,replace=TRUE) %>% group_by(destination, y, export) %>% summarize(me = sum(revenue)/n())
  for (j in 1:20){
    boot_dest_year[i,j] <- as.numeric(bounds_dest_year_b[j,4])
  }
}

LB_dest_year <- matrix(nrow = 10, ncol = 1)
UB_dest_year <- matrix(nrow = 10, ncol = 1)


for(j in 1:10){
  ci <- 0 
  ku_dest_year <- 0
  kl_dest_year <- 0
  while(ci < N*0.95){
    for (i in 1:N){
      UB_dest_year[j,1] <- as.numeric(bounds_dest_year_o[2*j,4]) + ku_dest_year
      LB_dest_year[j,1] <- as.numeric(bounds_dest_year_o[(2*j-1),4]) - kl_dest_year
      boot_dest_year[i,(20+j)] <- (boot_dest_year[i,2*j] < UB_dest_year[j,1])*(boot_dest_year[i,(2*j-1)] > LB_dest_year[j,1])
    }
    ku_dest_year <- ku_dest_year + sd(boot_dest_year[,2*j])/20
    kl_dest_year <- kl_dest_year + sd(boot_dest_year[,2*j-1])/20
    ci <- sum(boot_dest_year[,(20+j)])
  }
}
for(j in 1:10){
  LB_dest_year[j,1] <- as.numeric(LB_dest_year[j,1])
  UB_dest_year[j,1] <- as.numeric(UB_dest_year[j,1])
}

table_dest_year <- matrix(nrow = 10, ncol = 6)

for(j in 1:10){
  table_dest_year[j,1] <- ceiling(j/2)
  table_dest_year[j,2] <- (j%%2 == 0)*1
  table_dest_year[j,3] <- as.numeric(bounds_dest_year_o[2*j-1,4])
  table_dest_year[j,4] <- as.numeric(bounds_dest_year_o[2*j,4])
  table_dest_year[j,5] <- LB_dest_year[j,1]
  table_dest_year[j,6] <- UB_dest_year[j,1]
}

table_dest_year <- as.data.frame(table_dest_year)
names(table_dest_year) <- c("destination", "period", "LB", "UB", "LB for 95% CI", "UB for 95% CI")
rm(UB_dest_year,LB_dest_year,kl_dest_year,ku_dest_year,ci,boot_dest_year,bounds_dest_year_b,bounds_dest_year_o)

print(table_dest_year)





