library(tidyverse)
library(haven)

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
N <- 100

# Estimation with original dataset ----------------------------------------

bounds_dest_year_o <- da %>% group_by(destination, y, export) %>% summarize(me = sum(revenue)/n())

# Estimation with Bootstrapped Dataset ------------------------------------

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
      boot_dest_year[i,(10+j)] <- (boot_dest_year[i,2*j] < UB_dest_year[j,1])*(boot_dest_year[i,(2*j-1)] > LB_dest_year[j,1])
    }
    ku_dest_year <- ku_dest_year + sd(boot_dest_year[,2*j])/20
    kl_dest_year <- kl_dest_year + sd(boot_dest_year[,2*j-1])/20
    ci <- sum(boot_dest_year[,(10+j)])
  }
}
for(j in 1:10){
  LB_dest_year[j,1] <- as.numeric(LB_dest_year[j,1])
  UB_dest_year[j,1] <- as.numeric(UB_dest_year[j,1])
}

table_dest_year <- matrix(nrow = 5, ncol = 6)

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
table_dest_year

