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

N <- 1000


# Estimation with original dataset ----------------------------------------

bounds_all_o <- da %>% group_by(export) %>% summarize(me = sum(revenue)/n())


# Estimation with Bootstrapped Dataset ------------------------------------

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




