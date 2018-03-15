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

bounds_o <- da %>% group_by(export) %>% summarize(me = sum(revenue)/n())
bounds_dest <- da %>% group_by(destination, export) %>% summarize(me = sum(revenue)/n())
bounds_origin_dest <- da %>% group_by(origin, destination, export) %>% summarize(me = sum(revenue)/n())
bounds_dest_year <- da %>% group_by(destination, y, export) %>% summarize(me = sum(revenue)/n())

N <- 1000
boot_o <- matrix(nrow = N, ncol = 3)

for(i in 1:N){
  bounds_o <- da %>% sample_frac(1,replace=TRUE) %>% group_by(export) %>% summarize(me = sum(revenue)/n())
  boot_o[i,1] <- as.numeric(bounds_o[1,2])
  boot_o[i,2] <- as.numeric(bounds_o[2,2])
  }
ci <- 0 
k <- 0
while(ci < N*0.95){
  for (i in 1:N){
    boot_o[i,3] <- (boot_o[i,2]<(bounds[2,2] + k))*(boot[i,1]>(bounds[1,2]-k))
  }
  k <- k + 10000
  ci <- sum(boot[,3])
}

ku_o <- ku + sd(boot_dest[,2])/20
kl_o <- ku + sd(boot_dest[,2])/20

  LB <- as.numeric(bounds[1,2])
UB <- as.numeric(bounds[2,2] + (k-10000))
cat("95% CI is [", LB, ",",UB, "]")


table_o <- matrix(nrow = 1, ncol = 4)
table_o[1,1] <- as.numeric(bounds[1,2])
table_o[1,2] <- as.numeric(bounds[2,2])
table_o[1,3] <- LB_dest[j]
table_o[1,4] <- UB_dest[j]


table_o <- as.data.frame(table_o)
names(table_o) <- c("LB", "UB", "LB for 95% CI", "UB for 95% CI")





boot_dest <- matrix(nrow = N, ncol = 15)

for(i in 1:N){
  bounds_dest <- da %>% group_by(destination) %>% sample_frac(1,replace=TRUE) %>% group_by(destination, export) %>% summarize(me = sum(revenue)/n())
  for (j in 1:10){
  boot_dest[i,j] <- as.numeric(bounds_dest[j,3])
  }
}

LB_dest <- matrix(nrow = 5, ncol = 1)
UB_dest <- matrix(nrow = 5, ncol = 1)

LB_d <- matrix(nrow = 5, ncol = 1)
UB_d <- matrix(nrow = 5, ncol = 1)

for(j in 1:5){
  ci <- 0 
  ku <- 0
  kl <- 0
  while(ci < N*0.95){
    for (i in 1:N){
      #if(bounds_dest[(2*j-1),3]<k){
      #  UB_d[j] <- bounds_dest[2*j,3] + k
      #  LB_d[j] <- 0
      #  boot_dest[i,(10+j)] <- (boot_dest[i,2*j]<UB_d[j])
      #}
      #else{
        UB_d[j] <- bounds_dest[2*j,3] + ku
        LB_d[j] <- bounds_dest[(2*j-1),3] - kl
        boot_dest[i,(10+j)] <- (boot_dest[i,2*j] < UB_d[j])*(boot_dest[i,(2*j-1)] > LB_d[j])
      #}
    }
    ku <- ku + sd(boot_dest[,2*j])/20
    kl <- kl + sd(boot_dest[,2*j-1])/20
    ci <- sum(boot_dest[,(10+j)])
  }
  LB_dest[j] <- as.numeric(LB_d[j])
  UB_dest[j] <- as.numeric(UB_d[j])
  cat("95% CI is [", LB_dest[j], ",",UB_dest[j], "]", "\n")
}

for (j in 1:5){
  cat("Point estimates for destination country", j, "are [", as.numeric(bounds_dest[2*j-1,3]), ",", as.numeric(bounds_dest[2*j,3]), "]\n")
  cat("95% CI for destination country", j,  "is [", LB_dest[j], ",",UB_dest[j], "]", "\n")
}
cat("Country", "\t", "Point estimates for destination country", "\t", "95% CI for destination country", "\n")
for (j in 1:5){
  cat(j, "\t [", as.numeric(bounds_dest[2*j-1,3]), ",", as.numeric(bounds_dest[2*j,3]), "]\t\t", "[", LB_dest[j], ",",UB_dest[j], "]", "\n")
}

table_dest <- matrix(nrow = 5, ncol = 5)
for(j in 1:5){
  table_dest[j,1] <- j
  table_dest[j,2] <- as.numeric(bounds_dest[2*j-1,3])
  table_dest[j,3] <- as.numeric(bounds_dest[2*j,3])
  table_dest[j,4] <- LB_dest[j]
  table_dest[j,5] <- UB_dest[j]
}

table_dest <- as.data.frame(table_dest)
names(table_dest) <- c("country", "LB", "UB", "LB for 95% CI", "UB for 95% CI")
table_dest




# origin_dest -------------------------------------------------------------

tab_origin_dest <- bounds_origin_dest %>% filter((origin != destination))
tab_origin_dest_own <- bounds_origin_dest %>% filter((origin == destination))

boot_dest <- matrix(nrow = N, ncol = 60)


