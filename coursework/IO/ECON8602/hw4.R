library("tidyverse")
library("haven")

data <- read_dta("stata_rust_data.dta")
names(data) <- c("model","group", "year", "month", "i","xt", "xt1", "odometer", "xtmxt1")


data$bxt <- as.numeric(cut(data$xt, seq(-0.0001,450000,5000), labels = 1:90,include.lowest = TRUE))
data$bxt1 <- as.numeric(cut(data$xt1, seq(-0.001,450000,5000), labels = 1:90,include.lowest = TRUE))

data <- data %>% mutate(xtchange = (i==0)*(bxt1-bxt) + (i==1)*(bxt1-1))
data <- data %>% mutate(frag = (xtchange<=2)*xtchange + (xtchange >2)*2)
probs <- data %>% group_by(frag) %>% summarize(m = n()/nrow(data))