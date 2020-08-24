buyer_ratio <- read.csv(choose.files())
View(buyer_ratio)
attach(buyer_ratio)
library(dplyr)
BR1<-select(buyer_ratio,-1)
View(BR1)
row.names(BR1) <- c("male","female")
#Apply chisq test
chisq.test(BR1) 
# p-value = 0.6603 > 0.05, failed to reject null hypothesis
# Hence, all male-female buyer ratios are equal across the regions