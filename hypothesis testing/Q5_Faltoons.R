faltoons <- read.csv(choose.files())
View(faltoons)
attach(faltoons)
table1 <- table(Weekend,Weekdays)
table1
prop.test(x=c(120,47),n=c(287,113),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# Ho <- there is no difference in percentage of males vs females walking in to the store
# Ho <- there is a difference in percentage of males vs females walking in to the store
# p-value = 0.96 > 0.05, falied to reject Ho
# hence, there is no difference in percentage of males vs females walking in to the store