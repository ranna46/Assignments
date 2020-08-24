COF <- read.csv(choose.files())
View(COF)
attach(COF)

India<-ifelse(COF$India=="Defective",1,0)
Phillippines<-ifelse(COF$Phillippines=="Defective",1,0)
Malta<-ifelse(COF$Malta=="Defective",1,0)
Indonesia<-ifelse(COF$Indonesia=="Defective",1,0)

countries <- cbind.data.frame(Phillippines,Indonesia,Malta,India)
View(countries)

stacked_data <- stack(countries)
View(stacked_data)
colnames(stacked_data) <- c("Defective", "countries")
attach(stacked_data)
class(stacked_data)

write.csv(write.csv(stacked_data,file="COF_stacked.csv",col.names = F,row.names = F))
getwd()

COF1<-read.csv(choose.files())
View(COF1)
attach(COF1)
table(countries,Defective)
chisq.test(table(countries,Defective))
# p-value = 0.2771 > 0.05, failed to reject null hypothesis
# H0 = there is no defective%
# Ha = there is a defective % which varies by centre
# Hence, there is no defective% which varies by centre

