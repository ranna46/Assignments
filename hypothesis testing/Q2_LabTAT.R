reports <- read.csv(choose.files())
View(reports)
attach(reports)

Stacked_Data <- stack(reports)
View(Stacked_Data)
attach(Stacked_Data)

#############Normality test###############
library(nortest)
ad.test(Stacked_Data$values) 
# p-value=0.05072 > 0.05, data is normal
############# Variance test ###############
library(car)
leveneTest(Stacked_Data$values~Stacked_Data$ind, data = Stacked_Data)   #Test for equal Variance
# p-value=0.05161 > 0.05, data has equal variance

################ One-way Anova ########
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# H0 = there is no difference in avg.TAT in any of the 4 reports
# Ha = there is a difference in avg.TAT in any one of the 4 reports

# p-value = 0.2e-16 < 0.05 reject null hypothesis 
#  There is a difference in avg.TAT in any one of the 4 reports
