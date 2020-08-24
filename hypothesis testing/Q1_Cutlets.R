cutlets <- read.csv(choose.files())
View(cutlets)
attach(cutlets)
#############Normality test###############

shapiro.test(Unit.A)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(Unit.A,Unit.B)#variance test
# p-value = 0.31 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test

# null Hypothesis -> there is no significant difference in either unit
# Alternate Hypothesis -> there is a significant difference in 1 unit
# p-value = 0.47 > 0.05 accept null Hypothesis/failed to reject null hypothesis 




