library(arules)
library(arulesViz)
library(rmarkdown)
book <- read.csv(file.choose())
View(book)
summary(book)
str(book)
barplot(sapply(book,sum),col=1:10)
##############1#################
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=8))
inspect(rules)
inspect(head(sort(rules, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

##############2#################
# Applying apriori algorithm to get relevant rules
rules1 <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=4))
inspect(rules1)
inspect(head(sort(rules1, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules1,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules1,by="lift")
inspect(rules_lift)

plot(rules1,method = "scatterplot")
plot(rules1,method = "grouped")
plot(rules1,method = "graph")
plot(rules1,method = "mosaic")

##############3#################
# Applying apriori algorithm to get relevant rules
rules2 <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=10))
inspect(rules2)
inspect(head(sort(rules2, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules2,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules2,by="lift")
inspect(rules_lift)

plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")
plot(rules2,method = "mosaic")

##############4#################
# Applying apriori algorithm to get relevant rules
rules3 <- apriori(as.matrix(book),parameter = list(support=0.003,confidence=0.05,minlen=3))
inspect(rules3)
inspect(head(sort(rules3, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules3,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules3,by="lift")
inspect(rules_lift)

plot(rules3,method = "scatterplot")
plot(rules3,method = "grouped")
plot(rules3,method = "graph")
plot(rules3,method = "mosaic")

##############5#################
# Applying apriori algorithm to get relevant rules
rules4 <- apriori(as.matrix(book),parameter = list(support=0.003,confidence=0.05,minlen=5))
inspect(rules4)
inspect(head(sort(rules4, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules4,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules4,by="lift")
inspect(rules_lift)

plot(rules4,method = "scatterplot")
plot(rules4,method = "grouped")
plot(rules4,method = "graph")
plot(rules4,method = "mosaic")

##############6#################
# Applying apriori algorithm to get relevant rules
rules5 <- apriori(as.matrix(book),parameter = list(support=0.004,confidence=0.06,minlen=8))
inspect(rules5)
inspect(head(sort(rules5, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules5,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules5,by="lift")
inspect(rules_lift)

plot(rules5,method = "scatterplot")
plot(rules5,method = "grouped")
plot(rules5,method = "graph")
plot(rules5,method = "mosaic")

##############7#################
# Applying apriori algorithm to get relevant rules
rules6 <- apriori(as.matrix(book),parameter = list(support=0.004,confidence=0.05,minlen=7))
inspect(rules6)
inspect(head(sort(rules6, by='lift')))

# Sorting rules by confidence 
rules_conf <- sort(rules6,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules6,by="lift")
inspect(rules_lift)

plot(rules6,method = "scatterplot")
plot(rules6,method = "grouped")
plot(rules6,method = "graph")
plot(rules6,method = "mosaic")
