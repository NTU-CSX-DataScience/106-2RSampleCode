rm(list=ls())
load("titanic.raw.rdata")

str(titanic.raw)
require(arules)

rule <- apriori(titanic.raw, 
  # min support & confidence, 最小規則長度(lhs+rhs)
  parameter=list(minlen=3, supp=0.1, conf=0.7),  
  appearance = list(default="lhs",
                    rhs=c("Survived=No", "Survived=Yes") 
                    # 右手邊顯示的特徵
  )
)  

inspect(rule)
sort.rule <- sort(rule, by="lift")
inspect(sort.rule)

subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
sort.rule <- sort.rule[!redundant]
inspect(sort.rule)

require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")
