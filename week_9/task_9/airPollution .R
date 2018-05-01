rm(list = ls(all=TRUE))
library(dplyr)
rawdata = read.csv(
  'AQXDaily_20170409211519.csv',
  encoding = 'UTF-8')

df <- rawdata %>% group_by(X.U.FEFF.SiteId)
result1 = summarise(df, mean(SO2SubIndex), mean(COSubIndex))
result2 = rawdata %>% filter(PSI < 20)

