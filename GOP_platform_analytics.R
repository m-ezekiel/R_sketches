# 2016 Republican National Committee - Party Platform
# Created: 2017-03-07

source("Functions/wordVector_fxn.R")
GOP_platform <- wordVector_fxn("Data/2016_GOP_platform.txt", split = " ", removePunct = TRUE)
sort(table(GOP_platform))
unique(GOP_platform)
