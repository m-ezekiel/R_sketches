# 2016 Democratic National Committee - Party Platform
# Created: 2017-03-07

source("Functions/wordVector_fxn.R")
DNC_platform <- wordVector_fxn("Data/2016_DNC_platform.txt", split = " ", removePunct = TRUE)
sort(table(DNC_platform))
unique(DNC_platform)
