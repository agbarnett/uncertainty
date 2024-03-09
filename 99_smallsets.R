# 99_smallsets.R
# Code to run smallsets data processing explanation
# March 2024
library(smallsets)
TeachingDemos::char2seed('cambridge')

Smallset_Timeline(data = data,
#                  rowCount = 5,
                  rowNums = 1:5,
                  code = "0_read_qualtrics_api.R")
