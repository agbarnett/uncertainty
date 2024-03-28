# 99_data_to_share.R
# make a shareable version of the data
# March 2024
library(dplyr)
library(openxlsx)

# get the labels
load('data/0_Reviewers_Responses.RData')

# get the analysis data
load('data/2_data.RData')
# remove identifying information
data = select(data, -q1_4, # paper title
              -q1_5) %>% # comments as can't be sure of identified
  mutate(recorded_date = as.Date(recorded_date),
         date_paper_submitted = as.Date(date_paper_submitted))
# save R version
save(data, labels, file = 'rdata/2_data.RData')

# export to Excel
filename = 'rdata/2_data.xlsx'
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "data")
addWorksheet(wb, sheetName = "labels")
freezePane(wb, sheet = 1, firstRow = TRUE) ## freeze first column
writeDataTable(wb, sheet = 1, x = data,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = 2, x = labels,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)

