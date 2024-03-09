# 1_export_titles_data_entry.R
# export the titles of the papers so that the journals can provide the required data (e.g., date submitted)
# June 2023
library(stringr)
library(textclean) # for replace_non_ascii
library(openxlsx)
library(dplyr)
options(openxlsx.dateFormat = "dd/mm/yyyy")

# get data and slim down to necessary variables
load('data/0_Reviewers_Responses.RData') # from 0_read_qualtrics_api.R
for_export = select(data, id, journal, q1_4, recorded_date_time) %>%
  mutate(recorded_date_time = as.Date(recorded_date_time)) %>%
  rename('title' = 'q1_4',
         'review_date' = 'recorded_date_time') %>%
  filter(title != '', # cannot match if they did not provide title
         nchar(title) > 10) %>% # exclude at least one title (just said "What?")
  mutate(date_paper_submitted = '', # variables we need
         title = replace_non_ascii(title), # to avoid errors in Excel
         paper_type = '',
         version_number = '', # 1st, 2nd (1st resubmission), etc.
         decision = factor(NA, levels=1:3, labels=c('Accepted','Rejected','Ongoing'))) # accept/reject/ongoing
#for_export' ', )

# export to Excel by journal
journals = unique(for_export$journal)
for (j in journals){
  filename = paste('data/data_entry_', str_replace_all(j, ' ', '_'), '.xlsx', sep='')
  wb = createWorkbook(creator='Adrian Barnett')
  addWorksheet(wb, sheetName = "papers")
  to_export =  filter(for_export, journal == j) %>%
    arrange(review_date) %>%
    select(-journal)
  freezePane(wb, sheet = 1, firstRow = TRUE) ## freeze first column
  writeDataTable(wb, sheet = 1, x = to_export,
               colNames = TRUE, rowNames = FALSE)
  setColWidths(wb, sheet = 1, cols = c(1,2), widths = c(4, 70))
  saveWorkbook(wb, filename, overwrite = TRUE)
}

# had to create factors in Excel, see https://support.microsoft.com/en-us/office/video-create-and-manage-drop-down-lists-28db87b6-725f-49d7-9b29-ab4bc56cefc2
