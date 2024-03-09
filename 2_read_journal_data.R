# 2_read_journal_data.R
# read in the manually entered data from the journals; just need decision, version number and dates
# Excel sheets were created by 1_export_titles_data_entry.R
# December 2023
library(readxl)
library(dplyr)
library(stringr)

## had to read each journal separately because of date issues
## had to change from Excel to text because of dates; removed '#' from text files
jdata = NULL
#
j = 'BMJ Open'
filename = paste('data/completed_versions/data_entry_', str_replace_all(j, ' ', '_'), '.txt', sep='')
raw_bmj_open = read.table(filename, sep='\t', header=TRUE, fill=TRUE, quote='') %>%
    mutate(journal = j,
           review_date = as.Date(review_date, '%d-%m-%Y'),
           date_paper_submitted = as.Date(date_paper_submitted, '%d-%m-%Y')) %>%
    select(-starts_with('x')) %>% # remove empty columns
  filter(id != '') # remove empty rows
#
j = 'Epidemiology'
filename = paste('data/completed_versions/data_entry_', str_replace_all(j, ' ', '_'), '.txt', sep='')
raw_epidemiology = read.table(filename, sep='\t', header=TRUE, fill=TRUE, quote='') %>%
  mutate(journal = j,
         review_date = as.Date(review_date, '%m-%d-%Y'), # US format
         date_paper_submitted = as.Date(date_paper_submitted, '%d-%b-%y')) %>%
  select(-starts_with('x')) %>% # remove empty columns
  filter(id != '') # remove empty rows
#
j = 'F1000'
filename = paste('data/completed_versions/data_entry_', str_replace_all(j, ' ', '_'), '.txt', sep='')
raw_f1000 = read.table(filename, sep='\t', header=TRUE, fill=TRUE, quote='') %>%
  mutate(journal = j,
         review_date = case_when(
           reviewer == 0 ~ survey_date, # for unmatched reviews, assume these were reviews that were not posted
           reviewer == 1 ~ date_reviewer_1,
           reviewer == 2 ~ date_reviewer_2,
           reviewer == 3 ~ date_reviewer_3,
           reviewer == 4 ~ date_reviewer_4
         ),
         decision = case_when(
           reviewer == 0 ~ '', # for unmatched reviews, assume these were reviews that were not posted
           reviewer == 1 ~ decision_reviewer_1 ,
           reviewer == 2 ~ decision_reviewer_2 ,
           reviewer == 3 ~ decision_reviewer_3 ,
           reviewer == 4 ~ decision_reviewer_4 
         ),
         review_date = as.Date(review_date, '%d-%b-%y'),
         date_paper_submitted = as.Date(date_paper_submitted, '%d-%b-%y')) %>%
  select(-starts_with('x'), -ends_with('_1'), -ends_with('_2'), -ends_with('_3'), -ends_with('_4')) %>%
  filter(id != '')
# concatenate
jdata = bind_rows(raw_f1000, raw_bmj_open, raw_epidemiology) %>%
  mutate( # tidy up decision
  decision = case_when(
    str_detect(decision, "Reject, ") ~ 'Reject', # this paper was a resubmission and so it had been reviewed previously)
    str_detect(decision, "Major revision, ") ~ 'Major revision', # this paper was a resubmission and so it had been reviewed previously)
    decision == 'accept' ~ 'Accept',
    decision == 'reject' ~ 'Reject',
    decision == 'revise' ~ 'Revise',
    decision == 'withdrawn ' ~ 'Withdrawn',
    TRUE ~ as.character(decision) # otherwise
  ),
  time_diff = as.numeric(review_date - date_paper_submitted) # create date difference
)

# quick check of difference, should be no negatives
hist(jdata$time_diff)
filter(jdata, time_diff<0) %>% select(id, journal, review_date, date_paper_submitted)

## merge with survey data using `id`
jdata = select(jdata, id, version_number, decision, time_diff, paper_type, date_paper_submitted) %>% # only keep essential variables
  mutate(paper_type = str_to_sentence(paper_type), # use sentence case for consistency
         paper_type = str_squish(paper_type),
         paper_type = ifelse(paper_type=='', NA, paper_type),
         paper_type = ifelse(paper_type=='Study protocol', 'Protocol', paper_type), # combine
         paper_type = ifelse(paper_type=='Research article', 'Original research', paper_type) # combine
  )
#
load('data/0_Reviewers_Responses.RData') # from 0_read_qualtrics_api.R
data = full_join(data, jdata, by='id') %>%
  mutate(q1_3 = ifelse(q1_3=='', NA, q1_3)) # minor fix
# what is missing? 
missed = anti_join(data, jdata, by='id')
nrow(missed) # 31 papers where the reviewers did not give the title, so could not be matched

# small number of filling-in missing recommendation data based on answers to uncertainty question
data = mutate(data, 
              q1_3 = ifelse(id == 1 , 'Approved', q1_3), # f1000
              q1_3 = ifelse(id == 2 , 'Approved', q1_3), # f1000
              q1_3 = ifelse(id == 204 , 'Accept', q1_3), # bmj open
              q1_3 = ifelse(id == 385 , 'Major revisions', q1_3)) # bmj open
  
# save
save(data, file='data/2_data.RData')
