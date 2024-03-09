# 0_read_qualtrics_api.R
# read the latest survey data from Qualtrics using the API
# March 2024
library(tidyr)
library(qualtRics)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
source('99_functions.R')

### Part 1: labels ###
# get the variable names
names = read_excel('data/qualtrics_labels.xlsx', col_names = FALSE, n_max = 1) %>% clean_names()
names = janitor::make_clean_names(names)  
# ... then get the labels
in_labels = read_excel('data/qualtrics_labels.xlsx', col_names = FALSE, skip=1, n_max = 1)
labs = as.character(matrix(in_labels))
# make a frame of labels
labels = bind_cols(names=names, labels=labs) %>%
  filter(!str_detect(names, 'start_date'),
         !str_detect(names, 'end_date'),
         !str_detect(names, 'response_id'),
         !str_detect(names, 'user_language'),
         !str_detect(names, 'recipient'),
         !str_detect(names, 'policy'),
         !str_detect(names, 'sentiment')) 


### Part 2: data ###
## set up API access
source('0_my_key.R')
set_up = function(){ # no longer needed?
  qualtrics_api_credentials(api_key = key, 
                            base_url = base_url,
                            overwrite = TRUE,
                            install = TRUE)
  readRenviron("~/.Renviron") # reload your environment 
}

## list all available surveys
surveys = all_surveys() %>%
  filter(str_detect(name, pattern='^Uncertainty')) # just the three uncertainty surveys

## now get uncertainty surveys
data = NULL
for (k in 1:nrow(surveys)){ # BMJ Open, F1000, Epidemiology
  mysurvey = fetch_survey(surveyID = surveys$id[k], 
                          label = TRUE, # use text for surveys
                          add_column_map = FALSE,
                          verbose = TRUE)
  journal = case_when( # separate survey for each journal
    surveys$name[k] == 'Uncertainty' ~ 'F1000',
    surveys$name[k] == 'Uncertainty Epidemiology' ~ 'Epidemiology',
    surveys$name[k] == 'Uncertainty BMJ Open' ~ 'BMJ Open')
    
  mysurvey = clean_names(mysurvey) %>%
    select(-q_data_policy_violations) %>%
    mutate(journal = journal,
           q1_3 = as.character(q1_3), # for combining, turn into character (decision)
           q1_3 = ifelse(q1_3 =='Cannot remember', '', q1_3), # one minor edit
           q2_3 = as.character(q2_3), # gender
           q2_4 = as.character(q2_4), # experience
           q2_2 = as.character(q2_2),
           q2_2 = str_squish(q2_2)) #  remove double spaces
  data = bind_rows(data, mysurvey)
}

## removals ##
# remove people with zero progress ... 

# smallsets snap 88 data caption[Remove rows where the respondent gave no information.]caption
cat('There were ', nrow(filter(data, progress==0)),' respondents with a zero progress.\n', sep='')
data = filter(data, progress > 0)
# ... and who did not answer any questions
comment_questions = c('q1_5') # 
selected_questions = select(data, 'response_id', starts_with('q')) %>% # 
  select(-all_of(comment_questions)) %>% # remove comments
  mutate_all(as.character) %>%
  pivot_longer(cols = -response_id) %>%
  filter(!str_detect(name, pattern='q1_1_')) %>% # remove remove q1_1_[x] questions
  mutate(missing = is.na(value)) 
# overall missing
all_missing = group_by(selected_questions, response_id) %>%
  summarise(n=n(), miss = sum(missing)) %>%
  filter(miss == n)
cat('There were ', nrow(all_missing),' respondents who completed nothing\n', sep='')
data = filter(data, !response_id %in% all_missing$response_id)

## data edits

# smallsets snap 112 data caption[Convert survey duration to minutes; edit hours spent on review.]caption
experience_levels = c("Less than 5 years", "6 to 10 years", "11 to 15 years", "16 to 20 years", "21 years or more")
data = mutate(data, 
              duration_mins = duration_in_seconds/ 60, # use duration in minutes rather than seconds
              recorded_date_time = recorded_date,
              recorded_date = as.Date(recorded_date), # simplify
              q2_4 = factor(q2_4, levels=experience_levels), # make as a factor for ordering
              q1_4 = str_squish(q1_4)) # remove unwanted spaces
# edits to hours spent on review
data = mutate(data,
              is.minutes = str_detect(q2_2, 'min|minute'),
              q2_2 = q2_edits(q2_2), # q2_2 is in hours
              temp = q2_2, # for spotting missing edits
              q2_2 = as.numeric(str_remove(q2_2, pattern='hours|hrs')), # remove hours only from q2_2; anything else needs to be edited
              q2_2 = ifelse(is.minutes, q2_2 / 60, q2_2)) %>%  # adjust if given in minutes
  select(-user_language, -distribution_channel, 
         -start_date, -end_date, # just use recorded_date
         -duration_in_seconds, # use minutes instead
         -is.minutes # no longer needed
         ) 
# quick check for hours variable
filter(data, is.na(q2_2)) %>% select(temp) %>% filter(!is.na(temp))

## fix two long country labels
data = mutate(data,
              q2_5 = ifelse(q2_5 == 'United States of America', 'USA', q2_5),
              q2_5 = ifelse(q2_5 == 'United Kingdom of Great Britain and Northern Ireland', 'UK', q2_5))

## tidy up "cannot answer" question
# "If you cannot answer the question then please click this radio button"
data = mutate(data, 
              q1_2 = as.character(q1_2),
              q1_2 = ifelse(!is.na(q1_2), 'Ticked', "Not ticked")) %>%
  rename('cannot_answer' = 'q1_2')

## few more edits and drops
data = select(data, -temp, -status, -response_id) %>% # no longer needed
  arrange(recorded_date_time) %>% # sort from old to new
  mutate(id = 1:n()) %>% # make ID variable
  select(id, everything())

## tidy up F1000 three cut-and-paste titles

# smallsets snap 144 data caption[Make edits to titles from F1000.]caption
index = str_detect(data$q1_4, " ▬ ")
index[is.na(index)] = FALSE
for (i in which(index)){
  title = data$q1_4[i] # original title
  where = str_locate(title, " ▬ ")
  title = str_sub(title, where[2]+1, nchar(title)) # cut title from break
  data$q1_4[i] = title # replace
}

### save ###
save(data, labels, file='data/0_Reviewers_Responses.RData')
cat('Number of data rows = ', nrow(data), '.\n', sep='')

