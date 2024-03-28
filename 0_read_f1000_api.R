# 0_read_f1000_api.R
# read the F1000 data using the API
# historical data -- no longer used 
# Sep 2021
library(dplyr)
library(xml2)
library(rvest)
library(purrr)
library(stringr)

# links to the XML of the entire corpus
url1 = 'https://f1000research.com/published-xml-urls'
url2 = 'https://wellcomeopenresearch.org/published-xml-urls'
full_list1 = read_html(url1) %>% html_table()
full_list1 = mutate(full_list1[[1]], journal = 'F1000Research')
names(full_list1)[1:2] = c('link','title')
full_list2 = read_html(url2) %>% html_table()
full_list2 = mutate(full_list2[[1]], journal = 'Wellcome Open Research')
names(full_list2)[1:2] = c('link','title')
full_list = bind_rows(full_list1, full_list2)

# get most recent version for each paper
full_list = mutate(full_list,
       # extract id and version
       id = str_split(link, '/') %>% map_chr(., 5),
       version = str_split(link, '/') %>% map_chr(., 6),
       version = as.numeric(str_remove(version, pattern='v'))) %>%
  group_by(journal, id) %>%
  arrange(journal, id, desc(version)) %>% # earliest first
  slice(1) %>%
  ungroup()

# loop through papers
N = nrow(full_list)
all_papers = all_reviews = errors = NULL
for (k in 1:N){
  
  ## data for single paper
  # meta-data about the paper
  paper = tryCatch(read_html(full_list$link[k]),
                   error = function(e) print(paste('Did not work', k)))
  if(class(paper) == 'character'){
    errors = c(errors, full_list$link[k]) # for Elle
    next # skip if there's an error
  } 
                   
  doi = paper %>% xml_nodes('article-meta') %>% xml_nodes('article-id') %>% xml_text()
  title = paper %>% xml_nodes('article-meta') %>% xml_nodes('article-title') %>% xml_text()
  dates = paper %>% xml_find_all("//article-meta//pub-date[@pub-type='epub']") 
  day = dates %>% xml_nodes('day') %>% xml_text()
  month = dates %>% xml_nodes('month') %>% xml_text()
  year = dates %>% xml_nodes('year') %>% xml_text()
  date = as.Date(paste(c(year,month,day), collapse='-'))
  
  ## review data
  reviews = paper %>% xml_find_all("//sub-article[@article-type='ref-report']") 
  # remove responses to reviews
  xml_find_all(reviews, ".//sub-article[@article-type='response']") %>% xml_remove()
  # remove reference list as this contains dates
  xml_find_all(reviews, ".//ref-list") %>% xml_remove()
  n_reviews = length(reviews)
  
  ## store papers
  pframe = data.frame(num = k,
                      journal = full_list$journal[k],  
                      title = title[1], # in case of multiple titles
                      doi = doi,
                      date = date,
                      n_reviews = n_reviews)
  all_papers = bind_rows(all_papers, pframe)
  
  if(n_reviews == 0){next} # skip to next paper if there are no reviews
  for (i in 1:n_reviews){ # loop through reviewers
    this_review = reviews[i] 
    # dates
    day = this_review %>% xml_nodes('day') %>% xml_text()
    month = this_review %>% xml_nodes('month') %>% xml_text()
    year = this_review %>% xml_nodes('year') %>% xml_text()
    rdate = as.Date(paste(c(year,month,day), collapse='-'))
    # review version
    version = this_review %>% xml_nodes('article-title') %>% xml_text()
    version = str_remove(version, pattern='Reviewer response for version ')
    # name
    first = this_review %>% xml_nodes('given-names') %>% xml_text()
    surname = this_review %>% xml_nodes('surname') %>% xml_text()
    name = paste(first[1], surname[1]) # just take first if there are co-referees
    # orcid
    orcid = this_review %>% xml_nodes('uri') %>% xml_text()
    if(length(orcid) == 0){
      orcid = ''
    }
    # recommendation
    recommendation = this_review %>% xml_nodes('meta-value') %>% xml_text()
    # get length of review?
    
    # store reviews
    rframe = data.frame(num = k,
                        date = rdate,
                        version = version,
                        name = name, 
                        orcid = orcid[1],
                        recommendation = recommendation)
    all_reviews = bind_rows(all_reviews, rframe)
    
  }
  
  if(k%%100 == 0){cat('Up to ', k, '.\r', sep='')}
  
}

# save
today = as.Date(Sys.Date())
save(today, all_reviews, all_papers, file='data/historic.RData')


# to do
# - remove special chars from title
# - get affiliation data for reviewers, country, etc
# - get word counts of paper and reviews