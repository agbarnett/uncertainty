# 0_read_f1000_papers_readability.R
# get the reviewed papers from F1000 and calculate the readability scores
# March 2024
library(quanteda.textstats) # for readability scores
library(dplyr)
library(xml2)
library(readxl)
library(stringr)
# for unicode characters
load('data/unicode_characters.rda') # from PLOS study
unicode_spaces = unicode_lookup %>% filter(grepl("U\\+20(\\w+)",unicode)) %>% pull(unicode) 
unicode_spaces = gsub("U\\+(\\w+)", "\\U\\\\+\\1",unicode_spaces) %>% str_c(.,collapse='|')

# get the F1000 papers included in the study
f1000 = read_excel('data/completed_versions/data_entry_F1000.xlsx')
N = nrow(f1000)

# loop through papers
reading_data = NULL
for (k in 1:N){
  # make the xml link and download the file as an XML; link depends on paper version
  has_version = str_detect(f1000$link[k], 'v[1-9]$') # does the link already have the version?
  if(has_version == FALSE){xml_link = paste(f1000$link[k], '/v', f1000$version_number[k], '/xml', sep='')}
  if(has_version == TRUE){xml_link = paste(f1000$link[k], '/xml', sep='')}
  destfile = 'data/f1000.xml'
  download.file(xml_link, destfile = destfile)
  
  # read in the xml
  paper = read_xml(destfile, encoding='UTF-8')
  children = xml_children(paper)
  
  # remove everything bar the main text
  xml_remove(xml_find_all(paper, "front")) # abstract, etc
  xml_remove(xml_find_all(paper, "back")) # references
  xml_remove(xml_find_all(paper, "sub-article")) # comments
  xml_remove(xml_find_all(paper, "//title")) # subtitles
  xml_remove(xml_find_all(paper, "//table")) # tables
  xml_remove(xml_find_all(paper, "//fig")) # figures
  xml_remove(xml_find_all(paper, "//xref")) # references
  xml_remove(xml_find_all(paper, "//ext-link")) # hyper links
  xml_remove(xml_find_all(paper, "//inline-formula")) # math formulas
  xml_remove(xml_find_all(paper, "//disp-formula")) # math formulas
  # convert to text
  text_nodes <- xml_find_all(paper, "//p")
  text = paste(xml_text(text_nodes), sep=' ', collapse = " ") # keep spaces between paragraphs
  text = str_replace_all(text, '\\n', ' ')
  # replace/standardise unicode symbols
  text = str_replace_all(text, pattern = unicode_spaces, replacement=" ")
  # remove things that look like full-stops
  text = str_replace_all(text, 'e\\.g\\.', 'eg') 
  text = str_replace_all(text, 'i\\.e\\.', 'ie')
  text = str_replace_all(text, 'et.al\\.', 'et al')
  text = str_replace_all(text, '\\. , , ,|\\. , ,|\\. ,', '.') # remnants from references
  text = str_remove_all(text, '\\(.\\)')
  text = str_squish(text) # remove extra spaces
  text = str_remove_all(text, '\\(.\\)')
  
  # remove Extended data and Data availability
  
  # calculate the scores
  score = textstat_readability(text, c("Flesch","Dale.Chall",'meanSentenceLength'))
  n_words = str_count(text, '\\w+') # word count
  frame = data.frame(score) %>%
    mutate(n_words = n_words,
           id = f1000$id[k]) # for merging
  
  # concatenate
  reading_data = bind_rows(reading_data, frame)
  file.remove(destfile) # tidy up 
  
} # end of loop

# save
library(janitor)
reading_data = dplyr::select(reading_data, -document) %>%
  clean_names()
save(reading_data, file = 'data/0_f1000_reading.RData')

