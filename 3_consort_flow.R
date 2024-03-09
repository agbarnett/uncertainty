# 3_consort_flow.R
# CONSORT flow chart for included
# called by 3_summary.Rmd
# see https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
# November 2023
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(magrittr)

## Box numbers
start_bmj = filter(data, journal == 'BMJ Open') %>% nrow()
start_f1000 = filter(data, journal == 'F1000') %>% nrow()
start_epidemiology = filter(data, journal == 'Epidemiology') %>% nrow()
# excluded numbers
ex_bmj = filter(data, exclude == TRUE, journal == 'BMJ Open') %>% nrow()
ex_f1000 = filter(data, exclude == TRUE, journal == 'F1000') %>% nrow()
ex_epidemiology = filter(data, exclude == TRUE, journal == 'Epidemiology') %>% nrow()
# matched numbers
match_bmj = filter(data, exclude == FALSE, journal == 'BMJ Open', !is.na(version_number)) %>% nrow()
match_f1000 = filter(data, exclude == FALSE, journal == 'F1000', !is.na(version_number)) %>% nrow()
match_epidemiology = filter(data, exclude == FALSE, journal == 'Epidemiology', !is.na(version_number)) %>% nrow()
#
n_analysed = n_started - n_excluded # numbers from 3_summary.Rmd
n_analysed2 = filter(data, exclude==FALSE, !is.na(version_number)) %>% nrow()

#
consort_plot = grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, 
  rankdir = UD, # direction
   fontsize = 10]

  # several 'node' statements
  node [shape = rectange,
  style = filled,
  color = skyblue,
  fillcolor = white,
        fontname = Helvetica]
  S [label = '@@1']
  E [label = '@@2']
  A [label = '@@3']
  A2 [label = '@@4']
  
  # for empty node, point to split arrows
  node [shape = none,
  width = 0,
  height= 0,
  fillcolor = white]
  E1 [label = '']  

node[x=1, y=1, layout = neato] /* not working! */
S

node[x=0.5, y=0.6, layout = neato]
E1, E

node[pos=3]
A

node[pos=4]
A2

  edge [color = grey]

  # 'edge' statements
  S->E1 E1->A E1->E A->A2
}

[1]: paste0('Responded\\nn= ', n_started, '\\nBMJ Open= ', start_bmj, '\\nEpidemiology = ', start_epidemiology, '\\nF1000 = ', start_f1000, sep='')
[2]: paste0('Excluded\\nn= ', n_excluded, '\\nDid not answer\\nuncertainty question = ', n_excluded, '\\nBMJ Open= ', ex_bmj, '\\nEpidemiology = ', ex_epidemiology, '\\nF1000 = ', ex_f1000, sep='')
[3]: paste0('Analysed\\nn= ', n_analysed, sep='')
[4]: paste0('Review matched\\nto journal\\nn= ', n_analysed2, '\\nBMJ Open= ', match_bmj, '\\nEpidemiology = ', match_epidemiology, '\\nF1000 = ', match_f1000, sep='')

")

# export
outfile = "figures/consort_flow.pdf"
consort_plot %>%
  export_svg() %>%
  charToRaw %>%
  rsvg_pdf(outfile)
# does not work:
#outfile = "figures/consort_flow.png"
#consort_plot %>%
#  export_svg() %>%
#  charToRaw %>%
#  rsvg_png(outfile)

# does not work either
#library(trelliscope) # from github, for exporting to jpeg
#outfile = "figures/consort_flow.jpg"
#widgetThumbnail(p, outfile)
