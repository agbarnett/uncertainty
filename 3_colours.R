# 3_colours.R
# colours and breaks for each journal
# Jan 2024

# F1000
breaks_f1000 = c('Approved','Approved with reservations','Not approved')
breaks_f1000_return = c('Approved','Approved with\nreservations','Not\napproved') # version with carriage return
colours_f1000 = c('darkseagreen3','yellow2','indianred4')
# BMJ Open
breaks_bmj = c('Accept', 'Minor revisions', 'Major revisions', 'Reject')
breaks_bmj_return = c('Accept', 'Minor\nrevisions', 'Major\nrevisions', 'Reject') # version with carriage return
colours_bmj = c('darkseagreen3','yellow2','darkorange3','indianred4')
# Epidemiology
breaks_epi = c('Accept', 'Accept with minor revisions', 'Reconsider after revisions','Uncertain, needs major revision', 'Reject', 'No recommendation')
colours_epi = c('darkseagreen3','yellow2','orange','darkorange3','indianred4','grey22')
# Epidemiology - version without 'no recommendation'
breaks_epi_no_none = c('Accept', 'Accept with minor revisions', 'Reconsider after revisions','Uncertain, needs major revision', 'Reject')
colours_epi_no_none = c('darkseagreen3','yellow2','orange','darkorange3','indianred4')
breaks_epi_no_none_return = c('Accept', 'Accept with\nminor revisions', 'Reconsider after\nrevisions','Uncertain, needs\nmajor revision', 'Reject') # version with carriage return
