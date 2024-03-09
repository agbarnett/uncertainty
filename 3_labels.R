# 3_labels.R
# set up labels for recommendations; long versions that match Qualtrics and those shorter than actual
# useful for ordering in plots and tables
# November 2022


### F1000
rlabels_f1000 = c('Approved', 
            'Approved with reservations',
            'Not approved',
            'Missing')
rlabels_return_f1000 = c('Approved', # version with returns
                   'With\nreservations',
                   'Not\napproved')

### Epidemiology
rlabels_epi = c('Accept', 
            'Accept with minor revisions',
            'Reconsider after revisions',
            'Uncertain, needs major revision',
            'Reject',
            'None',
            'Missing')
rlabels_return_epi = c('Accept', 
                   'Accept\nminor', # shorter than above
                   'Reconsider',
                   'Uncertain',
                   'Reject',
                   'None')

### BMJ Open
rlabels_bmj = c('Accept', 
            'Minor revisions',
            'Major revisions',
            'Reject',
            'Missing')
rlabels_return_bmj = c('Accept', 
                   'Minor\nrevisions',
                   'Major\nrevisions',
                   'blank', # minor error in Qualtrics set up
                   'Reject')


## overall
big_rlabels = c('Accept',
                'Approved', 
                'Minor revisions',
                'Approved with reservations',
                'Accept with minor revisions',
                'Reconsider after revisions',
                'Uncertain, needs major revision',
                'Major revisions',
                'Reject',
                'Not approved',
                'Missing')