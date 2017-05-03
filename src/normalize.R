# File Contents:
## HOME
## WORK
## CALL TIMES
options(scipen=999)


## HOME
home <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/home_dist.csv', header=TRUE)
# c00 or the third column is the relevant data
# max <- max(home[3])
# min <- min(home[3])
h_sigma <- sum(home[3])
h_counts = home[3]
h_counts_pdf = h_counts / h_sigma
home[3] = h_counts_pdf
write.csv(file='~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/home_pdf.csv', x=home, row.names=F)


## WORK
work <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/work_dist.csv', header=TRUE)
# c00 or the third column is the relevant data
# max <- max(work[3])
# min <- min(work[3])
w_sigma <- sum(work[3])
w_counts = work[3]
w_counts_pdf = w_counts / w_sigma
work[3] = w_counts_pdf
write.csv(file='~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/work_pdf.csv', x=work, row.names=F)


## CALL TIMES
call_times <- read.csv('./diurnal_pattern.csv', header=TRUE)[[1]]
ct_sigma <- sum(call_times)
normalized_call_times <- call_times / ct_sigma
write.csv(file='./norm_diurnal.csv', x=normalized_call_times, row.names=F)
