# PDFs
options(scipen=999)

# HOME
home <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/home_dist.csv', header=TRUE)

# c00 or the third column is the relevant data
max <- max(home[3])
min <- min(home[3])

counts = home[3]

counts = counts / max

home[3] = counts

write.csv(file='~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/home_pdf.csv')

home <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/home_dist.csv', header=TRUE)

# WORK

work <- read.csv('~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/work_dist.csv', header=TRUE)

# c00 or the third column is the relevant data
max <- max(work[3])
min <- min(work[3])

counts = work[3]

counts = counts / max

work[3] = counts

write.csv(file='~/Dropbox/Harvard/Senior Spring/CS227/Final Project/src/work_pdf.csv')
