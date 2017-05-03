
mean_min = 1
mean_max = 100
std_min = 1
std_max = 20

## Laplace Mechanism Implementation
source("./Laplace.R")
# differentially private calls per day distribution
eps_cpday = 1
global_sensitivity = 2
synth_people <- read.csv('./synth_people3.csv', header=TRUE)
keep_columns <- c('mean_calls', 'std_calls')
CountAvgStd = table(synth_people[keep_columns])
M = CountAvgStd # duplicate for shape and then fill with noisy Laplace values
for (mean in mean_min:mean_max) {
	for (std in std_min:std_max) {
		tryCatch({
			M[mean, std] <- slaplace(CountAvgStd[mean, std], eps_cpday, global_sensitivity)
		}, error<-function(e) {}) # b/c of sampling not all mean, stds present in data
	}
}

write.csv(file='./dp_callsperday_dist.csv', x=M, row.names=F)
print("Finished")


## Extraneous Code
# library("Rcpp")
# Rcpp::sourceCpp("./smooth.cpp")
# returnVector  <- Smooth(cpd_cdf)
# POST-PROC
# write.csv(file='~./dp_home_cdf.csv', x=returnVector)
