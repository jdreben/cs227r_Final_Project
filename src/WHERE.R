# File Contents:
## SETUP
## MOVE
## CREATE
## VIZ
## double check everything not needed is commented out before you run !!


## SETUP (simulation specifics)
N = 100 # number of users to synthesize
NUM_DAYS = 30 # number of days in simulation
hour_of_day = 1:24 #

home_pdf <- read.csv('./home_pdf.csv', header=TRUE); h_ids <- home_pdf$id
work_pdf <- read.csv('./work_pdf.csv', header=TRUE); w_ids <- work_pdf$id
commute_dist <- read.csv('./commute_dist.csv', header=TRUE)
synth_people <- read.csv('./synth_people.csv', header=TRUE)
normalized_call_times <- read.csv('./norm_diurnal.csv', header=TRUE)[[1]]

# doubles as my hourly location distribution depending on hour
home_probabilities <- home_pdf[[3]]
work_probabilities <- work_pdf[[3]]

options(scipen=999) # grid ids are printed in scientific notation unless I include this
## END SETUP


## MOVE (generates CDRs)
CDRs <- data.frame(id = numeric(0), day = numeric(0), call_time = numeric(0), location = numeric(0))
move <- function(people, D) {
	#1
	# (1. 2. 3. 4.  5)  6.  7. 
	#  M, T, W, TH, F, SA, SU
	#2
	# (8. 9. 10 11 12) 13  14
	#  M, T, W, TH, F, SA, SU
	#3
	# (15 16 17 18 19) 20  21
	#  M, T, W, TH, F, SA, SU
	#4
	# (22 23 24 25 26) 27  28
	#  M, T, W, TH, F, SA, SU
	#5
	# (29 30)
	#  M, T
	is_weekday <- function(day, week) {
		test = (day - 6 - (week - 1) * 7)
		if (test == 0 || test == 1) { return(F) } else { return(T) }
	}
	week <- 1
	for(id in 1:N) {
		# cat("Currently recording calls for User ", toString(id)); cat('\n')
		# cat("Day:")
		for(day in 1:NUM_DAYS) {
			# cat(" ", toString(day)) 

			# At first I resampled if noise made the count negative, as below
			# calls_today <- 0 
			# while(calls_today <= 0) { calls_today <- rnorm(1, people$mean_calls[id], people$std_calls[id]) }
			# but reading through the literature it seems as though best practice is to replace with 0 like this
			calls_today <- rnorm(1, people$mean_calls[id], people$std_calls[id])
			if (calls_today < 0) { calls_today <- 0 }

			# a slight variant on the pseudo-code here, I sample all calls at once
			all_call_times <- sample(hour_of_day, round(calls_today), prob=normalized_call_times, replace=TRUE)

			for(call_time in all_call_times) {
				location <- numeric()
				# in 24 hour mode, 7am is 7 and 7pm is 19
				if(is_weekday(day, week) && call_time >= 7 && call_time <= 19) {
					# then hourly is drawn from work dist.
					location <- sample(w_ids, 1, prob=work_probabilities, replace=TRUE)
				} else {
					# then hourly is drawn from home dist. 
					location <- sample(h_ids, 1, prob=home_probabilities, replace=TRUE)
				}

				synthetic_call <- c(id, day, call_time, location)
				print(toString(synthetic_call))
				D[nrow(D)+1,] <- synthetic_call
			}
			if (day %% 7 == 0) { week <- week + 1 }
		}
		# cat('\n')
	}
	return(D)
}
CDRs = move(synth_people, CDRs)
write.csv(file='./synth_CDRs.csv', x=CDRs, row.names=F)
print("CDR database succesfully generated")
## END MOVE


# CREATE
p <- data.frame(id = numeric(0), home = numeric(0), work = numeric(0), commute = numeric(0), mean_calls = numeric(0), std_calls = numeric(0))
create <- function(people) {
	i <- 1
	sampled_homes = sample(h_ids, N, prob=home_probabilities, replace=TRUE)
	# print(length(sampled_homes))
	for (home_location in sampled_homes) {
		cat("Currently synthesizing User ", toString(i)); cat('\n')
		commute_distance = commute_dist[commute_dist$id==home_location,][3][1,]
		if(is.na(commute_distance)) {
			commute_distance = 0
		}
		work_location = sample(w_ids, 1, prob=work_probabilities, replace=TRUE)
		mean_calls = sample(1:100, 1, replace=TRUE)
		std_calls = sample(1:20, 1, replace=TRUE)
		synthetic_person <- c(i, home_location, work_location, commute_distance, mean_calls, std_calls)
		people[nrow(people)+1,] <- synthetic_person
		i <- i + 1
	}
	return(people)
}
people <- create(p)
# print(people)
write.csv(file="./synth_people3.csv", x=people, row.names=F)
## END CREATE



# ## VIZ
# lo <- loess(call_probability_by_hour~hour_of_day)
# smoothingSpline = smooth.spline(hour_of_day, call_probability_by_hour, spar=0.35)
# plot(hour_of_day,call_probability_by_hour, col="white")
# lines(smoothingSpline, col="blue", lwd=4)
# ## ENDVIZ


## Extraneous Code
# library(ggplot2)
# lines(predict(lo), col='blue', lwd=4)
# call_times <- data.frame(hour = numeric(0), probability = numeric(0))
# for (hour in hours) 
# {	
# 	call_times[nrow(call_times)+1,] <- c(hour, call_probability_by_hour[[hour]])
# }
# plot(call_times$hour, call_times$probability, type="l",col="red")
# qplot(diurnal, geom="histogram") 
# plot(df$a, df$c, type="l",col="red")
# sample(c("D2D", "F2F", "TM", "WW"), ndf, prob=c(0.25, 0.38, 0.17, 0.21), replace=TRUE)
# rpss <- function(stratum, n) {
#     props <- table(stratum)/length(stratum)
#     nstrat <- as.vector(round(n*props))
#     nstrat[nstrat==0] <- 1
#     names(nstrat) <- names(props)
#     stratsample(stratum, nstrat)
#     }