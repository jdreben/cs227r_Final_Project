
import numpy as np
N = 10000

# ex.
pop = [{'callsperday': (0.5,0,5), 'callsbehavior': 5, 'home': (72,73), 'work': (72, 73) }]


# ex.
dist = [(001, .1), (002, .11), (002, .11), (002, .11)]
home_dist = []
work_dist = []


def sample_from_normalized_distribution(dist):
	p = random()
	below = 0
	for row in dist:
		for id, prob in row:
			if p < below + prob:
				return id
			else:
				below += prob


# fills pop with synthetic users
def create():
	for user in range(0, N):
		pop[user].home = sample_from_normalized_distribution(home_dist)
		pop[user].commute = commute_dist[pop[user].home]
		# reduce work distribution to only those commute distance away
		conditioned_work_dist = work_dist[]
		pop[user].work = sample_from_normalized_distribution(conditioned_work_dist)
		pop[user].callsbehavior = call_time_dist
		pop[user].callsperday = per_user_calls_per_day


def which_dist(calltime, user):
	# if calltime between 7am - 7pm weekdays then WORK
	# otherwise HOME


def two_place_model(num_users, num_days):
	for user in range(0, num_users):
		for day in range(0, num_days):
			mu, sigma = pop[user].callsperday
			callstoday = np.random.normal(mu, sigma)
			for call in range(0, callstoday):

				calltime = pop[user].callsbehavior

				if calltime in WORK_HOURS:
					location = sample_from_normalized_distribution(work_dist)
				else:
					location = sample_from_normalized_distribution(home_dist)

				print user, day, calltime, location

