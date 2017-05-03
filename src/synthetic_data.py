
import os
import numpy as np
import pandas as pd

print "Generating randomized synthetic data for NYC, 2014"
# print(os.getcwd())
# os.chdir('crime_data/')
home_binary_file = 'otm_homes_2014/points_2014.dbf'
work_binary_file = 'otm_work_2014/points_2014.dbf'

# beginning = 1463
beg = 1378
row_shift = 1065
num_size = 24

def dissect(binary_row):
	census_id = float(binary_row[0:24].strip())
	work_home_count = float(binary_row[85:85+num_size].strip())
	return (census_id, work_home_count)	


## HOME
home_file = open(home_binary_file)
home_data = home_file.read()
raw_home_data = []
home_data_beginning = home_data[:beg]

num_rows = int(len(home_data[beg:len(home_data)])) / row_shift
print "{} home data points loaded...".format(num_rows)

home_data = home_data[beg:]
for row_num in range(num_rows):
	# print home_data[row_num*row_shift:row_num*row_shift+row_shift]
	census_id, count = dissect(home_data[row_num*row_shift:row_num*row_shift+row_shift])
	# print "row {}. census {}. count {}".format(row_num, census_id, count)
	# (census_id, total num jobs/homes)
	raw_home_data.append((census_id, count))

home_df = pd.DataFrame(raw_home_data)
# home_df.pivot(index=0, columns=1, values=2)
print home_df.head()

home_df.to_csv('home_distribution.csv')

## WORK
# work_file = open(work_binary_file)
# work_data = work_file.read()
# raw_work_data = []
# work_data_beginning = work_data[:beg]

# num_rows = int(len(work_data[beg:len(work_data)])) / row_shift
# print "{} work data points loaded...".format(num_rows)

# work_data = work_data[beg:]
# for row_num in range(num_rows):
# 	# print home_data[row_num*row_shift:row_num*row_shift+row_shift]
# 	census_id, count = dissect(work_data[row_num*row_shift:row_num*row_shift+row_shift])
# 	# print "row {}. census {}. count {}".format(row_num, census_id, count)
# 	# (census_id, total num jobs/homes)
# 	raw_work_data.append((census_id, count))

# work_df = pd.DataFrame(raw_work_data)
# print work_df.head()

# home_df.to_csv('work_distribution.csv')