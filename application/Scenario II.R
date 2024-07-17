library(rgTest)
library(gTests)
library(dplyr)
library(reshape2)
library(data.table)
library(energy)
library(Ecume)
library(lubridate)
# input data of taxi drop offs from https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew
# extract trip records with non-missing locations
# save the taxi drop-offs in Chicago in 2020 as 'input'
# results for pick-ups can be generated when the taxi drop-offs in Chicago in 2020 are saved as 'input'
# get taxi trips for each hour and store the results as 'count_hour' as shown in 'Scenario I'
# create function 'get_test_res' to print node degree information, edge-count tests and robust edge-count tests comparing two taxi trips as shown in 'Scenario I'

# get taxi trips from midnight to early morning
set.seed(1457)
count1 = Reduce("+", count_hour[2:5])
count2 = Reduce("+", count_hour[2:5])
wd = weekdays(as.Date("2020-01-01")+0:365)
# taxi trips in April and May
sp1 = months %in% 4:5
sp2 = months %in% 4:5
# taxi trips on weekdays and weekends
data1 = count1[sp1, ][!c(wd[sp1]) %in% c('Saturday', 'Sunday'), ]
data2 = count2[sp2, ][c(wd[sp2]) %in% c('Saturday', 'Sunday'), ]

data = rbind(data1, data2)
distances = dist(data, method="manhattan")
# test results for edge-count tests and robust edge-count tests
t1 = get_test_res(data1, data2)
# test results for energy test
t2 = eqdist.etest(distances, sizes=c(nrow(data1), nrow(data2)), distance=TRUE, R = 10000)$p.value
# test results for MMD
sig = 1/(median(distances)/2)
invisible(capture.output(mmd <- mmd_test(as.matrix(data[1:nrow(data1), ]), as.matrix(data[(nrow(data1)+1):(nrow(data1)+nrow(data2)), ]),sigma = sig,iterations = 10000)))
t3 = mmd$p.value

print(c(t1, t2, t3))