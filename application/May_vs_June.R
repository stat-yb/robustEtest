library(rgTest)
library(gTests)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(gplots)
library(viridis)
library(data.table)
library(rgTest)
# input data of taxi drop offs from https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew
# extract trip records with non-missing locations
# save the taxi drop-offs in Chicago in 2020 as 'input'
# results for pick-ups can be generated when the taxi drop-offs in Chicago in 2020 are saved as 'input'
# get taxi trips for each hour and store the results as 'count_hour' as shown in 'drop-offs_March_rush_hour'
# create function 'get_test_res' to print node degree information, edge-count tests and robust edge-count tests comparing two taxi trips as shown in 'drop-offs_March_rush_hour'

# get taxi trips from midnight to early morning
hours = 1:6
trial = 39
count = count_hour[hours]
#comparison based on all combination of months
combinations = combn(1:12, 2)
# get two samples in May and June
data1 = do.call(rbind, lapply(count, function(x) x[months == combinations[1, trial], ]))
data2 = do.call(rbind, lapply(count, function(x) x[months == combinations[2, trial], ]))
# test results for edge-count tests and robust edge-count tests
get_test_res(data1, data2)

# test results for energy test
data = rbind(data1, data2)
distances = dist(data, method="manhattan")
eqdist.etest(distances, sizes=c(nrow(data1), nrow(data2)), distance=TRUE, R = 10000)$p.value
# test results for MMD
sig = 1/(median(distances)/2) # sigma: The inverse kernel width used by the Gaussian the Laplacian, the Bessel and the ANOVA kernel
invisible(capture.output(mmd <- mmd_test(as.matrix(data[1:nrow(data1), ]), as.matrix(data[(nrow(data1)+1):(nrow(data1)+nrow(data2)), ]),sigma = sig)))
mmd$p.value
# check two-sample tests for each hour from 12 am to 6 am between May and June
for(t in 1:6){
  print(paste(c("case", t)))
  hours = t
  trial = 39
  count = count_hour[hours]
  combinations = combn(1:12, 2)
  data1 = count[[1]][months == combinations[1, trial], ]
  data2 = count[[1]][months == combinations[2, trial], ]
  get_test_res(data1, data2)
}
