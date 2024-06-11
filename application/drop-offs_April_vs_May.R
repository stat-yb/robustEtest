library(rgTest)
library(gTests)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(gplots)
library(viridis)
library(data.table)
library(energy)
library(Ecume)
# input data of taxi drop offs from https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew
# extract trip records with non-missing locations
# save the taxi drop-offs in Chicago in 2020 as 'input'
# get taxi trips for each hour and store the results as 'count_hour' as shown in 'drop-offs_March_rush_hour'
# create function 'get_test_res' to print node degree information, edge-count tests and robust edge-count tests comparing two taxi trips as shown in 'drop-offs_March_rush_hour'
# get taxi trips from midnight to early morning
hours = 1:6
trial = 31
count = Reduce("+", count_hour[hours])
#all combination of months
combinations = combn(1:12, 2)
# get two samples in April and May
data1 = count[months == combinations[1, trial], ]
data2 = count[months == combinations[2, trial], ]

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

# code below is for drop-off data only
# remove hub one at a time
# remove hub in May
data1 = count[months == combinations[1, trial], ]
data2 = count[months == combinations[2, trial], ]
data2 = data2[-11, ]
get_test_res(data1, data2)

data1 = count[months == combinations[1, trial], ]
data2 = count[months == combinations[2, trial], ]
data2 = data2[-18, ]
get_test_res(data1, data2)
 
#remove hub in April
data1 = count[months == combinations[1, trial], ]
data2 = count[months == combinations[2, trial], ]
data1 = data1[-29, ]
get_test_res(data1, data2)

# information about nodes related to May 11 May 18 
data1 = count[months == combinations[1, trial], ]
data2 = count[months == combinations[2, trial], ]
data = rbind(data1, data2)
distances = dist(data, method="manhattan")
E = kmst(dis=distances,k=5)
n = nrow(data)
Ebynode = vector("list", n)
for(j in 1:n) Ebynode[[j]]=rep(0,0)
for(j in 1:nrow(E)){
  Ebynode[[E[j,1]]] = c(Ebynode[[E[j,1]]],E[j,2])
  Ebynode[[E[j,2]]] = c(Ebynode[[E[j,2]]],E[j,1])
}

nodedeg = rep(0,n)
for(j in 1:n) nodedeg[j] = length(Ebynode[[j]])
for (date in c(11, 18)+30) {
  print(list(d = nodedeg[date], 
             within_sample_edge = sum(Ebynode[[date]]>30), 
             between_sample_edge = sum(Ebynode[[date]]<=30)))
}
# information about nodes related to April 29
date = 29
list(d = nodedeg[date], 
     within_sample_edge = sum(Ebynode[[date]]<=30), 
     between_sample_edge = sum(Ebynode[[date]]>30))