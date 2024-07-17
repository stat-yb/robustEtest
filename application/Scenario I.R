library(rgTest)
library(gTests)
library(dplyr)
library(lubridate)
library(reshape2)
library(data.table)
library(energy)
library(Ecume)
first_date = "2020-01-01"
last_date = "2020-12-31"
# input data of taxi drop offs from https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew
# extract trip records with non-missing locations
# save the taxi drop-offs in Chicago in 2020 as 'input'
input = setDT(input)
date_hour = input[, .N, by = .(year, month, day, hour, loc_ind)] 
dates = seq(as.Date(first_date),as.Date(last_date),by="1 day")
months = month(dates)
days = day(dates)
# get taxi trips for each hour
count_hour = list()
for (h in 0:23) {
  count_h = matrix(0, nrow = length(days), ncol = length(unique(date_hour$loc_ind)))
  for (i in 1:length(days)) {
    temp = date_hour[hour == h &
                       month == months[i] &
                       day == days[i]]
    if(nrow(temp) == 0){
      next
    }
    row = rep(0, length(unique(date_hour$loc_ind)))
    row[temp$loc_ind] = temp$N
    count_h[i, ] = row
  }
  count_hour[[h+1]] = count_h
  print(h)
}
# function to print node degree information, edge-count tests and robust edge-count tests comparing two taxi trips
get_test_res = function(data1, data2){
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
  print(max(nodedeg))
  orit = unlist(g.tests(E, 1:nrow(data1), (nrow(data1)+1):nrow(data), perm = 10000))[c(6, 12)]
  robt = unlist(rg.test(E = E, n1 = nrow(data1), n2 = nrow(data2), weigh.fun = weiMax, perm.num = 10000, test.type = list("gen", "max"))[c(5, 6)])
  c(orit, robt)
}


# get taxi trips from 7 am to 8 am
set.seed(1457)
count1 = Reduce("+", count_hour[8:10])
count2 = Reduce("+", count_hour[8:10])
# taxi trips in September and November
sp1 = months %in% 9
sp2 = months %in% 11
data1 = count1[sp1, ]
data2 = count2[sp2, ]

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