library(energy)

for(d in seq(500, 2000, by = 100)){
  # read distances matrix of simulated data from 'filepath'
  dists= readRDS(filepath)
  num1 = 100
  num2 = 100
  n = num1 + num2
  test_sta = rep(0, 100)
  p_value = rep(0, 100)
  for (j in 1:100) {
    dist = dists[[j]]
    et = eqdist.etest(dist, sizes=c(num1, num2), distance=TRUE, R = 10000)
    p_value[j] = et$p.value
    test_sta[j] = et$statistic
    if(j %% 10 == 0){
      print(paste0(c(d, '-', j), collapse = ""))
    }
  }
  # set the path for the results as 'savepath'
  # saveRDS(list(mmd = p_value), savepath)
}

