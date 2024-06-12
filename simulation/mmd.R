library(Ecume)

for(d in seq(500, 2000, by = 100)){
  # read simulated data from 'filepath'
  dataall= readRDS(filepath)
  num1 = 200
  num2 = 200
  n = num1 + num2
  test_sta = rep(0, 100)
  p_value = rep(0, 100)
  for (j in 1:100) {
    data = dataall[[j]]
    sig = 1/(median(dist(data))/2) # sigma: The inverse kernel width used by the Gaussian the Laplacian, the Bessel and the ANOVA kernel
    invisible(capture.output(mmd <- mmd_test(as.matrix(data[1:num1, ]), as.matrix(data[(num1+1):(num1+num2), ]),sigma = sig)))
    p_value[j] = mmd$p.value
    test_sta[j] = mmd$statistic
    if(j %% 10 == 0){
      print(paste0(c(d, '-', j), collapse = ""))
    }
  }
  # set the path for the results as 'savepath'
  # saveRDS(list(mmd = p_value), savepath)
}