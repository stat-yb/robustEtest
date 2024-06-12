library(ade4)
library(cccd)

# functions for Rg-NN and Ro-MST 
ROMST = function(dist, num1, num2, n_per, mst.k){
  n = num1+num2
  edges = kmst(dis=dist, k=mst.k)
  edgedists = sapply(1:nrow(edges), function(x) dist[edges[x, 1], edges[x, 2]])
  Ro = rank(-edgedists)
  
  G1 = 1:num1
  e1 = edges[, 1] %in% G1
  e2 = edges[, 2] %in% G1
  Ux = 2*sum(Ro[e1 + e2 == 2])
  Uy = 2*sum(Ro[e1 + e2 == 0])
  
  Rij = vector("list", n)
  for(i in 1:n) Rij[[i]]=rep(0,0)
  for(i in 1:nrow(edges)){
    Rij[[edges[i,1]]] = c(Rij[[edges[i,1]]],Ro[i])
    Rij[[edges[i,2]]] = c(Rij[[edges[i,2]]],Ro[i])
  }
  bar_Ri = sapply(Rij, function(x) sum(x)/(n-1))
  r0 = sum(bar_Ri)/n
  r12 = sum(bar_Ri^2)/n
  rd2 = sum(sapply(Rij, function(x) sum(x^2)/(n-1)))/n
  
  Vr = r12 - r0^2
  Vd = rd2 - r0^2
  
  mux = num1*(num1-1)*r0
  muy = num2*(num2-1)*r0
  
  VarX = 2*num1*num2*(num1-1)/(n-2)/(n-3)*((num2-1)*Vd + 2*(num1 - 2)*(n-1)*Vr)
  VarY = 2*num1*num2*(num2-1)/(n-2)/(n-3)*((num1-1)*Vd + 2*(num2 - 2)*(n-1)*Vr)
  
  CovXY = 2*num1*(num1-1)*num2*(num2-1)/(n-2)/(n-3)*(Vd - 2*(n-1)*Vr)
  
  sigma = matrix(c(VarX, CovXY, CovXY, VarY), nrow = 2)
  temp_asy = c(Ux - mux, Uy - muy) %*% solve(sigma)
  z_gen = temp_asy %*% c(Ux - mux, Uy - muy)
  test_statistic0 = z_gen[1, 1]
  obs = num1+num2
  temp = sapply(1:n_per, function(peri){
    per = sample(obs)
    new_E = matrix(per[edges], ncol = 2)
    G1 = 1:num1
    e1 = new_E[, 1] %in% G1
    e2 = new_E[, 2] %in% G1
    Ux = 2*sum(Ro[e1 + e2 == 2])
    Uy = 2*sum(Ro[e1 + e2 == 0])
    sigma = matrix(c(VarX, CovXY, CovXY, VarY), nrow = 2)
    temp_asy = c(Ux - mux, Uy - muy) %*% solve(sigma)
    z_gen = temp_asy %*% c(Ux - mux, Uy - muy)
    test_statistic = z_gen[1, 1]
  })
  return(list(test_statistic = test_statistic0, p_value = 1 - sum(test_statistic0 > temp)/n_per, asy_p = 1- pchisq(test_statistic0, df = 2)))
}


RGNN = function(dist, num1, num2, n_per, nn.k = 5){
  obs = num1+num2
  n = num1+num2
  edgesig = nng(dx=dist, k=nn.k)
  edges = as.matrix(edgesig, "edgelist")
  edgedists = sapply(1:nrow(edges), function(x) dist[edges[x, 1], edges[x, 2]])
  Rg = c(sapply(1:obs, function(x) {nn.k - rank(edgedists[edges[, 1] == x]) + 1}))
  
  G1 = 1:num1
  e1 = edges[, 1] %in% G1
  e2 = edges[, 2] %in% G1
  Ux = 2*sum(Rg[e1 + e2 == 2])
  Uy = 2*sum(Rg[e1 + e2 == 0])
  
  Rij = vector("list", n)
  for(i in 1:n) Rij[[i]]=rep(0,0)
  for(i in 1:nrow(edges)){
    Rij[[edges[i,1]]] = c(Rij[[edges[i,1]]], Rg[i])
    Rij[[edges[i,2]]] = c(Rij[[edges[i,2]]], Rg[i])
  }
  bar_Ri = sapply(Rij, function(x) sum(x)/(n-1))
  r0 = sum(bar_Ri)/n
  r12 = sum(bar_Ri^2)/n
  rd2 = sum(sapply(Rij, function(x) sum(x^2)/(n-1)))/n
  
  Vr = r12 - r0^2
  Vd = rd2 - r0^2
  
  mux = num1*(num1-1)*r0
  muy = num2*(num2-1)*r0
  
  VarX = 2*num1*num2*(num1-1)/(n-2)/(n-3)*((num2-1)*Vd + 2*(num1 - 2)*(n-1)*Vr)
  VarY = 2*num1*num2*(num2-1)/(n-2)/(n-3)*((num1-1)*Vd + 2*(num2 - 2)*(n-1)*Vr)
  
  CovXY = 2*num1*(num1-1)*num2*(num2-1)/(n-2)/(n-3)*(Vd - 2*(n-1)*Vr)
  
  sigma = matrix(c(VarX, CovXY, CovXY, VarY), nrow = 2)
  temp_asy = c(Ux - mux, Uy - muy) %*% solve(sigma)
  z_gen = temp_asy %*% c(Ux - mux, Uy - muy)
  test_statistic0 = z_gen[1, 1]
  
  temp = sapply(1:n_per, function(peri){
    per = sample(obs)
    new_E = matrix(per[edges], ncol = 2)
    G1 = 1:num1
    e1 = new_E[, 1] %in% G1
    e2 = new_E[, 2] %in% G1
    Ux = 2*sum(Rg[e1 + e2 == 2])
    Uy = 2*sum(Rg[e1 + e2 == 0])
    sigma = matrix(c(VarX, CovXY, CovXY, VarY), nrow = 2)
    temp_asy = c(Ux - mux, Uy - muy) %*% solve(sigma)
    z_gen = temp_asy %*% c(Ux - mux, Uy - muy)
    test_statistic = z_gen[1, 1]
  })
  return(list(test_statistic = test_statistic0, p_value = 1 - sum(test_statistic0 > temp)/n_per, asy_p = 1- pchisq(test_statistic0, df = 2)))
}

for(d in seq(500, 2000, by = 100)){
  # read distances matrix of simulated data from 'filepath'
  dists= readRDS(filepath)
  num1 = 100
  num2 = 100
  n = num1 + num2
  test_sta1 = rep(0, 100)
  test_sta2 = rep(0, 100)
  p_value1 = rep(0, 100)
  p_value2 = rep(0, 100)
  asy_p_value1 = rep(0, 100)
  asy_p_value2 = rep(0, 100)
  for (j in 1:100) {
    dist = dists[[j]]
    Romst = ROMST(dist, num1 = num1, num2 = num2, n_per = 10000, mst.k = 5)
    p_value1[j] = Romst$p_value
    test_sta1[j] = Romst$test_statistic
    asy_p_value1[j] = Romst$asy_p
    Rgnn = RGNN(dist, num1 = num1, num2 = num2, n_per = 10000, nn.k = 10)
    p_value2[j] = Rgnn$p_value
    test_sta2[j] = Rgnn$test_statistic
    asy_p_value2[j] = Rgnn$asy_p
  }
  # set the path for the results as 'savepath'
  # saveRDS(list(ROMST = p_value1, RGNN = p_value2, asy_ROMST = asy_p_value1, asy_RGNN = asy_p_value2), savepath)
}

