# get simulation results for edge-count tests and proposed robust edge-count tests
library(rgTest)
library(gTests)
B=100
for(d in seq(500, 2000, by = 100)){
  # read distances matrix of simulated data from 'filepath'
  dists= readRDS(filepath)
  num_perm = 10000
  Ori_PermPval = matrix(0, nrow = B, ncol = 2)
  max_PermPval = matrix(0, nrow = B, ncol = 2)
  Ori_asyPval = matrix(0, nrow = B, ncol = 2)
  max_asyPval = matrix(0, nrow = B, ncol = 2)
  max_nodedeg = rep(0, B)
  pos = 1
  find_quantile_max_nodedeg = rep(0, B)
  for (j in 1:B) {
    num1 = 100
    num2 = 100
    
    dist = dists[[j]]
    
    E = kmst(dis=dist, k=5)
    sample1ID = 1:num1
    sample2ID = (num1+1):(num2+num1)
    n = num2+num1
    ### Check properties of graph
    Ebynode = vector("list", n)
    for(i in 1:n) Ebynode[[i]]=rep(0,0)
    for(i in 1:nrow(E)){
      Ebynode[[E[i,1]]] = c(Ebynode[[E[i,1]]],E[i,2])
      Ebynode[[E[i,2]]] = c(Ebynode[[E[i,2]]],E[i,1])
    }
    
    nodedeg = rep(0,n)
    for(i in 1:n) nodedeg[i] = length(Ebynode[[i]])
    max_nodedeg[j] = max(nodedeg)
    
    res0 = g.tests(E, sample1ID,  sample2ID, perm = num_perm, maxtype.kappa = 1)
    Ori_PermPval[j, ] = c(res0$generalized$pval.perm, res0$maxtype$pval.perm)
    Ori_asyPval[j, ] = c(res0$generalized$pval.approx, res0$maxtype$pval.approx)
    
    res = rg.test(E = E, n1 = num1, n2 = num2, weigh.fun = weiMax, perm.num = num_perm, test.type = list("gen", "max"))
    max_PermPval[j, ] = c(res$perm.gen.pval, res$perm.max.pval)
    max_asyPval[j, ] = c(res$asy.gen.pval, res$asy.max.pval)
    if(j %% 10 == 0){
      print(paste0(c(d, '-', j), collapse = ""))
    }
  }
  # set the path for the results as 'savepath'
  # saveRDS(list(ori = Ori_PermPval, rg = max_PermPval, nd = max_nodedeg, asy_ori = Ori_asyPval, asy_rg = max_asyPval), savepath)
}

