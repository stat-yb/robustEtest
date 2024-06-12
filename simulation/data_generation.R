# generate simulated data


# simulation I --------------------------------------------------------------
B=100
mstk = 5
for (d in seq(500, 2000, by = 100)) {
  j = 0
  temp = vector(mode = 'list', length = B)
  dis_temp = vector(mode = 'list', length = B)
  while (j < B) {
    vmu2 = rep(1+sqrt(0.01*(log(d))/d),d)
    num1 = 100
    num2 = 100
    y1a = matrix(0,num1,d)
    y2a = matrix(0,num2,d)
    
    for (i in 1:num1) {
      y1a[i,] = rlnorm(d, meanlog = 1, sdlog = 0.6)
    }
    for (i in 1:(num2)) {
      y2a[i,] = rlnorm(d, meanlog = vmu2, sdlog = rep(0.6+1.8*log(d)/d, d))
    }
    y = rbind(y1a, y2a)
    dist = getdis(y)
    E = kmst(dis=dist, k=mstk)
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
    # select data with maximum node degree in the similarity graph appears in the sample with a larger variance
    if(which.max(nodedeg) > num1){
      j = j+1
      temp[[j]] = y
      dis_temp[[j]] = dist
    }
  }
  # save data and distances into 'filepath'
  # saveRDS(temp, filepath)
  # saveRDS(dis_temp, filepath)
}

# simulation II -----------------------------------------------------------
B=100
for (d in seq(500, 2000, by = 100)) {
  temp = vector(mode = 'list', length = B)
  dis_temp = vector(mode = 'list', length = B)
  for (j in 1:B) {
    vmu2 = rep(1+sqrt(0.01*(log(d))/d),d)
    num1 = 100
    num2 = 100
    y1a = matrix(0,num1,d)
    y2a = matrix(0,num2,d)
    
    for (i in 1:num1) {
      y1a[i,] = rlnorm(d, meanlog = 1, sdlog = 0.6)
    }
    for (i in 1:(num2)) {
      y2a[i,] = rlnorm(d, meanlog = vmu2, sdlog = rep(0.6+1.8*log(d)/d, d))
    }
    y = rbind(y1a, y2a)
    temp[[j]] = y
    dist = getdis(y)
    dis_temp[[j]] = dist
  }
  # save data and distances into 'filepath'
  # saveRDS(temp, filepath)
  # saveRDS(dis_temp, filepath)
}


# simulation III ----------------------------------------------------------
B=100
mstk = 5
for (d in seq(500, 2000, by = 100)) {
  j = 0
  temp = vector(mode = 'list', length = B)
  dis_temp = vector(mode = 'list', length = B)
  while (j < B) {
    mu = 0
    vmu2 = rep(mu+sqrt((0.1*log(d))/d),d)
    num1 = 100
    num2 = 100
    y1a = matrix(0,num1,d)
    y2a = matrix(0,num2,d)

    for (i in 1:num1) {
      y1a[i,] = rnorm(d, mean = mu, sd = 1)
    }
    for (i in 1:(num2-10)) {
      y2a[i,] = rnorm(d, mean = vmu2, sd = 1+2.5*log(d)/d)
    }
    for (i in (num2-9):(num2)) {
      y2a[i,] = rnorm(d, mean = mu, sd = 1)
    }
    y = rbind(y1a, y2a)

    dist = getdis(y)
    E = kmst(dis=dist, k=mstk)
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
    # select data with maximum node degree in the similarity graph appears in the sample with a larger variance
    if(which.max(nodedeg) > num1){
      j = j+1
      temp[[j]] = y
      dis_temp[[j]] = dist
    }
  }
  # save data and distances into 'filepath'
  # saveRDS(temp, filepath)
  # saveRDS(dis_temp, filepath)
}


# simulation IV -----------------------------------------------------------
B=100
for (d in seq(500, 2000, by = 100)) {
  temp = vector(mode = 'list', length = B)
  dis_temp = vector(mode = 'list', length = B)
  for (j in 1:B) {
    mu = 0
    vmu2 = rep(mu+sqrt((0.1*log(d))/d),d)
    num1 = 100
    num2 = 100
    y1a = matrix(0,num1,d)
    y2a = matrix(0,num2,d)

    for (i in 1:num1) {
      y1a[i,] = rnorm(d, mean = mu, sd = 1)
    }
    for (i in 1:(num2-10)) {
      y2a[i,] = rnorm(d, mean = vmu2, sd = 1+2.5*log(d)/d)
    }
    for (i in (num2-9):(num2)) {
      y2a[i,] = rnorm(d, mean = mu, sd = 1)
    }
    y = rbind(y1a, y2a)
    temp[[j]] = y
    dist = getdis(y)
    dis_temp[[j]] = dist
  }
  # save data and distances into 'filepath'
  # saveRDS(temp, filepath)
  # saveRDS(dis_temp, filepath)
}


# Observations with sparse signal -----------------------------------------
B = 100
dset = seq(500, 2000, by = 100)
datal = vector(mode = 'list', length = B)
dis_temp = vector(mode = 'list', length = B)
for (d in dset) {
  for (j in 1:B) {
    n1 = 200
    n2 = 200
    n = n1+n2
    y = matrix(0,n,d)
    
    for (i in 1:n1) y[i,] = rnorm(d)
    
    s = floor(sqrt(d))
    for (i in (n1 + 1): (n1 + n2)) {
      pos = 1:s
      y[i,pos] = rnorm(s, mean = sqrt(0.2*log(s)/s), sd = 1+3*log(s)/s)
      y[i,-pos] = rnorm(d-s)
    }
    datal[[j]] = y
    dis_temp[[j]] = getdis(data)
  }
  # save data and into 'filepath'
  # saveRDS(datal, filepath)
  # saveRDS(dis_temp, filepath)
}
