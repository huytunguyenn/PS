mean_conf_interval=function(data, sigma, alpha){
  x = mean(data)
  n = length(data)
  s = sd(data)
  if(n >= 30 && sigma >= 0){        # trý???ng h???p 1: n>=30, sigma ð?? bi???t 
    z = qnorm(1-alpha/2)
    e = z*(sigma/(sqrt(n)))
  }
  else if(n >= 30 && sigma < 0){    # trý???ng h???p 2: n>=30, sigma chýa bi???t
    z = qnorm(1-alpha/2)
    e = z*(s/(sqrt(n)))
  }
  else if(n < 30){                 # trý???ng h???p 3: n<30, sigma chýa bi???t
    t = qt(1- alpha/2, df=n-1)
    e = t*(s/(sqrt(n)))
  }
  lower = x - e
  upper = x + e	
  print(lower)
  print(upper)
}


prop_conf_interval=function(m,n,alpha){
  p=m/n
  if((n*p >= 5)&&(n*(1-p) >= 5)){
    z = qnorm(1-alpha/2)
    e = z*sqrt(p*(1-p)/n)
    lower = p-e
    upper = p+e
    print(lower)
    print(upper)
  }
  else{
    print('khong thoa dieu kien')
  }
}


mean_hypothesis_onesample=function(data, mu0, sigma, alpha,HA=c('two.side', 'greater', 'smaller')){
  x=mean(data)
  n=length(data)
  s=sd(data)
  # tính th???ng kê ki???m ð???nh
  if(n >= 30){    
    if(sigma >= 0){                   # trý???ng h???p 1: bi???t sigma
      z0 = (x-mu0)/(sigma/sqrt(n))
    }
    else{                            # trý???ng h???p 2: không bi???t sigma và m???u l???n (n>=30)
      z0 = (x-mu0)/(s/sqrt(n))
    }
    # tính p-value
    if(HA == 'two.side'){
      p.value = 2*(1-pnorm(abs(z0)))
    }
    else if(HA == 'greater'){
      p.value = 1 - pnorm(z0)
    }
    else if(HA == 'smaller'){
      p.value = pnorm(z0)
    }
  }
  else{                             # trý???ng h???p 3: không bi???t sigma và m???u nh??? (n<30)
    t0 = (x-mu0)/(s/sqrt(n))
    # tính p-value
    if(HA == 'two.side'){
      p.value = 2*pt(abs(t0),n-1,lower.tail = FALSE)
    }
    else if(HA == 'greater'){
      p.value = pt(t0,n-1,lower.tail = FALSE)
    }
    else if(HA == 'smaller'){
      p.value = pt(t0,n-1)
    }
  }
  # k???t lu???n
  if(p.value < alpha){
    print ('Bac bo')
  }
  else{
    print('Chua du co so bac bo')
  }
  print(x)
  print(p.value)
}


mean_hypothesis_twosample=function(data1, data2, mu0, sigma1, sigma2, alpha, HA=c('two.side','greater','smaller')){
  x = mean(data1)
  n = length(data1)
  sx = sd(data1)
  y = mean(data2)
  m = length(data2)
  sy = sd(data2)
  if(n>=30 && m>=30){
    if(sigma1 >=0 && sigma2 >=0){         #trý???ng h???p 1: bi???t sigma
      z0 = (x-y-mu0)/(sqrt(sigma1^2/n+sigma2^2/m))
    }
    else{                                 #trý???ng h???p 2: không bi???t sigma, m???u l???n
      z0 = (x-y-mu0)/(sqrt(sx^2/n+sy^2/m))
    }
    # tinh p-value
    if(HA == 'two.side'){
      fiz0 = pnorm(abs(z0))
      p.value = 2*(1-fiz0)
    }
    else if(HA == 'greater'){
      fiz0 = pnorm(z0)
      p.value = 1 - fiz0
    }
    else if(HA =='smaller'){
      fiz0 = pnorm(z0)
      p.value = fiz0
    }
  }
  else{                              #trý???ng h???p 3: không bi???t sigma, m???u nh???
    sigma1 = sx
    sigma2 = sy
    if(simga1^2 == sigma2^2){                         #trý???ng h???p 3.1: phýõng sai b???ng nhau
      s = sqrt(((n-1)*sx^2+(m-1)*xy^2)/(n+m-2))
      t0 = (x-y-mu0)/(s*sqrt(1/n+1/m))
      df = m+n-2
    }
    else{                               #trý???ng h???p 3.2: phýõng sai không b???ng nhau
      t0 = (x-y-mu0)/(sqrt(sx^2/n+sy^2/m))
      df = (sx^2/n+sy^2/m)^2/(((sx^2/n)^2/(n-1))+((sy^2/m)^2/(m-1)))
    }
    #tinh p-value
    if(HA == 'two.side'){
      p.value = 2*pt(abs(t0),df,lower.tail = FALSE)
    }
    else if(HA == 'greater'){
      p.value =  pt(t0,df,lower.tail = FALSE)
    }
    else if(HA =='smaller'){
      p.value = pt(t0,df)
    }
  }
  #k???t lu???n
  if(p.value < alpha){
    print ('Bac bo')
  }
  else{
    print('Chua du co so bac bo')
  }
  print(p.value)
}
