setwd("D:/18120254_r/Tuan06")
load('heights.rda')
summary(heights)
hist(heights)
t.test(heights,mu = 160, conf.level=0.95)

#### cau 1
#alternative hypothesis: true mean is less: cho biet kiem dinh 2 phia hay 1 phia (less, greater, two side)
#99 percent confidence interval, alpha=0.01
#chi duoc ghi bac bo H0 hoac chua du co so bac bo H0 (khong dung vo H1)
setwd("D:/18120254_r/Tuan06")
data=read.csv('profit.csv')
attach(data)
summary(profit)
#a
hist(profit)
#b
result=t.test(profit,alternative="less",mu=65,conf.level=0.99)
result
#c
result=t.test(profit,alternative="two.sided",mu=60,conf.level=0.99)
result

#### cau 2b
#2 phia thi 1-alpha/2,n-1
#1 phia thi 1-alpha,n-1
test.geq.oneside=function(x, mu , alpha){
n=length(x)
X=mean(x)
s=sd(x)
t0=(X-mu)/(s/sqrt(n))
t=qt(1-alpha,n-1)
if(t0>t)
print ('bac bo')
else
print('khong bac bo')
}

x=c(5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10)
alpha=0.05
result1=test.geq.oneside(x,8,alpha)
result1
check1=t.test(x,alternative = "greater",mu=8, conf.level=0.95) 
check1

#### cau 2c
test.leq.oneside=function(x, mu , alpha){
n=length(x)
X=mean(x)
s=sd(x)
t0=(X-mu)/(s/sqrt(n))
t=qt(1-alpha,n-1)
if(t0<t)
print ('bac bo')
else
print('khong bac bo')
}

x=c(5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10)
alpha=0.05
result2=test.leq.oneside(x,8,alpha)
check2=t.test(x,alternative = "less",mu=8, conf.level=0.95) 
check2

#cach 2b dung ham tinh p-value pt(t0,n–1)
test.a=function(x, mu , alpha){
n=length(x)
X=mean(x)
s=sd(x)
t0=(X-mu)/(s/sqrt(n))
p=pt(t0,n–1,lower.tail=FALSE)
if(p<alpha)
print ('bac bo')
else
print('khong bac bo')
}
x=c(5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10)
alpha=0.05
result=test.a(x,8,alpha)
result





