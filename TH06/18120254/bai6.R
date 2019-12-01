setwd('D:/BaiTH06')
load('heights.rda')
summary(heights)
hist(heights)
t.test(heights,mu = 160, conf.level=0.95)

###########
setwd('D:/BaiTH06')
data=read.csv('profit.csv')
#attach(data)
#data
summary(profit)
hist(profit)
result = t.test(profit,alternative = "less",mu=65, conf.level=0.99) 

result
n=qnorm(0.99)
-n1
result$statistic
if (result$statistic<-n || result$p.value<0.05) {print('bac bo')} else{print('chua du dieu kien')}


result2 = t.test(profit,alterantive = "two.sided",mu=60, conf.level=0.99)
result2$statistic
n2=qnorm(0.995)
if (((result2$statistic< -n2) || (result2$statistic>n2)) || result2$p.value<0.05) {print('bac bo')} else{print('chua du dieu kien')}

##########
x<-c(5,6,7,8,9,10)
n=length(x)
k<-c(5,10,15,20,12,8)
mu=8,
s=sd(x)
alpha=5%
test.geq.oneside<-function(x,mu,alpha)
{
	t0=(mean(x)-mu/s/sqrt(n))
	if (t0>qt(1-alpha,n-1))
	{
		print('bac bo')
		return
	}
}