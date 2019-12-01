X=rnorm(35,10,5)

alpha=0.05
epsilon=qnorm(1-alpha/2)*5/sqrt(35)

mean(X)-epsilon
mean(X)+epsilon

#bai 8
setwd("D:/Bai TH04-05/Bai TH04-05/Data cho cac bai thuc hanh")
data = read.csv("data31.csv")
attach(data)
x=data["profit"]
typeof(x)
X=unlist(x)
n=length(X)


ci.mean = function(X,alpha)
{
	epsilon=qnorm(1-alpha/2)*sd(X)/sqrt(n)
	kq1=mean(X)-epsilon
	kq2=mean(X)+epsilon
	print(kq1)
	print(kq2)
}
ci.mean(X,0.05)
ci.mean(X,0.01)

#BAI 9
setwd("D:/Bai TH04-05/Bai TH04-05/Data cho cac bai thuc hanh")
data = read.csv("data32.csv")
attach(data)
x=data["KHTN"]
typeof(x)
X=unlist(x)
n=length(X)
n

ci.mean = function(X,alpha)
{
	epsilon=qnorm(1-alpha/2)*sd(X)/sqrt(n)
	kq1=mean(X)-epsilon
	kq2=mean(X)+epsilon
	print(kq1)
	print(kq2)
}
ci.mean(X,0.05)


a=x[x>5]
f=length(a)
f
ci.prop=function(f,n,alpha)
{
	p=f/n
	if(p*n>=5 && n*(1-p)>=5)
	{
		z=qnorm(1-(alpha/2))
		epsilon = z*sqrt(p*(1-p)/n)
		print(p-epsilon)
		print(p+epsilon)
	}
}
ci.prop(f,n,0.05)
