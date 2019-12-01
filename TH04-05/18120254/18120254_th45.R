%%bai7
data=rnorm(35,10,5)
alpha=0.05
z=qnorm(1-alpha/2)
e=z*sd(data)/sqrt(35)
lower=mean(data)-e
upper=mean(data)+e

%%bai8
n=90
ci.mean=function(x, alpha){
z=qnorm(1-alpha/2)
e=z*sd(x)/sqrt(n)
lower=mean(x)-e
upper=mean(x)+e
result=c(lower,upper)
result
}

setwd('D:/18120254')
data=read.csv('data31.csv')
attach(data)
profit
typeof(profit)
unlist(profit)
TH1=ci.mean(profit,0.05)
TH1
TH2=ci.mean(profit,0.01)
TH2


%%bai9b
ci.prop=function(f, n, alpha){
p=f/n
if((n*p>=5)&&(n*(1-p)>=5)){
z=qnorm(1-alpha/2)
e=z*sqrt(p*(1-p)/n)
lower=p-e
upper=p+e
result=c(lower,upper)
result
}
else{}
}

n=120
setwd('D:/18120254')
data=read.csv('data32.csv')
attach(data)
KHTN
typeof(KHTN)
unlist(KHTN)
f=0
for(i in 1:n){
if(KHTN[i]>5){f=f+1}
}
TH1=ci.prop(f,n,0.1)
TH1
TH2=ci.prop(f,n,0.05)
TH2
TH3=ci.prop(f,n,0.01)
TH3

