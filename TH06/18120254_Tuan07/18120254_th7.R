#su dung var.test, p-value<alpha -> 2 phuong sai ko = nhau va ngc lai
#var.equal=FALSE la 2 phuong sai khac nhau
#MSSV_T62.PDF
setwd('D:/18120254_r/Tuan07')
data=read.csv('volume.csv')
data
alpha=0.05
#a chi su dung var.test khi chua bik psai (var.equal = var.test(data$machine1, data$machine2))

#b
result1=t.test(data$machine1, data$machine2, "two.sided",var.equal = FALSE)
result1

#c
result2=t.test(data$machine1, data$machine2, "two.sided",var.equal = FALSE,conf.level=0.95)
result2

#d
test.leq.oneside=function(x, y,u,sd1,sd2,alpha){
	n=length(x)
	m=length(y)
	if(sd1==sd2){
		df=n + m – 2
		s=sqrt((n-1)*sd1^2+(m-1)*sd2^2/(n+m-2))
		t=(mean(x)-mean(y))/s*sqrt(1/n+1/m)
	}
	else{
		df=((sd1^2/n)+(sd2^2/m))^2 / ((sd1/n)^2/(n-1)+(sd2/m)^2/(m-1))
		t=(mean(x)-mean(y))/sqrt(sd1^2/n+sd2^2/m)
	}
	if(u==0){
		p=qt(t,df,lower.tail = FALSE)
		p
	}
	else{
		p=qt(t,df)
		p
	}
	if (p<=alpha){
		print("Bac bo")
	} 
	else{
		print("Khong bac bo")
	}
}
setwd('D:/18120254_r/Tuan07')
data=read.csv('volume.csv')
attach(data)
alpha=0.05
s1=0.002
s2=0.0025
result=test.leq.oneside(data$machine1, data$machine2,0,s1,s2,alpha)
result


