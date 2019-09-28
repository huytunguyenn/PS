setwd('C:/Users/sv/Desktop/18120254')

#Bai1
#x=c(1,2,3,4)
x=runif(4)
x
tinh_tong=function(x)
{
	sum=0
	for(i in 1:length(x))
	{
 		sum=sum+x[i]
	}
	sum
}
tinh_tong(x)


#Bai2
radius=c(seq(3,20,1))
tinh_the_tich=function(x)
{	
	volume=c(length(x))
	for(i in 1:length(x))
	{
		volume[i]=4*pi*x[i]*x[i]*x[i]/3
	}
}
tinh_the_tich(radius)

data=data.frame(radius,volume)
data


#Bai3
setwd('C:/Users/sv/Desktop/18120254')
data3 = read.csv('data01.csv', header =T)
data3
attach(data1)
index=c(length(ï..Age))
for(i in 1:length(ï..Age))
{
	if(ï..Age[i]<=60)
	{
		index[i]=0
	}
	else if(ï..Age[i]>60 && ï..Age[i]<=70)
	{
		index[i]=1
	}
	else if(ï..Age[i]>70&&ï..Age[i]<=80)
	{
		index[i]=2
	}
	else
	{
		index[i]=3
	}
}
index

#Bai4
setwd('C:/Users/sv/Desktop/18120254')
data4 = read.csv('data11.csv', header =T)
data4
attach(data4)
data_4=data.frame(ï..a,b,n)
data_4

tinh_tong_chieu_cao=function(ï..a,b,n)
{
   max_a=max(n)
   min_a=min(n)
}

