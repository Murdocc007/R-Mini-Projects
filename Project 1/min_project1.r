i=0
time_taken=array(10000)

#use replicate instead of the while loop
while(i<10000)
{
  x1=rexp(1,1/5)
  x2=rexp(1,1/5)
  x3=rexp(1,1/5)
  time_taken[i]=max(x1,x2,x3)
  i=i+1
}

hist(time_taken,probability = T,col=gray(0.9),main = "Exponential mean=5")

curve(0.6*(1-exp(-1*0.2*x))*exp(-0.2*x),col="red",add = T)

final_time=mean(time_taken)

variance=var(time_taken)
  

# Alternative way of getting the same result

# time_taken=array(10000)
# time_taken<-replicate(10000,max(rexp(1,1/5),rexp(1,1/5),rexp(1,1/5)))
# density_func <-function(x){0.6*(1-exp(-1*0.2*x))*exp(-0.2*x)}
# hist(time_taken,probability = T,col=gray(0.9),main = "Exponential mean=5",ylim = c(0,max(density_func(time_taken))))
# curve(density_func,col="red",add = T)
# 
# final_time=mean(time_taken)
# 
# variance=var(time_taken)


