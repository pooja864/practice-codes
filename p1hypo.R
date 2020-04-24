x<- c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)
mu=mean(x)
mu
##Ho<=0.3
##ha>=0.3
sd=sd(x)
sm= 0.3
n=9
 t = (mu-sm)*sqrt(9)/sd
 t
pt=1- pt(t,n-1)
pt
sl=0.05

if(pt<sl){
  print("reject null hypothesis")
}else{
  print("accept null hypo")
}

t.test(x,alternative = "greater",mu=0.3)
 