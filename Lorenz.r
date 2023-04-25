library(copent)
library(nonlinearTseries)
library(rgl)

derivative<-function(x,y){
  dydx = 0
  for(i in 2:length(x)){
    dydx[i-1] = (y[i]-y[i-1]) / (x[i]-x[i-1])
  }
  dydx
}

s1 = 10 * runif(3)
lor96 = lorenz(sigma = 10, beta = 8/3, rho = 28, start = s1, time = seq(0,30,by=0.01), do.plot = T)
rglwidget()

x11(width = 15, height = 5); 
par(mfrow = c(1,3))
plot(lor96$time,lor96$x,type = "l", xlab = "t", ylab = "x")
plot(lor96$time,lor96$y,type = "l", xlab = "t", ylab = "y")
plot(lor96$time,lor96$z,type = "l", xlab = "t", ylab = "z")

dxdt = derivative(lor96$time,lor96$x)
dydt = derivative(lor96$time,lor96$y)
dzdt = derivative(lor96$time,lor96$z)

dn = length(dxdt)
# dx/dt
cex = 0
cex[1] = copent(cbind(dxdt,lor96$x[1:dn]))
cex[2] = copent(cbind(dxdt,lor96$y[1:dn]))
cex[3] = copent(cbind(dxdt,lor96$z[1:dn]))
cex[4] = copent(cbind(dxdt,lor96$x[1:dn]*lor96$y[1:dn]))
cex[5] = copent(cbind(dxdt,lor96$x[1:dn]*lor96$z[1:dn]))
cex[6] = copent(cbind(dxdt,lor96$z[1:dn]*lor96$y[1:dn]))
# dy/dt
cey = 0
cey[1] = copent(cbind(dydt,lor96$x[1:dn]))
cey[2] = copent(cbind(dydt,lor96$y[1:dn]))
cey[3] = copent(cbind(dydt,lor96$z[1:dn]))
cey[4] = copent(cbind(dydt,lor96$x[1:dn]*lor96$y[1:dn]))
cey[5] = copent(cbind(dydt,lor96$x[1:dn]*lor96$z[1:dn]))
cey[6] = copent(cbind(dydt,lor96$z[1:dn]*lor96$y[1:dn]))
# dz/dt
cez = 0
cez[1] = copent(cbind(dzdt,lor96$x[1:dn]))
cez[2] = copent(cbind(dzdt,lor96$y[1:dn]))
cez[3] = copent(cbind(dzdt,lor96$z[1:dn]))
cez[4] = copent(cbind(dzdt,lor96$x[1:dn]*lor96$y[1:dn]))
cez[5] = copent(cbind(dzdt,lor96$x[1:dn]*lor96$z[1:dn]))
cez[6] = copent(cbind(dzdt,lor96$z[1:dn]*lor96$y[1:dn]))

# plot
cename = c("x","y","z","xy","xz","yz")
names(cex) = names(cey) = names(cez) = cename
x11(width = 10, height = 10); 
par(mfrow = c(2,2))
barplot(cex,main = "dx/dt")
barplot(cey,main = "dy/dt")
barplot(cez,main = "dz/dt")

