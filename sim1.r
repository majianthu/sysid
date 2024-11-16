library(copent)
library(nonlinearTseries)
#library(rgl)
library(plot3D)

derivative<-function(x,y){
  dydx = 0
  for(i in 2:length(x)){
    dydx[i-1] = (y[i]-y[i-1]) / (x[i]-x[i-1])
  }
  dydx
}

# 1. Lorenz system
s1 = 10 * runif(3)
sys1 = lorenz(sigma = 10, beta = 8/3, rho = 28, start = s1, time = seq(0,30,by=0.01), do.plot = TRUE)

# 2. Rossler system
sys1 = rossler(a = 0.38, time = seq(0,30,by=0.01), do.plot = TRUE)

#rglwidget()

x11()
scatter3D(sys1$x, sys1$y, sys1$z, col = 'black', ticktype = "detailed", pch = 20,
          xlab = 'x(t)', ylab = 'y(t)', zlab = 'z(t)', theta = 15, phi = 15, r = 10)

x11(width = 15, height = 5); 
par(mfrow = c(1,3))
plot(sys1$time,sys1$x,type = "l", xlab = "t", ylab = "x")
plot(sys1$time,sys1$y,type = "l", xlab = "t", ylab = "y")
plot(sys1$time,sys1$z,type = "l", xlab = "t", ylab = "z")

dxdt = derivative(sys1$time,sys1$x)
dydt = derivative(sys1$time,sys1$y)
dzdt = derivative(sys1$time,sys1$z)

dn = length(dxdt)
# dx/dt
cex = 0
cex[1] = copent(cbind(dxdt,sys1$x[1:dn]))
cex[2] = copent(cbind(dxdt,sys1$y[1:dn]))
cex[3] = copent(cbind(dxdt,sys1$z[1:dn]))
cex[4] = copent(cbind(dxdt,sys1$x[1:dn]*sys1$y[1:dn]))
cex[5] = copent(cbind(dxdt,sys1$x[1:dn]*sys1$z[1:dn]))
cex[6] = copent(cbind(dxdt,sys1$z[1:dn]*sys1$y[1:dn]))
# dy/dt
cey = 0
cey[1] = copent(cbind(dydt,sys1$x[1:dn]))
cey[2] = copent(cbind(dydt,sys1$y[1:dn]))
cey[3] = copent(cbind(dydt,sys1$z[1:dn]))
cey[4] = copent(cbind(dydt,sys1$x[1:dn]*sys1$y[1:dn]))
cey[5] = copent(cbind(dydt,sys1$x[1:dn]*sys1$z[1:dn]))
cey[6] = copent(cbind(dydt,sys1$z[1:dn]*sys1$y[1:dn]))
# dz/dt
cez = 0
cez[1] = copent(cbind(dzdt,sys1$x[1:dn]))
cez[2] = copent(cbind(dzdt,sys1$y[1:dn]))
cez[3] = copent(cbind(dzdt,sys1$z[1:dn]))
cez[4] = copent(cbind(dzdt,sys1$x[1:dn]*sys1$y[1:dn]))
cez[5] = copent(cbind(dzdt,sys1$x[1:dn]*sys1$z[1:dn]))
cez[6] = copent(cbind(dzdt,sys1$z[1:dn]*sys1$y[1:dn]))

# plot
cename = c("x","y","z","xy","xz","yz")
names(cex) = names(cey) = names(cez) = cename
x11(width = 10, height = 10); 
par(mfrow = c(2,2))
barplot(cex,main = "dx/dt")
barplot(cey,main = "dy/dt")
barplot(cez,main = "dz/dt")
