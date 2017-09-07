#
# MIT License
# 
# Copyright (c) 2017 ab5trakkt
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

frame()

x <- seq(0.0,0.8,length=25)

## Gain factor
K <- 5

## Capital gains inclusion
r <- 1/2
#r <- 2/3
#r <- 3/4
Khat=(K-1)/K

## Future tax spread
delta <- 0.1

## RRSP performance over cash
RRSPvCash_Eq <-((1-x)/((1-x)*(1-Khat*x*r))-1)*100

RRSPvCash_U1 <- ((1-(x+delta))/((1-x)*(1-Khat*(x+delta)*r))-1)*100
RRSPvCash_D1 <- ((1-(x-delta))/((1-x)*(1-Khat*(x-delta)*r))-1)*100
RRSPvCash_U2 <- ((1-(x+2*delta))/((1-x)*(1-Khat*(x+2*delta)*r))-1)*100
RRSPvCash_D2 <- ((1-(x-2*delta))/((1-x)*(1-Khat*(x-2*delta)*r))-1)*100

plot(0,0,
     main="RRSP vs non-registered performance difference on 400% cap gain",
     xlab="Current tax rate (points)",
     ylab="RRSP performance difference (%)",
     xlim = c(20,70), ylim = c(-100,100), type = "n")
cl = c("#ff5500", "#FF0000", "#000000", "#0000FF", "#0055ff")

lines(100*x,RRSPvCash_U2,type="o", col=cl[1])
lines(100*x,RRSPvCash_U1,type="o", col=cl[2])
lines(100*x,RRSPvCash_Eq,type="o", col=cl[3])
lines(100*x,RRSPvCash_D1,type="o", col=cl[4])
lines(100*x,RRSPvCash_D2,type="o", col=cl[5])
lines(100*x,x/x,type="l", lty=5, col="#555555")
legend("topleft",legend=c("Future tax rate 20 points more",
                          "Future tax rate 10 points more",
                          "Future tax rate unchanged",
                          "Future tax rate 10 points less",
                          "Future tax rate 20 points less"
                          ),
                          fil=cl)

abline(h=seq(-100, 100, by=25),v=seq(20,70,by=5), col="lightgray", lty=3)

# TFSA performance over RRSP
plot(0,0,
     main="RRSP vs TFSA performance difference",
     xlab="Current tax rate (%)",
     ylab="RRSP performance difference (%)",
     xlim = c(20,70),ylim = c(-100,100))
RRSPvTFSA_Eq <-(((1-x)/(1-x))-1)*100

RRSPvTFSA_U1 <-(((1-(x+delta))/(1-x))-1)*100
RRSPvTFSA_D1 <-(((1-(x-delta))/(1-x))-1)*100
RRSPvTFSA_U2 <-(((1-(x+2*delta))/(1-x))-1)*100
RRSPvTFSA_D2 <-(((1-(x-2*delta))/(1-x))-1)*100

lines(100*x,RRSPvTFSA_U2,type="o", col=cl[1])
lines(100*x,RRSPvTFSA_U1,type="o", col=cl[2])
lines(100*x,RRSPvTFSA_Eq,type="o", col=cl[3])
lines(100*x,RRSPvTFSA_D1,type="o", col=cl[4])
lines(100*x,RRSPvTFSA_D2,type="o", col=cl[5])

legend("topleft",legend=c("Future tax rate 20 points more",
                          "Future tax rate 10 points more",
                          "Future tax rate unchanged",
                          "Future tax rate 10 points less",
                          "Future tax rate 20 points less"),
       fil=cl)

abline(h=seq(-100, 100, by=25),v=seq(20,70,by=5), col="lightgray", lty=3)

## Max future tax rate to breakeven with RRSP
x <- seq(0.0,1.0,length=25)
f <- (x/(1-r+r*x)-x)*100
freal <- (x/(1-(1-x)*r*0.8)-x)*100
freal2 <- (x/(1-(1-x)*r*0.5)-x)*100
plot(0,0,
     main="RRSP vs non-registered break-even",
     xlab="Current tax rate (points)",
     ylab="Max future tax increase (points)",
     xlim = c(0,100),ylim = c(-2,40),type = "n")
lines(100*x,f,type="o", col=cl[3])
lines(100*x,freal,type="o", col=cl[2])
lines(100*x,freal2,type="o", col=cl[1])
legend("topleft",legend=c("Infinite gain",
                          "400% gain",
                          "100% gain"),
       fil=c(cl[3],cl[2],cl[1]))
grid(20, NULL, col = "lightgray", lty = "dotted")
