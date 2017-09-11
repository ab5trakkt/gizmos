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

options(scipen = 999)

#W <- 50000 
#HR <- 0.33
#M <- HR*W/12

M                 <- 3000        # Monthly payment
bShorterTerm      <- FALSE       # Set to TRUE to fix a shorter term (by SHORTER_TERM), FALSE to overpay monthly (by SCALED_PAYMENT)
SHORTER_TERM      <- 15          # Number of years to pay off (if bShorterTerm)
SCALED_PAYMENT    <- 1.2         # Increase payment by this factor (if !bShorterTerm)
I_MIN             <- 0.0         # Minimum interest rate to graph
I_MAX             <- 10.0        # Maximum interest rate to graph
YEARS_DEFAULT     <- 25          # Default term (in years)
PERIODS_PER_YEAR  <- 12          # Payment terms per year

N_DEFAULT         <- YEARS_DEFAULT*PERIODS_PER_YEAR

if (bShorterTerm) {
  YEARS_FAST <- SHORTER_TERM
  print(sprintf("Overpay: %d years", YEARS_FAST))
} else {
  FAST_M <- floor(M*SCALED_PAYMENT)
  print(sprintf("Overpay: %d pmnth", FAST_M))
}

source("utility.R")

i <- seq(I_MIN, I_MAX, by=0.25)
y <- M*inv_sum_k(i_eff(i, PERIODS_PER_YEAR), N_DEFAULT)

P_MIN <- 0
P_MAX <- max(y)

if (bShorterTerm) {
  fast_n <- YEARS_FAST*PERIODS_PER_YEAR
  fast_m <- y / (inv_sum_k(i_eff(i,PERIODS_PER_YEAR), fast_n))
  fast_y <- fast_m*fast_n
  fast_y_tot <- fast_m*fast_n
} else {
  fast_m <- rep(FAST_M,length(i))
  fast_y <- rep(0, length(i))
  fast_n <- rep(0, length(i))
  
  # Find the number of years required to pay off when overpaying monthly
  for (j in seq(1,length(i)))
  {
    for (yrs in seq(1,YEARS_DEFAULT))
    {
      if (y[j] <= fast_m[j]*inv_sum_k(i_eff(i[j], PERIODS_PER_YEAR), PERIODS_PER_YEAR*yrs))
      {
        fast_n[j] <- yrs*PERIODS_PER_YEAR
        fast_y[j] <- fast_m[j]*inv_sum_k(i_eff(i[j], PERIODS_PER_YEAR), fast_n[j])
        break
      }
    }
  }
  fast_y_tot <- fast_m*fast_n
}

fast_savings <- N_DEFAULT*M-(fast_n*fast_m)

par(cex=0.7, mai=c(0.5, 0.5, 0.2, 0.1), mfrow=c(2,1))
plot(0,0,xlim=c(I_MIN, I_MAX), yaxt='n', ylim=c(P_MIN, P_MAX), xlab="X", ylab="Y")
axis(2, ylim=c(P_MIN, P_MAX), at=seq(P_MIN, P_MAX, by=100000))

cols=rainbow(6)
cols[2]="#FFAA55"

legendStr = c()
lines(i, y, type='o', col=cols[1]); legendStr = c(legendStr, "Max mortgage @ 25")
lines(i, N_DEFAULT*rep(M,length(y)), type='o', col=cols[2]); legendStr = c(legendStr, "Total cost")
lines(i, fast_y_tot, type='o', col=cols[3]); legendStr = c(legendStr, "Total cost (overpay)")
lines(i, N_DEFAULT*M-y-fast_savings, type='o', col=cols[4]); legendStr = c(legendStr, "Interest (overpay)")
lines(i, N_DEFAULT*M-y, type='o', col=cols[5]); legendStr = c(legendStr, "Interest @ 25")
#lines(i, fast_y, type='o', col=cols[2]); legendStr = c(legendStr, "Morgage (overpay)")
#lines(i, fast_savings, type='o', col=cols[3]); legendStr = c(legendStr, "Savings (overpay)")
#lines(i, y-fast_savings, type='o', col=cols[4]); legendStr = c(legendStr, "")
#abline(h=12*25*M, col="#ff0000")

abline(h=seq(P_MIN, P_MAX, by=100000), v=seq(I_MIN, I_MAX, by=0.25), col="gray", lty=3)
legend('topright', legendStr, title="", lty=1, col=cols, bty='n', cex=.75)

M_MIN=0
M_MAX=6000
plot(0,0,xlim=c(I_MIN, I_MAX), yaxt='n', ylim=c(M_MIN, M_MAX), xlab="X", ylab="Y")
axis(2, ylim=c(M_MIN, M_MAX), at=seq(0, M_MAX, by=1000))
abline(h=seq(M_MIN, M_MAX, by=1000), v=seq(I_MIN, I_MAX, by=0.25), col="gray", lty=3)
lines(i, rep(M,length(fast_m)), type='o', col=cols[1])
lines(i, fast_m, type='o', col=cols[2])
legend('topright', c("Monthly cost", "Monthly cost (overpay)"), title="", lty=1, col=cols, bty='n', cex=.75)
