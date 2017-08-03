# Wage
W <- 50000 # Yearly wage
HR <- 0.33 # Ratio of take-home-pay to housing
M <- HR*W/12

# Rent range
R_MIN <- 500
R_MAX <- 4000

# Price range
P_MIN <- 0
P_MAX <- 1100

# 10 year GOC bond rate
I_MIN <- 2.0
I_MAX <- 5.0

x <- seq(R_MIN, R_MAX, by=200)
i <- seq(I_MIN, I_MAX, by=0.5)

plot(0,0,xlim=c(R_MIN, R_MAX), yaxt='n', ylim=c(P_MIN, P_MAX), xlab="Rent", ylab="Max price (1000s)")
axis(2, ylim=c(P_MIN, P_MAX), at=seq(P_MIN, P_MAX, by=250))

# Ross K Rule of 2.3
cols = rainbow(length(i))
for (j in seq(1,length(i)))
{
  y <- 1.2*x/(2.3+i[j])
  lines(x, y, type='l', col=cols[j])
}

# "Rule of 15"
x <- seq(R_MIN, R_MAX, by=50) 
y <- 12*x*15/1000
lines(x, y, type='p', col="#000000")
abline(v=M, col="#000000")
abline(h=W*4/1000, col="#000000")

iStr = sprintf("%.2f", i)
abline(h=seq(P_MIN, P_MAX, by=100), v=seq(R_MIN, R_MAX, by=100), col="gray", lty=3)
legend('topright', iStr, title="10-year Rate", lty=1, col=cols, bty='n', cex=.75)