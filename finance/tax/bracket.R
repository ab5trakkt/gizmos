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

WAGE_MAX=150

bracket <- function(w)
{
    t <- 0
    x <- w
    
    # Ontario + Canada cumulative brackets
    tbl <- data.frame(
        L = c(220000, 200000, 150000, 140388, 90563, 86176, 83075, 73145, 45282, 41536, 0),
        R = c(0.5353, 0.5197, 0.4797, 0.4641, 0.4341, 0.3791, 0.3389, 0.3148, 0.2965, 0.2415, 0.2005))

    for (i in seq(1, length(tbl$L)))
    {
        l <- tbl$L[i]
        r <- tbl$R[i]
        if (x > l)
        {
            t <- t + (x-l)*r
            x <- l
        }
    }
    return(t/w)
}

wage_before <- seq(1,WAGE_MAX*1000, by=5000)
wage_after <- c()
tax_paid <- c()
for (X in wage_before)
{
    wage_after <- c(wage_after, X*(1.0-bracket(X))/1000)
    tax_paid <- c(tax_paid, X*(bracket(X)/1000))
}

plot(wage_before/1000, xlim=c(0,WAGE_MAX), ylim=c(0,WAGE_MAX), type='n', axes=FALSE, xlab="Before-tax", ylab="After-tax")
lines(wage_before/1000, wage_after, type='o')
lines(wage_before/1000, tax_paid, type='o', col="#ff0000")
legend('topright', c("After-tax wage", "Tax paid"), title="", lty=1, col=c("#000000", "#ff0000"), bty='n', cex=.75)

ticks = seq(0,WAGE_MAX,by=20)

tbl <- data.frame(x=wage_before/1000, y=wage_after, z=tax_paid, r=tax_paid/wage_before*1000)
tbl

axis(side = 1, at = ticks)
axis(side = 2, at = ticks)
abline(h=ticks, v=ticks, col="#222222", lty=3)

#w <- 40000
#c(w, w*bracket(w), w*(1.0-bracket(w)))
#w <- 60000
#c(w, w*bracket(w), w*(1.0-bracket(w)))
#w <- 80000
#c(w, w*bracket(w), w*(1.0-bracket(w)))
