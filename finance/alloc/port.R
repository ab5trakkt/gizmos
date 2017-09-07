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

library(dplyr)
library(quantmod)

filename <- file.path(getwd(), "port-smpl.csv")

## Target allocations (%)
trE <- 80 # Equity
trF <- 15 # Fixed-income
trC <- 5  # Cash

symCADperUSD <- suppressWarnings(getSymbols("CADUSD=X", src="yahoo", auto.assign = F))
CADperUSD <- 1.0/as.numeric(Cl(last(symCADperUSD)))
CADperUSD

port <- read.csv(filename, header=TRUE, stringsAsFactors=FALSE)
port <- data.frame(port)

quotes <- port %>%
  filter(SecType != 'C') %>%
  filter(Name != 'NULL') %>%
  distinct(Name) %>%
  mutate(Price=0,Div=0)

for (q in quotes$Name)
{
  sym = suppressWarnings(getSymbols(q, src="yahoo", auto.assign = F))
  div = suppressWarnings(getDividends(q, src="yahoo", auto.assign = F, from=Sys.Date()-365))
  quotes$Price[quotes$Name==q] <- as.numeric(Cl(last(sym)))
  if (length(last(div)) != 0)
  {
      d <- colSums(div)
      quotes$Div[quotes$Name==q] <- d
  }
}

port <- left_join(port, quotes)
port$Div[is.na(port$Div)] <- 0
port$Div[port$Currency=="U"] <- CADperUSD*port$Div[port$Currency=="U"]
port$Price[is.na(port$Price)] <- 1
port$Price[port$Currency=="U"] <- CADperUSD*port$Price[port$Currency=="U"]
port$Tgt[port$Currency=="U"] <- CADperUSD*port$Tgt[port$Currency=="U"]
port$ACB[port$Currency=="U" & port$SecType != "C"] <- CADperUSD*port$ACB[port$Currency=="U" & port$SecType != "C"]
port <- port %>%
    mutate(trail_yield=round(Div/Price*100,1)) %>%
    mutate(val=round(Price*Amount/1000,1)) %>%
    mutate(gain=round((Price-ACB)*Amount,1)) %>%
    mutate(gain_pct=round(gain/(ACB*Amount)*100,1)) %>%
    mutate(yearly_dist=round(Div*Amount,1)) %>%
    mutate(tgt_pct=round(100*(Tgt-Price)/Price,1)) %>%
    arrange(val)
port$tgt_pct[port$tgt_pct<0] <- 0
port$gain_pct[is.nan(port$gain_pct)] <- 0

# Port breakdown
tblC <- port %>%
  group_by(AcctType) %>%
  filter(SecType=='C') %>%
  summarize(totC=sum(val))

tblE <- port %>%
  group_by(AcctType) %>%
  filter(SecType=='E') %>%
  summarize(totE=sum(val))

tblF <- port %>%
  group_by(AcctType) %>%
  filter(SecType=='F') %>%
  summarize(totF=sum(val))

tbl <- left_join(left_join(tblE, tblF), tblC)

totE <- tbl %>%
  summarize(sum(totE)) %>%
  pull(1)

totC <- tbl %>%
  summarize(sum(totC)) %>%
  pull(1)

totF <- tbl %>%
  summarize(sum(totF)) %>%
  pull(1)

tot = totE+totC+totF

# Port performance
port <- port %>%
  mutate(r=round(val/tot*100,1)) %>%
  mutate(gain_pct_tot=round(gain_pct*0.01*r,1))
port %>%
    select(Name, ACB, Price, val, trail_yield, gain, gain_pct, yearly_dist, r, gain_pct_tot, AcctType, SecType, Div, Tgt, tgt_pct)

total_yearly_dist <- port %>%
    summarize(sum(yearly_dist)) %>%
    pull(1)

tblGC <- port %>%
  group_by(AcctType) %>%
  filter(SecType=='C') %>%
  summarize(gainC=sum(gain), gain_pctC=sum(gain_pct_tot))

tblGE <- port %>%
  group_by(AcctType) %>%
  filter(SecType=='E') %>%
  summarize(gainE=sum(gain), gain_pctE=sum(gain_pct_tot))

tblGF <- port %>%
  group_by(AcctType) %>%
  filter(SecType=='F') %>%
  summarize(gainF=sum(gain), gain_pctF=sum(gain_pct_tot))

tblG <- left_join(left_join(tblGE, tblGF), tblGC)
tblG %>%
  select(AcctType, gainE, gainF, gainC, gain_pctE, gain_pctF, gain_pctC)

tbl <- tbl %>%
  mutate(totA=round(totE+totF+totC,1)) %>%
  mutate(rE=round(totE/totA*100,1), 
         rF=round(totF/totA*100,1), 
         rC=round(totC/totA*100,1), 
         RE=round(totE/tot*100,1), 
         RF=round(totF/tot*100,1), 
         RC=round(totC/tot*100,1), 
         RA=round(totA/tot*100,1),
         targetE=round(trE/100*totA,1),
         targetF=round(trF/100*totA,1),
         targetC=round(trC/100*totA,1),
         deltaE=round(targetE-totE,1),
         deltaF=round(targetF-totF,1),
         deltaC=round(targetC-totC,1))

tbl

liquidity <- tbl %>%
  filter(AcctType=='T' | AcctType=='M' | AcctType=='S') %>%
  summarize(sum(totA)) %>%
  pull(1)

tbl %>%
  summarize(sum(totA)/CADperUSD, liquidity/CADperUSD, sum(totE)/CADperUSD, sum(totF)/CADperUSD, sum(totC)/CADperUSD, total_yearly_dist/CADperUSD, sum(RE), sum(RF), sum(RC))

r_yearly_dist <- round(total_yearly_dist/(1000*tot)*100,1)
tbl %>%
  summarize(sum(totA), liquidity, sum(totE), sum(totF), sum(totC), total_yearly_dist, r_yearly_dist,  sum(RE), sum(RF), sum(RC))
