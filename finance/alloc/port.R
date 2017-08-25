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
  mutate(Price=0)

for (q in quotes$Name)
{
  sym = suppressWarnings(getSymbols(q, src="yahoo", auto.assign = F))
  quotes$Price[quotes$Name==q] = as.numeric(Cl(last(sym)))
}

port <- left_join(port, quotes)
port$Price[is.na(port$Price)] <- 1
port$Price[port$Currency=="U"] <- CADperUSD*port$Price[port$Currency=="U"]
port$ACB[port$Currency=="U" & port$SecType != "C"] <- CADperUSD*port$ACB[port$Currency=="U" & port$SecType != "C"]
port <- port %>%
    mutate(val=round(Price*Amount/1000,1)) %>%
    mutate(gain=round((Price-ACB)*Amount,1)) %>%
    mutate(gain_pct=round(gain/(ACB*Amount)*100,1)) %>%
    arrange(val)
#port

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

port <- port %>%
  mutate(r=round(val/tot*100,1)) %>%
  mutate(gain_pct_tot=round(gain_pct*0.01*r,1))
port

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
  summarize(sum(totA), liquidity, sum(totE), sum(totF), sum(totC), sum(RE), sum(RF), sum(RC))
