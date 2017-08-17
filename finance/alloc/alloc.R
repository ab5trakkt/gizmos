library(dplyr)
library(quantmod)

amount = 10000
filename <- file.path(getwd(), "model-etf.csv")

symCADperUSD <- suppressWarnings(getSymbols("CADUSD=X", src="yahoo", auto.assign = F))
CADperUSD <- 1.0/as.numeric(Cl(last(symCADperUSD)))
CADperUSD

port <- read.csv(filename, header=TRUE, stringsAsFactors=FALSE)
port <- data.frame(port)
port <- port %>%
    mutate(Price=0)

for (q in port$Ticker)
{
  sym = suppressWarnings(getSymbols(q, src="yahoo", auto.assign = F))
  port$Price[port$Ticker==q] = as.numeric(Cl(last(sym)))
}
port$Price[port$Currency=="USD"] <- CADperUSD*port$Price[port$Currency=="USD"]
port <- port %>%
    mutate(cad=Alloc/100*amount) %>%
    mutate(usd=cad/CADperUSD) %>%
    mutate(shrs=floor(cad/Price))
port
