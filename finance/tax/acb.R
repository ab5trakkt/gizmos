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

#
# Script for calculating average cost basis (ACB)
# as well as ESPP and RSU benefits
#

library(dplyr)
library(lubridate)

options(max.print=10000)

trns_filename <- file.path(getwd(), "trns-smpl.csv")
xchg_filename <- file.path(getwd(), "xchg-smpl.csv")

xchg <- read.csv(xchg_filename, header=TRUE, stringsAsFactors=FALSE)
xchg <- data.frame(xchg)

trns <- read.csv(trns_filename, header=TRUE, stringsAsFactors=FALSE)
trns <- data.frame(trns)
trns <- left_join(trns, xchg, by = c("transaction_date"="date"))

if (trns$currency[1] == "USD")
{
  trns$price = trns$price*trns$usd2cad  
  trns$fmv = trns$fmv*trns$usd2cad
  trns$comission = trns$comission*trns$usd2cad
}

trns <- trns %>%
  mutate(year=year(as.Date(transaction_date)),
         acb=0,
         shares=0,
         acb_per_share=0, 
         ap=0,
         cap_gain=0,
         div=0,
         covered=0,
         benefit_rsu=0,
         benefit_espp=0,
         cost_espp=0)

ap <- 0
acb <- 0
shares <- 0
last_rsu <- -1
for (i in 1:length(trns$transaction_date))
{
    cap_gain <- 0
    covered <- 0

    transaction_type <- trns$transaction_type[i]
    if (transaction_type == "BUY")
    {
        # For BUY transactions, benefit is based on price paid, 
        # while cap-gains are based on fmv

        if (trns$type[i] == "RSU")
        {
            print(trns$price[i])
            # For RSU, price paid is 0
            if (trns$price[i] != 0)
            {
                stop("ERROR: RSU price paid should be 0")
            }

            last_rsu <- i
            price <- trns$fmv[i] 
            trns$benefit_rsu[i] <- trns$fmv[i]*trns$num_shares[i]
        }
        else
        {
            if (trns$type[i] == "ESPP")
            {
                trns$benefit_espp[i] <- (trns$fmv[i]-trns$price[i])*trns$num_shares[i]
                trns$cost_espp[i] <- trns$price[i]*trns$num_shares[i]
            }
            price <- trns$price[i]
        }

        ap <- (shares*ap + price*trns$num_shares[i])/(trns$num_shares[i]+shares)
        acb <- acb + trns$comission[i] + trns$fmv[i]*trns$num_shares[i]
        shares <- shares + trns$num_shares[i]

        trns$acb[i] <- acb
        trns$shares[i] <- shares
        trns$acb_per_share[i] <- acb/shares
        trns$ap[i] <- ap
    }
    else if (transaction_type == "SELL" || transaction_type == "TAX")
    {
        if (shares == trns$num_shares[i])
        {
            ap <- 0
        }
        else
        {
            ap <- (shares*ap)/(shares-trns$num_shares[i])
        }

        if (transaction_type == "TAX")
        {
            # Sell-to-cover for tax, reverse last RSU transaction
            # but record capital gain due to difference in bought and sold price
            if (trns$num_shares[i] != trns$num_shares[last_rsu])
            {
                print(i)
                stop("ERROR: TAX should offset a BUY purchase")
            }
            covered <- trns$price[i]*trns$num_shares[i]
            cover_gain <- (trns$price[i] - trns$fmv[last_rsu])*trns$num_shares[last_rsu]
            cap_gain <- cover_gain
            acb <- acb - trns$comission[last_rsu] - trns$fmv[last_rsu]*trns$num_shares[last_rsu]
        }
        else
        {
            cap_gain <- (trns$price[i]*trns$num_shares[i]) - trns$comission[i] - ((acb/shares)*trns$num_shares[i])
            if (shares == trns$num_shares[i])
            {
                acb <- 0
            }
            else
            {
                acb <- acb - (acb/shares)*trns$num_shares[i]
            }
        }

        shares <- shares - trns$num_shares[i]

        if (shares == 0)
        {
            trns$acb_per_share[i] <- 0
        }
        else
        {
            trns$acb_per_share[i] <- acb/shares
        }
        trns$shares[i] <- shares
        trns$acb[i] <- acb
        trns$ap[i] <- ap
        trns$covered[i] <- covered
        trns$cap_gain[i] <- cap_gain
    }
    else if (transaction_type == "DIV")
    {
        trns$div[i] <- trns$price[i]
    }
}

trns %>%
    mutate(val=shares*fmv) %>%
    select(year, type, transaction_type, price, fmv, num_shares, shares, val, acb, acb_per_share, ap, cap_gain)

year_summary <- trns %>%
    group_by(year) %>%
    summarize(year_cap_gain=sum(cap_gain),
              year_dividend=sum(div),
              year_covered=sum(covered),
              year_benefit_rsu=sum(benefit_rsu),
              year_benefit_rsu_net=sum(benefit_rsu)-year_covered,
              year_benefit_espp=sum(benefit_espp),
              year_cost_espp=sum(cost_espp),
              year_benefit=year_benefit_rsu + year_benefit_espp)

tot_summary <- year_summary %>%
    summarize(tot_cap_gain=sum(year_cap_gain),
              tot_dividend=sum(year_dividend),
              tot_covered=sum(year_covered),
              tot_benefit_rsu=sum(year_benefit_rsu),
              tot_benefit_rsu_net=sum(year_benefit_rsu_net),
              tot_benefit_espp=sum(year_benefit_espp),
              tot_cost_espp=sum(year_cost_espp),
              tot_benefit=tot_benefit_rsu + tot_benefit_espp)

print(year_summary, width=Inf)
print(tot_summary, width=Inf)
