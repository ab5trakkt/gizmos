visualize <- TRUE
amount = 10000
filename <- file.path(getwd(), "model-etf.csv")

########################################################
# Calculation
library(dplyr)
library(quantmod)

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

sum <- port %>%
    summarize(sum(cad))
cash <- amount-sum
cash

cash_frame = data.frame(Comment="Cash", Currency="CAD", Ticker="CADUSD=X", Alloc=(cash/amount*100), Price=1.0, cad=cash, usd=(cash/CADperUSD), 1)
names(cash_frame) <- c("Comment", "Currency", "Ticker", "Alloc", "Price", "cad", "usd", "shrs")
port <- port %>%
  rbind(cash_frame)
port

########################################################
# Visualization
if(visualize == TRUE) {
    library(ggplot2)
    library(cowplot)

    # Model allocation
    model <- ggplot(port, aes(x ="", y=Alloc, fill = Comment)) +
        geom_col(width = 0.3) +
        scale_fill_brewer(palette = "Set3") +
        theme_void() +
        geom_text(aes(label = percent(Alloc/100), hjust = -0.2), size = 4, position = position_stack(vjust = 0.5), colour = "#4d4e50") +
        geom_text(data = port, aes(label = Ticker, hjust = 1), position = position_stack(vjust = 0.5), colour = "#4d4e50") +
        labs(title = "Model Portfolio")

    # Ideal allocation in CAD
    allocCad <- ggplot(port, aes(x ="", y=cad, fill = Comment)) +
        geom_col(width = 0.4) +
        scale_fill_brewer(palette = "Set3") +
        geom_text(aes(label = dollar(cad)), size = 4, position = position_stack(vjust = 0.5), colour = "#4d4e50") +
        geom_text(data = port, aes(label = percent(Alloc/100), hjust = -1.5), position = position_stack(vjust = 0.5), colour = "#4d4e50") +
        labs(title = "Ideal Allocation in CAD") +
        theme_void() +
        theme(legend.position = "none")


    # Ideal allocation in USD
    allocUsd <- ggplot(port, aes(x ="", y=usd, fill = Comment)) +
        geom_col(width = 0.4) +
        scale_fill_brewer(palette = "Set3") +
        geom_text(aes(label = dollar(round(usd,-1))), size = 4, position = position_stack(vjust = 0.5), colour = "#4d4e50") +
        geom_text(data = port, aes(label = percent(Alloc/100), hjust = -1.5), position = position_stack(vjust = 0.5), colour = "#4d4e50") +
        labs(title = "Ideal Allocation in USD") +
        theme_void() +
        theme(legend.position = "none")


    # Number of shares to purchase
    shares <- ggplot(port, aes(x =Ticker, y=shrs)) +
        geom_col(width = 0.8, fill = "#d9d9d9") +
        geom_text(aes(label = shrs), vjust = -1, colour = "#4d4e50") +
        scale_y_continuous(limits = c(0,170)) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "Number of shares to purchase as of today",
             subtitle = Sys.Date(),
             x = NULL,
             y = NULL)

        plot2 <- plot_grid(allocCad, allocUsd)
        plot1 <- plot_grid (model, plot2, shares, nrow = 3, scale = 0.9)
        plot1
}

