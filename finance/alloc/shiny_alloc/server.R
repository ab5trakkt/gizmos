shinyServer(function(input, output, session) { 
  observe({
    if (input$Amount_Input == 0) {
      disable("update")
    } else {
      enable("update")
    }
  })
  
  port_react <- eventReactive(input$update, ignoreNULL = FALSE, {
    port <- mutate(port, Price = 0, Div = 0)
  
  for (q in port$Ticker)
  {
    sym = suppressWarnings(getSymbols(q, src="yahoo", auto.assign = F))
    div = suppressWarnings(getDividends(q, src="yahoo", auto.assign = F, from=Sys.Date()-365))
    port$Price[port$Ticker==q] = as.numeric(Cl(last(sym)))
    if (length(last(div)) != 0)
    {
      port$Div[port$Ticker==q] <- colSums(div)
      # port$Div[port$Ticker==q] <- d
    }
  }
  
  port$Price[port$Currency=="USD"] <- CADperUSD*port$Price[port$Currency=="USD"]
  port$Div[port$Currency=="USD"] <- CADperUSD*port$Div[port$Currency=="USD"]
  port <- port %>%
    mutate(cad = Alloc / 100 * input$Amount_Input,
           usd = round(cad / CADperUSD,0),
           shrs = floor(cad / Price))

  sum <- port %>%
    tally(cad)
  cash <- input$Amount_Input - sum %>%
    pluck(1)

  cash_frame <- data.frame(Comment = "Cash",
                           Currency = "CAD",
                           Ticker = "CADUSD=X",
                           Alloc = (cash/input$Amount_Input*100),
                           Price = 1,
                           Div = 0,
                           cad = cash,
                           usd = (cash / CADperUSD),
                           shrs = 1)

  port <- port %>%
    bind_rows(cash_frame) %>%
    mutate(trail_yield = round(Div / Price * 100, 1),
           yearly_dist = Div * shrs,
           combined_lab = paste(percent(Alloc/100), "CA$", cad, "[ US$", round(usd,0), "]"))
  })
  
  total_yearly_dist <- reactive({
    port_react() %>%
    tally(yearly_dist) %>%
    pluck(1)
  })

  r_total_yearly_dist <- reactive({
    round(total_yearly_dist() / input$Amount_Input * 100, 1)
  })
  
  ########################################################

    #revised version bar
   
  my_chart <- eventReactive(input$update, ignoreNULL = FALSE, {
   modelbar <- ggplot(port_react(), aes(x =Ticker, y= Alloc, fill = Comment)) +
      geom_col(width = 0.4) +
      coord_flip() +
      geom_text(aes(label = combined_lab, hjust = -0.05), colour = "#4d4e50") +
      scale_y_continuous(limits = c(0,50), expand = c(0,0), labels = NULL) +
      labs(title = paste("Model Portfolio based on", dollar(input$Amount_Input), "CAD"),
           x = NULL,
           y = NULL) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "#4d4e50"))
   
    # Number of shares to purchase
    port2 <- port_react() %>%
      filter(Ticker != "CADUSD=X")

    shares <- ggplot(port2, aes(x =Ticker, y=shrs)) +
      geom_col(width = 0.8, fill = "#d9d9d9") +
      geom_text(aes(label = shrs), vjust = 1, colour = "#4d4e50", size = 3.5) +
      scale_y_continuous() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Number of shares to hold as of today",
           subtitle = Sys.Date(),
           x = NULL,
           y = NULL)

    # Trail Yield
    plotyield <- ggplot(port2, aes(x =Ticker, y=trail_yield)) +
      geom_col(width = 0.8, fill = "#d9d9d9") +
      geom_text(aes(label = trail_yield), vjust = 1, colour = "#4d4e50") +
      scale_y_continuous() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Trail Yield (dividends/price*100)",
           x = NULL,
           y = NULL)

    #Total Year Distribution
    plotdist <- ggplot(port2, aes(x =Ticker, y=yearly_dist)) +
      geom_col(width = 0.8, fill = "#d9d9d9") +
      geom_text(aes(label = dollar(round(yearly_dist,1))), vjust = 1, colour = "#4d4e50") +
      annotate("text", x = 5.5, y = Inf, label = paste("Your Total Year Distribution =", dollar(round(total_yearly_dist(),1))), vjust = "top") +
      scale_y_continuous() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Dividends: Yearly Distribution",
           x = NULL,
           y = NULL)

    plot3 <- plot_grid(shares, plotyield)
    plot4 <- plot_grid(modelbar, plot3, plotdist, nrow = 3, scale = 0.9)
    plot4
  })

    output$my_chart <- renderPlot({ my_chart() })
    # output$report <- downloadHandler(
    #  filename = "report.pdf",
    #  content = function(file) {
    #      file.copy("report.pdf", file)
    #   }
    #     )
})


# ignoreNULL = FALSE
# >don't ignore null = run the app with the null 20,000 number
# >when running the app, i won't see anything because I didn't click the update.
# >don't run the app with the null number (here 20,000) unless I click the update