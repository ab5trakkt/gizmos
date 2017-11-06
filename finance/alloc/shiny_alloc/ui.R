dashboardPage(
  dashboardHeader(title = "Minjung's Dashboard"),
  dashboardSidebar(
    useShinyjs(),
    h4("Input"),
    numericInput("Amount_Input", "Asset", value = 20000, min = 1000, max = Inf),
    HTML("<hr>"),
    actionButton("update", label = "Update Chart", icon("refresh"),
                  class = "btn-primary", width = "60%")
    #downloadButton("report", "Download", class = "btn-primary", width = "60%")
  ),
  
  dashboardBody(
    plotOutput("my_chart", height = "800px") %>% withSpinner()
  )
)

# what is h4?
# size of the font h1 the bigger, h5 smaller
# what is class = "btn-primary", width = "60%"
# the class argument makes the button look blue. width is how wide you want the button to be. I set it at 60% so that it doesn't become overly long.
# what is html hr?
# horizontal bar. It draws a white line to separate between widgets