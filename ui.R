possible.methods <- c("Loess regression" = "loess", 
                      "Polynomial regression" =  "poly", 
                      "B-splines" = "bs",
                      "Natural cubic spline" = "ns", 
                      "Smoothing cubicspline" = "smooth.spline")

shinyUI(pageWithSidebar(
 
  headerPanel("S&P 500 implied volatility surface"),
  sidebarPanel(
    tags$head(
      tags$style(type = "text/css", "select { max-width: 150px; }"),
      tags$style(type = "text/css", "pre { max-width: 220px; }"),
      tags$style(type = "text/css", "input { max-width: 100px; }"),  
      tags$style(type = "text/css", ".span4 { max-width: 270px; }"),
      tags$style(type = "text/css", ".dataTable {font-size: 10px; }")
    ),
    div(selectInput("trade.dt", "Choose a trade date:",  choices = as.character(unique(options$TRADE_DT))),
         style = "text-align: center;"),
    
    wellPanel( 
      selectInput("y.axis", "Choose variable on y-axis:",  choices = c("Strike Price" = "STRK_PRC", 
                                                                                "Log-moneyness" = "LOG_M", 
                                                                                "Delta Forward" = "DELTA_FW",
                                                                                "Delta" = "DELTA")),
      radioButtons("PC", "and option's type:",
                   list("calls" = "C",
                        "puts" = "P"), selected = "P")
      ),
    wellPanel(
      h6("Trim input dataset"),
        sliderInput("IV", "Implied Volatility bandwidth:", min = 0, max = 3, value = c(0.01, 1.2), step = 0.01, ticks = FALSE),
        numericInput("L_BID", "Option's bid price greater or equal to:", min = 0, value = 0.5, step = 0.1),
        numericInput("VEGA", "Option's vega greater or equal to:", min = 0, value = 20),
        checkboxInput("intrinsic.value.bid.ask", "Remove quotes where the midpoint of the bid/ask price is below intrinsic value.", value = FALSE)
      ),
    wellPanel(
      selectInput("method", "Choose a regression method:", choices = possible.methods, selected = possible.methods[2L] ),
      conditionalPanel(
        condition = "input.method == 'poly'", 
        sliderInput("poly.degree", "polynominal degree:", min = 1L, max = 12L, step = 1L, value = 2L)
      ),
      conditionalPanel(
        condition = "input.method == 'loess'", 
        sliderInput("loess.span", "loess span parameter:", min = 0.15, max = 5, step = 0.05, value = 0.75)
      ),
      conditionalPanel(
        condition = "input.method == 'bs'", 
        sliderInput("bs.degree", "degree of the piecewise polynomial:", min = 1L, max = 10L, step = 1L, value = 3L),
        sliderInput("bs.df", "degrees of freedom:", min = 1L, max = 10L, step = 1L, value = 3L)
      ),
      conditionalPanel(
        condition = "input.method == 'ns'", 
        sliderInput("ns.df", "degrees of freedom", min = 0L, max = 10L, step = 1L, value = 3L)
      ),
      conditionalPanel(
        condition = "input.method == 'smooth.spline'",
        sliderInput("smooth.spline.df", "the desired equivalent number of degrees of freedom:", min = 1L, max = 9L, step = 1L, value = 3L),
        sliderInput("smooth.spline.spar", "smoothing parameter:", min = 0.01, max = 2, step = 0.05, value = 0.3)
      )
    )
    
  ),

   mainPanel(
     tabsetPanel(
        tabPanel("About",
                 p("This application shows implied volatility surface generated from S&P500 options quoted on Chicago Board Option Exchange (CBOE).  
                   Main reason this app has been created is to demonstrate how different smoothing techniques affect on volatility surface."),
                 strong("Be carefull. Using inappropriate smoothing method may cause unstable behavior of IV surface!"),
                 hr(),
                 
                 h5("How to use app:"),
                 strong("1. "), span("Go to 'volatility surface' panel."), br(),
                 strong("2. "), span("Choose trade date."), br(),
                 strong("3. "), span("Choose a variable to be assigned to y-axis (by the default it is strike price.) and option's type (put or call)."), br(),
                 strong("4. "), span("Trim dataset to remove outstnading quotes."), br(),
                 strong("5. "), span("Choose regression method. Attached presentation contains more details about each smootching method."), br(),
                 br(),
                 strong("Note."), span("You can rotate image with implied volatility surface using slider below. To rotate constantly use 'play' button.")
        ),
        tabPanel("volatility surface", plotOutput("fig"),
                 sliderInput("theta", "rotate:", min = 0L, max = 360L, value = 45L, step = 5L, width = "100%",
                             animate = animationOptions(loop = T, interval = 400L)),
                 
                 br(), br(),
                 h4("Statistics"),
                 verbatimTextOutput("trade.day.summary"),
                 br()
        ),
        tabPanel("Option's quotes",  dataTableOutput("options.quotes"))
      )
    )
  
  )
)