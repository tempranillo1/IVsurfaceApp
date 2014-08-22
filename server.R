library(fields)
library(splines)
source("R/functions.R")
source("R/volatility-surface-sp500app.R")

shinyServer(function(input, output) {
  
  intrinsic.value.bid.ask <- reactive({ ifelse(input$intrinsic.value.bid.ask, 0, -Inf) })
  
  ops <- reactive({ subset(options, TRADE_DT == input$trade.dt & PC == input$PC &  
                                    VOLATILITY >= input$IV[1L] & VOLATILITY < input$IV[2L] &
                                    VEGA >= input$VEGA & L_BID >= input$L_BID &
                                    L_MEAN - INTRINSIC_VALUE > intrinsic.value.bid.ask()) 
                   
                 })
  
  
  
  vol.surf <-  reactive({ volatility.surface(dataset = ops(),
                                             y = input$y.axis,
                                             method = input$method, 
                                             poly.degree = input$poly.degree,
                                             loess.span = input$loess.span,
                                             bs.degree = input$bs.degree,
                                             bs.df = input$bs.df,
                                             ns.df = input$ns.df,
                                             smooth.spline.df = input$smooth.spline.df,
                                             smooth.spline.spar = input$smooth.spline.spar,
                                             trade.date = input$trade.dt) })
  
  output$fig <- renderPlot({
    theta <- input$theta  
      plot(vol.surf(), theta = theta, q.t = 0.15, q.y.left = 0.1, q.y.right = 0.05,z.lim = c(0, 0.8)) 
    }) 

  output$trade.day.summary <- renderPrint({
    undly <- ops()[1L, "UNDL_PRC"]
    interest.rate <-  round(ops()[1L, "INTEREST_RATE"], digits = 4)
    dividend.yield <-   round(ops()[1L, "DIVIDEND_YIELD"], digits = 4)
    
    d.f <-  rbind(c("Underlying price: ", undly),
                 c("Interest rate: ", interest.rate),
                 c("Divident yield: ", dividend.yield))
    d.f <- as.data.frame(d.f)
    name <- as.character(d.f[, 1])
    d.f <- subset(d.f, select = "V2")
    row.names(d.f) <- name
    
   names(d.f) <- "value"
    d.f
  })
  
  output$options.quotes = renderDataTable({
    ops()
  })
  
})

  
