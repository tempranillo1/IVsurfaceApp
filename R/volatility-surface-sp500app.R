volatility.surface <- function(dataset, trade.date = NULL, n.grid = 60L, 
                               y = "STRK_PRC", poly.degree = 4L, poly.raw = TRUE, 
                               method = "poly", loess.span = 0.25, loess.degree = 2L,
                               ns.df = NULL, smooth.spline.df = NULL, smooth.spline.spar = NULL,
                               bs.df = NULL, bs.knots = NULL, bs.degree = 3L, bs.intercept = FALSE, ...){       
        n <- n.grid      
        
        y <- match.arg(y, c("STRK_PRC", "LOG_M", "DELTA", "DELTA_FW"))
      
        if(is.null(trade.date)){
          trade.date <- dataset$TRADE_DT[1L]
        }
         
        dataset <- subset(dataset,  !is.na(y))
             
        exprs <- unique(dataset$EXPR_DT)
        y.axis.range <- range(dataset[y])
        y.axis <- seq(y.axis.range[1L], y.axis.range[2L], len = n)
        volatility.matrix <- matrix(numeric(), ncol = length(y.axis), nrow = length(exprs))      
        
        # y-axis smoothing
  
        for(i in 1L:length(exprs)) {
           
          o <- subset(dataset, EXPR_DT == exprs[i])
          .temp <- o[[y]]
          if(method == "poly") {
            fit.vol <- lm(VOLATILITY ~ poly(.temp, poly.degree, raw = poly.raw), data = o)
          } else if(method == "loess") {
            fit.vol <- loess(VOLATILITY ~ .temp, span = loess.span, degree = loess.degree, data = o)
          } else if(method == "bs") {
            suppressWarnings(fit.vol <- lm(VOLATILITY ~ bs(.temp, df = bs.df, degree = bs.degree, knots = bs.knots), data = o))
          } else if(method == "ns") {
            suppressWarnings(fit.vol <- lm(VOLATILITY ~ ns(.temp, df = ns.df), data = o))
          } else if (method == "smooth.spline") {
            suppressWarnings(fit.vol <- lm(VOLATILITY ~ smooth.spline(.temp, df = smooth.spline.df, spar = smooth.spline.spar)$y, data = o))
          }
          
          new <- data.frame(y.axis)
          names(new) <- ".temp"
          one.expr.vol <- predict(fit.vol, newdata = new, type = "response")
          volatility.matrix[i, ] <- one.expr.vol
        } 
        
        # time smoothing   
        volatility.matrix.smooth <- matrix(numeric(), ncol = n, nrow = n)
        correct.cols <- which(colSums(!is.na(volatility.matrix)) >= 2L)
        for(i in  correct.cols) {
           one.approx <- approx(unique(dataset$YEAR_FRAC), volatility.matrix[, i]^2L, n = n)
           volatility.matrix.smooth[, i] <- sqrt(one.approx$y)
        }  
        
        colnames(volatility.matrix.smooth) <- y.axis
        t.range <- range(dataset$YEAR_FRAC)
        rownames(volatility.matrix.smooth) <- seq(t.range[1L], t.range[2L], length.out = n)   
        
        undly.price <- dataset[1L, "UNDL_PRC"]
        r <- dataset[1L, "INTEREST_RATE"]
        b <- dataset[1L, "DIVIDEND_YIELD"]
        
        attr(volatility.matrix.smooth, "trade.date") <-  trade.date
        attr(volatility.matrix.smooth, "underlying.price") <-  undly.price
        attr(volatility.matrix.smooth, "r") <- r
        attr(volatility.matrix.smooth, "b") <- b
        attr(volatility.matrix.smooth, "y.axis") <- y       
        class(volatility.matrix.smooth) <- c("volatility.surface", "matrix")
        
        volatility.matrix.smooth
}     
        
plot.volatility.surface <- function(volatility.surface, q.y.left = .01,  q.y.right = .003,  q.t = .03, 
                                    z.lim = NULL, z.lim2 = NULL, theta = 45L, ...){
          if(is.null(z.lim2)){
            z.lim2 <- c(0, 1)
          }     
          s.range <- quantile(as.numeric(colnames(volatility.surface)), c(q.y.left, 1 - q.y.right))
          strikes <- as.numeric(colnames(volatility.surface))
          option.matrix.smooth <-  volatility.surface[, strikes > s.range[1L] & strikes < s.range[2L]]
          
            t.treshold <- quantile(as.numeric(rownames(volatility.surface)), q.t)
          option.matrix.smooth <-  option.matrix.smooth[as.numeric(rownames(volatility.surface)) > t.treshold, ]
          par(cex = .6) 
          if(is.null(z.lim)){
            z.lim <- 1.2 * range(option.matrix.smooth, na.rm = TRUE)
          } 
          suppressWarnings(
              drape.plot(x = as.numeric(rownames(option.matrix.smooth)), 
                         y = as.numeric(colnames(option.matrix.smooth)), 
                         z = option.matrix.smooth,
                         zlim = z.lim,
                         zlim2 = z.lim2,
                         horizontal = FALSE,
                         main = paste("TRADE_DT:  ", attr(volatility.surface, "trade.date")),
                         theta = theta, phi = 25L, 
                         xlab = "YEAR_FRAC", 
                         ylab = attr(volatility.surface, "y.axis"), 
                         zlab = "Volatility", ticktype = "detailed", 
                         expand = 5/6, r = 5L, d = .5,
                         shade = .3, col = rainbow(100), ...)
          )             
}     

.check.v.s <- function(e1, e2) {  
  attr(e1, "underlying.price") == attr(e2, "underlying.price") &&
    attr(e1, "trade.date") == attr(e2, "trade.date")  &&
    attr(e1, "r") == attr(e2, "r") && 
    attr(e1, "b") == attr(e2, "b") &&
    colnames(e1) == colnames(e2) 
}

'-.volatility.surface' <- function(e1, e2) {
  if(!.check.v.s(e1, e2)) {
    stop("Variables have diffrent parameters.")
  }
  class(e1) <- "matrix"
  class(e2) <- "matrix"
  e <- e1 - e2
  class(e) <- c("volatility.surface", "matrix")
  e
}
'+.volatility.surface' <- function(e1, e2) {
  if(!.check.v.s(e1, e2)) {
    stop("Variables have diffrent parameters.")
  }
  class(e1) <- "matrix"
  class(e2) <- "matrix"
  e <- e1 + e2
  class(e) <- c("volatility.surface", "matrix")
  e
}