## payoff class
payoff <- function(payoff.fun, K, p = NA,  x.min = 1000, x.max = 2600, ...) {
  
  f <- match.fun(payoff.fun)
  x <- seq(from = x.min, to =  x.max)
  if(!is.na(p)) {
    payoff <- sapply(x, f, K = K,  p = p, ...)
  } else {
    payoff <- sapply(x, f, K = K, ...)
  }
  
  payoff <- data.frame(x = x, pay.off = payoff)
  class(payoff) <- c("payoff", "data.frame")
  
  payoff
}

'+.payoff' <- function(e1, e2) {
  if(any(e1$x != e2$x)) {
    stop("Different x asis!")
  }
  else {
    e = data.frame(x = e1$x, pay.off = e1$pay.off + e2$pay.off)
  }
  class(e) <- c("payoff", "data.frame")
  
  e
}
'-.payoff' <- function(e1, e2) {
  if(any(e1$x != e2$x)) {
    stop("Different x asis!")
  }
  else {
    e = data.frame(x = e1$x, pay.off = e1$pay.off - e2$pay.off)
  }
  class(e) <- c("payoff", "data.frame")
  
  e
}
'*.payoff' <- function(e1, e2) {
  if(!any(class(e1)   %in% c("numeric", "integer") & any(class(e2) == "payoff") )) {
    stop("")
  }
  else {
    e = data.frame(x = e2$x, pay.off = e1 * e2$pay.off)
  }
  class(e) <- c("payoff", "data.frame")
  
  e
}


load.packages <- function(packages){
  load.package <-  function(x) {
    if (!suppressPackageStartupMessages(require(x, character.only = TRUE))) {
      install.packages(x, quiet = TRUE, verbose = FALSE)
      suppressPackageStartupMessages(library(x, character.only = TRUE))
    }
  }
  invisible(sapply(packages, FUN = load.package))
}


# options filter
options.filter <- function(ops, flag = c('P'),
                           strk.prc.l.treshold.m = .4,
                           strk.prc.r.treshold.m = 1.6,
                           volatility.l.treshold = 0.001,
                           volatility.r.treshold = 0.8) {
  
       ops <- subset(ops, PC %in% flag &
                          VOLATILITY > volatility.l.treshold & 
                          VOLATILITY < volatility.r.treshold &
                          STRK_PRC > strk.prc.l.treshold.m * UNDL_PRC & 
                          STRK_PRC < strk.prc.r.treshold.m * UNDL_PRC &
                          VEGA > 0.5 &
                          L_BID > 0.5 &
                          L_MEAN > INTRINSIC_VALUE)
    ops
}

extracted.rates <- function(calls, puts, s0, k, t) {
  y = puts - calls
  x = k
  lm.model = lm(y ~ x)
  slope = lm.model$coeff[[2]]
  intercept = lm.model$coeff[[1]]
  r = -1 * log(slope)/t
  y = -1 * log(-1 * intercept/s0)/t
  output = c(r, y)
  output
}

# N(-d1)
delta.forward <- function(S, K, r, b = 0, vol, T) {
  d1 <- (log(S/K) + (r-b + 0.5*(vol^2))*T)/(vol*sqrt(T))
  pnorm(-d1)	
}

log.moneyness <- function(S, K, r, b = 0, T) {
  log(K/S) - (r-b)*T
}

bs.rnd <- function(ST, S0, r, b, t, sigma){
  bs.rnd <- 1/(ST*sqrt(2*pi*sigma^2*t)) * exp(-(log(ST/S0) - (r-b-0.5*sigma^2)*t)^2/(2*sigma^2*t)) 
}

put.PC.parity <- function(C, S, K,  r, b) {
  C - S*exp(-b*t) + K*exp(-r*t)
}

strike.from.delta.fw <- function(S, delta.fw, r, b, vol, t) {
  S*exp(qnorm(delta.fw)*sqrt(t)*vol + (r - b + 0.5*(vol^2))*t)
}