PlotCorrelation = function(x,y,makeFigure = FALSE){
  # Calculate the Pearson product-moment correlation coefficient, using 
  # equation 19.2 in Zar (1999, p. 378).
  #
  # Args:
  #  x: series 
  #  y: series to correlated against x
  #
  # Returns:
  #  r: Pearson product-moment correlation coefficient.
    
  n = length(x)  # Sample size
  
  x.mu = mean(x)  # Mean of x
  y.mu = mean(y)  # Mean of y
  
  x.sd = sd(x)  # Standard deviation of x
  y.sd = sd(y)  # Standard deviation of y
  
  r = (sum((x - x.mu)*(y - y.mu))) / ((n-1) * x.sd * y.sd)
  
  if (makeFigure == 1){
    win.graph(3.5,4)  # Make a figure that is x, y inches wide, tall.
    xlim = c(min(min(x),min(y)), max(max(x),max(y)))  
    plot(x,y,
         xlim = xlim,
         ylim = xlim,
         main =  paste("Correlation  = ",round(r*100)/100))
  }
  return(r)
}

#### Create random series that are correlated with eachother, to test function.

# x = rnorm(100)  # 100 normally distributions values, with mean = 0, std = 1.
# alpha = 0  # Alpha = Y-intercept
# beta = 2  # Beta = slope
# noise = rnorm(length(x),10,5)  # Noise = random normal values. User sets mu & sd
# 
# y = alpha + beta*x + noise  # Define y as a function of x: y = f(x) + noise.
# 
# #' ** Call the function, using random data **
# PlotCorrelation(x,y,1)