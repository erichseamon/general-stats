PlotR = function(x,y,makeplot){
  # Calculate the zscore for a vector data by subtracting the mean and
  # dividing by the standard deviation.
  # 
  # Args:
  # x = vector in numbers
  # 
  # Returns:
  # x.zscore = standardized variable, with mean = 0 and sd = 1.
  #
  # Philip Higuera
  # University of Idaho
  # September 2014
  #
  
  n = length(x)
  x.mu = mean(x)    # Mean of x
  y.mu = mean(y)
  x.sd = sd(x)      # Standard deviation of x
  y.sd = sd(y)      # standard deviation of y
  #x.zscore = (x - x.mu) / x.sd    # Z-score of x
  cor = (sum((x - x.mu)*(y - y.mu))) / ((n-1) * x.sd * y.sd)
  
    
  if (makeplot == TRUE){
    win.graph(3.5,4)  # Make a figure that is x, y inches wide, tall.
    xlim = c(min(min(x),min(y)), max(max(x),max(y)))  
    plot(x,y,
         xlim = xlim,
         ylim = xlim,
         main =  paste("Correlation  = ",round(r*100)/100))
}
    return(r)
}