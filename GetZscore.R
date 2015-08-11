GetZscore = function(x, makePlot=TRUE){
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
  
  x.mu = mean(x)    # Mean of x
  x.sd = sd(x)      # Standard deviation of x
  x.zscore = (x - x.mu) / x.sd    # Z-score of x
    
  if (makePlot == TRUE) {    # If plotting flag set to TRUE...make a plot.
    nbins = 25
    win.graph(4,8)  # Make a figure that is x,y inches wide, tall.
    par(mfrow=c(2,1))
    hist(x, breaks = nbins)
    hist(x.zscore, breaks = nbins)
  }
  return(list(x.zscore = x.zscore, x.mu = x.mu, x.sd = x.sd))
}