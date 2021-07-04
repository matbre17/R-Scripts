require(ggplot2)
require(ggQC)

x1 <- c(1,11,21,31,41,51,61,71,81,91,101,111,121,131,141,151,161,171,180)
y1 <- c(0.9370,0.9399,0.9458,0.9714,0.9543,0.9488,0.9566,0.9559,0.9621,0.9480,
         0.9515,0.9554,0.9591,0.9646,0.9599,0.9618,0.9516,0.9478,0.9563)

my.data <- data.frame(x1,y1) 

mean.y <- mean(y)
sd.y <- sd(y)

XmR_Plot <- 
  ggplot(my.data, aes(x = x1, y = y1)) + #init ggplot
  geom_point(size = 3) + #Add points and adjust point sizes
 geom_line(size = 0.7) + # add lines and adjust linesize
  stat_QC(method = "XmR",      # specify QC charting method
          auto.label = TRUE,      # Use Autolabels
          label.digits = 3,    # Use two digit in the label
          limit.txt.label = NaN, #Removes "UCL" and "LCL" labels
          show.1n2.sigma = TRUE   # Show 1 and two sigma lines
  ) +  
  scale_x_continuous(expand =  expand_scale(mult = .15)) +  # Pad the x-axis
  scale_y_continuous(
  labels = scales::number_format(accuracy = 0.001)) #Adjusts digits in y-axis by
#adding or removing 0's

### Draw the plot - Done
XmR_Plot
