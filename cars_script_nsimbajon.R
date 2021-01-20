# setwd("C:/Users/marguerite/Documents/Rclass")

# cars <- read.csv( "cars.csv") # this is how you would read it if it were in your working directory
cars     # built in dataset - comes with R - take a look at it

speed <-  cars$speed  # Saved speed as a vector.
dist <-  cars$dist    # Saved distance as a vector

par(mfrow=c(1,2)) # Splits the plot environments so that both data sets are visible.


plot( speed, dist, cex=1)  # Plotted x, y
speed.lm <- lm (dist ~ speed)   # Fit regression line, saved as speed.lm

abline( speed.lm, col="red" )   # Plotted regression line - best fit line to the points

# Get the coefficients of the line in y= slope*x + intercept form:
coef(speed.lm)

# Assigned the values of "slope" and "intercept" by pulling the values directly from the coef function:
slope <- coef(speed.lm)["speed"]
intercept <- coef(speed.lm)["(Intercept)"]

# The regression distance ~ (as a function of) speed predicts distance from the speed data. Compute the fitted distance (if they fell perfectly on the line) from speed:
dist_fit <- slope*speed + intercept

# Plotted line of best fit for distance versus speed:
points( dist_fit ~ speed, pch=16, col="red" )

# Plotted specifically the 49th point on the graph to show as a solid red dot:
points(speed[49], dist[49], cex=1, pch=16, col="red" )

# Computed the absolute difference between the distance and fitted distance:
Diffdist <- abs( dist - dist_fit )

 # Is point 49 the most extreme point? Yes.  This code tells me which point in "Diffdist" is equal to the maximum value in "Diffdist".
which(Diffdist==max(Diffdist))
 

 # We can plot the deviation of point 49 from the regression line using the segments() function. Look up the help page to understand how it works. 
 i=49
segments( speed[i], dist[i], speed[i], dist_fit[i], col="red", lty=2 )


# Assigned z to be points 1 through 50, then applied z to each segments function:
z=(1:50)
segments( speed[z], dist[z], speed[z], dist_fit[z], col="red", lty=2 )


# Subseted the data frame to only include the faster half of the data set:
fastcars <- cars[26:50,]

# Plotted the fast data and regression line for the faster data set:
 fspeed <- fastcars$speed
 fdist <- fastcars$dist
 plot( fspeed, fdist, cex=1)
 fspeed.lm <- lm ( fdist ~ fspeed)
 abline( fspeed.lm, col="blue")
 
 
 
# Saved data to csv file in working directory
write.csv(cars, "cars.csv") 

# Saved the "fast" data to another file "fastcars.csv"
write.csv(fastcars, "fastcars.csv")
