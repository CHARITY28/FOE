#####################################################################
################### Basics of R-base Graphics #######################
#####################################################################

foo <- c(1.1,2,3.5,3.9,4.2)

bar <- c(2,2.2,-1.3,0,0.2)

plot(foo,bar)

### Plotting a martrix

baz <- cbind(foo,bar)

baz

plot(baz)

#####################################################################
########## Basics of the parameters #################################
#####################################################################

#### type argument ####
plot(foo,bar,type="l")

plot(foo,bar,type="b")

plot(foo,bar,type="o")

#### labeling ####

plot(foo,bar,type="b",main="My first plot",xlab="x axis label",
        ylab="location y")

plot(foo,bar,type="b",main="My first plot\ntitle on two lines",
     xlab="", ylab="")

#### Colors ####
plot(foo,bar,type="b",main="My first plot",
     xlab="",ylab="",col=2)

plot(foo,bar,type="b",main="My first plot",xlab="",
     ylab="",col="seagreen4")

#### Other options ####
plot(foo,bar,type="b",main="My first plot",xlab="",ylab="",
        col=3,pch=8,lty=1,cex=3,lwd=1)

plot(foo,bar,type="b",main="My first plot",xlab="",ylab="",
        col=6,pch=15,lty=2,cex=1.7,lwd=4)

### Setting the region limits ####

plot(foo,bar,type="b",main="My first plot",xlab="",ylab="",
        col=4,pch=8,lty=2,cex=2.3,lwd=3.3,xlim=c(-10,5),ylim=c(-3,3))

plot(foo,bar,type="b",main="My first plot",xlab="",ylab="",
        col=6,pch=15,lty=3,cex=0.7,lwd=2,xlim=c(3,5),ylim=c(-0.5,0.2))

#### Adding points, lines, texts, etc. ####
x <- 1:20
y <- c(-1.49,3.37,2.59,-2.78,-3.94,-0.92,6.43,8.51,3.41,-8.23,
       -12.01,-6.58,2.87,14.12,9.63,-4.58,-14.78,-11.67,1.17,15.62)

plot(x,y,type="n",main="")
plot(x,y,type="p",main="")
abline(h=c(-5,5),col="red",lty=2,lwd=2)
segments(x0=c(5,15),y0=c(-5, -5),x1=c(5,15),y1=c(5, 5),col="red",lty=3,
         lwd=2)
points(x[y>=5],y[y>=5],pch=4,col="darkmagenta",cex=2)
points(x[1], y[1], pch=3, col=2, cex=2)
points(x[y<=-5],y[y<=-5],pch=3,col="darkgreen",cex=2)
points(x[(x>=5&x<=15)&(y>-5&y<5)],y[(x>=5&x<=15)&(y>-5&y<5)],pch=19,
       col="blue")
points(x[(x<5|x>15)&(y>-5&y<5)],y[(x<5|x>15)&(y>-5&y<5)])
lines(x,y,lty=4)
arrows(x0=8,y0=14,x1=11,y1=2.5, col=3)
text(x=8,y=15,labels="Critical Region")
legend("bottomleft",
       cex = 0.5,
       legend=c("overall process","Critical","standard",
                "too big","too small","Critical y range","Critical x range"),
       pch=c(NA,19,1,4,3,NA,NA),lty=c(4,NA,NA,NA,NA,2,3),
       col=c("black","blue","black","darkmagenta","darkgreen","red","red"),
       lwd=c(1,NA,NA,NA,NA,2,2),pt.cex=c(NA,1,1,2,2,NA,NA))


x= seq(1,10, by=0.2)
y= jitter(x*2+ x*x*0.5-0.7, 100)
plot(x,y)
reg=lm(y~x)
abline(reg, col="blue")
coefficients(reg)

###################################################################
############ Commonly used Graphs #################################
###################################################################

par(mfrow = c(2,2))
plot(x,y)
plot(x,y)
plot(x,y)
plot(x,y)
##### scatter plot ######

x <- mtcars$wt
y <- mtcars$mpg
z <- mtcars$hp
# Plot 
plot(x, y) 
# Plot with main and axis titles
plot(x, y, main = "Main title",
     xlab = "Weight", ylab = "Miles per galon")
# Change the marker point shape (pch = 19) and remove frame of the plot.
plot(x, y, main = "Main title",
     xlab = "Weight", ylab = "Miles per galon",
     pch = 19, frame = FALSE, col ="red")
# Add line around the mean of the data
abline(mean(y),0, lty=2 , col = "blue")

# legend Plotting different colors based on a field on the data
plot(x, y, col=mtcars$cyl)
legend("topright", legend=c(4,6,8),
       col=c(1,2,8), lty=2, cex=0.7)


#### Line plots
# Create some variables
x <- 1:10
y1 <- x*x
y2  <- 2*y1
plot(x, y1, col = "red", xlab = "x", ylab = "y")

plot(x, y1, type = "l", col = "red", xlab = "x", ylab = "y")

#line type (lty) can be specified using either text 
# (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”) 
# or number (0, 1, 2, 3, 4, 5, 6). Note that lty = “solid” is identical to lty=1.

# type= "p" point default, type="l" line, type="b" point over line

plot(x, y1, type = "b", col = "red", xlab = "x", ylab = "y")

# to add a new line we use 
lines(x, y2, col = "blue", type = "b")

# use pch to change the type of bullet
plot(x, y1, type = "b", col = "red", xlab = "x", ylab = "y", pch=19)
lines(x, y2, col = "blue", type = "b", pch=19)

# lty to change the line type
plot(x, y1, type = "b", col = "red", xlab = "x", ylab = "y", pch=19, lty=2)
lines(x, y2, col = "blue", type = "b", pch=19, lty=2)

# to add a legend 
legend("topleft", legend=c("y1", "y2"),
       col=c("red", "blue"), lty=2, cex=0.7)

#### Plot Histogram
hist(mtcars$mpg)

# change the binning using breaks
hist(mtcars$mpg,  breaks = 10)

# Change the x-axis limit 
hist(mtcars$mpg,  breaks = 10, xlim = c(10, 35))
# change color
hist(mtcars$mpg, freq = FALSE, col = "steelblue",   breaks = 10, xlim = c(10, 35))


#### plot density 

plot(density(mtcars$mpg))
ds<-density(mtcars$mpg)
# Fill the density plot using polygon()
plot(ds, col = "steelblue", main = "Density plot of mpg") 

# add a density on top of a histogram
hist(mtcars$mpg, freq = FALSE, col = "steelblue",   breaks = 10, xlim = c(10, 35))
ds<-density(mtcars$mpg)
lines(ds, col="red", lwd=2)

#### Box plot

# Box plot of one variable
boxplot(mtcars$mpg)
# Box plots by groups (dose)
# remove frame
boxplot(mtcars$mpg ~ mtcars$cyl, xlab="cyliner", ylab="Miles per gallon")
# Horizontal box plots
boxplot(mtcars$mpg ~ mtcars$cyl, xlab="cyliner", ylab="Miles per gallon", 
        horizontal = TRUE)

# Change the color of border using one single color
boxplot(mtcars$mpg ~ mtcars$cyl, xlab="cyliner", ylab="Miles per gallon",
        border = "steelblue")

# Change fill color : single color
boxplot(mtcars$mpg ~ mtcars$cyl, xlab="cyliner", ylab="Miles per gallon",
        col = "steelblue")

# Change fill color: multiple colorsboxplot
boxplot(mtcars$mpg ~ mtcars$cyl, xlab="cyliner", ylab="Miles per gallon",
        col = c("red", "blue", "green"))


#### Bar plot
#We need tabular data, to show us the frequency in certain categories
# to do that we use the table function 
cyl_tb<-table(mtcars$cyl)

barplot(cyl_tb, xlab="cylinders",ylab="Frequency")
# Change border and fill color using one single color
barplot(cyl_tb, xlab="cylinders",ylab="Frequency",
        col = "white", border = "steelblue")
barplot(cyl_tb, xlab="cylinders",ylab="Frequency",
        col = "steelblue")

# Change fill color: multiple colors
barplot(cyl_tb, xlab="cylinders",ylab="Frequency",
        col = c("red", "blue", "green"))
