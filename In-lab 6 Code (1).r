############ In-lab 6 code #####################################
######## Loading libraries ##########
library(pacman)
p_load(tidyverse, 
       ggthemes,MASS)
##### Loading the Mock dataset ######
mock <- read_csv("Mockdata.csv")
mock$x2 <- as_factor(mock$x2)
mock$x3 <- as_factor(mock$x3)
################ Essentials of ggplot ##########################
# Data
# Aesthatics - Mapping variabels
# geom - Data represntation
######### Univariate plots ########################
#### Barcharts ####
# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
mock %>%
  ggplot(aes(x = x2))

ggplot(data= mock, aes(x=x2))


mock %>%
  ggplot(aes(x = x2)) +
  geom_bar()

mock %>%
  ggplot(aes(x = x2)) +
  geom_bar()+
  facet_wrap(vars(x3))
  
  
##### Coordinates ##### 
  
library(MASS)
### ading coordinates
  mammals %>%
    ggplot(aes(x = body, y = brain)) +
    geom_point(aes(, color="red")) +
    geom_smooth(method = "lm", color = "red",se = FALSE)  # Add linear Model
    
mammals %>%
    ggplot(aes(x = body, y = brain)) +
    geom_point() +
    geom_smooth(method = "lm",color = "red", se = FALSE)  + # Add linear Model
    scale_x_log10() +
    scale_y_log10() # changing x and y axises to log scale

########## Distribution plots ###############
### Histogram ####
mock %>%
  ggplot(aes(x = x)) +
  geom_histogram()

# changing number of bins and binwidth
mock %>%
  ggplot(aes(x = x)) +
  geom_histogram(bins = 10)

mock %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 3)

mock %>%
  ggplot(aes(x = x)) +
  geom_histogram(fill = NA, colour = "tomato1",
                 binwidth = 2)

######## Density plots ###########
mock %>%
  ggplot(aes(x = x)) +
  geom_density()

mock %>%
  ggplot(aes(x = x)) +
  geom_density(fill = "steelblue1")

mock %>%
  ggplot(aes(x = x)) +
  geom_density(fill = "steelblue1", alpha = 0.2)

### Combining the two ##
## Histogram y-scale is count data while density is probability
## To combine both we need to make sure they are both in the
## same scale, that is why we need to redefine the scale
## within geom_histogram() function.
## aes(y = (..density..)) changes the statistic from count
## to density

mock %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = (..density..)), binwidth = 2, 
                 fill = "white", colour = "black") +
  geom_density()
  
mock %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = (..density..)), binwidth = 2, 
                 fill = "white", colour = "black") +
  geom_density(fill = "steelblue1", alpha = 0.4)

######## Boxplot #########
mock %>%
  ggplot(aes(x = x)) +
  geom_boxplot()

mock %>%
  ggplot(aes(y = y)) +
  geom_boxplot()

mock %>%
  ggplot(aes(y = y)) +
  geom_boxplot(outlier.colour = "red")

mock %>%
  ggplot(aes(y = y)) +
  geom_boxplot(outlier.colour = "red",
               outlier.alpha = 0.5) #It helps when there are many
                                    # outliers stacked over each other

######### Bivariate and Multivariate plots ########################
##### Bar Charts #######
# Bar charts are automatically stacked when multiple bars are placed
# at the same location. The order of the fill is designed to match
# the legend

mock %>%
  ggplot(aes(x = x2, fill = x3)) +
  geom_bar()

# flipping coordinates
mock %>%
  ggplot(aes(y = x2, fill = x3)) +
  geom_bar()

mock %>%
  ggplot(aes(y = x2, fill = x3)) +
  geom_bar(position = position_stack(reverse = TRUE))

# sid-by-side bars
mock %>%
  ggplot(aes(y = x2, fill = x3)) +
  geom_bar(position = "dodge")

######### Scatter plot ###################################
mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

mock %>%
  ggplot(aes(x = x, y = y, color=x2)) +
  geom_point()
######## Boxplot #########################################
###### Boxplot numeric vs categorical - Factoring ########
mock %>%
  ggplot(aes(x = x2, y = y)) +
  geom_boxplot()

### Labelling outliers ####
# First we create a function for detecting outliers using IQR

find_outlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

mock <- mock %>%
  group_by(x2) %>%
  mutate(outlier = ifelse(find_outlier(y), y, NA))

mock %>%
  ggplot(aes(x = x2, y = y)) +
  geom_boxplot(outlier.colour = "red") +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5,
            vjust = 0.1) # hjust negative moving to the right horizantally
                        # vjust positive moving up vertically

############ Other options in ggplot ############################

############ Statistics ######################################
##### Scatter plot example ###################################
### Adding smoothers ######
## Linear regression ######
mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")

# Removing standard error ribbon
mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)

#### LOESS: Locally weighted scatter smoother ############
mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "loess")

mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE)

######## More than one regression line - Factoring #######
mock %>%
  ggplot(aes(x = x, y = y, color = x2)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)

mock %>%
  ggplot(aes(x = x, y = y, color = x3)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE)


##### Boxplot example - Adding means #####################
mock %>%
  ggplot(aes(x = x3, y = y)) +
  geom_boxplot() +
  stat_summary(fun.y="mean")

### Question ###
# Which one is more symmatric?#

mock %>%
  ggplot(aes(x = x3, y = y)) +
  geom_boxplot() +
  stat_summary(fun.y="mean", colour = "purple", size = 0.5)

######## More than one regression line - Factoring #######
mock %>%
  ggplot(aes(x = x, y = y, color = x2)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)

mock %>%
  ggplot(aes(x = x, y = y, color = x3)) +
  geom_point() +
  geom_smooth(method = "loess",
              se = FALSE)

######### Faceting - Small multiple ##########################
mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(vars(x3))

mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_col(vars(x3))

mock %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_grid(rows = vars(x2), cols = vars(x3))

########### Segmants, text, and other options ####################
# geom_segment to create horizontal of vertical lines
# the logic is similar to r-base graphics. You need to assign
# a starting point and an ending point for each segment

# geom_curve can be used to add arrows. The arguments requrie
# a starting point, an ending point, and an angle of curve.
# 0 is for straight line, 90 is for the highest curve for the arrow

# annotate can be used to add user defined text inside the plot
# it is different from geom_text, be careful! geom_text is used
# as any geometric when you want to represent your data as a text
# not as a point or something else.


mock %>%
  ggplot(aes(x = x, y = y, color = x2)) +
  geom_point() +
  geom_segment(aes(x = 5, xend = 5, y = 0, yend = 7), color = "black") +
  geom_segment(aes(x = 0, xend = max(x) + 2, y = 0, yend = 0),, color = "black") +
  geom_segment(aes(x = 12, xend = 12, y = 0, yend = 7), color = "black") +
  geom_segment(aes(x = 0, xend = max(x) + 2, y = 7, yend = 7),, color = "black") +
  geom_segment(
    x = 8, xend = 8,
    y = 12, yend = 6,
    lineend = "round",
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.5, "cm")),
    colour = "maroon" # Also accepts "red", "blue' etc
  ) +
  geom_smooth(method = "lm",
              se = FALSE) +
  annotate(geom="text", x=8.5, y=14, label="Interesting Area!",
           color="dark green") +
  geom_curve(aes(x = 19, xend = 17, y = 10, yend = 5), 
             arrow = arrow(
               length = unit(0.03, "npc"), 
               type="closed" # Describes arrow head (open or closed)
             ),
             colour = "violet",
             size = 1.2,
             angle = 90 # Anything other than 90 or 0 can look unusual
  ) +
  annotate(geom="text", x=22, y=10, label="Something Wrong!",
           color="red") +
  geom_point(data=mock %>% filter(x > 20, y < -11),
             pch=21,
             size=4.5,
             stroke = 2,
             colour="blue") +
  theme_classic()
