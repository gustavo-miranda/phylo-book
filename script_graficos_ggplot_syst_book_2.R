#######################################################################################################
####################################### Script for graphs #############################################
#################################### Gustavo Silva de Miranda #########################################

# Install
install.packages("ggplot2", dependencies = T)
install.packages("ggalt", dependencies = T)
install.packages("ggExtra", dependencies = T)

# Load
library(ggplot2)
library(ggalt)
library(ggExtra)

############################################### Examples ################################################
#########################################################################################################
#### Modified from: 
### http://girke.bioinformatics.ucr.edu/GEN242/mydoc_Rgraphics_5.html#ggplot-function) 
### http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source

########## Scatter plots
p <- ggplot(dsmall, aes(carat, price, color=color)) + 
  geom_point(size=4)
print(p)


########## Regression line
p <- ggplot(dsmall, aes(carat, price)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  theme(panel.background=element_rect(fill = "white", colour = "black"))
print(p)


########## Several regression lines
p <- ggplot(dsmall, aes(carat, price, group=color)) + 
  geom_point(aes(color=color), size=2) + 
  geom_smooth(aes(color=color), method = "lm", se=FALSE) 
print(p)


########## Local regression curve (loess)
p <- ggplot(dsmall, aes(carat, price)) + geom_point() + geom_smooth() 
print(p) # Setting se=FALSE removes error shade


########## Another scatter plot
options(scipen=999)  # turn-off scientific notation like 1e+48
#theme_set(theme_bw())  # pre-set the bw theme.
data("midwest", package = "ggplot2")

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)


########## Scatterplot With Encircling
midwest_select <- midwest[midwest$poptotal > 350000 & 
                            midwest$poptotal <= 500000 & 
                            midwest$area > 0.01 & 
                            midwest$area < 0.1, ]
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) +   # draw smoothing line
  geom_encircle(aes(x=area, y=poptotal), 
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   # encircle
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")


########## Line plot
p <- ggplot(iris, aes(Petal.Length, Petal.Width, group=Species, 
                      color=Species)) + geom_line() 
print(p)


########## Faceting
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_line(aes(color=Species), size=1) + 
  facet_wrap(~Species, ncol=1)
print(p)


########## Bar Plots

### Calculate mean values for aggregates given by Species column in iris data set
iris_mean <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean) 
### Calculate standard deviations for aggregates given by Species column in iris data set
iris_sd <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=sd) 
### Reformat iris_mean with melt
library(reshape2) # Defines melt function
df_mean <- melt(iris_mean, id.vars=c("Species"), variable.name = "Samples", value.name="Values")
### Reformat iris_sd with melt
df_sd <- melt(iris_sd, id.vars=c("Species"), variable.name = "Samples", value.name="Values")
### Define standard deviation limits
limits <- aes(ymax = df_mean[,"Values"] + df_sd[,"Values"], ymin=df_mean[,"Values"] - df_sd[,"Values"])
### Verical orientation
p <- ggplot(df_mean, aes(Samples, Values, fill = Species)) + 
  geom_bar(position="dodge", stat="identity")
print(p)


########## Horizontal orientation
p <- ggplot(df_mean, aes(Samples, Values, fill = Species)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip() + 
  theme(axis.text.y=element_text(angle=0, hjust=1))
print(p) 


########## Faceting
p <- ggplot(df_mean, aes(Samples, Values)) + geom_bar(aes(fill = Species), stat="identity") +
  facet_wrap(~Species, ncol=1)
print(p)


########## Error bars
p <- ggplot(df_mean, aes(Samples, Values, fill = Species)) + 
  geom_bar(position="dodge", stat="identity") + geom_errorbar(limits, position="dodge") 
print(p) 


########## Mirrored
df <- data.frame(group = rep(c("Above", "Below"), each=10), x = rep(1:10, 2), y = c(runif(10, 0, 1), runif(10, -1, 0)))
p <- ggplot(df, aes(x=x, y=y, fill=group)) + 
  geom_bar(stat="identity", position="identity")
print(p) 


########## Changing Color Settings
library(RColorBrewer)
p <- ggplot(df_mean, aes(Samples, Values, fill=Species, color=Species)) +
  geom_bar(position="dodge", stat="identity") + geom_errorbar(limits, position="dodge") + 
  scale_fill_brewer(palette="Blues") + scale_color_brewer(palette = "Greys") 
print(p)


########## Data reformatting example
y <- matrix(rnorm(500), 100, 5, dimnames=list(paste("g", 1:100, sep=""), paste("Sample", 1:5, sep="")))
y <- data.frame(Position=1:length(y[,1]), y)
y[1:4, ] # First rows of input format expected by melt()

df <- melt(y, id.vars=c("Position"), variable.name = "Samples", value.name="Values")
p <- ggplot(df, aes(Position, Values)) + geom_line(aes(color=Samples)) + facet_wrap(~Samples, ncol=1)
print(p)


########## Same data represented in box plot as follows
ggplot(df, aes(Samples, Values, fill=Samples)) + geom_boxplot()


########## Jitter Plots
p <- ggplot(dsmall, aes(color, price/carat)) + 
  geom_jitter(alpha = I(1 / 2), aes(color=color))
print(p)


########## Box plots
p <- ggplot(dsmall, aes(color, price/carat, fill=color)) + geom_boxplot()
print(p) 


########## Density plots
p <- ggplot(dsmall, aes(carat)) + geom_density(aes(color = color)) # Line coloring
print(p) 
p <- ggplot(dsmall, aes(carat)) + geom_density(aes(fill = color)) # Area coloring
print(p) 


########## Histograms
p <- ggplot(iris, aes(x=Sepal.Width)) + geom_histogram(aes(y = ..density.., 
      fill = ..count..), binwidth=0.2) + geom_density()  
print(p) 


########## Marginal Histogram / Boxplot
data(mpg, package="ggplot2")
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)
ggMarginal(g, type = "histogram", fill="transparent") # Histograma
ggMarginal(g, type = "boxplot", fill="transparent") # Boxplot
ggMarginal(g, type = "density", fill="transparent") # Densidade


########## Pie Chart
df <- data.frame(variable=rep(c("cat", "mouse", "dog", "bird", "fly")), 
                 value=c(1,3,3,4,2)) 
p <- ggplot(df, aes(x = "", y = value, fill = variable)) + 
  geom_bar(width = 1, stat="identity") + 
  coord_polar("y", start=pi / 3) + ggtitle("Pie Chart") 
print(p) 


########## Wind Rose Pie Chart
p <- ggplot(df, aes(x = variable, y = value, fill = variable)) + 
  geom_bar(width = 1, stat="identity") + coord_polar("y", start=pi / 3) + 
  ggtitle("Pie Chart") 
print(p) 


########## Arranging Graphics on Page
library(grid)
a <- ggplot(dsmall, aes(color, price/carat)) + geom_jitter(size=4, alpha = I(1 / 1.5), aes(color=color))
b <- ggplot(dsmall, aes(color, price/carat, color=color)) + geom_boxplot()
c <- ggplot(dsmall, aes(color, price/carat, fill=color)) + geom_boxplot() + theme(legend.position = "none")
grid.newpage() # Open a new page on grid device
pushViewport(viewport(layout = grid.layout(2, 2))) # Assign to device viewport with 2 by 2 grid layout 
print(a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(c, vp = viewport(layout.pos.row = 2, layout.pos.col = 2, width=0.3, height=0.3, x=0.8, y=0.8))


########## Inserting Graphics into Plots
library(grid)
print(a)
print(b, vp=viewport(width=0.3, height=0.3, x=0.8, y=0.8))
