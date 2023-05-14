#Tal_Shamir 313227001
# general setup
rm(list=ls()) # del all objects and functions
gc() #cleans memory
options(scipen=999) # tell R not to use Scientific notation
options(digits = 5) # controls how many digits are printed by default

#set dir
setwd("C:/Users/talsh/Google Drive/לימודים/economics/big-data")
install.packages("modelsummary")
install.packages("visdat")
install.packages("skimr")
install.packages("magrittr") # Install the magrittr package
install.packages("ggplot2")
install.packages("ggpubr") # Install the ggpubr package
install.packages("psych") # Install the psych package
install.packages("hrbrthemes")
install.packages("ggthemes")
install.packages("DescTools")
library(tidyverse) 
library(gridExtra)
library(glue) # Format And Interpolate A String
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(cowplot) # themes and theme components
library(hrbrthemes) # themes and theme components
library(ggthemes) # Visualization Themes
library(DescTools)
library(psych) # Load the psych package
library(ggpubr) # Load the ggpubr package
library(magrittr) # Load the magrittr package
library(modelsummary)
library(visdat)
library(skimr)
listings <- unique(read.csv("listings_clean.csv", header=T))
listings$beds %>% skim
boxplot(listings$beds, main="Box plot of beds", ylab="beds")
ggecdf(listings$beds)
describe(listings$beds)
hist(listings$beds)
listings_new <- listings[listings$beds <= 5, ]
options(repr.plot.width=20, repr.plot.height=6)



a<-ggplot(listings_new, aes(x=beds))+
  geom_histogram(aes(y = ((..count..)/sum(..count..))),bins=50, col='black', fill='#00BFFF', alpha=.7, size=1.1)+
  theme_economist()+
  scale_y_continuous(labels = scales::percent,breaks = seq(0,8,0.15))+
  ggtitle("Distribution of Beds")+
  ylab("Percentage")+
  xlab("Number of Beds") + 
  theme(plot.title = element_text(size=21, hjust=0.7),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20)
  )
a

# Create the box plot using ggplot
b<-ggplot(listings, aes(x=" ",y=beds)) +
  geom_boxplot(fill="#00BFFF",coef=1.5, alpha=0.7, size=1.1) + stat_boxplot(geom = 'errorbar')+
  scale_y_continuous(breaks = seq(0, 16, 1),labels = seq(0,16,1))+
  # Add the custom theme
  theme_economist() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color="black", size=0.5),
        plot.title = element_text(size=21, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=21),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15),
        legend.position="none",
        ) +
  
  # Add title and labels
  ggtitle("Box plot of Beds") +
  ylab("Number of Beds")
grid.arrange(a, b, ncol=2, top="Beds")
  

p <- ggplot(listings, aes(x=" ",y=beds,fill='#00BFFF')) + # fill=name allow to automatically dedicate a color for each group
  
  geom_violin()+
  theme_economist()
plot(p)
describe(listings_new$beds)
prices <-ggplot(listings, aes(x=" ",y=price_dollars)) +
  geom_boxplot(fill="#00BFFF",coef=1.5, alpha=0.7, size=1.1) + stat_boxplot(geom = 'errorbar')+
  #scale_y_continuous(breaks = seq(0, 16, 1),labels = seq(0,16,1))+
  # Add the custom theme
  theme_economist() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color="black", size=0.5),
        plot.title = element_text(size=21, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=21),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15),
        legend.position="none",
  ) +
  
  # Add title and labels
  ggtitle("Box plot of Beds") +
  ylab("Number of Beds")
plot(prices)

#there are only 10 observations that bigger than 1200.
listing_price_clean <- listings[listings$price_dollars<1200,]

options(repr.plot.width=14, repr.plot.height=6)
a<-ggplot(listing_price_clean, aes(x=price_dollars))+
  geom_histogram(bins=25, col='black', fill='#00BFFF', alpha=.7, size=1.1)+
  theme_economist()+
  ggtitle("Prices Histogram")+
  ylab("Frequency")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15)
  )

b<-ggplot(listing_price_clean, aes(x=price_dollars, y=..density..))+
  geom_density(col='black', fill='#00BFFF', alpha=.7, size=1)+
  theme_economist()+
  ggtitle("Prices Density")+
  ylab("Density")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))
plot_grid(a, b, nrow=1, ncol=2)


###########-----------review_scores_rating--------------###############

####BOX-PLOT#####

# Create the box plot using ggplot
b<-ggplot(listings, aes(x=" ",y=review_scores_rating)) +
  geom_boxplot(fill="#00BFFF",coef=1.5, alpha=0.7, size=1.1) + stat_boxplot(geom = 'errorbar')+
  # Add the custom theme
  theme_economist() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color="black", size=0.5),
        plot.title = element_text(size=21, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=21),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15),
        legend.position="none",
  ) +
  
  # Add title and labels
  ggtitle("Box plot of overall score review") +
  ylab("score")
  listings$review_scores_rating %>%
plot(b)
