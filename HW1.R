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
  geom_boxplot(fill="#00BFFF",coef=1.5, alpha=0.7, size=1.1) + stat_boxplot(geom = 'errorbar', width = 0.2)+
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
  
  
  
  #-----------------------1.B---------------------------#
  
  ####-------------Average reviews per month vs summary_length---------###
  listings$summary_len <- nchar(listings$summary)
  # Replace NA with 0 in the reviews_per_month column
  listings$reviews_per_month <- ifelse(is.na(listings$reviews_per_month), 0, listings$reviews_per_month)
  listings$neighbourhood[is.na(listings$neighbourhood) | listings$neighbourhood == ""] <- "Unknown"
  listings <- listings[!is.na(listings$beds), ]
  View(listings)
  listings_summary <- listings %>%
    group_by(summary_len) %>%
    summarize(avg_reviews_per_month = mean(reviews_per_month))
  
  ggplot(listings_summary, aes(x = summary_len, y = avg_reviews_per_month)) +
    geom_point() +
    geom_smooth(method = "lm",se=FALSE)+
    labs(title = "Average Reviews per Month per Summary Length",
         x = "Summary Length",
         y = "Average Reviews per Month")
correlation <- by(listings[, c("price_dollars", "beds")], listings$neighbourhood, cor)

listings_filtered <- listings %>%
  group_by(neighbourhood) %>%
  filter(n() >= 200,price_dollars < 3000)

ggplot(listings_filtered, aes(x = beds, y = price_dollars, color = neighbourhood)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot of Price vs Beds by Neighbourhood",
       x = "Beds",
       y = "Price",
       color = "Neighbourhood") +
  theme(legend.position = "right")
  
ggplot(listings_filtered, aes(x = beds, y = price_dollars)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot of Price vs Beds",
       subtitle = "This plot shows prices per neighbourhood that has at least 200 listings",
       x = "Beds",
       y = "Price",
       color = "Neighbourhood") +
  facet_wrap(~ neighbourhood, nrow = 3, ncol = 3) +
  theme(legend.position = "right")
###------------------location per neighborhood we filtered N.as and neighbers with less then 30 reviews"
listings_filtered <- listings %>%
  drop_na(review_scores_location) %>%
  group_by(neighbourhood) %>%
  filter(n() >= 30)


mean_review <- listings_filtered %>%
  group_by(neighbourhood) %>%
  summarize(mean_review_score = mean(review_scores_location))

ggplot(mean_review, aes(x = reorder(neighbourhood, mean_review_score), y = mean_review_score)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Mean Review Score by Neighbourhood",
       x = "Neighbourhood",
       y = "Mean Review Score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(8.25, 10))  


#------------------Q2---------METRICS------------------#


#----------------preperations----------#
library(tidyverse)
library(lubridate)
calendar = read.csv("calendar_clean.csv", header=T)
# Replace NA with 0 in the price_dollars column
calendar <- calendar %>%
  mutate(price_dollars = ifelse(is.na(price_dollars), 0, as.numeric(gsub("\\$|,", "", price_dollars))))

calendar$date <- as.Date(calendar$date)

# Summarize price per week
weekly_sum <- calendar %>%
  mutate(week = format(date, "%Y-%W")) %>%
  group_by(week) %>%
  summarise(total_price = sum(price_dollars, na.rm = TRUE)) %>%
  mutate(total_price = total_price/1000)

# convert week to date
weekly_sum$date <- as.Date(paste(weekly_sum$week, "1", sep = "-"), format = "%Y-%U-%u")

# order data by date
weekly_sum <- weekly_sum[order(weekly_sum$date), ]

ggplot(weekly_sum, aes(x = week, y = total_price)) +
  geom_line(aes(group = 1)) +
  labs(x = "Week", y = "Sum per week(Thousands)") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#New Hosts by quarter and neighborhood

library(forcats)

listings %>%
  mutate(host_since_quarter = paste0("Q", quarter(host_since), "-", year(host_since)),
         neighbourhood = factor(neighbourhood)) %>%
  group_by(host_since_quarter, neighbourhood) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = fct_reorder(host_since_quarter, as.Date(paste0("01-", gsub("Q", "", host_since_quarter), "-01"), format = "%d-%m-%Y")), y = count, fill = neighbourhood)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  scale_fill_discrete(name = "Neighbourhood") +
  xlab("Quarter") +
  ylab("Number of new hosts") +
  ggtitle("New Hosts by quarter and neighbourhood")

# metrics number #3 mean price for night per neighberhood across time##

library(dplyr)
merged_df <- merge(listings, calendar, by.x = "id", by.y = "listing_id")
merged_df <- merged_df[complete.cases(merged_df$price_dollars.y), ]
merged_df <- merged_df %>% group_by(neighbourhood) %>% filter(n() >= 2000)
merged_df <- merged_df %>%
  mutate(price_dollars.y = (as.numeric(gsub("\\$|,", "", price_dollars.y))))

#test1#

new_df <- merged_df %>%
  filter(!is.na(price_dollars.y)) %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(neighbourhood, month) %>%
  summarise(avg_price = mean(price_dollars.y)) %>%
  select(avg_price, month, neighbourhood)


ggplot(new_df, aes(x = month, y = avg_price, group = neighbourhood, color = neighbourhood)) +
  geom_line() +
  labs(x = "Month", y = "Average Price", color = "Neighborhood") +
  ggtitle("Average Price by Neighborhood over Time") +
  theme(plot.title = element_text(hjust = 0.5))




