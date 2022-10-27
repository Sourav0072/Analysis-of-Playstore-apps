setwd("F:\\DhanaLaxmi 	Study\\Sem 1\\MSIS 642 M&R\\Final Project01")
library(readxl)
library(dplyr)
library(ggplot2)
library(highcharter)
library(nortest)
playstore_data <- read.csv("googleplaystore.csv")
dim(playstore_data)
summary(playstore_data)

#how many rows are unique?

nrow(playstore_data %>%
       distinct())

##-------data cleaning--------------
playstore_data <- na.omit(playstore_data)

#removing the 
playstore_data.df <- subset(playstore_data, Size!= 'Varies with device')
str(playstore_data.df)
attach(playstore_data.df)

playstore_data.df$Installs <- as.character(gsub("\\+","",playstore_data.df$Installs))
playstore_data.df$Installs <- as.numeric(gsub(",","",playstore_data.df$Installs))

playstore_data.df$Size <- gsub("M","",playstore_data.df$Size)
playstore_data.df$Size <- ifelse(grepl("k",playstore_data.df$Size),(as.numeric(gsub("k","",playstore_data.df$Size)))/1024,playstore_data.df$Size )
playstore_data.df$Size <- as.numeric(playstore_data.df$Size)

playstore_data.df$Price <- as.numeric(gsub("\\$","",as.character(playstore_data.df$Price)))
str(playstore_data.df)

#parameter summary statistics
summary(playstore_data.df)

#Find max and min rating in Dataset

max(playstore_data.df$Rating)
min(playstore_data.df$Rating)

#selecting the important category

category_important <- playstore_data.df %>% group_by(Category) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(per = round(cumsum(n)/sum(n),2)) %>%
  filter(per<0.2)
category_important

#Unique names of category
unique(playstore_data.df$Category)

#which app has maximum  price in dataset

max_price <- playstore_data.df[which.max(playstore_data.df$Price),]
max_price %>% select('App','Price')

#which app has maximum and minimum Installs in Datasets

Installs_max <- max(playstore_data.df$Installs)
retval_max <- subset(playstore_data.df, Installs_max == max(Installs))
head(retval_max)

Installs_min <- min(playstore_data.df$Installs)
retval_min <- subset(playstore_data.df, Installs_min == min(Installs))
tail(retval_min)


#which are top 10 installed apps

playstore_data.df %>%
  select(App,Installs) %>%
  arrange(-Installs) %>%
  head(10)

##=======Visualising the data=============

#Scatterplot for price and Rating

plot(playstore_data.df$Rating,playstore_data.df$Price,xlab="Rating",ylab = "Price",col = "blue",main = "PRICE ON RATING")

#boxplot for rating

x = playstore_data.df$Rating
boxplot(x,ylab="Rating",main="Boxplot of Rating",horizontal = FALSE,col = c('yellow'))

#How many apps included in each category?
c <- playstore_data.df %>%
  group_by(Category) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
c <- head(c,10)
ggplot(c,aes(x = Category,y = count)) +
  geom_bar(stat = "identity",width =.5, fill = "lightblue") +
  labs(title = "Top 10 Categories")+
  theme(axis.text.x=element_text(angle=65,vjust=0.6))

#find most popular category, by number of installs

playstore_data.df %>%
  count(Category, Installs) %>%
  group_by(Category) %>%
  summarize(
    TotalInstalls = sum(as.numeric(Installs))
  ) %>%
  arrange(-TotalInstalls) %>%
  hchart("scatter",hcaes(x="Category",y="TotalInstalls",size="TotalInstalls",color="Category")) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Most Popular categories(# of installs)")

#Find distribution of application size(in MB) against Rating.
qqplot(playstore_data.df$Rating,playstore_data.df$Size)


#which app are Free vs Paid by Category of Percentage

playstore_data.df %>%
  group_by(Category,Type) %>%
  summarize(
    n=n()
  ) %>%
  mutate(perc = round(n/sum(n)*100)) %>%
  hchart('bar',hcaes(x="Category",y="perc",group="Type")) %>%
  hc_plotOptions(series=list(stacking = "normal")) %>%
  hc_title(text = "Percentage of Free vs Paid by Category") %>%
  hc_add_theme(hc_theme_flat())


#which categoris have most expensive apps
playstore_data.df %>%
  filter(Type == "Paid") %>%
  group_by(Category) %>%
  summarize(
    Price = median(Price)
  ) %>%
  arrange(-Price) %>%
  hchart("bar",hcaes(x="Category",y="Price",color = "Price")) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_title(text = "Median price per category")
  

#how much money was earned per category
playstore_data.df %>%
  filter(Type == "Paid") %>%
  mutate(
    Total.Paid = Price*Installs
  ) %>%
  group_by(Category) %>%
  summarize(USD.Paid = sum(Total.Paid))%>%
  arrange(-USD.Paid) %>%
  hchart("bar",hcaes(x="Category",y="USD.Paid",color="USD.Paid")) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_title(text = "Total amount earned by category (installs * Price)")


#Normality test
#H0:The data for Installs is normally distributed
#H1: The data for Installs is not normally distributed
ad.test(Installs)

#From the output, the p-value is less than 0.05 therefore, rejecting the Null hypothesis.

#Non Parametric test
#H0:Avg no. of Installs for paid apps is greater than Avg no. of Installs for free
#H1:Avg no. of installs for paid apps is not greater than avg no. of installs for free
wilcox.test(Installs ~ Type,data = playstore_data.df,paired = FALSE,alternative = "greater")

#As p-value is less than 0.05 we reject the null hypothesis


#correlation
cor(Installs,Size)
cor(Installs,Rating)
cor(Installs,Reviews)
cor(Installs,Price)

#equality of variance 
var.test(Installs~Size,playstore_data.df)

