#Omit Null Values 
library(ggplot2)
library(ggthemes)
library(dplyr)
library(rpart)
library(rpart.plot)
#plot 1 

ggplot(bestsellers.with.categories, aes(x=User.Rating, fill=Genre)) + geom_histogram() + facet_wrap(~Genre) +
  ggtitle("Fiction vs Non-Fiction affect on User Rating(histogram)") +
  labs(x="User Rating", y="Count")  +theme_classic() + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=9, face="bold"),
    axis.title.y = element_text(color="black", size=9, face="bold"),
    legend.background = element_rect(fill = "orange",colour = "black"),
    plot.background = element_rect(fill = "grey90",colour = "black"),
  )


#plot 2 
ggplot(bestsellers.with.categories, aes(x=User.Rating, y=Reviews, color=Genre)) + geom_point() + ggtitle("Fiction vs Non-Fiction affect on User Rating(Scatterplot)") +
  xlab("Rating out of 5.0") + ylab("Total Amazon Reviews") + theme_classic() + theme(
    plot.title = element_text(color="black", size=12, face="bold.italic"),
    axis.title.x = element_text(color="black", size=9, face="bold"),
    axis.title.y = element_text(color="black", size=9, face="bold"),
    legend.background = element_rect(fill = "orange",colour = "black"),
    plot.background = element_rect(fill = "grey90",colour = "black"),
    )


# hypothesis testing to determine if non-fiction or fiction has a significant affect on User Rating 
# Null Hypothesis : On average Non-Fiction and Fiction books have the same User Rating 
# Alternate Hypothesis : On average Non-Fiction book have a higher user rating than Fiction Books 

#Subset the two groups 
Nonfiction_Books <- subset(bestsellers.with.categories, bestsellers.with.categories$Genre == "Non Fiction")
Fiction_Books <- subset(bestsellers.with.categories, bestsellers.with.categories$Genre == "Fiction")

# Standard deviation of two groups 
sd.Nonfiction_Books <- sd(Nonfiction_Books$User.Rating)
sd.Fiction_Books <- sd(Fiction_Books$User.Rating)

#Length 
Len_Nonfiction_Books <- length(Nonfiction_Books$User.Rating)
Len_Fiction_Books <-length(Fiction_Books$User.Rating)

#standard deviation of difference traffic
sd.Nonfiction_Fiction <- sqrt(sd.Nonfiction_Books^2/Len_Nonfiction_Books + sd.Fiction_Books^2/Len_Fiction_Books)

#Means of Samples 

Mean.Nonfiction <- mean(Nonfiction_Books$User.Rating)
Mean.Fiction <- mean(Fiction_Books$User.Rating)

# Z-Score 

Zeta <- (Mean.Nonfiction-Mean.Fiction)/sd.Nonfiction_Fiction


#Get P value 
P_Value = 1-pnorm(Zeta)

print(P_Value)

# Since P value = 0.995 which is > 0.05 we fail to reject the null hypothesis and can conclude
# that there is no significant affect on User Rating between Non-Fiction and Fiction Books. 

# Now that we can confirm through data visualization and hypothesis testing that there is no signigcant affect on User Rating due to Genre. 
# I will see if there is any correlation between the number of Reviews and User Rating 

summary(bestsellers.with.categories$Reviews)

# Creating new categorical column that splits the Reviews into either V1 or V2 V1 = 0-40000 reviews while V2 = 40000-80000 reviews. 
bestsellers.with.categories$ReviewGroup <- cut(bestsellers.with.categories$Reviews, breaks=c(0,10000,90000),labels=c("V1","V2"))

Group1 <- subset(bestsellers.with.categories, bestsellers.with.categories$ReviewGroup == "V1" )
Group2 <- subset(bestsellers.with.categories, bestsellers.with.categories$ReviewGroup == "V2" )
# Alternate Hypothesis Group2 (V2) which comprises of data that only has more then 10000 User Reviews has on average a higher User Rating then books with less then 10000 User Reviews 
# Null Hypothesis on average both groups of User Ratings have the same User Rating

sd.Group2 <- sd(Group2$User.Rating)
sd.Group1 <- sd(Group1$User.Rating)

#Length 
Len_Group2 <- length(Group2$User.Rating)
Len_Group1 <-length(Group1$User.Rating)

#standard deviation of difference traffic
sd.Group1_Group2 <- sqrt(sd.Group2^2/Len_Group2 + sd.Group1^2/Len_Group1)

#Means of Samples 

Mean.Group2 <- mean(Group2$User.Rating)
Mean.Group1 <- mean(Group1$User.Rating)

# Z-Score 

Zeta <- (Mean.Group2-Mean.Group1)/sd.Group1_Group2


#Get P value 
P_Value = 1-pnorm(Zeta)
print(P_Value)
# Since the resulting test ended with a P value of 0.01 which is less then the 
# significance level of 0.05 we can reject the null hypothesis and can conclude that 
# Books with more then 10000 User Reviews have on average a higher User Rating. 




########################################################################################

# Training Models 
#making reviews into categorical data for decision tree 
bestsellers.with.categories$UserReviewGroup <- cut(bestsellers.with.categories$Reviews, breaks=c(0,100,200,400,800,1600,3200,6400,12800,25600,51200,102400),labels=c("U1","U2", "U3", "U4","U5" ,"U6","U7","U8","U9","U10","U11"))

#making price into categorical data for decision tree

bestsellers.with.categories$PriceGroup <- cut(bestsellers.with.categories$Price, breaks=c(0,10,20,30,40,50,60,70,80,90,100,110),labels=c("P1","P2", "P3", "P4","P5" ,"P6","P7","P8","P9","P10","P11"))

# making year into categorical data for decision tree
bestsellers.with.categories$YearGroup <- cut(bestsellers.with.categories$Year, breaks=c(2009,2011,2013,2015,2017,2019),labels=c("Y1","Y2", "Y3", "Y4","Y5"))


range(bestsellers.with.categories$Reviews)
range(bestsellers.with.categories$Year)

decisiontree_1=rpart(bestsellers.with.categories$User.Rating ~bestsellers.with.categories$Genre  + bestsellers.with.categories$UserReviewGroup + bestsellers.with.categories$Price + bestsellers.with.categories$YearGroup,data = bestsellers.with.categories, method = "class",minsplit=1,minbucket=1)
printcp(decisiontree_1)
rpart.plot(decisiontree_1)

#final visualization 

ggplot(bestsellers.with.categories, aes(x=User.Rating, y=Reviews)) + geom_point(shape=10, color="purple") + ggtitle("User Rating vs. Reviews") + 
  labs(x="User Rating", y="Review Count") + theme_economist() + theme(
    plot.title = element_text(color="purple", size=12, face="bold.italic"),
    axis.title.x = element_text(color="purple", size=9, face="bold"),
    axis.title.y = element_text(color="purple", size=9, face="bold"))





