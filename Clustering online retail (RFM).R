library(tidyverse)
library(readxl)
library(cluster)
MAIN=read_xlsx("D:/Data Analysis/Clustering/Online Retail.xlsx")
backup.MAIN=read_xlsx("D:/Data Analysis/Clustering/Online Retail.xlsx")

#We'll call the dataset we're working with as df
df=MAIN

#structure
str(df)
#Getting a summary of the data
summary(df)

#Dropping NA values from the dataset
df=na.omit(df)
MAIN=df
summary(df)

#Getting only the values from England
df = df %>%
  filter(Country=="United Kingdom")

#Removing rows with minus values
#Minus Inducates that the item has been returned
df=df %>%
  filter(Quantity>0)

summary(df)
str(df)
MAIN=df
#EDA

#We mainly consider RFM method
#So we mainly analyse data from Quanitity and unit price

#Quantity

#Unit Price
#Percentage over 1.5IQR
sum(df$Quantity>= 12+IQR(df$Quantity))/length(df$Quantity)

sum(df$UnitPrice>= 3.750+IQR(df$UnitPrice))/length(df$UnitPrice)

#Since the percentage of outlieres are relatively low in Unit Price we first we what those valeus are

Price.outliers=df %>%
  filter(UnitPrice > 3.750+IQR(df$UnitPrice))

P.Anal=Price.outliers %>% filter(UnitPrice > 1000)

Q.outliers=df %>%
  filter(Quantity > 12+IQR(df$Quantity))

Q.Anal=Q.outliers %>% filter(Quantity > 300)


#Thus we remove all Unit Price items greater than 1000
#We also remove all Quantity greater than 300

df = df %>%
  filter(UnitPrice < 1000 & Quantity < 300)

summary(df)
#Checking the distribution of customers across countries
length(unique(df$Country))
table.country=table(df$Country)

#EDA
boxplot(df$Quantity)

#IN this dataset we'll be doing an RFM Analysis
#Recency - Number of days since last purchase
#Frequency - Number of times a item was purchased
#Monetary - Total amount spent by a customer

MAIN=df

#Feature  Engineering
amount=df$Quantity*df$UnitPrice

df=df %>% mutate(Monetary=amount)

rfm_m=df %>% 
  group_by(CustomerID) %>%
  summarise(M=sum(Monetary))

#Frequency
rfm_f=df %>%
  count(CustomerID,InvoiceNo) %>%
  group_by(CustomerID) %>%
  summarise(Freq=sum(n))

#Checking the recency
head(df$InvoiceDate)
tail(df$InvoiceDate)
da=as.Date(df$InvoiceDate)

max_date=max(da)
df=df %>% mutate(Recency=max_date - da)

#Recency
rfm_r=df %>%
  group_by(CustomerID) %>%
  summarise(R=min(Recency))

#SPLITTING INTO A NEW DATASET CALLED RFM
RFM_MAIN=data.frame(Customer_ID=rfm_r$CustomerID,Recency=rfm_r$R,Freq=rfm_f$Freq,Monetary=rfm_m$M)
str(RFM_MAIN)
write.csv(RFM_MAIN,"D:/Data Analysis/Clustering/RFM_MAIN.csv",row.names = FALSE)
summary(RFM_MAIN)
#Processing RFM_MAIN so all values are numeric or integer
RFM_BACKUP=RFM_MAIN

#Analysing RFm_MAIN
RFM_MAIN  %>%
  ggplot(aes(x=Freq,y=Monetary))+
  geom_point()

#Remove data points above 100000 monetary
RFM_MAIN=RFM_MAIN %>% filter(Monetary<10000)

RFM_MAIN=RFM_MAIN[,2:4]
RFM_MAIN$Recency=as.numeric(RFM_MAIN$Recency)

str(RFM_MAIN)
summary(RFM_MAIN)



#VISUAL ANalysis of what shit is over 10000
RFM_MAIN %>% ggplot(aes(Freq))+
  geom_histogram()

RFM_MAIN %>% ggplot(aes(Freq))+
  geom_histogram()

#REMOVE ALL VALUES

RFM_MAIN$Recency=scale(RFM_MAIN$Recency,center = T,scale = T)
RFM_MAIN$Freq=scale(RFM_MAIN$Freq,center = T,scale = T)
RFM_MAIN$Monetary=scale(RFM_MAIN$Monetary,center = T,scale = T)

summary(RFM_MAIN)
#FITTING THE K MEANS CLUSTERING MODEL with 3 clusters

kmeans.re=kmeans(RFM_MAIN[1:3],centers = 3,nstart = 20)
table(kmeans.re$cluster)

library(rgl)
plot3d(RFM_MAIN[,1],RFM_MAIN[,2],RFM_MAIN[,3],col = kmeans.re$cluster,type = "s",radius = 0.2)
library(magick)
#play3d(spin3d(axis = c(0,0,1),rpm = 30),duration = 1000000)

#GETTING THE NUMBER OF CLUSTERS

#HERE WE UE THE factoextra library
library(factoextra)
fviz_nbclust(RFM_MAIN, # data  
             kmeans, # clustering algorithm 
             nstart = 25, # if centers is a number, how many random sets should be chosen?(default is 25)
             iter.max = 200, # the maximum number of iterations allowed.
             method = "wss") # elbow method

#USING THE SILHOUTTE ANALYSIS
fviz_nbclust(RFM_MAIN, # data
             kmeans, # clustering algorithm
             method = "silhouette") # silhouette

#USING GAP STATISTIC

#Calculating silhouettescore manually