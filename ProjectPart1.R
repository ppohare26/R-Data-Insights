#*************************Priliminary Data Analysis********************

install.packages("openxlsx")
library("openxlsx")


#***********************Data wrangling************************************

#Load the data into an R dataframe
myData<-read.xlsx("Team4.xlsx",1)
myData
View(myData)

#Check the structure of the dataframe
str(myData)

#identify missing values in individual columns
which(is.na(myData$ID))
which(is.na(myData$Year_Birth))
which(is.na(myData$Education))
which(is.na(myData$Marital_Status))
which(is.na(myData$Income))
which(is.na(myData$Kidhome))
which(is.na(myData$Teenhome))
which(is.na(myData$Dt_Customer))
which(is.na(myData$Recency))
which(is.na(myData$MntWines))
which(is.na(myData$MntFruits))
which(is.na(myData$MntMeatProducts))
which(is.na(myData$MntFishProducts))
which(is.na(myData$MntSweetProducts))
which(is.na(myData$MntGoldProds))
which(is.na(myData$NumDealsPurchases))
which(is.na(myData$NumWebPurchases))
which(is.na(myData$NumCatalogPurchases))
which(is.na(myData$NumStorePurchases))
which(is.na(myData$NumWebVisitsMonth))
which(is.na(myData$AcceptedCmp3))
which(is.na(myData$AcceptedCmp4))
which(is.na(myData$AcceptedCmp5))
which(is.na(myData$AcceptedCmp1))
which(is.na(myData$AcceptedCmp2))
which(is.na(myData$Response))
which(is.na(myData$Complain))
which(is.na(myData$Country))

#identify missing values in whole dataset
myData[!complete.cases(myData), ]

#apply data imputation techniques
incomeMean <- mean(myData$Income, na.rm = TRUE)
incomeMean
myData$Income[is.na(myData$Income)] <-incomeMean

recencyMean <- mean(myData$Recency, na.rm = TRUE)
recencyMean
myData$Recency[is.na(myData$Recency)] <-recencyMean

wineMean <- mean(myData$MntWines, na.rm = TRUE)
wineMean
myData$MntWines[is.na(myData$MntWines)] <-wineMean

cmp3Mean <- mean(myData$AcceptedCmp3, na.rm = TRUE)
cmp3Mean
myData$AcceptedCmp3[is.na(myData$AcceptedCmp3)] <-cmp3Mean

goldMean <- mean(myData$MntGoldProds, na.rm = TRUE)
goldMean
myData$MntGoldProds[is.na(myData$MntGoldProds)] <-goldMean

install.packages("modeest")
library(modeest)

#use in categorical columns
maritalMode <- mfv(myData$Marital_Status)
maritalMode
myData$Marital_Status[is.na(myData$Marital_Status)] <- maritalMode


countryMode <- mfv(myData$Country)
countryMode
myData$Country[is.na(myData$Country)] <- countryMode


#Convert date columns to show appropriate date values
myData$Dt_Customer <- as.Date.numeric(myData$Dt_Customer, origin = '1899-12-30')
View(myData)

#Convert categorical variables into factors
myData$Education <- as.factor(myData$Education)
myData$Marital_Status <- as.factor(myData$Marital_Status)
myData$Country <- as.factor(myData$Country)
str(myData)
View(myData)

#***********************Data transformation************************************

#Identify appropriate variables and apply binning for numerical data transformations
myData$IncomeBin <- cut(myData$Income,
                             breaks = c(0, 20000, 70000, Inf),
                             labels = c("Low", "Medium", "High"),
                             include.lowest = TRUE, right=FALSE)
table(myData$IncomeBin)
levels(cut(myData$Income,
           breaks = c(0, 20000, 70000, Inf),
           include.lowest = TRUE, right=FALSE))

#Transforming date columns to months
myData$Mnt_Customer <- months(myData$Dt_Customer)


#Transforming categorical data to numbers
myData$Education_Num <- as.numeric(myData$Education)
myData$Marital_Status_Num <- as.numeric(myData$Marital_Status)


#dummy variables
myData$Education_Basic <- ifelse(myData$Education == "Basic",1,0)
myData$Education_Graduation <- ifelse(myData$Education == "Graduation",1,0)
myData$Education_Master <- ifelse(myData$Education == "Master",1,0)
myData$Education_PhD <- ifelse(myData$Education == "PhD",1,0)
View(myData)


#***********************Summary Measures************************************

#Calculate all  possible summary measures for at least one  numerical and one categorical variables. 


#mean
mean(myData$Income)
mean(myData$MntWines)

#median
median(myData$MntFruits)
median(myData$MntFishProducts)

#mode
mfv(myData$Marital_Status)
mfv(myData$Education)


#skewness
install.packages("moments")
library(moments)
skewness(myData$NumStorePurchases)
skewness(myData$NumWebPurchases)

#kurtosis
kurtosis(myData$NumStorePurchases)
kurtosis(myData$NumWebPurchases)

# Finding the average for each of the categories
tapply(myData$Income, myData$Kidhome,mean)
tapply(myData$Income, myData$Teenhome,mean)

tapply(myData$Income, myData$Kidhome,median)
tapply(myData$Income, myData$Teenhome,median)

tapply(myData$Income, myData$Kidhome,mfv)
tapply(myData$Income, myData$Teenhome,mfv)


#variance
var(myData$NumStorePurchases)
var(myData$NumWebPurchases)

#standard deviation
sd(myData$NumStorePurchases)
sd(myData$NumWebPurchases)

#coefficient of variance
sd(myData$NumStorePurchases)/mean(myData$NumStorePurchases)
sd(myData$NumWebPurchases)/mean(myData$NumWebPurchases)

#coefficient of variation
coefStore<- sd(myData$NumStorePurchases)/mean(myData$NumStorePurchases)*100
coefStore
coefWeb<- sd(myData$NumWebPurchases)/mean(myData$NumWebPurchases)*100
coefWeb

# Covariance and Correlation
covariance <- cov(myData$NumStorePurchases, myData$NumWebPurchases)
covariance
correlation <- cor(myData$NumStorePurchases, myData$NumWebPurchases)
correlation

# Range, Interquartile range, Mean absolute deviation (MAD)
max(myData$NumStorePurchases) - min(myData$NumStorePurchases) #range
max(myData$NumWebPurchases) - min(myData$NumWebPurchases) #range

#IQR
quantile(myData$NumStorePurchases,0.75) - quantile(myData$NumStorePurchases,0.25)
quantile(myData$NumWebPurchases,0.75) - quantile(myData$NumWebPurchases,0.25)

#MAD
mean(abs(myData$NumStorePurchases -mean(myData$NumStorePurchases)))
mean(abs(myData$NumWebPurchases -mean(myData$NumWebPurchases)))


#Create box plot
boxplot (myData$NumStorePurchases,myData$NumWebPurchases,
         main="Boxplots for Number of Store and Web Purchases",
         xlab ="Number of Store and Web Purchases",
         names = c("StorePurchase", "WebPurchase"),
         horizontal = TRUE, col="cyan")

boxplot (myData$MntWines,myData$MntFruits,myData$MntMeatProducts,myData$MntFishProducts,
         myData$MntSweetProducts,myData$MntGoldProds,
         main="Boxplots for amount spent on different categories",
         xlab ="amount spent in last 2 years",
         names = c("Wines", "Fruits", "MeatProducts", "FishProducts","SweetProducts","GoldProducts"),
         horizontal = TRUE, col="pink")

#identify outliers. 
outliersStore <- boxplot(myData$NumStorePurchases)$out
outliersStore

outliersWeb <- boxplot(myData$NumWebPurchases)$out
outliersWeb

outliersWines <- boxplot(myData$MntWines)$out
outliersWines

#ommitting outliers
myData$newStore <- ifelse(myData$NumStorePurchases%in% outliersStore, NA, myData$NumStorePurchases)
myData1=na.omit(myData)

summary(myData1)

myData$newWeb <- ifelse(myData$NumWebPurchases%in% outliersWeb, NA, myData$NumWebPurchases)
myData1=na.omit(myData)

summary(myData1)

myData$newWines <- ifelse(myData$MntWines%in% outliersWines, NA, myData$MntWines)
myData1=na.omit(myData)

summary(myData1)

#calculating z score
myData$z_scores_Store <- (myData$NumStorePurchases-mean(myData$NumStorePurchases))/sd(myData$NumStorePurchases)
myData$z_scores_Web <- (myData$NumWebPurchases-mean(myData$NumWebPurchases))/sd(myData$NumWebPurchases)
myData$z_scores_Wines <- (myData$MntWines-mean(myData$MntWines))/sd(myData$MntWines)
View(myData)



#***********************Data Visualization************************************

#Creating frequency distributions for numerical variable
intervals <- seq(0, 700000, by=50000)
intervals

Income.cut <- cut(myData$Income, intervals, left=FALSE, right=TRUE)
Income.cut

Income.frequency <- table(Income.cut)
Income.frequency
View(Income.frequency)

Income.prop<-prop.table(Income.frequency)
Income.prop
View(Income.prop)

intervals <-format(intervals,scientific=FALSE)
intervals

#Creating frequency distributions for categorical variable
Repeat_Frequency <- table(myData$Country)
Repeat_Frequency
View(Repeat_Frequency)


#histogram
intervals <- seq(0, 30, by=5)
intervals

hist(myData$NumWebPurchases, breaks=intervals, right=TRUE,
     main="Histogram for Web Purchases",
     xlab="Number of Web Purchase",
     col ="red")

intervals <- seq(0, 15, by=3)
intervals

hist(myData$NumStorePurchases, breaks=intervals, right=TRUE,
     main="Histogram for Store Purchases",
     xlab="Number of Store Purchases",
     col ="blue")

#barchart
Edufreq <- table(myData$Education)
Edufreq

barplot(Edufreq, 
        main = "Education Level of Customers", 
        xlab = "Education Level", 
        ylab = "Frequency", 
        col = c("red", "green", "yellow", "blue","pink"), 
        legend = rownames(Edufreq),
        border = "black",ylim=c(0,1200))
abline(h=0)

Martfreq <- table(myData$Marital_Status)
Martfreq

barplot(freq, 
        main = "Marital Status of Customers", 
        xlab = "Marital Status", 
        ylab = "Frequency", 
        col = c("red", "green", "yellow", "blue","pink","cyan","maroon","gold"), 
        legend = rownames(Martfreq),
        border = "black",ylim=c(0,1000))
abline(h=0)

#contingency table
contingency_table <- table(myData$IncomeBin, myData$Education)
contingency_table

#stacked column chart
barplot(contingency_table, col=c('yellow','orange','pink'),
        legend = rownames(contingency_table), xlab='Education',
        ylab='Income Bin',ylim= c(0,1300))
abline(h=0)

#scatter plot
plot(myData$MntMeatProducts ~ myData$MntWines,
     main="Scatterplot of Meat Products against Wine",
     xlab = "Amount spent on Wines",
     ylab = "Amount spent on Meat Products",
     col = "violet",
     pch=10, cex=1,font=15)


#bubble chart
plot(myData$MntFruits ~ myData$MntSweetProducts, type="n")+
  symbols(myData$MntFruits ~ myData$MntSweetProducts, circles = myData$MntWines,
          inches=0.2, bg="orange",
          main="A bubble plot of Fruits, Sweet and Wine products",
          xlab = "Amount spent on Sweets",
          ylab = "Amount spent on Fruits")

#line chart
plot(myData$Recency, myData$NumWebVisitsMonth,
     main="Number of web visits and purchases",
     xlab="Number of days since last purchase", ylab="Number of Web visits in last month",
     col ="blue", type ="l", ylim=c(0,25))

#heatmap

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("fields")
library(fields)

selected_data <- myData[, c("NumWebPurchases", "NumStorePurchases", "NumDealsPurchases", 
                            "MntMeatProducts", "MntWines", "MntFishProducts", "MntGoldProds")]

heatmap_corr_matrix <- cor(selected_data, use = "complete.obs")

single_color_palette <- colorRampPalette(c("white", "blue"))(256)

heatmap(heatmap_corr_matrix, 
        col = single_color_palette, 
        scale = "none", 
        cexCol = 1.2,  
        cexRow = 1.2,  
        Rowv = NA, 
        Colv = NA,
        main = "Heatmap of Correlations Among Spending and Purchase Variables (Single Color)",
        margins = c(8, 8)) 


image.plot(legend.only = TRUE,
           zlim = range(heatmap_corr_matrix),
           col = single_color_palette,
           legend.shrink = 0.5,
           legend.width = 1.2, 
           axis.args = list(cex.axis = 1)) 


