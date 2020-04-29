#Case Study for making credit Segments

credit=read.csv("C:/Users/INDIA/agno next_CC GENERAL.csv",sep=',')
credit

summary(credit)
# There is 1 NA value in CREDIT_LIMIT 
# 313 NA value in MINIMUM_PAYMENT 

sum(is.na(credit$MINIMUM_PAYMENTS))
sum(is.na(credit$CREDIT_LIMIT))

# Missing Value Treatment
credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)] <- mean(credit$MINIMUM_PAYMENTS, na.rm = TRUE)
summary(credit$MINIMUM_PAYMENTS)
credit$MINIMUM_PAYMENTS
# Missing value imputed for MINIMUM_PAYMENT columns

credit$CREDIT_LIMIT[is.na(credit$CREDIT_LIMIT)]<-mean(credit$CREDIT_LIMIT,na.rm=TRUE)
summary(credit)
# Missing Value imputed for CREDIT_LIMIT

sum(is.na(credit$MINIMUM_PAYMENTS))
sum(is.na(credit$CREDIT_LIMIT))

# DERIVING KPI
credit$Monthly_avg_Purchase=credit$PURCHASES/credit$TENURE
sum(is.na(credit$Monthly_avg_Purchase))
#Box Plot of monthly average purchase
boxplot(credit$Monthly_avg_Purchase,main='monthly Average Purchase',col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)

# KPI 2nd Monthly_cash_Advance
credit$Monthly_Cash_Advance=credit$CASH_ADVANCE/credit$TENURE

#box plot of monthly cash advance
boxplot(credit$Monthly_Cash_Advance,main='monthly cash Advance',col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE)

# Accessing credit one off purchase and installement purchase
credit[,5:6]

#There are four types of purchase manner
#1. Those customer who does only one off purchase 
#2.Those customer who does installement Purchase 
#3. those customer who does neither installement purchase neither one off purchase
#4. Those customer who does voth one-off purchase as well as installement purchase


#number of customers whose does not do one off purchase
length(credit$ONEOFF_PURCHASES[credit$ONEOFF_PURCHASES==0])

#number of customers whose does not do installement 
length(credit$INSTALLMENTS_PURCHASES[credit$INSTALLMENTS_PURCHASES==0])

#number of customers whose one_off purchase>0
length(credit$ONEOFF_PURCHASES[credit$ONEOFF_PURCHASES>0])

#number of customers whose installment purchase>0
length(credit$INSTALLMENTS_PURCHASES[credit$INSTALLMENTS_PURCHASES>0])

#number of customers whose one_off purchase>0 and installement purchase>0
dim(subset(credit,credit$ONEOFF_PURCHASES>0 & credit$INSTALLMENTS_PURCHASES==0))

#number of customer whose one_off purchase >0 and installment_purchase>0
dim(subset(credit,ONEOFF_PURCHASES>0 & INSTALLMENTS_PURCHASES>0))

#number of customers whose one_off purchase=0 and installment purchase>0
dim(subset(credit,ONEOFF_PURCHASES==0 & INSTALLMENTS_PURCHASES>0))

#number of customers whose oneoff purchase and installement purchase both are zero
dim(subset(credit,ONEOFF_PURCHASES==0 & INSTALLMENTS_PURCHASES==0))

#number of customers one off prchase and installment both are greater than zero 
dim(subset(credit,ONEOFF_PURCHASES>0 & INSTALLMENTS_PURCHASES>0))

# using ifelse function created new variable called purchase and added as the column in credit
credit['PURCHASE']=ifelse(credit$ONEOFF_PURCHASES>0 & credit$INSTALLMENTS_PURCHASES==0,'one off purchase',ifelse(credit$ONEOFF_PURCHASES==0 & credit$INSTALLMENTS_PURCHASES>0,'installment_purchase',ifelse(credit$ONEOFF_PURCHASES>0 & credit$INSTALLMENTS_PURCHASES>0,"both are available",'both are unavailable')))

#counting the number of distinct values in PURCHASE column
table(credit$PURCHASE)

#here  not any null value available in purchase column
sum(is.na(credit$PURCHASE))
#column names of credit
colnames(credit)

#Limit Usage new KPI balance to credit ratio
credit['Limit_Usage']=credit$BALANCE/credit$CREDIT_LIMIT
head(credit$Limit_Usage)

#BoxPlot
boxplot(credit$Limit_Usage)

#their is no null value present in Limit_usage columns
sum(is.na(credit$Limit_Usage))

#Payment to minimum ratio
credit['Payment_usage']=credit$PAYMENTS/credit$MINIMUM_PAYMENTS
sum(is.na(credit$MINIMUM_PAYMENTS))
boxplot(credit$Payment_usage)

#Advance Reporting
#Relation between monthly_average_purchase and Purchase Type
agg_mean<-aggregate(credit$Monthly_avg_Purchase,by=list(credit$PURCHASE),FUN=mean,na.rm=TRUE)
agg_mean

#barplot of monthly_average_purchase and purchase_type
barplot(x~Group.1,data=agg_mean)

#customer with one off and installment purchase in a month
#while customers with no oneoff and no installment has low interest to purchase in a month
#We should use incentive to attract them

#relation between monthly cash advance and purchase type
agg_mean1<-aggregate(credit$Monthly_Cash_Advance,by=list(credit$PURCHASE),FUN=mean,na.rm=TRUE)
agg_mean1
#barplot of monthly_cash_advance and purchase type
barplot(x~Group.1,data=agg_mean1)

#customer with no oneoff and no installement take high cash advance
#if we target them with incentives in cash advance rate card could help them sign up them more

#Relationship between limit usage and purchase type
agg_mean2<-aggregate(credit$Limit_Usage,by=list(credit$PURCHASE),FUN=mean,na.rm=TRUE)
#barplot of limit usage and purchase type
barplot(x~Group.1,data=agg_mean2)
#customers with no oneoff and no installment has highest limit usage which is a risk
#while customer with installment purchase have good credit score because their limit usage is less


#Relation between Payment Usage and Purchase Type
agg_3<-aggregate(credit$Payment_usage,by=list(credit$PURCHASE),FUN=mean,na.rm=TRUE)
#Barplot of payment usage and purchase
barplot(x~Group.1,data=agg_3)
#customer with installment purchase are paying dues

agg_4<-aggregate(credit$PURCHASES,by=list(credit$PURCHASE),FUN=mean,na.rm=TRUE)
boxplot(x~Group.1,data=agg_4)
#There are many variables wih extreme values so log transformation is required to mitigate the extreme values

#For identifying classes
total_var=sapply(credit,class)
total_var
numeric_var=which(total_var=="numeric" | total_var=="integer")
numeric_var

categorical_var=which(total_var=='factor' | total_var=='character')
categorical_var

#As the variables Cust_id and Purchase variables are categorical variables so we can perform log transformation in remaining other variables
credit_log=credit[,-which(names(credit)=="CUST_ID" | names(credit)=="PURCHASE")]
colnames(credit_log)

#now applying natural logarithm 

credit_log=log(credit_log+1)

#getting dummies variable for categorical variables
library(dummies)
df1=cbind(credit_log,dummy(credit$PURCHASE,sep=","))
df1

sum(is.na(df1$`credit_log,both are unavailable`))

#GETTING RID OF THOSE VARIABLES WHICH ARE USED IN DERIVING NEW KPI
var=list('PURCHASES','TENURE','CASH_ADVANCE','BALANCE','CREDIT_LIMIT','PAYMENTS','MINIMUM_PAYMENTS')
df1=df1[,-which(names(credit)=='PURCHASES' | names(credit)=='TENURE'|names(credit)=='CASH_ADVANCE'|names(credit)=='BALANCE'|names(credit)=='CREDIT_LIMIT'|names(credit)=='PAYMENTS'|names(credit)=='MINIMUM_PAYMENTS')]
df1
summary(df1)

#Getting Heatmap of our Current Variable
library("d3heatmap")
d3heatmap(scale(df1), colors = "RdBu")
credit_scaled=scale(df1)
dim(scale(df1))

#Applying PCA

df1.pca<-prcomp(credit_scaled,center=TRUE,scale. = TRUE)
df1.pca
summary(df1.pca)

#Getting the Scree Plot
screeplot(df1.pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(df1.pca$sdev^2 / sum(df1.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)
rotation_vector=df1.pca$rotation[,1:5]
rotation_vector
View(credit_scaled %*% rotation_vector)
credit_original=credit_scaled %*% rotation_vector

#K-means
library(cluster)
library(factoextra)
set.seed(123) #since this algorithm starts with k randomly selected centroids, its recomended to set the seed for Rs random number generator 
km.res <- kmeans(credit_original, centers = 3, iter.max = 250, nstart =25) #nstart is recomended to be 25 - 50
head(km.res) 
### Method 1: Elbow Method
wss <- function(k) {
        kmeans(credit_original, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- sapply(k.values,wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#The elbow plot shows that the gain in explained variance reduces significantly
#from 4 to 5 .So the optimal number of clusters is either 4 or 5
#The Actual number of clusters finally depends on buisness context.
actual_clustering_4=kmeans(credit_original, centers = 4, iter.max = 250, nstart =25)
actual_clustering_4
actual_clustering_5=kmeans(credit_original, centers = 5, iter.max = 250, nstart =25)
actual_clustering_5

