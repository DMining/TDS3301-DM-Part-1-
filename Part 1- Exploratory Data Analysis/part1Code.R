#Please remember to set the working directory
#read the data
sales <- read.csv("sales_data.csv", header = TRUE ,stringsAsFactors = FALSE)
#view the summary of the dataset
summary(sales)

#Drop insignificant columns
sales<-subset(sales, select = -c(PHONE, ADDRESSLINE1, ADDRESSLINE2, POSTALCODE, CONTACTLASTNAME, CONTACTFIRSTNAME))

#Change "NA" in TERRITORY to "N.America" because "NA" was read as not available
sales$TERRITORY[is.na(sales$TERRITORY)] <- "N.America"

#Convert class of ORDERDATE from CHARACTER to Date, uses lubridate package
install.packages("lubridate")
library(lubridate)
sales$ORDERDATE <- mdy_hm(sales$ORDERDATE)
#DEALSIZE TO FACTOR
sales$DEALSIZE <- factor(sales$DEALSIZE, levels = "Small", "Medium", "Large")


#Place NA to all blank cells
sales$STATE[sales$STATE == ""]<-NA #set all blank cells to NA


#INSIGHT 1
#identify unique transactions based on ORDERNUMBER
uniqtransaction <- sales[!duplicated(sales[,"ORDERNUMBER"]),]
#Plot barchart
barplot(table(uniqtransaction$COUNTRY), ylim=c(0,150), xlab="Country", ylab="Num of Order", col=rainbow(30), las=2, cex.names = 0.6, cex.axis = 1, main="Number of Transactions in Each Country")

#INSIGHT 2
#Product popularity based on number of transactions
counts <- table(sales$PRODUCTLINE)
barplot(counts, main="Popularity of each product line", ylim=c(0,1000),xlab="Product Line", ylab="Transaction Count", cex.names = 0.6, col=rainbow(30), border="black", las=2)
#Annually


#INSIGHT 3
#Identify number of unique customer from each country
uniqCustomer <- sales[!duplicated(sales[,"CUSTOMERNAME"]),]
#Identify number of customers in each country
barplot(table(uniqCustomer$COUNTRY), ylim = c(0,40), col=rainbow(30), main="Number of Customers in Each Country", xlab="Country", ylab="Num of Customer", las=2, cex.names = 0.7)


#INSIGHT 4
install.packages("dplyr")
library(dplyr)
quarterSum<-sales %>% 
  group_by(QTR_ID) %>%
  mutate(sumOfSale = sum(SALES)) %>%
  select(c(quarter=QTR_ID, sumOfSale=sumOfSale))%>%
  distinct(sumOfSale)
#plot of insight 4
barplot(drop$sumOfSale, ylim=c(0,10000000), names.arg = c("Q1","Q2","Q3","Q4"), col=rainbow(30), main="Sales in the 4 quarters of years 2003-2005", ylab="Sales total", xlab="Quarter of year")



#INSIGHT 5
#to show correlation between season and sales
install.packages("car")
library(car)
scatterplot(sales$QTR_ID, sales$SALES)
season<-cor.test(sales$QTR_ID, sales$SALES)
season #p-value > 0.05, insignificant


#INSIGHT 6
#status with !Shipped and !InProcess
delete<-subset(sales, STATUS != "Shipped" & STATUS != "In Process")
#only data with status of Shipped or InProcess
sales<-subset(sales, STATUS == "Shipped" | STATUS == "In Process")
#group by customername and ordernumber and count status
drop2<-delete %>% 
  group_by(CUSTOMERNAME, ORDERNUMBER) %>% 
  count(STATUS)
colnames(drop2)[4]<-"OCCURRENCE"          #rename column
drop2


#DETECT OUTLIERS
boxplot(sales$SALES)
boxplot(sales$QUANTITYORDERED)
boxplot(sales$MSRP)



