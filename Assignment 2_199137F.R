customerData=read.csv("C:/Users/MISCO/Desktop/New folder (2)/datasets_13996_18858_WA_Fn-UseC_-Telco-Customer-Churn.csv",header = TRUE)
customerData
head(customerData)




#clean data
which(is.na(customerData),arr.ind = TRUE)
customerDataClean=customerData[rowSums(is.na(customerData))==0,]
nrow(customerData)
nrow(customerDataClean)

head(customerDataClean)



customerDataClean$gender
customerDataClean$gender[customerDataClean$gender=="Male"]=0
customerDataClean$gender[customerDataClean$gender=="Female"]=1
customerDataClean$gender=as.integer(customerDataClean$gender)


customerDataClean$Partner
customerDataClean$Partner[customerDataClean$Partner=="No"]=0
customerDataClean$Partner[customerDataClean$Partner=="Yes"]=1
customerDataClean$Partner=as.integer(customerDataClean$Partner)

customerDataClean$Dependents
customerDataClean$Dependents[customerDataClean$Dependents=="No"]=0
customerDataClean$Dependents[customerDataClean$Dependents=="Yes"]=1
customerDataClean$Dependents=as.integer(customerDataClean$Dependents)

customerDataClean$PhoneService
customerDataClean$PhoneService[customerDataClean$PhoneService=="No"]=0
customerDataClean$PhoneService[customerDataClean$PhoneService=="Yes"]=1
customerDataClean$PhoneService=as.integer(customerDataClean$PhoneService)

customerDataClean$MultipleLines
customerDataClean$MultipleLines[customerDataClean$MultipleLines=="No"]=0
customerDataClean$MultipleLines[customerDataClean$MultipleLines=="Yes"]=1
customerDataClean$MultipleLines[customerDataClean$MultipleLines=="No phone service"]=2
customerDataClean$MultipleLines=as.integer(customerDataClean$MultipleLines)

customerDataClean$InternetService
customerDataClean$InternetService[customerDataClean$InternetService=="No"]=0
customerDataClean$InternetService[customerDataClean$InternetService=="DSL"]=1
customerDataClean$InternetService[customerDataClean$InternetService=="Fiber optic"]=2
customerDataClean$InternetService=as.integer(customerDataClean$InternetService)

customerDataClean$OnlineSecurity
customerDataClean$OnlineSecurity[customerDataClean$OnlineSecurity=="No"]=0
customerDataClean$OnlineSecurity[customerDataClean$OnlineSecurity=="Yes"]=1
customerDataClean$OnlineSecurity[customerDataClean$OnlineSecurity=="No internet service"]=2
customerDataClean$OnlineSecurity=as.integer(customerDataClean$OnlineSecurity)

customerDataClean$OnlineBackup
customerDataClean$OnlineBackup[customerDataClean$OnlineBackup=="No"]=0
customerDataClean$OnlineBackup[customerDataClean$OnlineBackup=="Yes"]=1
customerDataClean$OnlineBackup[customerDataClean$OnlineBackup=="No internet service"]=2
customerDataClean$OnlineBackup=as.integer(customerDataClean$OnlineBackup)

customerDataClean$DeviceProtection
customerDataClean$DeviceProtection[customerDataClean$DeviceProtection=="No"]=0
customerDataClean$DeviceProtection[customerDataClean$DeviceProtection=="Yes"]=1
customerDataClean$DeviceProtection[customerDataClean$DeviceProtection=="No internet service"]=2
customerDataClean$DeviceProtection=as.integer(customerDataClean$DeviceProtection)

customerDataClean$TechSupport
customerDataClean$TechSupport[customerDataClean$TechSupport=="No"]=0
customerDataClean$TechSupport[customerDataClean$TechSupport=="Yes"]=1
customerDataClean$TechSupport[customerDataClean$TechSupport=="No internet service"]=2
customerDataClean$TechSupport=as.integer(customerDataClean$TechSupport)

customerDataClean$StreamingTV
customerDataClean$StreamingTV[customerDataClean$StreamingTV=="No"]=0
customerDataClean$StreamingTV[customerDataClean$StreamingTV=="Yes"]=1
customerDataClean$StreamingTV[customerDataClean$StreamingTV=="No internet service"]=2
customerDataClean$StreamingTV=as.integer(customerDataClean$StreamingTV)

customerDataClean$StreamingMovies
customerDataClean$StreamingMovies[customerDataClean$StreamingMovies=="No"]=0
customerDataClean$StreamingMovies[customerDataClean$StreamingMovies=="Yes"]=1
customerDataClean$StreamingMovies[customerDataClean$StreamingMovies=="No internet service"]=2
customerDataClean$StreamingMovies=as.integer(customerDataClean$StreamingMovies)

customerDataClean$Contract
customerDataClean$Contract[customerDataClean$Contract=="Month-to-month"]=0
customerDataClean$Contract[customerDataClean$Contract=="One year"]=1
customerDataClean$Contract[customerDataClean$Contract=="Two year"]=2
customerDataClean$Contract=as.integer(customerDataClean$Contract)

customerDataClean$PaperlessBilling
customerDataClean$PaperlessBilling[customerDataClean$PaperlessBilling=="No"]=0
customerDataClean$PaperlessBilling[customerDataClean$PaperlessBilling=="Yes"]=1
customerDataClean$PaperlessBilling=as.integer(customerDataClean$PaperlessBilling)

customerDataClean$PaymentMethod
customerDataClean$PaymentMethod[customerDataClean$PaymentMethod=="Bank transfer (automatic)"]=0
customerDataClean$PaymentMethod[customerDataClean$PaymentMethod=="Credit card (automatic)"]=1
customerDataClean$PaymentMethod[customerDataClean$PaymentMethod=="Electronic check"]=2
customerDataClean$PaymentMethod[customerDataClean$PaymentMethod=="Mailed check"]=3
customerDataClean$PaymentMethod=as.integer(customerDataClean$PaymentMethod)

head(customerDataClean)
str(customerDataClean)

#preapre train and test datasets
set.seed(1234)
indx=sample(2,nrow(customerDataClean),replace = TRUE,prob = c(0.8,0.2))
customerData.train=customerDataClean[indx==1,2:20]
customerData.test=customerDataClean[indx==2,2:20]
summary(customerData.train)
customerData.trainLabels=customerDataClean[indx==1,21]
customerData.testLabels=customerDataClean[indx==2,21]

nrow(customerData.train)

#knn prediction k=3
library(class)
customerData.predict=knn(train = customerData.train, test = customerData.test, cl=customerData.trainLabels,k=3)
customerData.predict

#test accuracy method 1
customerData.predict==customerData.testLabels
sum(customerData.testLabels==customerData.predict)
#test accuracy method 2
#install.packages("gmodels")
library(gmodels)
CrossTable(x=customerData.testLabels,y=customerData.predict,prop.chisq = FALSE)

#test accuracy method 3
#install.packages("caret")
library(caret)
confusionMatrix(table(customerData.predict,customerData.testLabels))






#optimal K

for (i in 1:100) {

  customerData.predict=knn(train = customerData.train, test = customerData.test, cl=customerData.trainLabels,k=i)
  accuracy.kvalue=sum(customerData.testLabels==customerData.predict)
  
  cat(i,"=",accuracy.kvalue,"\n")
  
  }




#knn prediction k=11
library(class)
customerData.predict=knn(train = customerData.train, test = customerData.test, cl=customerData.trainLabels,k=11)
customerData.predict

#test accuracy method 1
customerData.predict==customerData.testLabels
sum(customerData.testLabels==customerData.predict)
#test accuracy method 2
#install.packages("gmodels")
library(gmodels)
CrossTable(x=customerData.testLabels,y=customerData.predict,prop.chisq = FALSE)

#test accuracy method 3
#install.packages("caret")
library(caret)
confusionMatrix(table(customerData.predict,customerData.testLabels))



#knn prediction k=12
library(class)
customerData.predict=knn(train = customerData.train, test = customerData.test, cl=customerData.trainLabels,k=12)
customerData.predict

#test accuracy method 1
customerData.predict==customerData.testLabels
sum(customerData.testLabels==customerData.predict)
#test accuracy method 2
#install.packages("gmodels")
library(gmodels)
CrossTable(x=customerData.testLabels,y=customerData.predict,prop.chisq = FALSE)

#test accuracy method 3
#install.packages("caret")
library(caret)
confusionMatrix(table(customerData.predict,customerData.testLabels))




#knn prediction k=13
library(class)
customerData.predict=knn(train = customerData.train, test = customerData.test, cl=customerData.trainLabels,k=13)
customerData.predict

#test accuracy method 1
customerData.predict==customerData.testLabels
sum(customerData.testLabels==customerData.predict)
#test accuracy method 2
#install.packages("gmodels")
library(gmodels)
CrossTable(x=customerData.testLabels,y=customerData.predict,prop.chisq = FALSE)

#test accuracy method 3
#install.packages("caret")
library(caret)
confusionMatrix(table(customerData.predict,customerData.testLabels))

