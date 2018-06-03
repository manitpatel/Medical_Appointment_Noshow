#import the data####

library(readr)
Appointment_Data <- read_csv("C:/Users/manit_patel/Downloads/Spring 18/R SCRIPTS REPOSITORY/KAGGLE PROJECTS/Medical Appointment No Shows/KaggleV2-May-2016.csv/KaggleV2-May-2016.csv")
View(Appointment_Data)

# exploring missing values

colSums(is.na(Appointment_Data))


# Dependent variable : No Show

class(Appointment_Data$`No-show`)

table(Appointment_Data$`No-show`)

Appointment_Data$`No-show`=ifelse(Appointment_Data$`No-show`=="Yes",1,0)

#converting the dependent variable into factor from character
Appointment_Data$`No-show`=as.factor(as.character(Appointment_Data$`No-show`))


table(Appointment_Data$`No-show`)

library(ggplot2)
ggplot(Appointment_Data, aes(x= Appointment_Data$`No-show`, fill= Appointment_Data$`No-show`)) + geom_bar()


#structure of the dataset#######

str(Appointment_Data)

#Univariate analysis

#Gender
table(Appointment_Data$Gender)
library(gridExtra)
g_Gender_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Gender, fill=Appointment_Data$Gender)) + geom_bar(position="dodge")
g_Gender_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Gender, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_Gender_1, g_Gender_2,ncol=2, top='Gender distribution')

#it is clear that gender does not seem to be related with no shows


#Age

range(Appointment_Data$Age)
class(Appointment_Data$Age)

#removing negative age

Appointment_Data = subset(Appointment_Data, Appointment_Data$Age > 0)

hist(Appointment_Data$Age)

library(gridExtra)
g_Age_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Age)) + geom_histogram(bins=40)
g_Age_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$`No-show`, y=Appointment_Data$Age, col=Appointment_Data$`No-show`)) + geom_boxplot()
grid.arrange(g_Age_1, g_Age_2,ncol=2, top='Age distribution, outliers and Status implication')

#it is clear that less age people are more likely to no show

#Scholarship

class(Appointment_Data$Scholarship)
table(Appointment_Data$Scholarship)

Appointment_Data$Scholarship=as.factor(as.integer(Appointment_Data$Scholarship))

library(gridExtra)
g_Scho_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Scholarship, fill=Appointment_Data$Scholarship)) + geom_bar(position="dodge")
g_Scho_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Scholarship, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_Scho_1, g_Scho_2,ncol=2, top='Scholarship distribution')

#it is clear that person with scholarship is more likely to no show


#Hipertension
class(Appointment_Data$Hipertension)
table(Appointment_Data$Hipertension)

Appointment_Data$Hipertension=as.factor(as.integer(Appointment_Data$Hipertension))

library(gridExtra)
g_Hip_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Hipertension, fill=Appointment_Data$Hipertension)) + geom_bar(position="dodge")
g_Hip_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Hipertension, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_Hip_1, g_Hip_2,ncol=2, top='Hipertension distribution')

# it is clear that people with hipertension are less likely to no show


#Diabetes
class(Appointment_Data$Diabetes)
table(Appointment_Data$Diabetes)

Appointment_Data$Diabetes=as.factor(as.integer(Appointment_Data$Diabetes))

library(gridExtra)
g_Dia_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Diabetes, fill=Appointment_Data$Diabetes)) + geom_bar(position="dodge")
g_Dia_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Diabetes, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_Dia_1, g_Dia_2,ncol=2, top='Diabetes distribution')

#it is clear that people with diabetes are less likely to no show


#Alcoholism
class(Appointment_Data$Alcoholism)
table(Appointment_Data$Alcoholism)

Appointment_Data$Alcoholism=as.factor(as.integer(Appointment_Data$Alcoholism))

library(gridExtra)
g_Alc_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Alcoholism, fill=Appointment_Data$Alcoholism)) + geom_bar(position="dodge")
g_Alc_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Alcoholism, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_Alc_1, g_Alc_2,ncol=2, top='Alcoholism distribution')

# it is clear that alcoholism doesnt seem to be related with the no showing of the appointment


#Handcap
class(Appointment_Data$Handcap)
table(Appointment_Data$Handcap)

#recoding handcap of 2,3,4 as 1

Appointment_Data$Handcap= ifelse(Appointment_Data$Handcap== 0 , 0 ,1)

Appointment_Data$Handcap=as.factor(as.integer(Appointment_Data$Handcap))

library(gridExtra)
g_Han_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Handcap, fill=Appointment_Data$Handcap)) + geom_bar(position="dodge")
g_Han_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$Handcap, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_Han_1, g_Han_2,ncol=2, top='Handcap distribution')


#SMS Received
class(Appointment_Data$SMS_received)
table(Appointment_Data$SMS_received)

Appointment_Data$SMS_received=as.factor(as.integer(Appointment_Data$SMS_received))

library(gridExtra)
g_SMS_1 <- ggplot(Appointment_Data, aes(x=Appointment_Data$SMS_received, fill=Appointment_Data$SMS_received)) + geom_bar(position="dodge")
g_SMS_2 <- ggplot(Appointment_Data, aes(x=Appointment_Data$SMS_received, fill=Appointment_Data$`No-show`)) + geom_bar(position="fill")
grid.arrange(g_SMS_1, g_SMS_2,ncol=2, top='SMS distribution')

# it is clear that SMS_recieved are more likely to no show



### Splitting the Data Set ####
ratio = sample(1:nrow(Appointment_Data), size = 0.4*nrow(Appointment_Data))
Test = Appointment_Data[ratio,] #Test dataset 40% of total
Training = Appointment_Data[-ratio,] #Train dataset 75% of total




#### MODEL 1 : LOGISTIC REGRESSION ####
attach(Training)
str(Training)

Log_model=glm( Training$`No-show`~ Age +  Scholarship + Hipertension +  Alcoholism  + SMS_received, data = Training, family = "binomial")
summary(Log_model)

#Accuracy of log model on test data
predict_Log_test=predict(Log_model, type="response", newdata=Test)
table(Test$`No-show`,predict_Log_test>0.5)

#Calculating c-stat on Test data
library(ROCR)
pred_input_test=prediction(predict_Log_test,Test$`No-show`)
AUC= performance(pred_input_test,"auc")
print(AUC@y.values)
#c-stat = 0.59




