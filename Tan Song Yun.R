#========================================================================================================
# Purpose:      RE6013 Computer Based Assignment
# Author:       Tan S. Y.
# DOC:          5 Apr 2023
# Data:         homeloan3.csv
#=========================================================================================================


# Load Packages ------------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)
install.packages("caTools")
library(caTools)
install.packages("caret")
library(caret)
install.packages("rpart")
library(rpart)
library(rpart.plot)
library(dplyr)

# Load Data ----------------------------------------------------------------------------------------------

setwd("/Users/songyun/Documents/Y4S2/Business\ Analytics\ and\ Machine\ Learning/CBA")

homeloan <- data.table::fread("homeloan3.csv", na.strings=c("NA", ""), stringsAsFactors = T)
dim(homeloan)

head(homeloan)
summary(homeloan)

homeloan$Gender <- factor(homeloan$Gender) 
homeloan$Married <- factor(homeloan$Married) 
homeloan$Dependents <- factor(homeloan$Dependents) 
homeloan$Education <- factor(homeloan$Education) 
homeloan$Self_Employed <- factor(homeloan$Self_Employed) 
homeloan$Credit_Score <- factor(homeloan$Credit_Score) 
homeloan$Property_Area <- factor(homeloan$Property_Area) 
homeloan$Loan_Status <- factor(homeloan$Loan_Status)

summary(homeloan)

# Data Exploration ---------------------------------------------------------------------------------------

# bar graph with Gender
ggplot(homeloan, aes(x=Gender, fill=Gender)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("Female" = "slateblue","Male" = "blueviolet"), na.value = "thistle1")+
  labs(title="Distribution of Gender", x="Gender", y="Count")

# bar graph with Married
ggplot(homeloan, aes(x=Married, fill=Married)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("No" = "slateblue","Yes" = "blueviolet"), na.value = "thistle1")+
  labs(title="Distribution of Married", x="Married", y="Count")

# bar graph with Dependents
ggplot(homeloan, aes(x=Dependents, fill=Dependents)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("0" = "slateblue","1" = "blueviolet","2" = "violet", "3+" = "plum1"), na.value = "thistle1")+
  labs(title="Distribution of Dependents", x="Dependents", y="Count")

# bar graph with Education
ggplot(homeloan, aes(x=Education, fill=Education)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("Graduate" = "slateblue","Not Graduate" = "blueviolet"))+
  labs(title="Distribution of Education", x="Education", y="Count")

# bar graph with Self Employed
ggplot(homeloan, aes(x=Self_Employed, fill=Self_Employed)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("No" = "slateblue","Yes" = "blueviolet"), na.value = "thistle1")+
  labs(title="Distribution of Self Employed", x="Self Employed", y="Count")

# density plot with Applicant Income
ggplot(homeloan, aes(x=ApplicantIncome)) +
  geom_histogram(aes(y=..density..), colour="navyblue", fill="white")+
  geom_density(alpha=.2, color="navyblue", fill="slateblue")+
  labs(title="Distribution of Applicant Income", x="Applicant Income", y="Density")

# density plot with Coapplicant Income
ggplot(homeloan, aes(x=CoapplicantIncome)) +
  geom_histogram(aes(y=..density..), colour="navyblue", fill="white")+
  geom_density(alpha=.2, color="navyblue", fill="slateblue")+
  labs(title="Distribution of Coapplicant Income", x="Coapplicant Income", y="Density")

# density plot with Loan Amount
ggplot(homeloan, aes(x=LoanAmount)) +
  geom_histogram(aes(y=..density..), colour="navyblue", fill="white")+
  geom_density(alpha=.2, color="navyblue", fill="slateblue")+
  labs(title="Distribution of Loan Amount", x="Loan Amount (in thousands)", y="Density")

# density plot with Loan Amount Term
ggplot(homeloan, aes(x=Loan_Amount_Term)) +
  geom_histogram(aes(y=..density..), colour="navyblue", fill="white")+
  geom_density(alpha=.2, color="navyblue", fill="slateblue")+
  labs(title="Distribution of Loan Amount Term", x="Loan Amount Term (months)", y="Density")

# bar graph with Credit Score
ggplot(homeloan, aes(x=Credit_Score, fill=Credit_Score)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("0" = "slateblue","1"= "blueviolet"), na.value = "thistle1")+
  labs(title="Distribution of Credit Score", x="Credit Score", y="Count")

# bar graph with Property Area
ggplot(homeloan, aes(x=Property_Area, fill=Property_Area)) +
  geom_bar(color="navyblue")+
  scale_fill_manual(values=c("A" = "slateblue","B" = "blueviolet", "C" = "violet"))+
  labs(title="Distribution of Property Area", x="Property Area", y="Count")

# bar graph with Loan Status
ggplot(homeloan, aes(x=Loan_Status, fill=Loan_Status)) +
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status", x="Loan Status", y="Count")

# bar chart with y = Loan Status, x = Gender
ggplot(homeloan, aes(Gender, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Gender", x="Gender", y="Count", fill="Loan Status")

# bar chart with y = Loan Status, x = Married
ggplot(homeloan, aes(Married, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Married", x="Married", y="Count", fill="Loan Status")

# bar chart with y = Loan Status, x = Dependents
ggplot(homeloan, aes(Dependents, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Dependents", x="Dependents", y="Count", fill="Loan Status")

# bar chart with y = Loan Status, x = Education
ggplot(homeloan, aes(Education, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Education", x="Education", y="Count", fill="Loan Status")

# bar chart with y = Loan Status, x = Self Employed
ggplot(homeloan, aes(Self_Employed, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Self Employed", x="Self Employed", y="Count", fill="Loan Status")

# box plot with y = Loan Status, x = Applicant Income
ggplot(homeloan, aes(x=ApplicantIncome, y=Loan_Status, fill=Loan_Status)) +
  geom_boxplot(outlier.colour="red")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Applicant Income", x="Applicant Income", y="Loan Status", fill="Loan Status")

# box plot with y = Loan Status, x = Coapplicant Income
ggplot(homeloan, aes(x=CoapplicantIncome, y=Loan_Status, fill=Loan_Status)) +
  geom_boxplot(outlier.colour="red")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Coapplicant Income", x="Coapplicant Income", y="Loan Status", fill="Loan Status")

# box plot with y = Loan Status, x = Loan Amount
ggplot(homeloan, aes(x=LoanAmount, y=Loan_Status, fill=Loan_Status)) +
  geom_boxplot(outlier.colour="red")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Loan Amount", x="Loan Amount", y="Loan Status", fill="Loan Status")

# bar chart with y = Loan Status, x = Credit Score
ggplot(homeloan, aes(Credit_Score, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Credit Score", x="Credit Score", y="Count", fill="Loan Status")

# bar chart with y = Loan Status, x = Property Area
ggplot(homeloan, aes(Property_Area, fill=Loan_Status))+
  geom_bar(colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Property Area", x="Property Area", y="Count", fill="Loan Status")

# Data Cleaning ------------------------------------------------------------------------------------------

sum(duplicated(homeloan)) # no duplicate rows
summary(homeloan) # no negative values

# replace values 3+ with 3
homeloan[, Dependents := Dependents][Dependents == "3+", Dependents := "3"]
homeloan$Dependents <- factor(homeloan$Dependents, levels=c("0","1","2","3")) 

# remove outliers for Applicant Income column
# calculate the interquartile range (IQR) of the Applicant Income column
Q1 <- quantile(homeloan$ApplicantIncome, 0.25)
Q3 <- quantile(homeloan$ApplicantIncome, 0.75)
IQR <- Q3 - Q1

# calculate the upper and lower bounds for outliers
upper_bound <- Q3 + 1.5*IQR
lower_bound <- Q1 - 1.5*IQR

# drop rows where Applicant Income is above upper bound or below lower bound
homeloan.dropAI <- homeloan %>%
  filter(ApplicantIncome >= lower_bound & ApplicantIncome <= upper_bound)

# compare density plot before and after removing outliers
ggplot(homeloan, aes(x=ApplicantIncome)) +
  geom_histogram(aes(y=..density..), colour="navyblue", fill="white")+
  geom_density(alpha=.2, color="navyblue", fill="slateblue")+
  labs(title="Distribution of Applicant Income", x="Applicant Income", y="Density")

ggplot(homeloan.dropAI, aes(x=ApplicantIncome)) +
  geom_histogram(aes(y=..density..), colour="navyblue", fill="white")+
  geom_density(alpha=.2, color="navyblue", fill="slateblue")+
  labs(title="Distribution of Applicant Income", x="Applicant Income", y="Density")

# remove outliers for Coapplicant Income column
Q1 <- quantile(homeloan.dropAI$CoapplicantIncome, 0.25)
Q3 <- quantile(homeloan.dropAI$CoapplicantIncome, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5*IQR
lower_bound <- Q1 - 1.5*IQR
homeloan.dropAI.dropCI <- homeloan.dropAI %>%
  filter(CoapplicantIncome >= lower_bound & CoapplicantIncome <= upper_bound)

# remove outliers for Loan Amount column
Q1 <- quantile(homeloan.dropAI.dropCI$LoanAmount, 0.25)
Q3 <- quantile(homeloan.dropAI.dropCI$LoanAmount, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5*IQR
lower_bound <- Q1 - 1.5*IQR
homeloan.dropAI.dropCI.dropLA <- homeloan.dropAI.dropCI %>%
  filter(LoanAmount >= lower_bound & LoanAmount <= upper_bound)

homeloan.clean <- homeloan.dropAI.dropCI.dropLA 

# create new column Total Income
homeloan.clean$TotalIncome <- homeloan.clean$ApplicantIncome + homeloan.clean$CoapplicantIncome
summary(homeloan.clean)


# NA values
sapply(homeloan.clean, function(x) sum(is.na(x)))

homeloan.clean.dropNA <- homeloan.clean

# create mode() function to calculate mode
mode <- function(x, na.rm = FALSE) {
  if(na.rm){ #if na.rm is TRUE, remove NA values from input x
    x = x[!is.na(x)]
  }
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

# replace NA values in Gender column with mode
mode.gender<-mode(homeloan$Gender, na.rm=TRUE)
homeloan.clean.dropNA[, Gender := replace(Gender, is.na(Gender), mode.gender)]
# replace NA values in Married column with mode
mode.married<-mode(homeloan$Married, na.rm=TRUE)
homeloan.clean.dropNA[, Married := replace(Married, is.na(Married), mode.married)]
# replace NA values in Dependents column with mode
mode.dependents<-mode(homeloan$Dependents, na.rm=TRUE)
homeloan.clean.dropNA[, Dependents := replace(Dependents, is.na(Dependents), mode.dependents)]
# replace NA values in Self_Employed column with mode
mode.selfemployed<-mode(homeloan$Self_Employed, na.rm=TRUE)
homeloan.clean.dropNA[, Self_Employed := replace(Self_Employed, is.na(Self_Employed), mode.selfemployed)]
# replace NA values in Credit_Score column with mode
mode.creditscore<-mode(homeloan$Credit_Score, na.rm=TRUE)
homeloan.clean.dropNA[, Credit_Score := replace(Credit_Score, is.na(Credit_Score), mode.creditscore)]
# replace NA values in Loan_Amount_Term column with median
homeloan.clean.dropNA[, Loan_Amount_Term := ifelse(is.na(Loan_Amount_Term), median(Loan_Amount_Term, na.rm = TRUE), Loan_Amount_Term)]

# homeloan.dropNA <- na.omit(homeloan)
sapply(homeloan.clean.dropNA, function(x) sum(is.na(x)))
summary(homeloan.clean.dropNA)

# create binary variables for one-hot encoding
homeloan.clean.dropNA.onehot <- data.table(model.matrix(~., data = homeloan.clean.dropNA[, -1]))
summary(homeloan.clean.dropNA.onehot)
head(homeloan.clean.dropNA.onehot)

# Logistic Regression Model ------------------------------------------------------------------------------
set.seed(10)
log.train <- sample.split(Y = homeloan.clean.dropNA.onehot$Loan_StatusY, SplitRatio = 0.8)
log.trainset <- subset(homeloan.clean.dropNA.onehot, log.train == T)
log.testset <- subset(homeloan.clean.dropNA.onehot, log.train == F)

summary(log.trainset)
summary(log.testset)

log.model <- glm(Loan_StatusY ~ . , family = binomial, data = log.trainset)
summary(log.model)
varImp(log.model, scale = FALSE, order = TRUE)

log.model.predict <- predict(log.model, newdata=log.testset)
threshold <- sum(log.trainset$Loan_StatusY == 1)/length(log.trainset$Loan_StatusY)
log.model.predict <- ifelse(log.model.predict > threshold, 1, 0)

confmat.log <- as.data.frame(table(log.testset$Loan_StatusY, log.model.predict))
confmat.log
confusionMatrix(table(log.testset$Loan_StatusY, log.model.predict))

ggplot(data = confmat.log, mapping = aes(x = Var1, y = log.model.predict)) +
  geom_tile(aes(fill = Freq)) + labs(x = "Actual",y = "Prediction") +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low="white", high="turquoise") + scale_x_discrete(labels=c("Loan Not Approved","Loan Approved")) +
  scale_y_discrete(labels=c("Loan Not Approved","Loan Approved"))


# Logistic regression with k-fold validation
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
log.model.kfold <- train(Loan_StatusY ~ . , data = log.trainset, method = "glm", trControl = train_control, na.action=na.exclude, trace=F)
print(log.model.kfold)
summary(log.model.kfold)
varImp(log.model.kfold, scale = FALSE, order = TRUE)

variables = c("GenderMale","MarriedYes","Dependents1","Dependents2","Dependents3","EducationNotGrad","SelfEmployedYes","ApplicantIncome","CoapplicantIncome","LoanAmount","LoanAmountTerm","CreditScore1","PropertyAreaB","PropertyAreaC")
ggplot(data.frame(imp = varImp(log.model.kfold, scale = FALSE)$importance, variables)) +
  geom_col(aes(x = reorder(variables, -Overall), y = Overall), color="navyblue", fill="slateblue") +
  labs(title="Variable Importance for CART model", x="Variables", y="Count")

log.model.kfold.predict <- predict(log.model.kfold, newdata=log.testset)
threshold <- sum(log.trainset$Loan_StatusY == 1)/length(log.trainset$Loan_StatusY)
log.model.kfold.predict <- ifelse(log.model.kfold.predict > threshold, 1, 0)

confmat.log.kfold <- as.data.frame(table(log.testset$Loan_StatusY, log.model.kfold.predict))
confusionMatrix(table(log.testset$Loan_StatusY, log.model.kfold.predict))

ggplot(data = confmat.log.kfold, mapping = aes(x = Var1, y = log.model.kfold.predict)) +
  geom_tile(aes(fill = Freq)) + labs(x = "Actual",y = "Prediction") +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low="white", high="turquoise") + scale_x_discrete(labels=c("Loan Not Approved","Loan Approved")) +
  scale_y_discrete(labels=c("Loan Not Approved","Loan Approved"))


# CART Model ---------------------------------------------------------------------------------------------
set.seed(10)
cart.train <- sample.split(Y = homeloan.clean$Loan_Status, SplitRatio = 0.8)
cart.trainset <- subset(homeloan.clean, cart.train == T, select=-Loan_ID)
cart.testset <- subset(homeloan.clean, cart.train == F, select=-Loan_ID)

cart.model <- rpart(Loan_Status ~ . , data = cart.trainset, method = 'class',
                       control = rpart.control(minsplit = 20, cp = 0))

# plots the maximal tree and results
rpart.plot(cart.model, nn= T, main = "Maximal Tree")
print(cart.model)
printcp(cart.model)
plotcp(cart.model, main="Subtrees in homeloan")
cart.model$variable.importance
data.frame(imp = cart.model$variable.importance, variables)

variables = c("Credit Score", "Total Income", "Applicant Income", "Loan Amount", "Property Area", "Coapplicant Income", "Self Employed", "Loan Amount Term")
ggplot(data.frame(imp = cart.model$variable.importance, variables)) +
  geom_col(aes(x = reorder(variables, -imp), y = imp), color="navyblue", fill="slateblue") +
  labs(title="Variable Importance for CART model", x="Variables", y="Count")

# automatically compute cp.opt
CVerror.cap <- cart.model$cptable[which.min(cart.model$cptable[,"xerror"]), "xerror"] + cart.model$cptable[which.min(cart.model$cptable[,"xerror"]), "xstd"]
i <- 1; j <- 4
while (cart.model$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cart.model$cptable[i,1] * cart.model$cptable[i-1,1]), 1)
cp.opt

cart.model.pruned <- prune(cart.model, cp = cp.opt)

rpart.plot(cart.model.pruned, nn= T, main = "Pruned Tree")
printcp(cart.model.pruned)

cart.model.pruned.predict <- predict(cart.model.pruned, cart.testset, type = "class")
cart.model.pruned$variable.importance

confmat.cart <- as.data.frame(table(cart.testset$Loan_Status, cart.model.pruned.predict))
confusionMatrix(table(cart.testset$Loan_Status, cart.model.pruned.predict))
confmat.cart

ggplot(data = confmat.cart, mapping = aes(x = Var1, y = cart.model.pruned.predict)) +
  geom_tile(aes(fill = Freq)) + labs(x = "Actual",y = "Prediction") +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low="white", high="turquoise") + scale_x_discrete(labels=c("Loan Not Approved","Loan Approved")) +
  scale_y_discrete(labels=c("Loan Not Approved","Loan Approved"))

# CART with k-fold validation
set.seed(10)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
cart.model.kfold <- train(Loan_Status ~ . , data = cart.trainset, method = "rpart", trControl = train_control, na.action = na.pass)
print(cart.model.kfold)

cart.model.kfold$finalModel

rpart.plot(cart.model.kfold$finalModel, nn= T, main = "Best Pruned Tree of 10-fold CART")

cart.model.kfold.predict = predict(cart.model.kfold, newdata = cart.testset, na.action = na.pass)
confmat.cart.kfold <- as.data.frame(table(cart.testset$Loan_Status, cart.model.kfold.predict))
confusionMatrix(table(cart.testset$Loan_Status, cart.model.kfold.predict))

ggplot(data = confmat.cart.kfold, mapping = aes(x = Var1, y = cart.model.kfold.predict)) +
  geom_tile(aes(fill = Freq)) + labs(x = "Actual",y = "Prediction") +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low="white", high="turquoise") + scale_x_discrete(labels=c("Loan Not Approved","Loan Approved")) +
  scale_y_discrete(labels=c("Loan Not Approved","Loan Approved"))


# Gender Discrimination ---------------------------------------------------------------------------------------------

# bar chart with y = Loan Status, x = Gender
ggplot(homeloan, aes(Gender, fill=Loan_Status))+
  geom_bar(position="fill", colour="grey25")+
  scale_fill_manual(values=c("Y" = "palegreen", "N" = "pink1"))+
  labs(title="Distribution of Loan Status across Gender", x="Gender", y="Proportion", fill="Loan Status")+
  scale_y_continuous(labels = scales::percent)

# slightly higher percentage of females do not get loan approvals as compared to males
# might be due to some other reason
ggplot(homeloan, aes(Gender, fill=Credit_Score))+
  geom_bar(position="fill", colour="grey25")+
  scale_fill_manual(values=c("1" = "slateblue", "0" = "thistle1"))+
  labs(title="Distribution of Credit Score across Gender", x="Gender", y="Proportion", fill="Credit Score")+
  scale_y_continuous(labels = scales::percent)

ggplot(homeloan, aes(Gender, fill=Property_Area))+
  geom_bar(position="fill", colour="grey25")+
  scale_fill_manual(values=c("A" = "slateblue", "B" = "violet", "C" = "thistle1"))+
  labs(title="Distribution of Property Area across Gender", x="Gender", y="Proportion", fill="Property Area")+
  scale_y_continuous(labels = scales::percent)

# box plot with y = Gender, x = Applicant Income
ggplot(homeloan, aes(x=ApplicantIncome, y=Gender, fill=Gender)) +
  geom_boxplot(outlier.colour="red")+
  scale_fill_manual(values=c("Male" = "lightskyblue", "Female" = "lightpink"))+
  labs(title="Distribution of Gender across Applicant Income", x="Applicant Income", y="Gender", fill="Gender")

# box plot with y = Gender, x = Loan Amount
ggplot(homeloan, aes(x=LoanAmount, y=Gender, fill=Gender)) +
  geom_boxplot(outlier.colour="red")+
  scale_fill_manual(values=c("Male" = "lightskyblue", "Female" = "lightpink"))+
  labs(title="Distribution of Gender across Loan Amount", x="Loan Amount", y="Gender", fill="Gender")


# Logistic Regression Model on dataset without one-hot encoding -------------------------------------------------------
set.seed(10)
log.train <- sample.split(Y = homeloan.clean.dropNA$Loan_Status, SplitRatio = 0.8)
log.trainset <- subset(homeloan.clean.dropNA, log.train == T, select=-Loan_ID)
log.testset <- subset(homeloan.clean.dropNA, log.train == F, select=-Loan_ID)

summary(log.trainset)
summary(log.testset)

log.model <- glm(Loan_Status ~ . , family = binomial, data = log.trainset)
summary(log.model)
varImp(log.model, scale = FALSE)

log.model.predict <- predict(log.model, newdata=log.testset)
threshold <- sum(log.trainset$Loan_Status == 1)/length(log.trainset$Loan_Status)
log.model.predict <- ifelse(log.model.predict > threshold, "Y", "N")

confmat.log <- as.data.frame(table(log.testset$Loan_Status, log.model.predict))
confmat.log
confusionMatrix(table(log.testset$Loan_Status, log.model.predict))

ggplot(data = confmat.log, mapping = aes(x = Var1, y = log.model.predict)) +
  geom_tile(aes(fill = Freq)) + labs(x = "Actual",y = "Prediction") +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low="white", high="turquoise") + scale_x_discrete(labels=c("Loan Not Approved","Loan Approved")) +
  scale_y_discrete(labels=c("Loan Not Approved","Loan Approved"))

# ============================================= END ====================================================