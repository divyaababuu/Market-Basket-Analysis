library("xtable") # processing of regression output
library("knitr") # used for report compilation and table display
library("ggplot2") # very popular plotting library ggplot2
install.packages("dfidx")
library("dfidx")
library("mlogit") # multinomial logit
library("ggplot2")
library("lattice")
library("caret")
cbc.df<-read.csv("Data_Conjoint_Choice.csv", stringsAsFactors = TRUE)
str(cbc.df)
head(cbc.df)
summary(cbc.df)
xtabs(Choice~Price, data=cbc.df) #how many times each price point was chosen
xtabs(Choice~Size, data=cbc.df)

#choosing reference levels
cbc.df$Brand <- relevel(cbc.df$Brand, ref = "Nexus")
cbc.df$Size <- relevel(cbc.df$Size, ref = "sz7inch")
cbc.df$Storage <- relevel(cbc.df$Storage, ref = "st16gb")
cbc.df$Ram <- relevel(cbc.df$Ram, ref = "r1gb")
cbc.df$Battery <- relevel(cbc.df$Battery, ref = "b7h")


library("dfidx")
cbc.mlogit <- dfidx(cbc.df, choice="Choice",
                    idx=list(c("ChoiceSetId", "ConsumerId"), "AlternativeIdInSet"))
head(cbc.mlogit)

#estimate model
library("mlogit")
model<-mlogit(Choice ~ 0+Brand+Size+Storage+Ram+Battery+Price, data=cbc.mlogit)
#in above func, we use choice ~ 1 if we want constant; we use 0 here because we do not want constant 
library("knitr")
kable(summary(model)$CoefTable) #if the pr value is less than 0.05, it's significant

head(cbc.mlogit)
library("mlogit")
model<-mlogit(Choice ~ 0+Brand+Size+Storage+Ram+Battery+Price, data=cbc.mlogit)
library("knitr")
kable(summary(model)$CoefTable) #if the pr value is less than 0.05, it's significant

model.constraint <-mlogit(Choice ~ 0+Brand, data = cbc.mlogit)
lrtest(model, model.constraint) #likelihood ratio test
#the larger model is the better one
#the model with the loglikelihood value close to 0 is the better one
#if the value is less than 0.05, then these two models are not very different from each other

#Predicted Market Share
kable(head(predict(model,cbc.mlogit)))

predicted_alternative <- apply(predict(model,cbc.mlogit),1,which.max)
selected_alternative <- cbc.mlogit$AlternativeIdInSet[cbc.mlogit$Choice>0]
library("caret")
confusionMatrix(table(predicted_alternative,selected_alternative),positive = "1")
#the pred_alt has correctly predicted 362 + 449 + 347 - out of 2K instances, 1158 has been predicted rightly. SO over 50% accuracy for this model

#willingness to pay - there's a formula - estimate of the feature/-estimate of price
## -0.0050888 #1 dollar price will lead to 0.05 decrease in utility
## 1/-(-0.0050888) #wtp for 1 utility
coef(model)["BrandiPad"]/-coef(model)["Price"]
coef(model)["BrandiPad"]-coef(model)["BrandGalaxy"] / (-coef(model)["Price"]) #125 is the right value - I'm not getting it
coef(model)["Ramr4gb"] /(-coef(model)["Price"])
coef(model)["Sizesz9inch"] / (-coef(model)["Price"])
