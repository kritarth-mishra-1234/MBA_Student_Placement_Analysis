
setwd("C:/Users/IIM")
library("psych", lib.loc="~/R/win-library/3.4")
mba<-read.csv(paste("MBA Starting Salaries Data.csv", sep = ""))
View(mba)
placed<-mba[which(mba$salary!=0 & mba$salary!=999 & mba$salary!=998),]
View(placed)

library("corrgram", lib.loc="~/R/win-library/3.4")
library("corrplot", lib.loc="~/R/win-library/3.4")
corrgram(placed, order = TRUE, upper.panel = panel.pie)

cor.test(placed$salary, placed$age) #a

mytable<-xtabs(~salary+sex, data=placed)  #b
mytable
chisq.test(mytable, simulate.p.value = TRUE)
cor.test(placed$salary, placed$sex)

cor.test(placed$salary, placed$gmat_tot)  #c

cor.test(placed$salary, placed$gmat_qpc)  #d

cor.test(placed$salary, placed$gmat_vpc)  #e

cor.test(placed$salary, placed$gmat_tpc)  #f

cor.test(placed$salary, placed$s_avg)  #g

cor.test(placed$salary, placed$f_avg)  #h

mytable1<-xtabs(~salary+quarter, data=placed)  #i
chisq.test(mytable1,simulate.p.value = TRUE)

cor.test(placed$salary, placed$work_yrs)  #j

mytable1<-xtabs(~salary+frstlang, data=placed)  #k
chisq.test(mytable1,simulate.p.value = TRUE)
cor.test(placed$salary, placed$frstlang)

cor.test(placed$salary,placed$satis)  #l

# NOW COMAPRING DIFFERENT REGRESSION MODELS :
model<-lm(salary~.,data = placed)
summary(model)

model1<-lm(salary~.-satis-quarter,data = placed) #As these failed the chi           
                                                   #square test
summary(model1)

model2<-lm(salary~.-satis-quarter-sex,data = placed)
summary(model2)

model3<-lm(salary~.-satis-quarter-gmat_tot,data = placed)
summary(model3)

model4<-lm(salary~.-satis-quarter-gmat_tot-gmat_qpc,data = placed)
summary(model4)

model5<-lm(salary~.-satis-quarter-gmat_tot-gmat_vpc,data = placed)
summary(model5)

model6<-lm(salary~.-satis-quarter-gmat_tot-gmat_tpc,data = placed)
summary(model6)

model7<-lm(salary~.-satis-quarter-gmat_tot-f_avg,data = placed)
summary(model7)

model8<-lm(salary~.-satis-quarter-gmat_tot-f_avg-s_avg,data = placed)
summary(model8)
        #model8 gives the maximum Adjusted R-squared: 0.275
