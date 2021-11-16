##################################################
###                                             ##
### RStudio - Assignment 4                      ## 
##################################################
#                                               ##
##################################################
# Written by Keerthy Raaj Shanmugam
# ID: 8779954
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear all plots
if(!is.null(dev.list())) dev.off()

# Clear entire console
cat("\014") 

# Clean and clear theworkspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/keert/Documents/Data")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

#Read the PROG8430_Assign_MLR_21F data which is in CSV format
pol_eng <- read.csv("PROG8430_Assign_MLR_21F.csv",header=TRUE,sep=",")
head(pol_eng)
str(pol_eng)

#Rename Variables 
names(pol_eng) <- c("id_KS", "group_KS", "hs.grad_KS", "nation_KS", "gender_KS",
                    "age_KS", "m.status_KS", "political_KS", "n.child_KS",
                    "income_KS","food_KS", "housing_KS", "other_KS", "score_KS",
                    "Pol_KS", "time1_KS", "time2_KS", "time3_KS", "scr_KS")
str(pol_eng)

#1.1. Apply the Missing Value Filter to remove appropriate columns of data.
summary(pol_eng)
pol_eng <- pol_eng[-c(17)]
str(pol_eng)

#1.2. Apply the Low Variance Filter to remove appropriate columns of data.
stat.desc(pol_eng)  #Consider coef of var
summary(pol_eng)

#1.3. Apply the High Correlation Filter to remove appropriate columns of data.
hcf <- pol_eng[,c("id_KS","age_KS","n.child_KS","income_KS","food_KS","housing_KS", "other_KS", "score_KS",
                  "Pol_KS", "time1_KS","time3_KS", "scr_KS")]

cor(hcf,method="spearman")

pol_eng <- pol_eng[-c(17)]
str(pol_eng)

#2. As demonstrated in class, transform any variables that are required to conduct the regression analysis (e.g. categorical variables to dummies). 

#integer to numeric data type
pol_eng$age_KS <- as.numeric(pol_eng$age_KS)
pol_eng$n.child_KS <- as.numeric(pol_eng$n.child_KS)
pol_eng$id_KS <- as.numeric(pol_eng$id_KS)

#group
#group - Convert to index (Dummy) Variables
grp_Dummies <- model.matrix(~group_KS -1, data=pol_eng)
head(grp_Dummies)
#Combine the Datasets again
pol_eng <- cbind(pol_eng, grp_Dummies)
head(pol_eng)

#hs_grad
#hs_grad - Convert to index (Dummy) Variables
hs_grd_Dummies <- model.matrix(~hs.grad_KS -1, data=pol_eng)
head(hs_grd_Dummies)
#Combine the Datasets again
pol_eng <- cbind(pol_eng, hs_grd_Dummies)
head(pol_eng)
str(pol_eng)

#nation
#nation - Convert to index (Dummy) Variables
nation_Dummies <- model.matrix(~nation_KS -1, data=pol_eng)
head(nation_Dummies)
#Combine the Datasets again
pol_eng <- cbind(pol_eng, nation_Dummies)
head(pol_eng)
str(pol_eng)

#gender
#gender - Convert to index (Dummy) Variables
gender_Dummies <- model.matrix(~gender_KS -1, data=pol_eng)
head(gender_Dummies)
#Combine the Datasets again
pol_eng <- cbind(pol_eng, gender_Dummies)
head(pol_eng)
str(pol_eng)

#political
#political - Convert to index (Dummy) Variables
political_Dummies <- model.matrix(~political_KS -1, data=pol_eng)
head(political_Dummies)
#Combine the Datasets again
pol_eng <- cbind(pol_eng, political_Dummies)
head(pol_eng)
str(pol_eng)

#m_status
#m_status - Convert to index (Dummy) Variables
m_status_Dummies <- model.matrix(~m.status_KS -1, data=pol_eng)
head(m_status_Dummies)
#Combine the Datasets again
pol_eng <- cbind(pol_eng, m_status_Dummies)
head(pol_eng)
str(pol_eng)

#remove unwanted variables
pol_eng <- pol_eng[-c(1,2,3,4,5,7,8)] 
str(pol_eng)

#rename variables
names(pol_eng) <- c("age_KS", "n.child_KS", "income_KS","food_KS","housing_KS", "other_KS", "score_KS",
                    "Pol_KS", "time1_KS", "scr_KS", "group.Control_KS", "group.Treat_KS","hs.grad.No_KS",
                    "hs.grad.Yes_KS", "nation.Asia_KS", "nation.Europe_KS", "nation.NorthAmerica_KS",
                    "nation.Southern_KS","gender.Female_KS", "gender.Male_KS", "gender.Undis_KS",
                    "political.Conservative_KS", "political.Liberal_KS", "political.NewDemocrat_KS",
                    "political.Other_KS","m.status_divorced_KS", "m.status_married_KS",
                    "m.status_never_KS", "m.status_widowed_KS" )
str(pol_eng)

#3.1. Create boxplots of all relevant variables (i.e. numeric, non-binary) to determine outliers.
par(mfrow=c(1,1))
# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(pol_eng), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(pol_eng[[cname]]))
    # use the `main` param to put column name as plot title
    print(boxplot(pol_eng[[cname]], main=cname))
})

#3.2. Comment on any outliers you see and deal with them appropriately.
summary(pol_eng)


#Deal with outliers (keep only, no of child < 201)
pol_eng <- subset(pol_eng, n.child_KS<201)
str(pol_eng) 

#4.1. Correlations: Create both numeric and graphical correlations (as demonstrated in class) and comment on noteworthy correlations you observe. Are these surprising? Do they make sense?

corrgram(pol_eng, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main=" Political Awareness Stats")

res <- cor(pol_eng, method="spearman")
round(res, 2)

#5.1. Create a simple linear regression model using Pol as the dependent variable and score as the independent. Create a scatter plot of the two variables and overlay the regression line.
polmodel1 <- lm(Pol_KS ~ score_KS, data=pol_eng)
polmodel1

#Scatter plot with Regression line
X <- pol_eng$score_KS
Y <- pol_eng$Pol_KS
plot(X, Y, main = "Pol by Score (with Regression Line)",
     xlab = "Score", ylab = "Pol",
     pch = 20, frame = FALSE)
#Regression line
plot(X, Y, main = "Pol by Score (with Regression Line)",
     xlab = "Score", ylab = "Pol",
     pch = 20, frame = FALSE)
abline(polmodel1, col = "red")

#5.2. Create a simple linear regression model using Pol as the dependent variable and scr as the independent. Create a scatter plot of the two variables and overlay the regression line.
polmodel2 <- lm(Pol_KS ~ scr_KS, data=pol_eng)
polmodel2

#Scatter plot with Regression line
X <- pol_eng$scr_KS
Y <- pol_eng$Pol_KS
plot(X, Y, main = "Pol by Scr (with Regression Line)",
     xlab = "Scr", ylab = "Pol",
     pch = 20, frame = FALSE)
#Regression line
plot(X, Y, main = "Pol by Scr (with Regression Line)",
     xlab = "Scr", ylab = "Pol",
     pch = 20, frame = FALSE)
abline(polmodel2, col = "red")

#5.3. Compare the models. Which model is superior? Why?
#correlation
cor(pol_eng$Pol_KS,pol_eng$score_KS)
cor(pol_eng$Pol_KS,pol_eng$scr_KS)
#summary
summary(polmodel1)
summary(polmodel2)

#6. As demonstrated in class, create two models using two automatic variable selection techniques discussed in class (Full, Backward). 

#Full Model
pol_lm = lm(Pol_KS ~ age_KS + n.child_KS + income_KS + food_KS + housing_KS + other_KS + score_KS +
            time1_KS + scr_KS + group.Control_KS + group.Treat_KS + hs.grad.No_KS +
            hs.grad.Yes_KS + nation.Asia_KS + nation.Europe_KS + nation.NorthAmerica_KS +
            nation.Southern_KS + gender.Female_KS + gender.Male_KS + gender.Undis_KS +
            political.Conservative_KS + political.Liberal_KS + political.NewDemocrat_KS +
            political.Other_KS + m.status_divorced_KS + m.status_married_KS +
            m.status_never_KS + m.status_widowed_KS, data=pol_eng, na.action=na.omit)
pol_lm
summary(pol_lm)


#Backward Model   
Bck_pol_lm = step(pol_lm, direction="backward", details=TRUE)
Bck_pol_lm
summary(Bck_pol_lm)

#7.1. For both models (as discussed and demonstrated in class) evaluate the main assumptions of regression: Error terms mean of zero, constant variance and normally distributed.
#full
plot(pol_lm)

pol_lm_res <- residuals(pol_lm)
shapiro.test(pol_lm_res)

#backward
plot(Bck_pol_lm)

Bck_pol_lm_res <- residuals(Bck_pol_lm)
shapiro.test(Bck_pol_lm_res)
