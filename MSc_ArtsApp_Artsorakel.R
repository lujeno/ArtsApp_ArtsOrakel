#####-----Read SPSS####
library(foreign)
MSc_ArtsApp_Artsorakel<-read.spss("ArtsApp_Artsorakel_questionnaire.sav", use.value.labels = F, to.data.frame = TRUE)

#####-----Check data frame####
View(MSc_ArtsApp_Artsorakel)
str(MSc_ArtsApp_Artsorakel)
head(MSc_ArtsApp_Artsorakel)
names(MSc_ArtsApp_Artsorakel)

####-----Data preparation####
library(memisc)
library(car)

options(scipen = 999)

####-----Citation packages####
citation("base")
citation("foreign")
citation("psych")
citation("multicon")
citation("memisc")
citation("car")
citation("ggplot2")
citation("apaTables")
citation("effectsize")

#--------------------------------------Creating scales and Cronbach´s alpha####
library(psych)
library(multicon)

#---Achievement
Achievement_scale <- data.frame(MSc_ArtsApp_Artsorakel$Algae1, MSc_ArtsApp_Artsorakel$Algae2, MSc_ArtsApp_Artsorakel$Algae3, MSc_ArtsApp_Artsorakel$Algae4, 
                                   MSc_ArtsApp_Artsorakel$Algae5, MSc_ArtsApp_Artsorakel$Algae6, MSc_ArtsApp_Artsorakel$Algae7, MSc_ArtsApp_Artsorakel$Algae8)

MSc_ArtsApp_Artsorakel$Achievement <- Achievement_sum <- composite(Achievement_scale, R=NULL, rel = FALSE, Zitems = F)

MSc_ArtsApp_Artsorakel$Achievement2 <- Achievement_scale2 <- data.frame(MSc_ArtsApp_Artsorakel$Algae1 + MSc_ArtsApp_Artsorakel$Algae2 + MSc_ArtsApp_Artsorakel$Algae3 + MSc_ArtsApp_Artsorakel$Algae4 + 
                                                                       MSc_ArtsApp_Artsorakel$Algae5 + MSc_ArtsApp_Artsorakel$Algae6 + MSc_ArtsApp_Artsorakel$Algae7 + MSc_ArtsApp_Artsorakel$Algae8)

#---Competence satisfaction
Competence_scale <- data.frame(MSc_ArtsApp_Artsorakel$Comp1, MSc_ArtsApp_Artsorakel$Comp2, MSc_ArtsApp_Artsorakel$Comp3)

MSc_ArtsApp_Artsorakel$Competence <- Competence_sum <- composite(Competence_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#---Autonomy satisfaction
Autonomy_scale <- data.frame(MSc_ArtsApp_Artsorakel$Auto1, MSc_ArtsApp_Artsorakel$Auto2, MSc_ArtsApp_Artsorakel$Auto3)

MSc_ArtsApp_Artsorakel$Autonomy <- Autonomy_sum <- composite(Autonomy_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#---Intrinsic motivation
#-Recode reversed worded items and then create scale
MSc_ArtsApp_Artsorakel$Intrinsic3_REV <- Recode(MSc_ArtsApp_Artsorakel$Intrinsic3_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

MSc_ArtsApp_Artsorakel$Intrinsic4_REV <- Recode(MSc_ArtsApp_Artsorakel$Intrinsic4_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

Intrinsic_mot_scale <- data.frame(MSc_ArtsApp_Artsorakel$Intrinsic1, MSc_ArtsApp_Artsorakel$Intrinsic2, MSc_ArtsApp_Artsorakel$Intrinsic3_REV, 
                                  MSc_ArtsApp_Artsorakel$Intrinsic4_REV, MSc_ArtsApp_Artsorakel$Intrinsic5, MSc_ArtsApp_Artsorakel$Intrinsic6, 
                                  MSc_ArtsApp_Artsorakel$Intrinsic7)

MSc_ArtsApp_Artsorakel$Intrinsic_motivation <- Intrinsic_mot_sum <- composite(Intrinsic_mot_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#---Effort
#-Recode reversed worded items and the create scale
MSc_ArtsApp_Artsorakel$Effort2_REV <- Recode(MSc_ArtsApp_Artsorakel$Effort2_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

MSc_ArtsApp_Artsorakel$Effort5_REV <- Recode(MSc_ArtsApp_Artsorakel$Effort5_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

Effort_scale <- data.frame(MSc_ArtsApp_Artsorakel$Effort1, MSc_ArtsApp_Artsorakel$Effort2_REV, MSc_ArtsApp_Artsorakel$Effort3, 
                                  MSc_ArtsApp_Artsorakel$Effort4, MSc_ArtsApp_Artsorakel$Effort5_REV)

MSc_ArtsApp_Artsorakel$Effort <- Effortt_sum <- composite(Effort_scale, R=NULL, rel = TRUE, Zitems = FALSE, maxScore = 7) 

#---Value/usefulness
Internalization_scale <- data.frame(MSc_ArtsApp_Artsorakel$Value_use1, MSc_ArtsApp_Artsorakel$Value_use2, MSc_ArtsApp_Artsorakel$Value_use3,
                                    MSc_ArtsApp_Artsorakel$Value_use4, MSc_ArtsApp_Artsorakel$Value_use5, MSc_ArtsApp_Artsorakel$Value_use6, 
                                    MSc_ArtsApp_Artsorakel$Value_use7)

MSc_ArtsApp_Artsorakel$Internalization <- Internalization_sum <- composite(Internalization_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#--------------------------------------Descriptive analyses####
library(psych)

#--Recode variables

#-Gender
MSc_ArtsApp_Artsorakel$Gender2 = factor(MSc_ArtsApp_Artsorakel$Gender, labels = c("Males", "Females", "Other", "Norespond")) #Males=0, Females=1, Other=2, Norespond=3
table(MSc_ArtsApp_Artsorakel$Gender)
table(MSc_ArtsApp_Artsorakel$Gender2)

#-Condition
MSc_ArtsApp_Artsorakel$Condition2 = factor(MSc_ArtsApp_Artsorakel$Condition, labels = c("Artsorakel", "ArtsApp")) #Artsorakel=0, ArtsaApp=1
table(MSc_ArtsApp_Artsorakel$Condition)
table(MSc_ArtsApp_Artsorakel$Condition2)

#-Class
MSc_ArtsApp_Artsorakel$Class2 = factor(MSc_ArtsApp_Artsorakel$Class, labels = c("First", "Second", "Third")) #First=1, Second=2, Third=3
table(MSc_ArtsApp_Artsorakel$Class)
table(MSc_ArtsApp_Artsorakel$Class2)

#Descriptive
describe(MSc_ArtsApp_Artsorakel$Age)
summary(MSc_ArtsApp_Artsorakel)
desc<- data.frame(MSc_ArtsApp_Artsorakel$Competence, MSc_ArtsApp_Artsorakel$Autonomy, MSc_ArtsApp_Artsorakel$Intrinsic_motivation,
                  MSc_ArtsApp_Artsorakel$Effort, MSc_ArtsApp_Artsorakel$Internalization, MSc_ArtsApp_Artsorakel$Achievement2)
describe(desc)

#--------------------------------------Preliminary analyses####

#--------Correlation for total sample 
corr_var <- data.frame(MSc_ArtsApp_Artsorakel$Competence, MSc_ArtsApp_Artsorakel$Autonomy, MSc_ArtsApp_Artsorakel$Intrinsic_motivation,
                       MSc_ArtsApp_Artsorakel$Effort, MSc_ArtsApp_Artsorakel$Internalization, MSc_ArtsApp_Artsorakel$Achievement)
corr.test(corr_var)

#-Create APA table
library(apaTables)
apa.cor.table(corr_var, filename = "Table2.doc", table.number = 2,
              show.conf.interval = TRUE, landscape = TRUE)

#--------------------------------------Primary analyses####

#--Main effects

#Checking assumptions
leveneTest(MSc_ArtsApp_Artsorakel$Competence ~ MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel)
leveneTest(MSc_ArtsApp_Artsorakel$Autonomy ~ MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel)
leveneTest(MSc_ArtsApp_Artsorakel$Intrinsic_motivation ~ MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel)
leveneTest(MSc_ArtsApp_Artsorakel$Effort ~ MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel)
leveneTest(MSc_ArtsApp_Artsorakel$Internalization ~ MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel)
leveneTest(MSc_ArtsApp_Artsorakel$Achievement ~ MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel)

#by condition
t.test(MSc_ArtsApp_Artsorakel$Competence ~ MSc_ArtsApp_Artsorakel$Condition)
t.test(MSc_ArtsApp_Artsorakel$Autonomy ~ MSc_ArtsApp_Artsorakel$Condition)
t.test(MSc_ArtsApp_Artsorakel$Intrinsic_motivation ~ MSc_ArtsApp_Artsorakel$Condition)
t.test(MSc_ArtsApp_Artsorakel$Effort ~ MSc_ArtsApp_Artsorakel$Condition)
t.test(MSc_ArtsApp_Artsorakel$Internalization ~ MSc_ArtsApp_Artsorakel$Condition)
t.test(MSc_ArtsApp_Artsorakel$Achievement ~ MSc_ArtsApp_Artsorakel$Condition)

#Effect sizes
install.packages("effectsize")
library(effectsize)

cohens_d(MSc_ArtsApp_Artsorakel$Competence~MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel, paired = F) #Competence
cohens_d(MSc_ArtsApp_Artsorakel$Autonomy~MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel, paired = F) #Autonomy
cohens_d(MSc_ArtsApp_Artsorakel$Intrinsic_motivation~MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel, paired = F) #Intrinsic motivation
cohens_d(MSc_ArtsApp_Artsorakel$Effort~MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel, paired = F) #Effort
cohens_d(MSc_ArtsApp_Artsorakel$Internalization~MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel, paired = F) #Internalization
cohens_d(MSc_ArtsApp_Artsorakel$Achievement~MSc_ArtsApp_Artsorakel$Condition2, data = MSc_ArtsApp_Artsorakel, paired = F) #achievement

#checking conventional effect sizes
library(pwr) 
cohen.ES(test = c("t"), size = c("small"))
cohen.ES(test = c("t"), size = c("medium"))
cohen.ES(test = c("t"), size = c("large"))

#Figures
library(ggplot2)
library(patchwork)

a <- ggplot(data = MSc_ArtsApp_Artsorakel,aes(x=Condition2, y=Competence)) + geom_boxplot() + #Competence
  ylab("Competence") +
  xlab("Condition")

b <- ggplot(data = MSc_ArtsApp_Artsorakel,aes(x=Condition2, y=Autonomy)) + geom_boxplot() + #Autonomy
  ylab("Autonomy") +
  xlab("Condition")

c <- ggplot(data = MSc_ArtsApp_Artsorakel,aes(x=Condition2, y=Effort)) + geom_boxplot() + #Effort
  ylab("Effort") +
  xlab("Condition")

d <- ggplot(data = MSc_ArtsApp_Artsorakel,aes(x=Condition2, y=Intrinsic_motivation)) + geom_boxplot() + #Intrinsic motivation
  ylab("Intrinsic motivation") +
  xlab("Condition")

e <- ggplot(data = MSc_ArtsApp_Artsorakel,aes(x=Condition2, y=Internalization)) + geom_boxplot() + #Internalization
  ylab("Internalization") +
  xlab("Condition")

ggplot(data = MSc_ArtsApp_Artsorakel,aes(x=Condition2, y=Achievement)) + geom_boxplot() + #Achievement
  ylab("Achievement") +
  xlab("Condition")

#ggpatch visualization of all ggplot figures except Achievement
a + b + c + d + e

