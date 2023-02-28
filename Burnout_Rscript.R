pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics
  dplyr,        # select
  ggplot2,      # ggplot2 graphics
  skimr,        # get overview of data
  tidymodels,   # for tidy modelling
  survey,       # for survey functions
  srvyr,        # for tidy survey work
  lubridate,    # for converting date character to date format
  tidyquant,    # for tidy time series functions
  patchwork,    # easily combine ggplot rasters
  plyr,         # for seeing the frequency
  freqtables,   # for frequency table
  corrplot,     # for plotting the correlation 
  glue,
  ggpubr,
  car,          # omparison of the regression coefficients
  lmtest,       # package is used to conduct the Wald test
  mice,         # multiple imputation 
  pROC,         # ROC curve
  
  caret,        # for easy machine learning workflow
  gmodels,      # for cross tab percentage
  readxl        #to read xlsx
)
options (scipen=999)

setwd("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Burnout")
save.image ("C:/Users/humay/Dropbox/Personal Drive/Own project/Nursing students/Main data/R/Nsg_St.RData")

options (scipen=999)

Nsg_St$BMS_total= Nsg_St$Tired_BMS_1+Nsg_St$Hopeless_BMS_2+Nsg_St$Disappointed_BMS_3+Nsg_St$Trapped_BMS_4+Nsg_St$Helpless_BMS_5+Nsg_St$Depressed_BMS_6+
  Nsg_St$Weak_BMS_7+Nsg_St$Worthless_BMS_8+Nsg_St$Sleeping_BMS_9+Nsg_St$Feeling_BMS_10

summary (Nsg_St$BMS_total)


Nsg_St$BMS_score= Nsg_St$BMS_total/10

summary (Nsg_St$BMS_score)


library (tidyverse)


summary (Nsg_St$BMS_cat)

Nsg_St$BMS_cat= cut (Nsg_St$BMS_score, breaks = c(0,4,Inf),
              labels = c("No", "Yes")) 
Nsg_St$BMS_cat= recode (Nsg_St$BMS_cat,
                        "Yes"="1",
                        "No"= "0")

levels (Nsg_St$BMS_cat)

Nsg_St$BMS_cat

Nsg_St$Division.fct
head (Nsg_St$Division.fct)
levels (Nsg_St$Division.fct)
Nsg_St %>% 
  freq_table(Division.fct) 

## data management of PHQ-9 ###

#Demographic information

Nsg_St$PHQ_cat= cut (Nsg_St$PHQ_Score, breaks = c(-1,4,9,14,19,Inf),
                     labels = c("Minimal", "Mild", "Moderate", "Moderately severe", "Sever")) 
summary (Nsg_St$PHQ_cat)

summary (Nsg_St$Age)
Nsg_St %>% 
  ggplot(aes (Nsg_St$Age))+
  geom_histogram ()

Nsg_St$Age_cat= cut (Nsg_St$Age, breaks = c(
  0,20,Inf))
summary (Nsg_St$Age_cat)

### Descriptive analysis ### 
#
library(dplyr)

library(freqtables)

Nsg_St %>% 
  freq_table(Age_cat) 

Nsg_St %>% 
  count(Age_cat) %>% 
  mutate(prop = (n / sum(n))*100)

Nsg_St %>% 
  count(Sex.fct) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Sex.fct) 

Nsg_St %>% 
  count(Father_education.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Father_education.fct_new) 

Nsg_St %>% 
  count(Mother_education.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Mother_education.fct_new) 

Nsg_St %>% 
  count(Family_income.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Family_income.fct_new) 

Nsg_St %>% 
  count(Division.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Division.fct) 

Nsg_St %>% 
  count(Current_residence.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Current_residence.fct_new) 

#
Nsg_St %>% 
  count(Course_type.fct_new) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Course_type.fct_new) 

Nsg_St %>% 
  count(Institution_type.fct) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Institution_type.fct) 

Nsg_St %>% 
  count(Academic_year.fct) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Academic_year.fct) 

Nsg_St %>% 
  count(Nursing_choice.fct) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Nursing_choice.fct) 

Nsg_St %>% 
  count(Have_qualified_teacher.fct) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Have_qualified_teacher.fct) 

Nsg_St %>% 
  count(Have_qualified_language_teacher.fct) %>% 
  mutate(prop = (n / sum(n))*100) 
Nsg_St %>% 
  freq_table(Have_qualified_language_teacher.fct) 

Nsg_St %>% 
  count(Have_qualified_clinical.teacher.fct) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Have_qualified_clinical.teacher.fct) 

Nsg_St %>% 
  count(BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(BMS_cat) 

Nsg_St %>% 
  count(PHQ_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(PHQ_cat) 


#Table 2: Un-adjusted association 

chisq.test( Nsg_St$PHQ_cat,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  freq_table(PHQ_cat, BMS_cat) #BMS means burnout, PHQ means depression 

Nsg_St %>% 
  count(Nsg_St$PHQ_cat, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100) 

#Socio-demographic information
chisq.test( Nsg_St$Age_cat,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Age_cat, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Age_cat, BMS_cat)

chisq.test( Nsg_St$Sex.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Sex.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>% 
  freq_table(Sex.fct, BMS_cat)

chisq.test( Nsg_St$Father_education.fct_new,Nsg_St$BMS_cat, correct=T)
Nsg_St %>%
  freq_table (Nsg_St$Father_education.fct_new, Nsg_St$BMS_cat)
  
Nsg_St %>% 
  count(Nsg_St$Father_education.fct_new, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)


chisq.test( Nsg_St$Mother_education.fct_new,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Mother_education.fct_new, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Mother_education.fct_new, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Family_income.fct_new,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Family_income.fct_new, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Family_income.fct_new, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Division.fct_new,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Division.fct_new, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Division.fct_new, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Current_residence.fct_new,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Current_residence.fct_new, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Current_residence.fct_new, Nsg_St$BMS_cat)

#Academic information 
chisq.test( Nsg_St$Course_type.fct_new,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Course_type.fct_new, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Course_type.fct_new, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Institution_type.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Institution_type.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Institution_type.fct, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Academic_year.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Academic_year.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Academic_year.fct, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Nursing_choice.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Nursing_choice.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Nursing_choice.fct, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Have_qualified_teacher.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Have_qualified_teacher.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Have_qualified_teacher.fct, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Have_qualified_language_teacher.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Have_qualified_language_teacher.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Have_qualified_language_teacher.fct, Nsg_St$BMS_cat)

chisq.test( Nsg_St$Have_qualified_clinical.teacher.fct,Nsg_St$BMS_cat, correct=T)
Nsg_St %>% 
  count(Nsg_St$Have_qualified_clinical.teacher.fct, Nsg_St$BMS_cat) %>% 
  mutate(prop = (n / sum(n))*100)
Nsg_St %>%
  freq_table (Nsg_St$Have_qualified_clinical.teacher.fct, Nsg_St$BMS_cat)


Nsg_St$PHQ_cat
 (Nsg_St$PHQ_cat)
length (Nsg_St$Sex.fct)

###adjusted analysis###

#model 1

#model 2
model_glm.1=glm (data= Nsg_St, BMS_cat~Age_cat+
                   Sex.fct+
                   Father_education.fct_new+
                   Mother_education.fct_new+
                   Family_income.fct_new+
                   Division.fct_new
                 ,family= binomial)

summary (model_glm.1)
model_glm.1

or_model.1=exp(cbind(coef(model_glm.1), confint(model_glm.1, level=0.95)))
or_model.1

#Reference change
Nsg_St$Have_qualified_clinical.teacher.fct <- relevel(Nsg_St$Have_qualified_clinical.teacher.fct, ref = "Yes") 

Nsg_St$BMS_cat

#model 2
model_glm.2=glm (data= Nsg_St, BMS_cat~Age_cat+
                   Sex.fct+
                   Father_education.fct_new+
                   Mother_education.fct_new+
                   Family_income.fct_new+
                   Division.fct_new+
                   Course_type.fct_new+
                   Institution_type.fct+
                   Academic_year.fct+
                   Nursing_choice.fct+
                   Have_qualified_teacher.fct+
                   Have_qualified_language_teacher.fct+
                   Have_qualified_clinical.teacher.fct
                 ,family= binomial)

summary (model_glm.2)
model_glm.2

or_model.2=exp(cbind(coef(model_glm.2), confint(model_glm.2, level=0.95)))
or_model.2



#model 3
model_glm.3=glm (data= Nsg_St, BMS_cat~PHQ_cat+
                 Age_cat+
               Sex.fct+
                 Father_education.fct_new+
                 Mother_education.fct_new+
                 Family_income.fct_new+
                 Division.fct_new+
                 Course_type.fct_new+
                 Institution_type.fct+
                 Academic_year.fct+
                 Nursing_choice.fct+
                 Have_qualified_teacher.fct+
                 Have_qualified_language_teacher.fct+
                 Have_qualified_clinical.teacher.fct
               ,family= binomial)

summary (model_glm.3)
model_glm.3

or_model.3=exp(cbind(coef(model_glm.3), confint(model_glm.3, level=0.95)))
or_model.3

#Fitness 

library (performance) #performance_hosmer function from the performance package

performance_hosmer(model_glm.3, n_bins = 10)

library(AICcmodavg) # for the AICc() function

AIC(model_glm.3)
AICc(model_glm.3)
BIC (model_glm.3)


#roc object
#ROC for the model 
library(pROC)

model_glm.3
model_glm.3.prob=predict(model_glm.3,type=c("response"))
model_glm.3.roc_object <- roc( Nsg_St$BMS_cat,  model_glm.3.prob)
model_glm.3.roc_object
auc( model_glm.3.roc_object )

#roc plot 
plot (model_glm.3.roc_object, col= "#008000", main="ROC curve for predicting burnout", print.auc=TRUE)

library("olsrr")

ols_vif_tol(model_glm.3)

#Multi-coliniearity 
library(corrplot)

Burnout.cor.data= data.frame( 
                             Nsg_St$BMS_cat, 
                             Nsg_St$PHQ_cat,
                             Nsg_St$Age_cat,
                             Nsg_St$Sex.fct,
                             Nsg_St$Father_education.fct_new,
                             Nsg_St$Mother_education.fct_new,
                             Nsg_St$Family_income.fct_new,
                             Nsg_St$Division.fct_new,
                             Nsg_St$Course_type.fct_new,
                             Nsg_St$Institution_type.fct,
                             Nsg_St$Academic_year.fct,
                             Nsg_St$Nursing_choice.fct,
                             Nsg_St$Have_qualified_teacher.fct,
                             Nsg_St$Have_qualified_language_teacher.fct,
                             Nsg_St$Have_qualified_clinical.teacher.fct)

Burnout.cor.data

head (Burnout.cor.data)
typeof (Nsg_St$BMS_cat)
typeof (Nsg_St$PHQ_cat)

typeof (Nsg_St$Age_cat)
typeof (Nsg_St$Sex.fct)
typeof (Nsg_St$Father_education.fct_new)
typeof (Nsg_St$Mother_education.fct_new)
typeof (Nsg_St$Family_income.fct_new)
typeof (Nsg_St$Division.fct_new)
typeof (Nsg_St$Course_type.fct_new)
typeof (Nsg_St$Institution_type.fct)
typeof (Nsg_St$Academic_year.fct)
typeof (Nsg_St$Nursing_choice.fct)
typeof (Nsg_St$Have_qualified_teacher.fct)
typeof (Nsg_St$Have_qualified_language_teacher.fct)
typeof (Nsg_St$Have_qualified_clinical.teacher.fct)

Nsg_St$BMS_cat= as.numeric(Nsg_St$BMS_cat)
Nsg_St$PHQ_cat= as.numeric(Nsg_St$PHQ_cat)
Nsg_St$Age_cat= as.numeric(Nsg_St$Age_cat)
Nsg_St$Sex.fct= as.numeric(Nsg_St$Sex.fct)
Nsg_St$Father_education.fct_new= as.numeric(Nsg_St$Father_education.fct_new)
Nsg_St$Mother_education.fct_new= as.numeric(Nsg_St$Mother_education.fct_new)
Nsg_St$Family_income.fct_new= as.numeric(Nsg_St$Family_income.fct_new)
Nsg_St$Division.fct_new= as.numeric(Nsg_St$Division.fct_new)
Nsg_St$Course_type.fct_new= as.numeric(Nsg_St$Course_type.fct_new)
Nsg_St$Institution_type.fct= as.numeric(Nsg_St$Institution_type.fct)
Nsg_St$Academic_year.fct= as.numeric(Nsg_St$Academic_year.fct)
Nsg_St$Nursing_choice.fct= as.numeric(Nsg_St$Nursing_choice.fct)
Nsg_St$Have_qualified_teacher.fct= as.numeric(Nsg_St$Have_qualified_teacher.fct)
Nsg_St$Have_qualified_language_teacher.fct= as.numeric(Nsg_St$Have_qualified_language_teacher.fct)
Nsg_St$Have_qualified_clinical.teacher.fct= as.numeric(Nsg_St$Have_qualified_clinical.teacher.fct)

Burnout.cor.data.cor_matrix <- cor((Burnout.cor.data))
Burnout.cor.data.cor_matrix 

Burnout.cor.data.cor_matrix=cor(Burnout.cor.data.cor_matrix, use = "pairwise.complete.obs")
Burnout.cor.data.cor_matrix #same as above 

corrplot(Burnout.cor.data.cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45)


### Reference change ####
Nsg_St$Have_qualified_clinical.teacher.fct <- relevel(Nsg_St$Have_qualified_clinical.teacher.fct, ref = "Yes") 

### Figures ##########

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

data(efc)
theme_set(theme_sjplot())

#Forest plot of the model

Forest_BMS_cat = plot_model(model_glm.3,
                               vline.color = "seagreen2",
                            show.values = F,
                            width = 0.1,
                            value.offset = 0.4,
                            cex= 0.1,
                            
                            p.shape = TRUE,
                            xmin=error_lower,
                            xmax=error_upper,
                            axis.labels = c("Have qualified clinical teachers = No",
                                            "Have qualified English language teachers = No",
                                            "Have qualified teachers = No",
                                            "Own choice of being nurse = No",
                                            "Academic year = Third year",
                                            "Academic year = Second year",
                                            "Academic year = First year",
                                            "Type of institution = Private",
                                            "Type of course = Diploma",
                                            "Division = Sylhet",
                                            "Division = Dhaka",
                                            "Family income = 15k-20k BDT",
                                            "Family income = >20k BDT",
                                            "Mother's education = Non-graduate",
                                            "Father's education = Non-graduate",
                                            "Sex = Male",
                                            "Age = >20 years", 
                                            "Depression = Severe",
                                            "Depression = Moderately severe",
                                            "Depression = Moderate",
                                            "Depression = Mild"), fontface = "bold",
                            title = "")+
  theme_minimal()


Forest_BMS_cat

#Frequency figure of PHQ by BMS

ggplot (Nsg_St, aes(x=BMS_cat, y= PHQ_cat))+
  geom_point (color = "seagreen2"
  )  +
  geom_line()#
  labs (x= "Burnout", y= "Depression")+
  geom_smooth ()+
  theme_bw()


Nsg_St$BMS_cat.fig= recode (Nsg_St$BMS_cat,
                        "1"="Yes",
                        "0"= "No")
Nsg_St$BMS_cat.fig

ggplot (Nsg_St, aes (x= PHQ_cat, 
                       fill = BMS_cat.fig))+
  geom_bar(show.legend=F)+
  scale_fill_brewer(palette="Dark2")


ggplot(Nsg_St, aes (x=PHQ_cat , 
                      fill= BMS_cat.fig))+
  geom_bar(position = "dodge", width=0.40)+
  labs (x= "Depression", y= "Burnout", fill= "Burnout")+
  theme_minimal()

#Final Figure 
ggplot(Nsg_St, aes (x=  PHQ_cat   , 
                    fill= BMS_cat.fig,
                    show.values = F,
                    value.offset = 0.02,
                    size=4))+
  geom_bar(position = "dodge", width=0.80)+
  labs (x= "Depression", y= "Burnout", fill= "Burnout", size="")+
  theme_minimal()+
  guides(size = FALSE)
