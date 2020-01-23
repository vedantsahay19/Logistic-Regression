####################### Run the required libraries ######################

library(tidyverse) 
library(cowplot)
library(ggplot2)
library(sjPlot)
library(dplyr)
library(corrplot)

####################### Data Cleaning and Preparation ######################

telco <- read.csv('Telco_new.csv')

#Rename values in senior citizen
telco$SeniorCitizen[telco$SeniorCitizen==1] <- "Yes"
telco$SeniorCitizen[telco$SeniorCitizen==0] <- "No"
View(telco)

#Replace blank values in the TotalCharges with the median values
med = telco$TotalCharges
median = median(med,na.rm=TRUE)
telco$SeniorCitizen[is.na(telco$SeniorCitizen)] <- median

####################### Data Visualization ######################

#First plot grid
plot_grid(ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=Dependents,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=Partner,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=InternetService,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group')+
            theme(axis.text.x= element_text(angle=90, hjust=1, size=10)),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+ 
            geom_bar(position = 'fill')+
            scale_y_continuous(labels = scales::percent_format())+
            theme(axis.text.x= element_text(angle=90, hjust=1, size=10))+
            ylab('Percent of group'),
          align='h')

#Gender: Not significant
ggplot(telco, aes(x=gender,fill=Churn))+
  geom_bar(position='fill')+
  scale_y_continuous(labels = scales::percent_format())+
  ylab('Percent of group')

#Second grid
plot_grid(ggplot(telco, aes(x=Churn, y=tenure, fill=Churn))+
            geom_boxplot(),
          ggplot(telco, aes(x=Churn,y=MonthlyCharges, fill=Churn))+
            geom_boxplot(),
          ggplot(telco, aes(x=Churn,y=TotalCharges, fill=Churn))+ 
            geom_boxplot(), nrow=1)

# Third grid
plot_grid(ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ 
            geom_bar(position = 'fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          align='h')

#Fourth grid
plot_grid(ggplot(telco, aes(x=Contract,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=StreamingMovies,fill=Churn))+
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format())+
            ylab('Percent of group'),
          ggplot(telco, aes(x=MultipleLines, fill=Churn))+ 
            geom_bar(position='fill')+
            scale_y_continuous(labels = scales::percent_format()),
          align='h')

#PhoneService not significant
ggplot(telco, aes(x=PhoneService, fill=Churn))+ 
  geom_bar(position='fill')+
  scale_y_continuous(labels = scales::percent_format())

#Interaction  

c<-filter(telco, telco$Churn=='Yes'& telco$StreamingMovies!='No internet service' & telco$StreamingTV!='No internet service' & telco$InternetService!='No') 
plot_grid(ggplot(c,aes(x=InternetService, fill=StreamingTV))+
  geom_bar(position='dodge')+
  ggtitle('InternetService vs StreamingTV (Churn)')+
  theme(title =element_text(size=8)),
ggplot(c,aes(x=InternetService, fill=StreamingMovies))+
  geom_bar(position='dodge')+
  ggtitle('InternetService vs StreamingMovies (Churn)')+
  theme(title =element_text(size=8)),
ggplot(c,aes(x=StreamingTV, fill=StreamingMovies))+
  geom_bar(position='dodge')+
  ggtitle('StreamingTV vs StreamingMovies (Churn)')+
theme(title =element_text(size=8)), nrow=1)


#Correlation using the availble continous variables
a <- select(telco,tenure,MonthlyCharges,TotalCharges)
b <- cor(a)
corrplot(b, type = "upper",order ="hclust", tl.col = "black", tl.srt = 90, tl.cex =1.5)


####################### Modelling ######################

#Intuituve Model 
model_1 = glm(Churn ~ as.factor(SeniorCitizen)+
                as.factor(InternetService)+
                as.factor(OnlineSecurity)+
                as.factor(StreamingTV)+
                as.factor(StreamingMovies)+
                as.factor(Contract)+
                as.factor(PaperlessBilling)+
                as.factor(PaymentMethod)+
                MonthlyCharges+
                as.factor(Dependents) +
                as.factor(Partner) +
                tenure + 
                as.factor(DeviceProtection)+
                as.factor(OnlineBackup) +
                as.factor(MultipleLines)+
                as.factor(TechSupport)  +
                as.factor(StreamingTV)*as.factor(StreamingMovies)*as.factor(InternetService)
              , data=telco, family = "binomial")

summary(model_1)
tab_model(model_1)

#Step Wise Method
fit.nothing=glm(Churn~1, data=telco, family='binomial')
FitAll=glm(Churn ~as.factor(SeniorCitizen)+
             as.factor(Partner)+
             as.factor(Dependents)+
             tenure+
             as.factor(InternetService)+
             as.factor(OnlineSecurity)+
             as.factor(DeviceProtection)+
             as.factor(StreamingTV)+
             as.factor(StreamingMovies)+
             as.factor(Contract)+
             as.factor(PaperlessBilling)+
             as.factor(PaymentMethod)+
             MonthlyCharges+
             as.factor(gender)+
             as.factor(PhoneService)+
             as.factor(MultipleLines)+
             as.factor(OnlineBackup)+
             as.factor(TechSupport)+
             as.factor(StreamingTV)*as.factor(StreamingMovies)*as.factor(InternetService)
           , data=telco, family='binomial')
f2= step(fit.nothing, direction='both', scope=formula('FitAll'))
summary(f2)
tab_model(f2)


#Forward Method
fit.nothing=glm(Churn~1, data=telco, family='binomial')
FitAll=glm(Churn ~as.factor(SeniorCitizen)+
             as.factor(Partner)+
             as.factor(Dependents)+
             tenure+
             as.factor(InternetService)+
             as.factor(OnlineSecurity)+
             as.factor(DeviceProtection)+
             as.factor(StreamingTV)+
             as.factor(StreamingMovies)+
             as.factor(Contract)+
             as.factor(PaperlessBilling)+
             as.factor(PaymentMethod)+
             MonthlyCharges+
             as.factor(gender)+
             as.factor(PhoneService)+
             as.factor(MultipleLines)+
             as.factor(OnlineBackup)+
             as.factor(TechSupport)+
             as.factor(StreamingTV)*as.factor(StreamingMovies)*as.factor(InternetService)
           , data=telco, family='binomial')
m1 = step(fit.nothing, direction='forward', scope=formula('FitAll'))
summary(m1)
tab_model(m1)


#Backward Method
fit.nothing=glm(Churn~1, data=telco, family='binomial')
FitAll=glm(Churn ~as.factor(SeniorCitizen)+
             as.factor(Partner)+
             as.factor(Dependents)+
             tenure+
             as.factor(InternetService)+
             as.factor(OnlineSecurity)+
             as.factor(DeviceProtection)+
             as.factor(StreamingTV)+
             as.factor(StreamingMovies)+
             as.factor(Contract)+
             as.factor(PaperlessBilling)+
             as.factor(PaymentMethod)+
             MonthlyCharges+
             as.factor(gender)+
             as.factor(PhoneService)+
             as.factor(MultipleLines)+
             as.factor(OnlineBackup)+
             as.factor(TechSupport) + 
             as.factor(StreamingTV)*as.factor(StreamingMovies)*as.factor(InternetService)
           , data=telco, family='binomial')
n1= step(FitAll, direction='backward', scope=formula('fit.nothing'))
summary(n1)
tab_model(n1)


##############################################################################