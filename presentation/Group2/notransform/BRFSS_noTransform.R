# import dataset
MyData <- read.csv("../Rime/BRFSS_Final_2.csv")

# extract factors
library(readxl)
Variables_BRFSS <- read_excel("G:/NIA/other_information/training/2021_HealthDisparitiesCodeathon/OurGroup/BRFSS/Variables_BRFSS.xlsx")

# select interesting factors
MyData_Variables <- MyData[colnames(MyData) %in% Variables_BRFSS$Variables]
colnames(MyData_Variables)
write.csv(MyData_Variables,"MyData_Variables_noTransform.csv", row.names = F)

# MyData_Variables <- read.csv("MyData_Variables_noTransform.csv")


# select predictor, outcome and mediators
pred <- MyData_Variables$ENDS_U
summary(pred)
class(pred)

y <- MyData_Variables$Current_Asthma
summary(y)
class(y)
 
colnames(MyData_Variables)
m <- MyData_Variables[,c(2,3,4,5,6,7,9,10,17,26,27)]
summary(m)
head(m)
colnames(m)


### use the package
library("mmabig")
# Data Organization and Identify Potential Moderators/Confounders
data.e1<-data.org.big(x=m,
                      y=data.frame(y),
                      mediator=1:ncol(m),
                      pred=data.frame(pred),
                      testtype=1)
head(data.e1)
summary(data.e1)

# Third-Variable Effect Analysis
med.e1<-med.big(data.e1)
head(med.e1)
summary(med.e1)

# Combined function for multiple TVE analysis with big datasets
mma.e1<-mma.big(x=m,y=data.frame(y), mediator=1:ncol(m),
                pred=data.frame(pred), alpha=1, alpha1=0.05, alpha2=0.05)
head(mma.e1$results)

save(MyData_Variables,
     data.e1, med.e1, mma.e1,
     file = "BRFSS_noTransform.Rdata",
     compress = TRUE)


