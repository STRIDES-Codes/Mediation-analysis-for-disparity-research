setwd("G:/NIA/other_information/training/2021_HealthDisparitiesCodeathon/OurGroup/BRFSS/R")

# import dataset
MyData <- read.csv("../Rime/BRFSS_Final_2.csv")

# extract factors
library(readxl)
Variables_BRFSS <- read_excel("G:/NIA/other_information/training/2021_HealthDisparitiesCodeathon/OurGroup/BRFSS/Rime/Variables_BRFSS.xlsx")

# select interesting factors
MyData_Variables <- MyData[colnames(MyData) %in% Variables_BRFSS$Variables]

# transfer columns into the type of factor
colnames(MyData_Variables)
MyData_Variables[,-c(4,5,11,12,23)] <- lapply(MyData_Variables[,-c(4,5,11,12,23)],as.factor)

write.csv(MyData_Variables,"MyData_Variables_transform_2.csv", row.names = F)

# MyData_Variables <- read.csv("MyData_Variables_transform.csv")
# colnames(MyData_Variables)
# MyData_Variables[,-c(4,5,11,12,23)] <- lapply(MyData_Variables[,-c(4,5,11,12,23)],as.factor)

# select predictor, outcome and mediators
#pred <- MyData_Variables$ENDS_U
pred <- MyData_Variables$Race_U
summary(pred)

y <- MyData_Variables$Current_Asthma
summary(y)
 
colnames(MyData_Variables)
m <- MyData_Variables[,c(2,3,4,5,6,7,9,10,17,25,27)]
summary(m)
colnames(m)

### use the package
library("mmabig")
# Data Organization and Identify Potential Moderators/Confounders
data.e1<-data.org.big(x=m,
                      y=data.frame(y),
                      mediator=c(1,4,5,6,7,8,9,10,11),
                      pred=data.frame(pred),
                      testtype=1,
                      w=MyData_Variables$Final_weight
                      )
summary(data.e1)


# Third-Variable Effect Analysis
med.e1<-med.big(data.e1)
summary(med.e1)

# Combined function for multiple TVE analysis with big datasets
mma.e1<-mma.big(x=m,y=data.frame(y), mediator=1:ncol(m),
                pred=data.frame(pred), alpha=1, alpha1=0.05, alpha2=0.05,
                w=MyData_Variables$Final_weight)
head(mma.e1$results)
summary(mma.e1)

save(MyData_Variables,
     data.e1,med.e1,mma.e1,
     file = "BRFSS_Transform_Race_weight.Rdata",
     compress = TRUE)

# setwd("G:/NIA/other_information/training/2021_HealthDisparitiesCodeathon/OurGroup/BRFSS/R")
# load("BRFSS_Transform_Race_weight.Rdata")

