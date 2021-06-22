# import dataset
MyData <- read.csv("../Rime/BRFSS_Final.csv")

# extract factors
library(readxl)
Variables_BRFSS <- read_excel("G:/NIA/other_information/training/2021_HealthDisparitiesCodeathon/OurGroup/BRFSS/Variables_BRFSS.xlsx")

# select interesting factors
MyData_Variables <- MyData[colnames(MyData) %in% Variables_BRFSS$Variables]


# install and load package
# install.packages("mmabig")
library("mmabig")

# make the data into the required structure and type by the package
# classify:

# 1: non-Hispanic white
# 2: non- black
# 3: non- Asian
# 4: non- American Indian
# 5: Hispanic
# 6: non others

# 7: 3, 4, 6: non others
# 1, 2, 5, 7
MyData_Variables[MyData_Variables$Race == 3 | MyData_Variables$Race == 4 | MyData_Variables$Race == 6,]$Race <- 7
#MyData_Variables$Race <- as.factor(MyData_Variables$Race)
#summary(MyData_Variables$Race)


# transfer columns into the type of factor
colnames(MyData_Variables)
head(MyData_Variables)
MyData_Variables[,-25] <- lapply(MyData_Variables[,-25],as.factor)

write.csv(MyData_Variables,"MyData_Variables.csv", row.names = F)




################################### Start
MyData_Variables <- read.csv("MyData_Variables.csv")
colnames(MyData_Variables)
MyData_Variables[,-25] <- lapply(MyData_Variables[,-25],as.factor)

# select predictor, outcome and mediators
pred <- MyData_Variables$Race
summary(pred)
class(pred)

y <- MyData_Variables$Depression
summary(y)
class(y)
 
m <- MyData_Variables[,-c(1,5)]
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

# Combined function for multiple TVE analysis with big datasets
mma.e1<-mma.big(x=m,y=data.frame(y), mediator=1:ncol(m),
                pred=data.frame(pred), alpha=1, alpha1=0.05, alpha2=0.05)




