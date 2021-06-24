
# import dataset
MyData <- read.csv("BRFSS_Final_new.csv")
colnames(MyData)

# extract factors
library(readxl)
Variables_BRFSS <- read_excel("Variables_BRFSS_new.xlsx", sheet =2 )
Variables_BRFSS$Variables

colnames(Variables_BRFSS)

# select interesting factors
MyData_Variables <- MyData[colnames(MyData) %in% Variables_BRFSS$Variables]

# transfer columns into the type of factor
colnames(MyData_Variables)
MyData_Variables[,-c(1,2,4,6,7,17)] <- lapply(MyData_Variables[,-c(1,2,4,6,7,17)],as.factor)

# save precleared dataset
saveRDS(MyData_Variables,
        file = "MyData_Variables_asthma.rds",
        compress = TRUE)

# select predictor, outcome and mediators
#pred <- MyData_Variables$ENDS_U
pred <- MyData_Variables$Race_U
summary(pred)

y <- MyData_Variables$CurrentA
summary(y)

colnames(MyData_Variables)
m <- MyData_Variables[,c(2,3,4,22,24,25,26,27,28,1,23)]
summary(m)
colnames(m)

### use the package
library("mmabig")
# Data Organization and Identify Potential Moderators/Confounders
data.e1<-data.org.big(x=m,
                      y=data.frame(y),
                      mediator=c(1:9),
                      pred=data.frame(pred),
                      testtype=1,
                      w=MyData_Variables$Final_weight
)
summary(data.e1)


# Third-Variable Effect Analysis
med.e1<-med.big(data.e1)
summary(med.e1)

# Combined function for multiple TVE analysis with big datasets
mma.e1<-mma.big(data=data.e1, alpha=1, alpha1=0.05, alpha2=0.05,
                w=MyData_Variables$Final_weight)
head(mma.e1$results)

summary(mma.e1)
summary(mma.e1, RE=T)

chisq.test(MyData_Variables$CurrentA, MyData_Variables$Race_U)
plot(mma.e1,vari ="DepU")
plot(mma.e1,vari ="CigU")
plot(mma.e1,vari ="Income")
plot(mma.e1,vari ="SexU")


save(data.e1,med.e1,mma.e1,
     file = "BRFSS_Transform_Race_weight_asthma.Rdata",
     compress = TRUE)
