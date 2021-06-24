# save precleared dataset
loadRDS(file = "MyData_Variables_asthma.rds")

# select predictor, outcome and mediators
#pred <- MyData_Variables$ENDS_U
pred <- MyData_Variables$Race_U
summary(pred)

y <- MyData_Variables$DepU
summary(y)

colnames(MyData_Variables)
m <- MyData_Variables[,c(2,3,4,21,22,24,25,27,28,1,23)]
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

plot(mma.e1,vari ="CurrentA")
plot(mma.e1,vari ="CigU")
plot(mma.e1,vari ="Income")
plot(mma.e1,vari ="MarijuC")

plot(mma.e1,vari ="SexU")


save(data.e1,med.e1,mma.e1,
     file = "BRFSS_Transform_Race_weight_depression.Rdata",
     compress = TRUE)
