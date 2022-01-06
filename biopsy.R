#Biopsy Prediction

#library(MLDataR)
#data("diabetes_data")
#db <- diabetes_data
preds<-db[,1:p] #Every predictor, except for Age, is qualitative
target<-db[,17] #target/dependent variable "DiabeticClass"
cpreds<-scale(preds, center=TRUE, scale=FALSE) #center the predictors

data("biopsy")
biopsy <- biopsy
str(biopsy)
visdat::vis_miss(biopsy)
biopsy<- na.omit(biopsy)

set.seed(0329)
split <- rsample::initial_split(biopsy, prop=3/4)
train_data <- rsample::training(split)
test_data <- rsample::testing(split)

preds<-biopsy[,2:10]
cpreds <- scale(preds, center=TRUE,scale=FALSE)
target<-biopsy[,11]

#function: Performance Table
performance <- function(tab){
  hitrate <- (sum(diag(tab))/sum(tab))
  error <- 1-hitrate
  sensitivity <- tab[2,2] / (tab[2,1] + tab[2,2])
  specificity <- tab[1,1] / (tab[1,1] + tab[1,2])
  performance <- c(hitrate=hitrate, error=error, sensitivity=sensitivity, specificity=specificity)
}

#LDA
lda.out <- lda(cpreds, target)
#predictions training data
pred.train<-predict(lda.out,cpreds)

#Bayes classifier (a)
tab<-table(target,pred.train$class)
lda.train.equal<-performance(tab)

#Test error
lda.out <- lda(cpreds, target, CV=TRUE)
tab<-table(target,lda.out$class)
lda.test.equal<-performance(tab)

lda.train.equal
lda.test.equal

#QDA
qda.out <- qda(cpreds, target)
#predictions training data
pred.train<-predict(qda.out,cpreds)

#Bayes classifier (a)
tab<-table(target,pred.train$class)
qda.train.equal<-performance(tab)

#Test error
qda.out <- qda(cpreds, target, CV=TRUE)
tab<-table(target,qda.out$class)
qda.test.equal<-performance(tab)

#################
#     LDA+PCA   #
#################
zpreds<-scale(cpreds, center=TRUE, scale=TRUE) #standardize the predictors
prcomp.out<-prcomp(zpreds)
scree(zpreds) #1st component
#paran(zpreds,iterations=5000,graph=TRUE,cfa=FALSE,centile=95) #Horn's procedure
ncomp<-1

#compute standardized principal components
train.comp<-zpreds%*%prcomp.out$rotation[,1]
#compute lda on principal components
ldacomp.out<-lda(train.comp,target)

#predictions training data
predlda.train<-predict(ldacomp.out,train.comp)
#Bayes classifier (a)
tab<-table(target,predlda.train$class)
lda.pca.train.equal<-performance(tab)

#predictions test data (LOOCV)
ldacomp.out<-lda(train.comp,target,CV=TRUE)
#Bayes classifier (a) 
tab<-table(target,ldacomp.out$class)
lda.pca.test.equal<-performance(tab) 



#############
# PCA+QDA   #
#############
#compute qda on principal components
qdacomp.out<-qda(train.comp,target)

#predictions training data
predqda.train<-predict(qdacomp.out,train.comp)
#Bayes classifier (a)
tab<-table(target,predqda.train$class)
qda.pca.train.equal<-performance(tab)

#predictions test data (LOOCV)
qdacomp.out<-qda(train.comp,target,CV=TRUE)
#Bayes classifier (a) 
tab<-table(target,qdacomp.out$class)
qda.pca.test.equal<-performance(tab) 


#########
# HDDA  #
#########
hdda.out<-hdda(cpreds, target, model="all", d_select="BIC")

#predictions training data
pred.train<-predict(hdda.out,cpreds)
#Bayes classifier (a)
tab<-table(target,pred.train$class)
hdda.train.equal<-performance(tab)

#predictions test data (LOOCV)
hdda.out #using BIC, the selected model is "AKJBKQKD" 
pred.test<-hdda(cpreds, target, model="AKJBKQKD", com_dim=3, LOO=TRUE)
#Bayes classifier (a)
tab<-table(target, pred.test$class)
hdda.test.equal<-performance(tab)

###############
#   bagging   #
###############
ctrain<-cbind(cpreds,target)
bag.mod=randomForest(as.factor(target)~.,data=ctrain,mtry=p,ntree=5000,importance=TRUE) #ntree can be changed to a higher number but becomes computationally heavy
bag.mod #OOB estimate of error rate = 3.66% (test error)

#predictions training data 
pred.train<-predict(bag.mod,newdata=ctrain,type="prob")
#Bayes classifier (a)
class.train<-ifelse(pred.train>0.5,"Yes","No")
tab<-table(target,class.train[,2])
bag.train.equal<-performance(tab)

#predictions test data (OOB)
pred.test<-bag.mod$votes #use OOB predictions for test performance
#Bayes classifier (a)
class.test<-ifelse(pred.test>0.5,"Yes","No") 
tab<-table(target,class.test[,2])
bag.test.equal<-performance(tab) #test error = OOB estimate of error rate = 7.87% 

################
# RandomFOREST #
################
set.seed(1)
ctrain<-cbind(preds,target)
rf.mod=randomForest(as.factor(target)~.,data=ctrain,mtry=3,ntree=5000,importance=TRUE) #9 because close to sqrt(p)=sqrt(11)
rf.mod #OOB estimate of error rate = 2.64% (test error)

#predictions training data 
pred.train<-predict(rf.mod,newdata=ctrain,type="prob")
#Bayes classifier (a)
class.train<-ifelse(pred.train>0.5,"Yes","No")
tab<-table(target,class.train[,2])
rf.train.equal<-performance(tab)

#predictions test data (OOB)
pred.test<-rf.mod$votes #use OOB predictions for test performance
#Bayes classifier (a)
class.test<-ifelse(pred.test>0.5,"Yes","No") 
tab<-table(target,class.test[,2])
rf.test.equal<-performance(tab) #test error = 6.84% = OOB estimate of error rate 


summary(Caravan)
n_no=5474 #n_1
n_yes=348 #n_2
K=2 #number of groups is 2 (yes & no)
p=83 #number of predictor variables
nr_params_QDA=K*p+K-1+0.5*K*p*(p+1) 
nr_params_LDA=K*p+K-1+0.5*p*(p+1)

preds<-Caravan[,1:p] 
target<-Caravan[,84] #target/dependent variable Purchase
cpreds<-scale(preds, center=TRUE, scale=FALSE) #center the predictors
#No data splitting because we use LOOCV and OOB predictions to estimate test performance

###training and LOOCV/OOB performance of classifiers





###overview table with training and test classification performance for each classifier and for both classification scenarios
#TRAINING ERRORS (equal=Bayes Classifier, unequal=unequal classification costs)
tab1<-round(rbind(lda.train.equal,lda.pca.train.equal,qda.pca.train.equal, hdda.train.equal, bag.train.equal, rf.train.equal),5)
tab1
#TEST ERRORS (equal=Bayes Classifier, unequal=unequal classification costs)
tab2<-round(rbind(lda.test.equal,lda.pca.test.equal,qda.pca.test.equal, hdda.test.equal, bag.test.equal, rf.test.equal),5)
tab2
#no column for QDA because it fails (class-specific covariance matrix is singular for both classes)