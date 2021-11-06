# Set up 
D<- read.csv("C:/Users/Yueru/Desktop/UMD/Fall 2020/STAT 426/HW 8/diabetes.csv",header=T,fileEncoding="UTF-8-BOM") 
D$Diabetes<-as.factor(D$Diabetes) 
DP<-D[,-8] 

#PC  See lab and other class work for details on contents of prDP.out 
prDP.out<-prcomp(DP,scale=T) 

# Setting up to run logistic on principal compenents 
DPC<-data.frame(prDP.out$x,D$Diabetes) 
names(DPC)[8]<-"Diabetes" 

# Logistic Regression on all principal components 
lr.pc.fit<-glm(Diabetes~.,family=binomial,data=DPC) 
summary(lr.pc.fit) 
lr.pc.pr<-rep("LO",dim(D)[1]) 
lr.pc.pr[predict(lr.pc.fit)>0]<-"HI" 
(t3<-table(lr.pc.pr,DPC$Diabetes)) 
sum(diag(t3))/sum(t3) 

# Logistic Regression on three principal components. 
lr.pc.r.fit<-glm(Diabetes~PC1+PC2+PC6,family=binomial,data=DPC) 
summary(lr.pc.r.fit) 
lr.pc.r.pr<-rep("LO",dim(D)[1]) 
lr.pc.r.pr[predict(lr.pc.r.fit)>0]<-"HI" 
(t4<-table(lr.pc.r.pr,DPC$Diabetes)) 
sum(diag(t4))/sum(t4) 

# Logistic Regression on first two principal components. 
lr.pc.fit1<-glm(Diabetes~PC1+PC2,family=binomial,data=DPC) 
summary(lr.pc.fit1) 
lr.pc.pr1<-rep("LO",dim(D)[1]) 
lr.pc.pr1[predict(lr.pc.fit1)>0]<-"HI" 
(t5<-table(lr.pc.pr1,DPC$Diabetes)) 
sum(diag(t5))/sum(t5)

# Logistic Regression on first two principal components and quadratic terms
lr.pc.fit2<-glm(Diabetes~poly(PC1,2,raw=T)+poly(PC2,2,raw=T),family=binomial,data=DPC) 
summary(lr.pc.fit2) 
lr.pc.pr2<-rep("LO",dim(D)[1]) 
lr.pc.pr2[predict(lr.pc.fit2)>0]<-"HI" 
(t6<-table(lr.pc.pr2,DPC$Diabetes)) 
sum(diag(t6))/sum(t6)

# Testing equivalence of models fit1 and fit2
anova(lr.pc.fit1,lr.pc.fit2,test="Chisq") 

# Plotting first 2 principal components along with Diabetes 
pc1<-prDP.out$x[,1] 
pc2<-prDP.out$x[,2] 
plot(pc1,pc2,type="n",bty="n",xlab="1st Principal Component",ylab="2nd Principal Component") 
points(pc1[D$Diabetes=="NO"], pc2[D$Diabetes=="NO"],cex=0.5,col="blue") 
points(pc1[D$Diabetes=="YES"],pc2[D$Diabetes=="YES"], cex=0.5,col="red") 
legend("topleft",legend=c("Diabetes: NO","Diabetes: YES","Linear","Quadratic"),
       col=c("blue","red","purple","green"),
       bty="n",pch=c(1,1,NA,NA),
       text.col=c("blue","red","purple","green"),
       lty=c(NA,NA,2,2),
       lwd=c(NA,NA,2,2)) 

#add boundaries
x1<-seq(min(DPC$PC1),max(DPC$PC1),length=1000) 
y1<--2.17626*x1+2.062375
lines(x1,y1,lty=2,lwd=2,col="purple") 
#qudratic boundary
x2<-seq(min(DPC$PC1),max(DPC$PC1),length=1000) 
y2<-(54547-sqrt(380413401+6758215360*x2 -974507368*x2^2))/29404
lines(x2,y2,lty=2,lwd=2,col="green") 
y3<-(54547 + sqrt(380413401 + 6758215360*x2 - 974507368*x2^2))/29404
lines(x2,y3,lty=2,lwd=2,col="green")
