library(class)

findno<-function(o,xcc)
{
  ff1=o[1]-xcc[1,1]
  ff2=o[2]-xcc[1,2]
  ff3=o[3]-xcc[1,3]
  
  binnum=0
  if(ff1>0)
    binnum=binnum+4
  if(ff2>0)
    binnum=binnum+2
  if(ff3>0)
    binnum=binnum+1
  binnum=binnum+1
  
  return(binnum)
}

myfunction<-function(b,c)
{
  #geting 30% of indexes randomly
  indexes = sample(1:nrow(b), size=0.3*nrow(b))
  
  # Split training data
  test = b[indexes,]
  
  
  
  # Split testing data
  train = b[-indexes,]
  
  
  columns<-names(b)
  
  nocol=ncol(b)
  cvec<-train[,nocol]
  
  start.time1 <- Sys.time()
  
  kfff<-knn(train,test,cvec,c)
  
  
  
  
  acc1=0
  
  for(i in 1:nrow(test))
  {
     integer(kfff[i])
    if(kfff[i]==test[i,nocol])
    {
      acc1=acc1+1
    }
   
  }
  
 acc1=acc1/nrow(test)
  
  zo=nocol-1
  countr=integer(zo)
  meann=integer(zo)
  
  
  for(i in 1:zo)
  {
    
    
    x<-data.frame(train[,i])
    
    x$c2=train[,nocol]
    
    names(x)[1]="c1"
    
    
    kf1<-knn(x,x,x$c2,c)
    
    
    
    for(j in 1:nrow(x))
    {
       integer(kf1[j])
      
      if(kf1[j]==x[j,2])
      {
        countr[i]=countr[i]+1
        
      }
      
      
      
    }
   
    
    meann[i]=mean(x$c1)
    
    
  }
  
  meanss=c(meann)
  maxmi=max(meann)
  
  counters=data.frame(names(train))
  counters=data.frame(counters[1:zo,])
  countr=c(countr)
 
  counters$b=countr
  counters$c=meanss
  counters$d=c(1:zo)
 
  names(counters)[1:4]=c("column name","Positive count","Mean ","Column number")
 # counters[nocol,]<-NULL

  
  counterss=counters[order(counters$'Positive count'),]
  
  counterss$iw=c(1:zo)
  counterss$fw=counterss$iw*maxmi/counterss$Mean
 
  
  countersss=counterss[order(counterss$fw,decreasing=TRUE),]
  
  
  
  fno=integer(3)
  fno[1]=countersss[1,4]
  fno[2]=countersss[2,4]
  fno[3]=countersss[3,4]
  
 
 
   x=data.frame(train[,fno[1]])
   x$c2=train[,fno[2]]
   x$c3=train[,fno[3]]
   x$c4=train[,nocol]
   
   names(x)[1]<-c("c1")
  
  
  
   y=data.frame(test[,fno[1]])
   y$c2=test[,fno[2]]
   y$c3=test[,fno[3]]
   y$c4=test[,nocol]
  
   names(y)[1]<-c("c1")
   
  
  
   ran=sample(1:nrow(x),1)
   c1=x[ran,1]
   c2=x[ran,2]
   c3=x[ran,3]
   
   
   
   oo=matrix(double(24),nrow=8,ncol=3,byrow = TRUE)
   
   
   
   
   e<-double(3)
   rr=max(x$c1)-min(x$c1)
   rr=rr*c
   rr=rr/nrow(x)
   
   e[1]=(max(x$c1)-min(x$c1))*c/nrow(x)
   e[2]=(max(x$c2)-min(x$c2))*c/nrow(x)
   e[3]=(max(x$c3)-min(x$c3))*c/nrow(x)
   
   
   
   
  
   oo[1,1]=c1-e[1]
   oo[1,2]=c2-e[2]
   oo[1,3]=c3-e[3]
   
   oo[2,1]=c1-e[1]
   oo[2,2]=c2-e[2]
   oo[2,3]=c3+e[3]
   
   oo[3,1]=c1-e[1]
   oo[3,2]=c2+e[2]
   oo[3,3]=c3-e[3]
   
   oo[4,1]=c1-e[1]
   oo[4,2]=c2+e[2]
   oo[4,3]=c3+e[3]
   
   oo[5,1]=c1+e[1]
   oo[5,2]=c2-e[2]
   oo[5,3]=c3-e[3]
   
   oo[6,1]=c1+e[1]
   oo[6,2]=c2-e[2]
   oo[6,3]=c3+e[3]
   
   oo[7,1]=c1+e[1]
   oo[7,2]=c2+e[2]
   oo[7,3]=c3-e[3]
   
   oo[8,1]=c1+e[1]
   oo[8,2]=c2+e[2]
   oo[8,3]=c3+e[3]
   
   
   
   
   
   
   binn<-list()
   binn[[1]]<-0
   binn[[2]]<-0
   binn[[3]]<-0
   binn[[4]]<-0
   binn[[5]]<-0
   binn[[6]]<-0
   binn[[7]]<-0
   binn[[8]]<-0
  
  
   for(i in 1:8)
   {
      binn[[i]]<-data.frame()
      tempp<-data.frame()
      rowss=1
      for(j in 1:nrow(x))
      {
          binnoo<-findno(oo[i,],x[j,]) 
          x[j,5]=binnoo
          names(x)[5]<-"binn"
        
        
        
      }
     
      binn[[i]]=subset(x,x$binn==i)
      binn[[i]]=binn[[i]][,1:4]
      x=x[,1:4]
     
     
   }
   acc=0
   for(i in 1:nrow(y))
   {
     ff1=c1-y[i,1]
     ff2=c2-y[i,2]
     ff3=c3-y[i,3]
     
     binnum=0
     if(ff1>0)
       binnum=binnum+4
     if(ff2>0)
       binnum=binnum+2
     if(ff3>0)
       binnum=binnum+1
     binnum=binnum+1
     
     kf1<-knn(binn[[binnum]],y,binn[[binnum]]$c4,k=c)
     resi=y[i,4]
 
     integer(resi)
     as.integer(kf1)
    
     zv=(kf1[2]==resi)
    if(zv)
    {
      acc=acc+1
    }
     
    
     
     
     
   }
   acc=acc/nrow(y)
  
   
   out<-c(acc1,acc)
   
   
   
   
   return(out)
   
   
   
   
   
  
}









#read dataset in a dataframe
dataset <- read.csv("lower.csv")




#renaming columns
names(dataset)[1:13]<-c('Pelvic Incidence','Pelvic Tilt','Lumbar Lordosis Angle'
                        ,'Sacral Slope','Pelvic Radius', 'Spondylolisthesis Degree', 
                        'Pelvic Slope', 'Direct Tilt', 'Thoracic Slope', 'Cervical Tilt',
                        'Sacrum Angle', 'Scoliosis Slope','Outcome')

  

#removing column X
dataset$X<- NULL


#set class labels normal and abnormal  to 1 and 0
levels(dataset$Outcome)[1]<-0
levels(dataset$Outcome)[2]<-1



a1<-myfunction(dataset,1)

dataset2 <- read.csv("hr.csv")

a2<-myfunction(dataset2,1)

#read dataset in a dataframe

dataset3 <- read.csv("iriss.csv")


levels(dataset3$Species)[1]<-1
levels(dataset3$Species)[2]<-2
levels(dataset3$Species)[3]<-3



a3<-myfunction(dataset3,1)


print("Backpain dataset :")
print("Improved knn:")
print("Accuracy:")
print(a1[1])
print("Standard knn:")
print("Accuracy:")
print(a1[2])


print("Human Resources dataset :")
print("Improved knn:")
print("Accuracy:")
print(a2[1])
print("Standard knn:")
print("Accuracy:")
print(a2[2])


print("Iriss dataset :")
print("Improved knn:")
print("Accuracy:")
print(a3[1])
print("Standard knn:")
print("Accuracy:")
print(a3[2])

