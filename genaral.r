library(class)
# Knn classsic

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


myfunction<-function(data_set,k)
{
  #get columns of data
  num_col = ncol(data_set)
  # we want scale our data
  #data_set[,1:(num_col-1)] <- scale(data_set[,1:(num_col-1)])
  
  # get ting 30% of indexes randomly
  indexes = sample(1:nrow(data_set), size = 0.3*nrow(data_set))
  
  
  # Split traning data
  train_data = data_set[-indexes,]
  #print.default(dim(train_data))
  #Split testing data
  test_data =  data_set[indexes,]
  # training labels
  train_labels<- data_set[-indexes,num_col]
  #print.default(dim(train_labels))
  #testing labels
  test_labels<-data_set[indexes,num_col]
  columns<-names(data_set)
  #prediction of knn
  #start.time <- Sys.time()
  train_pred <- knn(train_data, test_data, train_labels, k)
  
  
  #accualy of knn standard
  #acc1 <- sum(diag(train_pred))/length(train_labels)
  # time of knn standard
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #print.default(accuracy)
  acc1=0
  for(i in 1:nrow(test_data)) 
  { 
    integer(train_pred[i]) 
    if(train_pred[i]== train_labels[i]) 
    { 
      acc1=acc1+1 
    } 
  }
  acc1=acc1/nrow(test_data) 
  
  #improve KNN
  num_feature = num_col -1
  countr=integer(num_feature)
  meann=integer(num_feature)
  for(i in 1:num_feature)
  {
    x <- data.frame(train_data[,i])
    x$c2 = train_labels
    #print.default(x$c2)
    names(x)[1]="c1"
    
    knnf1<- knn(x, x, x$c2,k)
    #
    for(j in 1:nrow(x))
    {
      integer(knnf1[j]) 
      if(knnf1[j]==x[j,2]) 
      { 
        countr[i]=countr[i]+1 
      } 
      
    }
    meann[i]=mean(x$c1)
    #print.default(meann[i])
  }
  meanss=c(meann) 
  maxmi=max(meann) 
  #print.default(meanss)
  #print.default(maxmi)
  #start.time <- Sys.time()
  counters=data.frame(names(train_data))
  #print.default(counters)
  counters=data.frame(counters[1:num_feature,]) 
  #print.default(counters)
  countr=c(countr) 
  counters$b=countr 
  counters$c=meanss 
  counters$d=c(1:num_feature)
  names(counters)[1:4]=c("column name","Positive count","Mean ","Column number")
  #print.default(counters)
  counters_sort=counters[order(counters$'Positive count'),] 
  #print.default(counters_sort)
  counters_sort$iw=c(1:num_feature) 
  counters_sort$fw=counters_sort$iw*maxmi/counters_sort$Mean 
  counters_sort_fw=counters_sort[order(counters_sort$fw,decreasing=TRUE),] 
  #print.default(counters_sort_fw)
  
  # get 3 feature have weigth is sort
  fno=integer(3) 
  fno[1]=counters_sort_fw[1,4] 
  fno[2]=counters_sort_fw[2,4] 
  fno[3]=counters_sort_fw[3,4] 
  
  #print.default(fno)
  # set value for new train_data and test_data
  #new_train
  new_train=data.frame(train_data[,fno[1]])
  new_train$c2=train_data[,fno[2]] 
  new_train$c3=train_data[,fno[3]] 
  new_train$c4=train_labels
  names(new_train)[1]<-c("c1") 
  
  #print.default(new_train)
  #new_test
  new_test=data.frame(test_data[,fno[1]]) 
  new_test$c2=test_data[,fno[2]] 
  new_test$c3=test_data[,fno[3]] 
  new_test$c4=test_labels 
  names(new_test)[1]<-c("c1")
  
  #print.default(new_test)
  
  #
  ran=sample(1:nrow(new_train),1) 
  c1=new_train[ran,1] 
  c2=new_train[ran,2] 
  c3=new_train[ran,3] 
  
  #
  oo=matrix(double(24),nrow=8,ncol=3,byrow = TRUE)
  
  rr=max(new_train$c1)-min(new_train$c1) 
  rr=rr*k
  rr=rr/nrow(new_train) 
  ##finding error margins
  e<-double(3) 
  e[1]=(max(new_train$c1)-min(new_train$c1))*k/nrow(new_train) 
  e[2]=(max(new_train$c2)-min(new_train$c2))*k/nrow(new_train) 
  e[3]=(max(new_train$c3)-min(new_train$c3))*k/nrow(new_train) 
  
  # calculating centroid for each bin 
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
  
  #initialize bins 
  binn<-list() 
  binn[[1]]<-0 
  binn[[2]]<-0 
  binn[[3]]<-0 
  binn[[4]]<-0 
  binn[[5]]<-0 
  binn[[6]]<-0 
  binn[[7]]<-0
  binn[[8]]<-0
  
  
  #8 bin hashing 
  for(i in 1:8) 
  { 
    binn[[i]]<-data.frame() 
    tempp<-data.frame() 
    rowss=1 
    for(j in 1:nrow(new_train)) 
    { 
      binnoo<-findno(oo[i,],new_train[j,]) 
      new_train[j,5]=binnoo 
      names(new_train)[5]<-"binn" 
    } 
    binn[[i]]=subset(new_train,new_train$binn==i) 
    binn[[i]]=binn[[i]][,1:4] 
    new_train=new_train[,1:4] 
  }
  
  acc=0 
  for(i in 1:nrow(new_test)) 
  { 
    ff1=c1-new_test[i,1] 
    ff2=c2-new_test[i,2] 
    ff3=c3-new_test[i,3] 
    binnum=0 
    if(ff1>0) 
      binnum=binnum+4 
    if(ff2>0) 
      binnum=binnum+2 
    if(ff3>0) 
      binnum=binnum+1 
    binnum=binnum+1 
    knn_new<-knn(binn[[binnum]],new_test,binn[[binnum]]$c4,k) 
    resi=new_test[i,4] 
    integer(resi) 
    as.integer(knn_new) 
    #print.default(knn_new[2])
    zv=(knn_new[2]==resi) 
    if(zv) 
    { 
      acc=acc+1 
    } 
  } 
  acc=acc/nrow(new_test)
  output<-c(acc1, acc)
  return(output)
  

  
}



# D???c d??? li???u
dataset <- read.csv("H:/code-KPDL_BTL/lower.csv", header = TRUE)

dataset$X <- NULL
names(dataset)[1:13] <- c('Pelvic Incidence', 'Pelvic Tilt', 'Lumbar Lordosis Angle', 'Sacral Slope', 'Pelvic Radius', 'Spondylolisthesis Degree', 'Pelvic Slope', 'Direct Tilt', 'Thoracic Slope', 'Cervical Tilt', 'Sacrum Angle', 'Scoliosis Slope', 'Outcome')
levels(dataset$Outcome) <- c("0", "1")

dataset2 <- read.csv("H:/code-KPDL_BTL/hr.csv", header = TRUE)

dataset3 <- read.csv("H:/code-KPDL_BTL/iriss.csv", header = TRUE)
levels(dataset3$Species) <- c("1", "2", "3")

# D???nh nghia giá tr??? k cho t???ng b??? d??? li???u
k_values1 <- c(1, 3, 5, 7, 11)
k_values2 <- c(1, 3, 5, 7, 11)
k_values3 <- c(1, 4, 7, 11, 13)

# Hàm d??? ch???y, in k???t qu??? và v??? bi???u d???
run_print_and_plot <- function(dataset, k_values, dataset_name) {
  y_improve <- numeric(length(k_values))
  y_standard <- numeric(length(k_values))
  
  for (i in seq_along(k_values)) {
    results <- myfunction(dataset, k_values[i])
    y_improve[i] <- results[1]
    y_standard[i] <- results[2]
    cat(sprintf("Results for %s dataset with k = %d:\n", dataset_name, k_values[i]))
    cat(sprintf("Improved KNN Accuracy: %.2f\n", results[1]))
    cat(sprintf("Standard KNN Accuracy: %.2f\n\n", results[2]))
  }
  
  # Opening a new graphic window for each plot
  dev.new()
  plot(k_values, y_improve, type="o", col="blue", pch="o", xlab="K-values", ylab="Accuracy", main=paste("Accuracy of KNN with", dataset_name), lty=1)
  points(k_values, y_standard, col="red", pch="*")
  lines(k_values, y_standard, col="red", lty=2)
  legend("bottomright", legend=c("Improved KNN", "Standard KNN"), col=c("blue", "red"), pch=c("o", "*"), lty=c(1, 2))
}

# V??? bi???u d??? và in k???t qu??? cho t???ng b??? d??? li???u
run_print_and_plot(dataset, k_values1, "Lower")
run_print_and_plot(dataset2, k_values2, "HR")
run_print_and_plot(dataset3, k_values3, "Iris")