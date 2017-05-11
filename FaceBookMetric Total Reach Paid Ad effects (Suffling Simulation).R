###Facebook Metrics Searching

## Before starting Data Analysis, Our goal has to be classsification trough factor variables.
## At first, we need to omit or replace null data on dataset.


setwd("C:/Users/Caglan.Akpinar/Desktop/R Files/R Files")
Facebook = read.csv("dataset_Facebook.csv",sep=";",header=T)

############################################################################################################
############################################################################################################


for(COLUMN in 1: ncol(Facebook)){
  if(is.numeric(Facebook[,COLUMN])){
    A = as.factor(Facebook[,COLUMN])
    if(nrow(table(as.factor(A))) < 24) { 
      print(COLUMN) 
      Facebook[,COLUMN] = as.factor(Facebook[,COLUMN])
    }
  }
}


############################################################################################################
############################################################################################################

## Total.Interaction = Share + like + Comment.
## So we are analysing these columns indivisually. 
## We are not selecting Total.Interaction Variable into the Regression Model
Facebook = Facebook[ ,colnames(Facebook) != "Total.Interactions"]
Facebook$Paid     = as.factor(Facebook$Paid)
Facebook$Category = as.factor(Facebook$Category)
Facebook$Post.Month = as.factor(Facebook$Post.Month)
Facebook$Post.Weekday = as.factor(Facebook$Post.Weekday)
Facebook$Post.Hour = as.factor(Facebook$Post.Hour)

##Converting missig values
##Checking all variables that have missing values
##If it is a factor, it can be replaced by the highest probability of factors
##If it is a numeric, it can be replaced by the mean of variable

for(i in 1:ncol(Facebook)){
  set = Facebook[,i]
  if(is.factor(set))
     if(any(is.na(set)) == TRUE){
  {
    b = levels(set)
    c = (table(set) / length(set))
    a = data.frame(c, b)
    d = max(table(set) / length(set))
    z = a[c == d,]
    
    set[is.na(set)] = z$b
    Facebook[,i] = set
 
  }}  
  if(is.numeric(set))
    
  { if(any(is.na(set)) == TRUE){
    
    set[is.na(set)] = mean(set[!is.na(set)])
    Facebook[,i] = as.integer(set)
    
  }}
}





##Facebook$Paid     = as.numeric(Facebook$Paid)
Facebook$Category = as.numeric(Facebook$Category)
Facebook$Post.Month = as.numeric(Facebook$Post.Month)
Facebook$Post.Weekday = as.numeric(Facebook$Post.Weekday)
Facebook$Post.Hour = as.numeric(Facebook$Post.Hour)


#1 ------ Model Update Process
#Regression Model     : lm
#Dependent Variable   : Lifetime.Post.Total.Reach, 
#Independent Variable : *
#DataSet              : Facook (Total.Interaction has been omitted), (NA values are converted to Column mean)

Model = lm(Lifetime.Post.Total.Reach~.,data=Facebook)

## Is there any diffrenct between effect of advertisemnt paid and unpaid?
## H0: Mean_Paid - Mean_Unpaid = 0
## H1: Mean_Paid - Mean_Unpaid > 0
## Alpha = 0.05
## If H0 is accepted Ad Paid does not make any differances on Total Reach
## It doesn`not make sense adding on Regression model if we accept H0`


Facebook_Paid   = Facebook[Facebook$Paid == "1", ]
Facebook_Unpaid = Facebook[Facebook$Paid == "0", ]
Facebook_Paid = na.omit(Facebook_Paid)
Facebook_Unpaid = na.omit(Facebook_Unpaid)

MeanPaid   = mean(Facebook_Paid$Lifetime.Post.Total.Reach)
MeanUnpaid = mean(Facebook_Unpaid$Lifetime.Post.Total.Reach)
MeanDiff = MeanPaid - MeanUnpaid

SampleRow = nrow(Facebook_Paid) + nrow(Facebook_Unpaid)
Sampledata = c(Facebook_Paid$Lifetime.Post.Total.Reach, Facebook_Unpaid$Lifetime.Post.Total.Reach)

N = 50000

MeanDiffSample = numeric(N)
MeanDiffSample
counter = 0
asd = 0
NewPaid = 0
NewUnpaid = 0
#meanDiff  numeric(N) # N row vector has "0" value
set.seed(1000)
row = nrow(Facebook_Paid) + 1

for(j in 1:N)
{
  Data = sample(Sampledata,SampleRow,replace = FALSE)
  NewPaid = na.omit(Data[1:nrow(Facebook_Paid)])
  NewUnpaid = na.omit(Data[row : SampleRow])
  a = mean(NewPaid)
  b = mean(NewUnpaid)
  MeanDiffSample[j] =  a - b 
  if(MeanDiffSample[j] > MeanDiff){
     counter = counter + 1
  }
}



 
cat("Number of cases where Paid and Unpaid dataset Mean different > 4.4:",counter,'\n') 
cat("RAtion of counter appears on N : ", counter/N,'\n')
hist(MeanDiffSample,freq = FALSE,col = "Grey")
curve(dnorm(x, mean(MeanDiffSample), sd(MeanDiffSample)), add=TRUE, col="darkblue", lwd=2)
points(x=MeanDiff, y=0, pch=16, col= "Black")
abline(v=MeanDiff, pch=16, col= "Red")

"There is significant relationship which can be calculated by Statistics."

If(pnorm(MeanDiff, mean(MeanDiffSample), sd(MeanDiffSample)) > 0.05)
   {
     print("H0 Rejected There is not difference between Paid ad and unpaid ad according to Total Reach they are.")
     Facebook = Facebook[,colnames(Facebook) != "Paid"]
      
       
}

if(pnorm(MeanDiff, mean(MeanDiffSample), sd(MeanDiffSample)) < 0.05)
   {
             print("There is significant relationship which can be calculated by Statistics.")
             print("It is better not to ignore Paid Variable from Main Regresion Model.")
        }
         


#1 ------ Model Update Process
#Regression Model     : lm
#Dependent Variable   : Lifetime.Post.Total.Reach, 
#Independent Variable : * - Type
#DataSet              : Facook (Total.Interaction has been omitted), (NA values are converted to Column mean)



### Is Total Reach  effected by Category of People?


#NumberofDataSet
a = 0
for(z in 2 : (max(Facebook$Category)- 1)){
  a[z] = ncol(combn(3,z))
}

NumberofDataSet = sum(a)
NumberofDataSet

B0Matris = matrix(ncol = (NumberofDataSet), nrow = N)
B1Matris = matrix(ncol = (NumberofDataSet), nrow = N)
SubMainModelB0Matris = matrix(ncol = max(Facebook$Category), nrow = 1)
SubMainModelB1Matris = matrix(ncol = max(Facebook$Category), nrow = 1)

HyphothesisMatris = matrix(ncol = max(Facebook$Category), nrow = 1)

MainDataSet = Facebook

set.seed(100)

for(count in min(Facebook$Category) : max(Facebook$Category)) {
  
  NumberOfRow = 0
  
  dset = Facebook[Facebook$Category != count, ]
  x = dset$Category
  y = dset$Lifetime.Post.Total.Reach
  
  NumberOfRow = length(x)
  MainModel = lm(y~x)
  SubMainModelB0Matris[1,count] = MainModel$coefficients[[1]]
  SubMainModelB1Matris[1,count] = MainModel$coefficients[[2]]
  
  for(z in 1:  N){
    
    DataCategory   = sample(x,NumberOfRow,replace = FALSE) 
    DataTotalReach = sample(y,NumberOfRow,replace = FALSE)
    
    fixmodel = lm(DataTotalReach ~ DataCategory )
    
    
    B0Matris[z,count] = fixmodel$coefficients[[1]]
    B1Matris[z,count] = fixmodel$coefficients[[2]]
    
    
    
  }
}







SubMainModelB1Matris [,3]

B0Matris[,1]
B0Matris[,count]
pnorm(a, mean = mean(g), sd = sqrt(g))
pnorm(SubMainModelB1Matris [,3], mean = mean(B1Matris[,3]), sd = sqrt(var(B1Matris[,3])))


## Data Classification
## Testing the Category
LoaderCategory = 0
for(count in min(Facebook$Category) : max(Facebook$Category)) {
  if((pnorm(SubMainModelB1Matris [,count], mean = mean(B1Matris[,count]), sd = sqrt(var(B1Matris[,count])))) < 0.05) 
  {
    
    cat("H0 Rejected. ",pnorm(SubMainModelB1Matris [,count], mean = mean(B1Matris[,count]), sd = sqrt(var(B1Matris[,count])))," Except Category No = ", count,", There is a acceptable relationship between Lifetime Total Reach and Category of People",'\n')
    
      
    }
  if((pnorm(SubMainModelB1Matris [,count], mean = mean(B1Matris[,count]), sd = sqrt(var(B1Matris[,count])))) > 0.05) 
      {print("H0 Accepted")
       LoaderCategory = LoaderCategory + 1
       cat("Category is not equal to " ,count,". Acceptable os Hyphothesis means we are accepting b1 = 0 when Category = 1. ",'\n')
       cat("There is no relationship between Lifetime.Total.Reach and Category when it is","count",'\n')
       MainDataSet = MainDataSet[MainDataSet$Category != count,]
       if(LoaderCategory == (max(Facebook$Category) - 1)){
          MainDataSet = Facebook
          }
      }
}


  MainDataSetRandomB0 = matrix(ncol = 1, nrow = N)
  MainDataSetRandomB1 = matrix(ncol = 1, nrow = N)
  
    
  x = MainDataSet$Category
  y = MainDataSet$Lifetime.Post.Total.Reach
  
  NumberOfRow = length(x)
  PerfectModel = lm(y~x)
  
  for(z in 1:  N){
    
    DataCategory   = sample(x,NumberOfRow,replace = FALSE) 
    DataTotalReach = sample(y,NumberOfRow,replace = FALSE)
    
    fixmodel = lm(DataTotalReach ~ DataCategory)
    MainDataSetRandomB0[z,1] = fixmodel$coefficients[[1]]
    MainDataSetRandomB1[z,1] = fixmodel$coefficients[[2]]
  }
  
  


####Is there any relationship between Lifetime Total Reah and Type of post via Facebook

TrainSet = MainDataSet
TrainSet$Type = as.numeric(TrainSet$Type)
MaxType =max(TrainSet$Type)


if(MaxType == 4)
{
    NumberofDataSet = (ncol(combn(4,2)) + ncol(combn(4,3)))
    
    MainDataSetRandomB0 = matrix(ncol = NumberofDataSet, nrow = N)
    MainDataSetRandomB1 = matrix(ncol = NumberofDataSet, nrow = N)
    SubMainModelB0Matris = matrix(ncol = NumberofDataSet, nrow = 1)
    SubMainModelB1Matris = matrix(ncol = NumberofDataSet, nrow = 1)
  
    for(count in 1: 4){
       NumberOfRow = 0
       dset = TrainSet
  
       if(count <= 4){dset = TrainSet[TrainSet$Type != count, ]} #### combn(4,3)
       if(count > 5) {                                           #### combn(4,2)
                
                     Combination = combn(4,2)
                     column      = count - 4
      
                     for(combin in 1: nrow(Combination)){
                         dset = dset[dset$Type != Combination[combin,column], ]
                        }
                     }
  
        x = dset$Type
        y = dset$Type
  
        NumberOfRow = length(x)
        MainModel = lm(y~x)
        SubMainModelB0Matris[1,count] = MainModel$coefficients[[1]]
        SubMainModelB1Matris[1,count] = MainModel$coefficients[[2]] 

        for(z in 1:  N){
    
                     DataType       = sample(x,NumberOfRow,replace = FALSE) 
                     DataTotalReach = sample(y,NumberOfRow,replace = FALSE)
    
                     fixmodel = lm(DataTotalReach ~ DataType)
                     MainDataSetRandomB0[z,count] = fixmodel$coefficients[[1]]
                     MainDataSetRandomB1[z,count] = fixmodel$coefficients[[2]]
                     
                     }
  
     }


}

if(MaxType == 3)
{
  NumberofDataSet = ncol(combn(4,3))
  
  MainDataSetRandomB0 = matrix(ncol = NumberofDataSet, nrow = N)
  MainDataSetRandomB1 = matrix(ncol = NumberofDataSet, nrow = N)
  SubMainModelB0Matris = matrix(ncol = NumberofDataSet, nrow = 1)
  SubMainModelB1Matris = matrix(ncol = NumberofDataSet, nrow = 1)
  
  
  for(count in 1 : NumberofDataSet) {
    
    NumberOfRow = 0
    
    dset = TrainSet[TrainSet$Type != count, ]
    x = dset$Type
    y = dset$Lifetime.Post.Total.Reach
    
    NumberOfRow = length(x)
    MainModel = lm(y~x)
    SubMainModelB0Matris[1,count] = MainModel$coefficients[[1]]
    SubMainModelB1Matris[1,count] = MainModel$coefficients[[2]]
    
    for(z in 1:  N){
      
      DataCategory   = sample(x,NumberOfRow,replace = FALSE) 
      DataTotalReach = sample(y,NumberOfRow,replace = FALSE)
      
      fixmodel = lm(DataTotalReach ~ DataCategory )
      
      
      MainDataSetRandomB0[z,count] = fixmodel$coefficients[[1]]
      MainDataSetRandomB1[z,count] = fixmodel$coefficients[[2]]
      }
  }
}


if(MaxType == 2){
  
  " Suffling Sample Yapýlýp karar verilme"
}



if(MaxType == 1)
{
  
  "We can not use Type Variable on Regression Model"

  
  
  
}  




dset  = TrainSet 
LoaderType = 0

for(count in 1:MaxType)
{
   if(pnorm(SubMainModelB1Matris [,count], mean = mean(MainDataSetRandomB1[,count]), sd = sqrt(var(MainDataSetRandomB1[,count]))) > 0.05)
     {
     LoaderType = LoaderType + 1
     if(LoaderType == MaxType-1)
         {
             print("there is no linear Regression between Total Reach and Type of post")
             dset = MainDataSet
             
         }
     else{dset = dset[dset$Type !=count,]
          print("H0 Accepted!!!Accepting of H0 Hyphothesis means we are accepting b1 = 0.")
          cat("There is no relationship between Lifetime.Total.Reach and Type when it is",count,"",'\n')
         }
     }
}


##################################################
######======NUMERIC VARIABLES=======##############
##################################################
dset
Bound     = round((nrow(dset)*3) / 4)
TrainData = dset[1:Bound,]
TestData  = dset[Bound:nrow(dset),]




#Number of Numeric Column
NumberOfNumericVariables = 0
for(count in 1: ncol(TrainData)){
  column = TrainData[,count] 
  if(is.numeric(column) == TRUE){
  NumberOfNumericVariables = NumberOfNumericVariables + 1
  }
}

a = 0
for(z in 1 : NumberOfNumericVariables){
  a[z] = ncol(combn(NumberOfNumericVariables,z))
}

NumberofDataSet = sum(a)


TotalVariables = numeric(0)
DataNoTotalReach = TrainData[,colnames(TrainData) != "Lifetime.Post.Total.Reach"] 
Numcol = ncol(DataNoTotalReach)
RSquare = numeric()
R2 = numeric()
VariableNo = vector()
for(deg in 1 : Numcol) #####2 toplam Numeric Variable olmalý
{
  Variable = 0
  a = 0
  a = ncol(combn(NumberOfNumericVariables,deg))
  
  for(z in 1 : a){
    TotalVariables = numeric()
    Variable = data.frame(y = TrainData$Lifetime.Post.Total.Reach)
    Combination = combn(NumberOfNumericVariables,deg)
    for(m in 1: deg){
      ColumnNo = Combination[m,z] 
      Variable = cbind(Variable, DataNoTotalReach[,ColumnNo])
      colnames(Variable)[m+1] = paste("X",as.factor(ColumnNo), sep = "-")
      TotalVariables = paste(TotalVariables,ColumnNo, sep = ".")
    }
    VariableNo = rbind(VariableNo,TotalVariables)
    #RSquare = rbind(RSquare[,2],colnames(Variable[,2]))
    MainModel   = lm(y~., data = Variable)
    R2 = round(rbind(summary(MainModel)$r.squared),digit = 4)
    RSquare = round(rbind(RSquare,R2), digit = 4)
    
    
  }
}  

#Figuring out Perfect Model:

VariableCalculator = 0
TopRSquare = unlist(strsplit(VariableNo[which(RSquare == max(RSquare))[1],],"[.]"))
TopRSquare
length(TopRSquare)
Number = numeric()
PerfectVariable = data.frame(y = TrainData$Lifetime.Post.Total.Reach)
for(q in 2:length(TopRSquare)){
Number = as.numeric(unlist(strsplit(VariableNo[which(RSquare == max(RSquare))[1],],"[.]"))[q]) 
Names = colnames(DataNoTotalReach[Number])
PerfectVariable = cbind(PerfectVariable, DataNoTotalReach[,Number])
colnames(PerfectVariable)[q] = Names
VariableCalculator = VariableCalculator + 1
}


summary(lm(y ~.,data =PerfectVariable ))



PerfectDataSet = 0
Names = 0
model = summary(lm(y ~.,data =PerfectVariable ))
PerfectDataSet = data.frame(y=TrainData$Lifetime.Post.Total.Reach)

for(deger in 2 :nrow(model$coefficients)){
  if(model$coefficients[deger,4] <  0.05) {
    Names = names(PerfectVariable)[deger]
    PerfectDataSet = cbind(PerfectDataSet,PerfectVariable[,deger])
    
    C = ncol(PerfectDataSet)
    colnames(PerfectDataSet)[C] = Names
    }
}

summary(lm(y~.,data=PerfectDataSet))

anova(lm(y~.,data=PerfectDataSet))

Test = factor()



for(I in 8 : ncol(combn(ncol(PerfectDataSet),2)) ){
  
  Corelation = cor.test(PerfectDataSet[,combn(ncol(PerfectDataSet),2)[1,I]],PerfectDataSet[,combn(ncol(PerfectDataSet),2)[2,I]])
  if(round(Corelation$p.value,digit = 5) < 0.05){
    asd = cat("There is a corelation between ",
             names(PerfectDataSet)[combn(ncol(PerfectDataSet),2)[1,I]]," and ",names(PerfectDataSet)[combn(ncol(PerfectDataSet),2)[2,I]],
              
             '\n')
    Test = rbind(asd,Test)
  }
}


##That`s it


help(summary.lm)
