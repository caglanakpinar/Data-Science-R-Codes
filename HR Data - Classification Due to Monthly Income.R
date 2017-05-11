##Classification for Monthly Income
setwd("C:/Users/Caglan.Akpinar/Desktop/R Files/Documents")
Employee = read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",sep=",",header=T)

####### WORKING SPACE #######

library(ggvis)
names(Employee)

Employee %>% ggvis(~ï..Age, ~MonthlyIncome, fill = ~Attrition) %>% layer_points()

Employee[IndpendpentVariables[,v]=="Yes",]$MonthlyIncome

colnames(IndpendentVariables)[[v]]
levels(IndpendentVariables[,3])[[1]]

 IndpendentVariables = Employee[,colnames(Employee) != "MonthlyIncome" 
                                & colnames(Employee) != "EmployeeNumber"]

names(Employee)
summary(lm(Employee$MonthlyIncome[Employee$Attrition == "Yes"]~Employee$ï..Age[Employee$Attrition == "Yes"]))

counter = 0
for (v in 1: ncol(IndpendentVariables))
{
  if(is.factor(IndpendentVariables[,v])){
     Variable  = IndpendentVariables[,v]

  
  
  
  if(length(levels(Variable)) <= 2){
     if(length(levels(Variable)) == 1){
         Employee = Employee[,colnames(Employee) != names(IndpendentVariables)[v]]
        }else{
    
                one = levels(IndpendentVariables[,v])[[1]]
                two = levels(IndpendentVariables[,v])[[2]]
                First = Employee[Variable== one,]$MonthlyIncome
                Second = Employee[Variable==two,]$MonthlyIncome
                SampleRow = nrow(Employee)
                row = NROW(First) + 1
                MeanDiff = mean(First) - mean(Second)
                SampleData = c(First,Second)
                N = 50000
                MeanDiffSample = numeric(N)
   
                set.seed(14)
                
                for(iter in 1 : N){
                    Data = sample(SampleData,SampleRow,replace = TRUE)
                    Sample1 = na.omit(Data[1:NROW(First)])
                    Sample2 = na.omit(Data[row : SampleRow])
                    a = mean(Sample1)
                    b = mean(Sample2)
                    MeanDiffSample[iter] =  a - b 
                
                   if(MeanDiffSample[iter] > MeanDiff){
                      counter = counter + 1
                      }
                   if(pnorm(MeanDiff, mean(MeanDiffSample), sd(MeanDiffSample)) > 0.05)
                      {Employee = Employee[,colnames(Employee) != names(IndpendentVariables)[v]]}
                   }
             }
      }
  
  
 if(length(levels(Variable)) > 2)
   {
   
   
   
   
   
   }
  
  }
  
}




##########
Variable
c=0
if(length(levels(Variable)) > 2){"ssffggdh"
  names(IndependentVariable[v])
}
