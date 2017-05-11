  
setwd("C:/Users/Caglan.Akpinar/Desktop/R Files/R Files")
AirQualityUCI = read.csv("AirQualityUCI.csv",sep=";",header=T)
options(max.print=999999)

#0 Date	(DD/MM/YYYY) 
#1 Time	(HH.MM.SS) 
#2 True hourly averaged concentration CO in mg/m^3 (reference analyzer) 
#3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)	
#4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer) 
#5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer) 
#6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)	
#7 True hourly averaged NOx concentration in ppb (reference analyzer) 
#8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted) 
#9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)	
#10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)	
#11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted) 
#12 Temperature in °C	
#13 Relative Humidity (%) 
#14 AH Absolute Humidity 

AirQualityUCI$Date

AirQualityUCI$Date[AirQualityUCI$Date != ""]
NROW(AirQualityUCI$Date[AirQualityUCI$Date != ""])
is.na(AirQualityUCI$Date)

library(lubridate)
Month = month(as.POSIXlt(AirQualityUCI$Date[AirQualityUCI$Date != ""], format="%d/%m/%Y"))
Day = day(as.POSIXlt(AirQualityUCI$Date[AirQualityUCI$Date != ""], format="%d/%m/%Y"))

DayandMonth = as.factor(paste(Month,"-",Day))



NewDataSet = data.frame(y = AirQualityUCI$PT08.S5.O3[AirQualityUCI$Date != ""], x = DayandMonth)



##sub dataset which has two values

X2 = by(NewDataSet$y, NewDataSet$x, sum)

  
divide = 3

K = seq(1:NROW(X2))



###################################################################
###################################################################
summary(lm(X2~K, data = NewDataSet))

a = K[1:182]
FIRST = X2[1:182]

b      = K[182:NROW(X2)]
SECOND = X2[3]


Regression = lm(FIRST~a, data = NewDataSet)
lm(SECOND~b, data = NewDataSet)
S = summary(lm(SECOND~b, data = NewDataSet))
summary(lm(SECOND~b, data = NewDataSet))$coefficients[2]
plot.ts(a,FIRST)
plot.ts(b,SECOND)
####################################################################
####################################################################

plot(K,X2)

Start = 185.5
MaxPattern = 0
MaxStart = 0
MaxDivide = 0
MaxTValue = 0
BestOne = numeric()
TopRandomSelection = numeric()
for(Start in 1:NROW(X2)/2){
    Topdivide = round((NROW(X2) - Start) / 2, digit = 0)
    for(divide in 2: Topdivide){
        NumSubDataset =  round(NROW(X2) / divide,digit = 0)
        b1Values = numeric()
        for(SubSet in 1 :round(NROW(X2) / divide,digit = 0)){
            FirstValue = (SubSet * divide) - (divide-1)
            lastValue  = SubSet*divide 
            x1       = K[FirstValue:lastValue]
            y1       = X2[FirstValue:lastValue]
            Model    = lm(y1~x1)
            b1Values = rbind(b1Values,summary(lm(y1~x1))$coefficients[2])

        }
    
    ## Varity from first Subset  
    for(Random in 1: round(round(NROW(X2) / divide,digit = 0) / 2, digit = 0)){
          print("Random :")
          print(Random)
          counter = 0
          detect = 0
          Top = 0 
          Pattern = numeric()
          Deger = numeric()
          RandomSelection = numeric ()
          Acception = numeric()
          for(Minus in 1: round(NROW(X2) / divide,digit = 0)){
            if(Minus*Random < round(NROW(X2) / divide,digit = 0)){
              RandomSelection = rbind(RandomSelection,b1Values[(Minus*Random),1])
              Acception = rbind(Acception,Minus*Random)
              Multiple = Random
            }
          }
          TValue = numeric()
          PaternB1 = numeric()
          Sys.sleep(1)
          plot.ts(RandomSelection)
          Sys.sleep(2)
          if(NROW(RandomSelection) > 2){
            
            for(z in 1 : NROW(RandomSelection) ){
              
              if(abs((RandomSelection[z,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection)))  
                 >  qnorm(0.01, mean = 0, sd = 1, lower.tail = FALSE)){
                counter   = counter + 1
                
              } 
              if(abs((RandomSelection[z,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection) ))  
                 <  qnorm(0.01, mean = 0, sd = 1, lower.tail = FALSE)){
                detect = detect + 1
                Pattern = rbind(Pattern,z)
                PaternB1 = rbind(PaternB1,RandomSelection[z,1])
                
              }
              TValue = rbind(TValue,abs((RandomSelection[z,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection) )))
            }
            
          }
          
          if(NROW(RandomSelection) == 2){
            print("-----------")
            print("Only Two B1 coefficient of B1 vector")
            if(RandomSelection[1,1] == RandomSelection[2,1]){
              print("   ------------")
              print("   !!! DETECTED PATTERN !!!")
              cat("   Divide = ",divide,'\n')
              cat("   Start  = ",Start,'\n')
              print(PaternB1)
              Top = 2
              if(MaxPattern < Top){
                MaxPattern = Top           
                MaxStart   = Start
                MaxDivide  = divide
                BestOne = Acception
                TopRandomSelection = RandomSelection
              }
            } 
            if(RandomSelection[1,1] != RandomSelection[2,1]){
              print("------------")
              print("There is no pattern")
            }
            
          }
          
          
          
          
          #     for(Minus in 1: round(NROW(X2) / divide,digit = 0)){
          #         if(NROW(RandomSelection) > 1){
          #         if(Minus*Random < round(NROW(X2) / divide,digit = 0)){
          #            #RandomSelection = rbind(RandomSelection,b1Values[(Minus*Random),1])
          #            Deger = rbind(Deger,Minus*Random)
          #            if(abs((b1Values[Minus*Random,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection)))  
          #               >  qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE)){
          #               counter = counter + 1
          #               Acception = rbind(Acception,Minus*Random)
          #            } 
          #            if(abs((b1Values[Minus*Random,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection) ))  
          #               <  qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE)){
          #               detect = detect + 1
          #               Pattern = rbind(Pattern,Minus)
          #            }
          #         }
          #         }
          #     }
          
          
          if(NROW(RandomSelection) > 2){
            Top = NROW(PaternB1)  
            if((counter / NROW(RandomSelection) > 0.01)){
              print("------------")
              print("There is no pattern")
            } 
            if((counter / NROW(RandomSelection) < 0.01)){
              print("------------")
              print("!!! DETECTED PATTERN !!!")
              cat("Divide = ",divide,'\n')
              cat("Start  = ",Start,'\n')
              print("PaternB1 : ")
              print(PaternB1)
              cat("TValue = ",mean(TValue),'\n')
              cat("MaxTValue = ",MaxTValue,'\n')
              print("Top Random Selection :")
              print(TopRandomSelection)
              if(MaxPattern < Top){
                print("PATERN HAS BEEN CHANGED")
                MaxPattern = Top
                MaxStart   = Start
                MaxDivide  = divide
                BestOne = Acception
                TopRandomSelection = RandomSelection
                MaxTValue = mean(TValue)
                Sys.sleep(5)
                
              }
              
              if(MaxPattern == Top){
                if(MaxTValue > mean(TValue)){
                  print("PATERN HAS BEEN CHANGED")
                  MaxTValue = mean(TValue)
                  MaxPattern = Top
                  MaxStart   = Start
                  MaxDivide  = divide
                  BestOne = Acception
                  TopRandomSelection = RandomSelection
                  Sys.sleep(5)
                }
              }
              
              
            }
            Sys.sleep(1)
          }
        }
        
        
   }
}

plot.ts(b1Values)
Sys.sleep(3)
#StandartB1 = numeric()
#for(l in 1 : nrow(b1Values)){
#    StandartB1 = rbind(StandartB1,(b1Values[l,1] - mean(b1Values))  / sqrt(var(b1Values)/ (round(NROW(X2) / divide,digit = 0))))
#}
#plot.ts(StandartB1)
Sys.sleep(3)
for(Random in 1: round(round(NROW(X2) / divide,digit = 0) / 2, digit = 0)){
     print("Random :")
     print(Random)
     counter = 0
     detect = 0
     Top = 0 
     Pattern = numeric()
     Deger = numeric()
     RandomSelection = numeric ()
     Acception = numeric()
     for(Minus in 1: round(NROW(X2) / divide,digit = 0)){
         if(Minus*Random < round(NROW(X2) / divide,digit = 0)){
            RandomSelection = rbind(RandomSelection,b1Values[(Minus*Random),1])
            Acception = rbind(Acception,Minus*Random)
            Multiple = Random
         }
     }
     TValue = numeric()
     PaternB1 = numeric()
     Sys.sleep(1)
     plot.ts(RandomSelection)
     Sys.sleep(2)
     if(NROW(RandomSelection) > 2){
        
        for(z in 1 : NROW(RandomSelection) ){
          
          if(abs((RandomSelection[z,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection)))  
             >  qnorm(0.01, mean = 0, sd = 1, lower.tail = FALSE)){
            counter   = counter + 1
            
          } 
          if(abs((RandomSelection[z,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection) ))  
             <  qnorm(0.01, mean = 0, sd = 1, lower.tail = FALSE)){
             detect = detect + 1
             Pattern = rbind(Pattern,z)
             PaternB1 = rbind(PaternB1,RandomSelection[z,1])
             
          }
        TValue = rbind(TValue,abs((RandomSelection[z,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection) )))
        }
       
     }
     
     if(NROW(RandomSelection) == 2){
         print("-----------")
         print("Only Two B1 coefficient of B1 vector")
         if(RandomSelection[1,1] == RandomSelection[2,1]){
           print("   ------------")
           print("   !!! DETECTED PATTERN !!!")
           cat("   Divide = ",divide,'\n')
           cat("   Start  = ",Start,'\n')
           print(PaternB1)
           Top = 2
           if(MaxPattern < Top){
             MaxPattern = Top           
             MaxStart   = Start
             MaxDivide  = divide
             BestOne = Acception
             TopRandomSelection = RandomSelection
             }
           } 
         if(RandomSelection[1,1] != RandomSelection[2,1]){
            print("------------")
            print("There is no pattern")
         }
       
     }
     
     
     
     
     #     for(Minus in 1: round(NROW(X2) / divide,digit = 0)){
     #         if(NROW(RandomSelection) > 1){
     #         if(Minus*Random < round(NROW(X2) / divide,digit = 0)){
     #            #RandomSelection = rbind(RandomSelection,b1Values[(Minus*Random),1])
     #            Deger = rbind(Deger,Minus*Random)
     #            if(abs((b1Values[Minus*Random,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection)))  
     #               >  qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE)){
     #               counter = counter + 1
     #               Acception = rbind(Acception,Minus*Random)
     #            } 
     #            if(abs((b1Values[Minus*Random,1] - mean(RandomSelection))  / sqrt(var(RandomSelection)/ NROW(RandomSelection) ))  
     #               <  qnorm(0.05, mean = 0, sd = 1, lower.tail = FALSE)){
     #               detect = detect + 1
     #               Pattern = rbind(Pattern,Minus)
     #            }
     #         }
     #         }
     #     }
     
     
     if(NROW(RandomSelection) > 2){
        Top = NROW(PaternB1)  
        if((counter / NROW(RandomSelection) > 0.01)){
            print("------------")
            print("There is no pattern")
        } 
        if((counter / NROW(RandomSelection) < 0.01)){
            print("------------")
            print("!!! DETECTED PATTERN !!!")
            cat("Divide = ",divide,'\n')
            cat("Start  = ",Start,'\n')
            print("PaternB1 : ")
            print(PaternB1)
            cat("TValue = ",mean(TValue),'\n')
            cat("MaxTValue = ",MaxTValue,'\n')
            print("Top Random Selection :")
            print(TopRandomSelection)
            if(MaxPattern < Top){
               print("PATERN HAS BEEN CHANGED")
               MaxPattern = Top
               MaxStart   = Start
               MaxDivide  = divide
               BestOne = Acception
               TopRandomSelection = RandomSelection
               MaxTValue = mean(TValue)
               Sys.sleep(5)
            
            }
            
            if(MaxPattern == Top){
               if(MaxTValue > mean(TValue)){
                  print("PATERN HAS BEEN CHANGED")
                  MaxTValue = mean(TValue)
                  MaxPattern = Top
                  MaxStart   = Start
                  MaxDivide  = divide
                  BestOne = Acception
                  TopRandomSelection = RandomSelection
                  Sys.sleep(5)
               }
            }
            
                
       }
       Sys.sleep(1)
     }
}









MaxPattern
BestOne
plot.ts(TopRandomSelection)
X2
Sys.sleep(3)

ROW = 0
similarPatterns = numeric()
RowNo = numeric()
PatternDM = factor()

for(k in 1:MaxPattern){
   for(l in 0 : (divide-1)){
        ROW = (BestOne[k,1] ) +l
        RowNo = rbind(RowNo,ROW)
        #PatternDM = rbind(PatternDM,PatternDataSet[RowNo,]$x)
        similarPatterns = rbind(similarPatterns,X2[ROW]) 
   }
}



DayAndMonth = unique(NewDataSet$x)

as.vector(PatternDM)

for(k in 1:MaxPattern){
  for(l in 0 : (divide-1)){
    date = factor()
    ROW = (BestOne[k,1] ) +l
    RowNo = rbind(RowNo,ROW)
    date = names(X2[ROW])
    PatternDM = rbind(PatternDM,date)
    similarPatterns = rbind(similarPatterns,X2[ROW]) 
  }
}


PatternDataSet = data.frame( as.vector(similarPatterns),  as.vector(PatternDM))

similarPatterns
plot.ts(similarPatterns)

summary(lm(similarPatterns~ RowNo))
abline(lm(similarPatterns~ RowNo))
Sys.sleep(3)
print("----------------")




abline(lm(similarPatterns~ RowNo))



#Prediction of Future dataset og Employee

NROW(similarPatterns)
divide
Multiple
NROW(AirQualityUCI)

#Value = numeric()
#for(I in 1 : (NROW(similarPatterns) / divide)){
#   for(K in 1 : divide){
#       Value = summary(lm(similarPatterns~ RowNo))$coefficents[2] * 
#   }
#}

Numdataset = 5

FutureRowNo = numeric()
ROW = 0
for(m in 1 : Numdataset){
    for(k in 1:MaxPattern){
        for(l in 0 : (divide-1)){
            ROW = ((BestOne[k,1] ) +l) + (NROW(similarPatterns) * m)
            FutureRowNo = rbind(RowNo,ROW)
            Value = summary(lm(similarPatterns~ RowNo))$coefficents[2] * FutureRowNo
    }
  }
}






######################################
######################################
######################################

MaxPattern = 0
MaxStart = 0
MaxDivide = 0
BestOne = numeric()
TopRandomSelection = numeric()

