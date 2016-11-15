library(tidyverse)
library(apaTables)

#read data
my.data <- read_csv(file = "reg_quiz2_data.csv")

#create correlation table 
apa.cor.table(my.data, filename="Table1.doc", table.number=1)

#Question 4A. SE -> aSuc above and beyond PAS 
reg.SE.PAS <- lm(aSuc ~ PAS + selfEsteem, data=my.data) 
apa.reg.table(reg.SE.PAS, filename="Table2.doc", table.number=2)
##0.22 [.12,.33] sig 

#Question 4B. SE -> aSuc above and beyond NAS 
reg.SE.NAS <- lm(aSuc ~ NAS + selfEsteem, data=my.data)
apa.reg.table(reg.SE.NAS, filename="Table3.doc", table.number=3)
##.23[.12,.33] sig 

#Question 4C. Block 
reg1 <- lm(aSuc ~ NAS + PAS, data=my.data) 
reg2 <- lm(aSuc ~ NAS + PAS + selfEsteem, data=my.data)
apa.reg.table(reg1,reg2, filename="Table4.doc", table.number=4)
##.21[.11,.31] sig 

#to determine how much PAS and NAS contribute to aSuc 
reg.NAS <- lm(aSuc ~ NAS, data=my.data)
apa.reg.table(reg.NAS)
reg.PAS <- lm(aSuc ~ PAS, data=my.data)
apa.reg.table(reg.PAS)
