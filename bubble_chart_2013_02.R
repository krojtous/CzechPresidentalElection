#Bubble CHart 2013 02

library(foreign)

#LOAD DATA - from http://nesstar.soc.cas.cz
cvvm = read.spss("./V1302/V1302_F1.sav",
                 reencode = 'Windows-1250', 
                 to.data.frame = TRUE, 
                 use.missings = FALSE)                                          

#USED QUESTIONS
#
# PV_153 - Byl volit v 1. kole prezidentských voleb              
#   PV_149a - ktereho kandidata volil v 1. kole voleb
# VO_3 - Uvažoval v 1. kole prez. voleb o volbě jiného kandidáta 
#   VO_4a-k - O kterém kandidátovi uvažoval                      
# PV_153a - Byl volit ve 2. kole prezidentských voleb            
#   PV_149b Koho volil ve 2. kole prezidentských voleb

str(cvvm$PV_149a)

#people who vote in first or second round of president election and knows who they vote for
voters = subset(cvvm, (PV_153 == "ano" | PV_153a == "ano") & 
                PV_153 != "BEZ ODPOVĚDI"  & PV_153 != "NEMĚL VOLEBNÍ PRÁVO" &
                PV_153a != "BEZ ODPOVĚDI" & PV_153a != "NEMĚL VOLEBNÍ PRÁVO" &
                PV_149a != "BEZ ODPOVĚDI" & PV_149a != "NEVÍ"  & PV_149a != "VHODIL NEPLATNÝ HLAS" &
                PV_149b != "BEZ ODPOVĚDI" & PV_149b != "NEVÍ"  & PV_149b != "VHODIL NEPLATNÝ HLAS")

voters_consider = voters[,36:44]
aggregate(cbind(cvvm[,names(cvvm)[36:44]]) ~ PV_149a,data=voters,table, exclude = "NE")

aggregate(voters$, by = list(levels(voters$)), table)
table(voters$PV_153, voters$PV_153a)
table(voters$PV_149a)
