#VYTVORI BUBLINOVY GRAF VOLIVU JEDNOLTIVYCH KANDIDATU A JEJICH VYAHEMNE BLIZKOSTI (ZALOZENE NA SYMPATIICH)
#JSOU ROYDELENI JADROVY VOLICI A NAKLONENI VOLICI
#POMOCI KLASTROVKY JSOU ROZDELENI NEVOLICI A TAKE ZARAZENI DO GRAFU POMOCI MSD
  #MEZI SEBOU MAJI NULOVE VZTAHY - TO BY CHTELO UPRAVIT
#URCENO PRO DATA CVVM Z ROKU 2012 MESIC 12

library(foreign)

#zakldni nacteni
cvvm = read.spss("./V1212/V1212_F1.sav", reencode = 'Windows-1250', to.data.frame = TRUE, use.missings = FALSE)                                          

#vylouceni lidi bez volebniho prava
cvvm = cvvm[cvvm$PV_148 != "NEBUDE MÍT VOLEBNÍ PRÁVO", ]#voteless

#kategorizace volicu
cvvm[(cvvm$PV_148 == "rozhodně ano" | cvvm$PV_148 == "spíše ano") & cvvm$PV_150 == "svou volbu už určitě nezmění" & !is.na(cvvm$PV_150), "voter_type"] = 1 #core voters
cvvm[is.na(cvvm$voter_type) & !is.na(cvvm$PV_149), "voter_type"] = 2 #tilt_voters
cvvm[((cvvm$PV_149 == "NEVÍ" | cvvm$PV_149 == "BEZ ODPOVĚDI") & !is.na(cvvm$PV_149) ) , "voter_type"] = 3 #undicided_voters
cvvm[cvvm$PV_148 == "rozhodně ne" | cvvm$PV_149 == "ŽÁDNÉHO" , "voter_type"] = 4#non_voters

#vztvoreni data jen rozhodnutych volicu
dicided_voters = cvvm[cvvm$voter_type < 3 ,]

#ziskani jmen kandidatu
candidates = levels(cvvm$PV_149)
candidates = candidates[-c(1,13,14)]


#cyklus na vztvoreni sympatii meyi jednotlivimi kandidatz
count = length(candidates)

sympaties = matrix(c(rep(0, count*count )), nrow = count, ncol = count)
for(i in c(1:count)){
  for(j in c(1:count)){ #cyklus podle radku sympatii k jednotlivym kandidatum
    variable = colnames(cvvm)[34+j] #ziskani nazvu aktualne zpracovavane promenne
    sympaties[i,j] = (mean(dicided_voters[dicided_voters$PV_149 == candidates[i] &
                                          dicided_voters[,variable] != 99 & dicided_voters[,variable] != "98", #vylouceni lidi bez odpovedi 
                                          variable], na.rm = TRUE))
  }
}
rownames(sympaties) = candidates
colnames(sympaties) = candidates


# ###STOP-------PRESKOC NA VYPOCET CLUSTERU!!!
# #transformace sympatií pro MSD - symetrizace a vytvoreni vzdalenosti
# s = 0.5 * (sympaties + t(sympaties)) 
# s = 1/s * 10
# diag(s) = 0
# 
# fit = cmdscale(s, eig = TRUE, k = 2)
# 
# MSD = data.frame(fit$points[, 1])
# MSD = cbind (MSD, fit$points[, 2])
# 
# all_voters = table(dicided_voters$PV_149)[-c(1,13,14)]
# all_voters = (all_voters/nrow(cvvm[cvvm$voter_type < 4 ,]))*100
# MSD = cbind (MSD, as.vector(all_voters))
# 
# 
# core_voters = table(dicided_voters[dicided_voters$voter_type == 1,"PV_149"])[-c(1,13,14)]
# core_voters = (core_voters/nrow(cvvm[cvvm$voter_type < 4 ,]))*100
# MSD = cbind (MSD, as.vector(core_voters))
# names(MSD) = c("x", "y", "all_voters", "core_voters")
# 
# MSD = cbind (MSD, paste0(rownames(MSD),"\n" ,round(MSD$all_voters,1) ," %, ", round(MSD$core_voters, 1)," %"))
# names(MSD) = c("x", "y", "all_voters", "core_voters", "labels")
# 
# 
# #vytvoreni grafu
# library(ggplot2)
# 
# ggplot(MSD, aes(x, y) ) +
#   geom_point(size = all_voters, alpha = 0.3, colour = "blue") +
#   geom_point(size = core_voters, colour = "blue") +
#   geom_text(aes(label=labels, lineheight = 0.8), size=3, vjust = -1) +
#   xlim(-2.5, 2.5) +
#   ylim(-1.5,3.5)








#----------------------------------------------------
#pridani nerozhodnutych volicu
# K-Means Cluster Analysis
undicided_voters = cvvm[cvvm$voter_type == 3, c(35:45)]

variables = names(undicided_voters)
for(i in 1:length(variables) ){
  mean = mean(undicided_voters[undicided_voters[,i] != 99 ,i])
  undicided_voters[undicided_voters[,i] == 99 ,i] = mean 
}

cluster_count = 2
cluster_fit <- kmeans(undicided_voters, cluster_count)
# get cluster means
clusters = aggregate(undicided_voters,by=list(cluster_fit$cluster),FUN=mean)[2:12]
names(clusters) = candidates


sympaties = rbind(sympaties, clusters)

#vytvoreni matice nul, ktera se pripoji na konec (skupiny se navzajem nehodnotily)
empty = matrix(c(rep(100000, cluster_count*cluster_count )), nrow = cluster_count, ncol = cluster_count)
clusters = cbind(clusters,empty)
sympaties = cbind(sympaties, t(clusters))




s = 0.5 * (sympaties + t(sympaties)) 
s = 1/s * 10
diag(s) = 0

fit = cmdscale(s, eig = TRUE, k = 2)

MSD = data.frame(fit$points[, 1])
MSD = cbind (MSD, fit$points[, 2])

all_voters = table(dicided_voters$PV_149)[-c(1,13,14)]
all_voters = c(as.vector(all_voters), rep(0, cluster_count))
all_voters = (all_voters/nrow(cvvm[cvvm$voter_type < 4 ,]))*100
MSD = cbind (MSD, as.vector(all_voters))

core_voters = table(dicided_voters[dicided_voters$voter_type == 1,"PV_149"])[-c(1,13,14)]
core_voters = (core_voters/nrow(cvvm[cvvm$voter_type < 4 ,]))*100
core_voters = c(as.vector(core_voters),rep(0, cluster_count))
MSD = cbind (MSD, core_voters)

un_voters = c(rep(0, 11), cluster_fit$size)
un_voters = (un_voters/nrow(cvvm[cvvm$voter_type < 4 ,]))*100
MSD = cbind (MSD, un_voters)

names(MSD) = c("x", "y", "all_voters", "core_voters", "un_voters")

MSD = cbind (MSD, paste0(rownames(MSD),"\n" ,round(MSD$all_voters,1) ," %, ", round(MSD$core_voters, 1)," %"))
names(MSD) = c("x", "y", "all_voters", "core_voters", "un_voters", "labels")


#vytvoreni grafu
library(ggplot2)

ggplot(MSD, aes(x, y) ) +
  geom_point(size = un_voters, colour = "darkgreen", alpha = 0.5) +
  geom_point(size = all_voters, alpha = 0.3, colour = "blue") +
  geom_point(size = core_voters, colour = "blue") +
  geom_text(aes(label=labels, lineheight = 0.8), size=3, vjust = -1)

# +
#   ylim(-2.5,2.5)




