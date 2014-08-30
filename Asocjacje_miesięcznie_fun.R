
library (data.table)
library(arules)
library(arulesViz)
library(reshape)
library(ggplot2)
library(igraph)



source("functions.R")


# dane wejsciowe :
# pozycje.dt 
# pozycje.produkt.dt -> proba transakcji z  produktami na okreslonym poziomie szczegolowosci
#freq.dt   czestosc wystepowania produktow 


#selekcja danych z miesiaca 



load("PRODUKTY.dat")
setkey(produkty.dt, MATERIAL_ID)
produkty.dt<-produkty.dt[!duplicated(produkty.dt)]



# test --------------------------------------------------------------------


load("pozycje_201307.dat") #pozycje_201307

#probka danych 

trans_sample<-assocs_sample(pozycje_201307, 30000)
pp<-assocs_set_level(trans_sample, produkty.dt, 'PRODUKT6')
frequencies.df<-assoc_frequency(pp)
trans<-as(split(pp[,PRODUKT], pp[,ID]), "transactions")
assoc_affinity(trans, as.character(frequencies.df[1:10,'PRODUKT']) )



rules<-assoc_rules(trans)

rules.df<-assoc_rules_df(rules)






assoc_visualise(rules)
assoc_heatmap(rules.df)
assoc_graph(rules.df)

inspect(rules)
rules.df









