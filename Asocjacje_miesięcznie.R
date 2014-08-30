
library (data.table)
library(arules)
library(arulesViz)
library(reshape)
library(ggplot2)


# dane wejsciowe :
# pozycje.dt 
# pozycje.produkt.dt -> proba transakcji z  produktami na okreslonym poziomie szczegolowosci
#freq.dt   czestosc wystepowania produktow 


#selekcja danych z miesiaca 



load("PRODUKTY.dat")
setkey(produkty.dt, MATERIAL_ID)
produkty.dt<-produkty.dt[!duplicated(produkty.dt)]


load("pozycje_201307.dat")
pozycje<-pozycje_201307
pozycje.dt<-data.table(pozycje)
rm(pozycje_201307)


### wejscie do skryptu 
## 
## data table zawierajace dwa pola -> ID, PRODUKT  (zdeduplikowane)




#wyciagniecie listy identyfikatorow transakcji oraz pobranie probki 

trans_id<-pozycje$TRANSAKCJA_UNIKALNY_NR
#deduplikacja numerow transakcji
trans_id_dedup<-trans_id[!duplicated(trans_id)]  
#wylosowanie proby transkacji
trans_id_sample<-sample(trans_id_dedup, size=1000)

#nadanie nowych numerow 
trans_id.dt<-data.table(trans_id_sample, seq(1, length(trans_id_sample)))
setnames(trans_id.dt, "trans_id_sample", "TRANSAKCJA_UNIKALNY_NR")
setnames(trans_id.dt, "V2", "ID")


#selekcja pozycji transakcji 
setkey(trans_id.dt, TRANSAKCJA_UNIKALNY_NR)
setkey(pozycje.dt, TRANSAKCJA_UNIKALNY_NR)
pozycje.sample.dt<-pozycje.dt[trans_id.dt]








#okreslenie  szczegolowosci poziomu analizy 
setkey(pozycje.sample.dt , MATERIAL_ID)
## zmiana poziomu szczegolowpsci 
pozycje.produkt.dt<-produkty.dt[pozycje.sample.dt][,list(ID, as.character(PRODUKT))]

setnames(pozycje.produkt.dt, "V2", "PRODUKT")

pozycje.produkt.dt<-pozycje.produkt.dt[!duplicated(pozycje.produkt.dt)]

save(pozycje.produkt.dt, file="pozycje.produkt.dt.dat")



# czestosc wystepowanie produktow ------------------------------------------


pozycje.produkt.dt<-data.table(pozycje.produkt.dt)
freq<-pozycje.produkt.dt[,list(.N), by=PRODUKT]
freq<-freq[order(-N)]
n<-(nrow(pozycje.produkt.dt[!duplicated(pozycje.produkt.dt[,ID])]))

freq<-data.frame(freq)
freq$pct<-(freq$N)/n

freq.top30<-freq[1:30,]

p<-ggplot(freq.top30, aes(x=reorder(PRODUKT,-N), y=N)) +
   geom_bar(stat='identity',color="red", fill="white") + theme(axis.text.x=element_text(angle=90)) + 
   geom_text(aes(size=5, angle=45, label=paste(as.character(freq.top30$pct*100),"%",sep=""),show_guide=FALSE))+
   xlab("PRODUKT") + ylab("Liczba transakcji")
   
p



# struktura transakcji  ---------------------------------------------------



trans.set<-as(split(pozycje.produkt.dt[,PRODUKT], pozycje.produkt.dt[,ID]), "transactions")



# affinity - bliskosc -----------------------------------------------------

aff<-affinity(trans.set)
aff.matrix<-aff@.Data

# PRODUCTS
prod<-freq[1:50,"PRODUKT"]

aff.matrix.subset<-aff.matrix[prod, prod]

affinity.values<-melt(aff.matrix.subset)
names(affinity.values)[1]<-paste("PRODUKT")
names(affinity.values)[2]<-paste("PRODUKT1")
names(affinity.values)[3]<-paste("affinity")

affinity.values<-affinity.values[affinity.values$"affinity">0.1,]
affinity.values<-affinity.values[affinity.values$"affinity"< 0.95,]



affinity.values<-affinity.values[order(affinity.values$affinity,decreasing="TRUE"),]
affinity.values[,"PRODUKT"]<-as.character(affinity.values$PRODUKT)
affinity.values[,"PRODUKT1"]<-as.character(affinity.values$PRODUKT1)



affinity.values$PRODUKT3 <-(reorder(affinity.values$PRODUKT, -affinity.values$affinity))


p<-ggplot(affinity.values, aes(PRODUKT3, PRODUKT1)) +
  geom_tile(aes(fill=affinity)) + scale_fill_gradient(low = "yellow",    high = "red") 

p<-p+ theme(axis.text.x=element_text(angle=90))
p<-p+geom_text(aes(size=5, label=paste(round(affinity.values$affinity*100,2), "%", sep = "")), show_guide=FALSE)

p


# Analiza asocjacji  ------------------------------------------------------


rules<-apriori(trans.set, parameter=list(support=0.001, confidence=0.5, minlen=2, maxlen=2))

#usuniecie"sztywnych" pakietow produktowych
rules<-subset(rules, confidence<1)


#brak podpisow dla regula na osiach 
plot(rules, method="scatterplot", measure=c("confidence", "support"), 
     control=list( col=rainbow(16, start=0, end=0.15)), shading="lift",  interactive=FALSE)



plot(sort(rules, by="support")[1:30], method="grouped", measure=c("confidence"), 
     control=list(col=rainbow(16, start=0, end=0.15, alpha=1),main="Analiza koszyka"),  
     shading="support",  interactive=FALSE, font=4)





plot(sort(rules, by="support")[1:15], method="graph", measure=c("confidence"), 
     control=list(main="Analiza koszyka"),  
     shading="support",  interactive=FALSE)




#na poziomie PRODUKTU  L6
pozycje.trans6<-produkty.dt[pozycje.sample.dt][,list(ID, as.character(PRODUKT6))]
setnames(pozycje.trans6, "V2", "PRODUKT6")
pozycje.trans6<-pozycje.trans6[!duplicated(pozycje.trans6)]


#analiza asocjacji 

trans6<-as(split(pozycje.trans6[,PRODUKT6], pozycje.trans6[,ID]), "transactions")
rules6<-apriori(trans6, parameter=list(support=0.00001, confidence=0.5))
inspect(sort(rules6, by="confidence"))



#na poziomie PRODUKTU  L5
pozycje.trans5<-produkty.dt[pozycje.sample.dt][,list(ID, as.character(PRODUKT5))]
setnames(pozycje.trans5, "V2", "PRODUKT5")
pozycje.trans5<-pozycje.trans5[!duplicated(pozycje.trans5)]


#analiza asocjacji 

trans5<-as(split(pozycje.trans5[,PRODUKT5], pozycje.trans5[,ID]), "transactions")
rules5<-apriori(trans5, parameter=list(support=0.0001, confidence=0.2))
inspect(sort(rules5, by="confidence"))


