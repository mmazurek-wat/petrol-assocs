
# funkcja analizy asocjacji  ----------------------------------------------


library (data.table)
library(arules)
library(arulesViz)
library(reshape)
library(ggplot2)

library(RODBC)
library (data.table)
library(arules)


read_data<-function(){
  #dwh<-odbcConnect("SADT", uid="mazurekma")
  dwh<-odbcConnect("SADT", uid="sadt_microstrategy", pwd="Micro$trategy")
  
  pozycje_transakcji.dt<- data.table(sqlQuery(dwh, "  select distinct * from REP_ASOCJACJE_TRANSKACJE  "))
 
  colnames<-colnames(pozycje_transakcji.dt)
  pozycje.dt<-pozycje_transakcji.dt[, list(get(colnames[2]), TRANSAKCJA_UNIKALNY_NR)]                                   
  
  
  #wyciagniecie listy identyfikatorow transakcji oraz pobranie probki 
  trans_id<-pozycje.dt$TRANSAKCJA_UNIKALNY_NR
  #deduplikacja numerow transakcji
  trans_id_dedup<-trans_id[!duplicated(trans_id)] 

  #wylosowanie proby transkacji
  n=min(10000, length(trans_id_dedup))
  trans_id_sample<-sample(trans_id_dedup, size=n)
  
  #nadanie nowych numerow 
  trans_id.dt<-data.table(trans_id_sample, seq(1, length(trans_id_sample)))
  setnames(trans_id.dt, "trans_id_sample", "TRANSAKCJA_UNIKALNY_NR")
  setnames(trans_id.dt, "V2", "ID")
  
  #selekcja pozycji transakcji 
  setkey(trans_id.dt, TRANSAKCJA_UNIKALNY_NR)
  setkey(pozycje.dt, TRANSAKCJA_UNIKALNY_NR)
  pozycje.sample.dt<-pozycje.dt[trans_id.dt]

  setnames(pozycje.sample.dt, "V1", "PRODUKT")
  pozycje.sample.dt<-pozycje.sample.dt[,list(ID, PRODUKT)]
  
  pozycje.sample.dt
  
                                     
}


assocs_sample<-function (pozycje.df, sample_size){ 
  pozycje.dt<-data.table(pozycje.df)
  #wyciagniecie listy identyfikatorow transakcji oraz pobranie probki 
  trans_id<-pozycje.dt$TRANSAKCJA_UNIKALNY_NR
  #deduplikacja numerow transakcji
  trans_id_dedup<-trans_id[!duplicated(trans_id)]  
  #wylosowanie proby transkacji
  trans_id_sample<-sample(trans_id_dedup, size=sample_size)
  
  #nadanie nowych numerow 
  trans_id.dt<-data.table(trans_id_sample, seq(1, length(trans_id_sample)))
  setnames(trans_id.dt, "trans_id_sample", "TRANSAKCJA_UNIKALNY_NR")
  setnames(trans_id.dt, "V2", "ID")
  
  #selekcja pozycji transakcji 
  setkey(trans_id.dt, TRANSAKCJA_UNIKALNY_NR)
  setkey(pozycje.dt, TRANSAKCJA_UNIKALNY_NR)
  pozycje.sample.dt<-pozycje.dt[trans_id.dt]
  pozycje.sample.dt[,list(ID, MATERIAL_ID)]
} 




assocs_sample1<-function (pozycje.df, sample_size){ 
  pozycje.dt<-data.table(pozycje.df)
  #wyciagniecie listy identyfikatorow transakcji oraz pobranie probki 
  trans_id<-pozycje.dt$TRANSAKCJA_UNIKALNY_NR
  #deduplikacja numerow transakcji
  trans_id_dedup<-trans_id[!duplicated(trans_id)]  
  #wylosowanie proby transkacji
  trans_id_sample<-sample(trans_id_dedup, size=sample_size)
  
  #nadanie nowych numerow 
  trans_id.dt<-data.table(trans_id_sample, seq(1, length(trans_id_sample)))
  setnames(trans_id.dt, "trans_id_sample", "TRANSAKCJA_UNIKALNY_NR")
  setnames(trans_id.dt, "V2", "ID")
  
  #selekcja pozycji transakcji 
  setkey(trans_id.dt, TRANSAKCJA_UNIKALNY_NR)
  setkey(pozycje.dt, TRANSAKCJA_UNIKALNY_NR)
  pozycje.sample.dt<-pozycje.dt[trans_id.dt]
  pozycje.sample.dt[,list(ID, MATERIAL_ID)]
} 

assocs_set_level<-function(pozycje.dt,produkty.dt, level, filter_col, filter_val){
  

  exist <-  hasArg(filter_col)
  
  
  setkey(pozycje.dt , MATERIAL_ID)
  if (exist){
      #funkcja get: umozliwia wybor kolumny na podstawie wartosci zmiennej 
      pozycje.produkt.dt<-produkty.dt[pozycje.dt][,list(ID, eval(get(level)), get(filter_col))]    
      pozycje.produkt.dt<-subset(pozycje.produkt.dt,V3==filter_val )
      pozycje.produkt.dt<-pozycje.produkt.dt[,list(ID, V2)]
    } else {      
      pozycje.produkt.dt<-produkty.dt[pozycje.dt][,list(ID, get(level))]                
  }
  setnames(pozycje.produkt.dt, "V2", "PRODUKT")  
  pozycje.produkt.dt<-pozycje.produkt.dt[!duplicated(pozycje.produkt.dt)]
  pozycje.produkt.dt  
  
  
}


assoc_frequency<-function(pozycje.produkt.dt){
  
  freq<-pozycje.produkt.dt[,list(.N), by=PRODUKT]
  freq<-freq[order(-N)]
  #liczba transakcji 
  n<-(nrow(pozycje.produkt.dt[!duplicated(pozycje.produkt.dt[,ID])]))
  
  freq<-data.frame(freq)
  freq$pct<-(freq$N)/n
  nx<-min(30,nrow(freq))
  freq.top30<-freq[1:nx,]
  
  p<-ggplot(freq.top30, aes(x=reorder(PRODUKT,-N), y=N)) +
    geom_bar(stat='identity',color="red", fill="red") + theme(axis.text.x=element_text(angle=90))     
  p<-p+ geom_text(aes(size=5, angle=45, 
                      label=paste(as.character(round((pct)*100),3),"%",sep=""),
                      show_guide=FALSE))   
  p<-p+ xlab("PRODUKT") + ylab("Liczba transakcji")  
  p<-p+theme(plot.title = element_text(lineheight=.8, face="bold")) + labs(title="Czêstoœæ wystêpowania produktów w transkacjach")   
  p<-p + theme(legend.position="none")
  
  print(p)  
  freq
  
}


assoc_affinity<-function(trans.set, prod){
  
  aff<-affinity(trans.set)
  aff.matrix<-aff@.Data
  
  
  aff.matrix.subset<-aff.matrix[prod, prod]
  
  affinity.values<-melt(aff.matrix.subset)
  names(affinity.values)[1]<-paste("PRODUKT")
  names(affinity.values)[2]<-paste("PRODUKT1")
  names(affinity.values)[3]<-paste("affinity")
  
  affinity.values<-affinity.values[affinity.values$"affinity">0.01,]
  affinity.values<-affinity.values[affinity.values$"affinity"< 0.95,]
  
  
  
  #affinity.values<-affinity.values[order(affinity.values$affinity,decreasing="TRUE"),]
  ##affinity.values[,"PRODUKT"]<-as.character(affinity.values$PRODUKT)
  #affinity.values[,"PRODUKT1"]<-as.character(affinity.values$PRODUKT1)
  
  
  
  #affinity.values$PRODUKT3 <-(reorder(affinity.values$PRODUKT, -affinity.values$affinity))
  
  
  p<-ggplot(affinity.values, aes(PRODUKT, PRODUKT1)) +
    geom_tile(aes(fill=affinity)) + scale_fill_gradient(low = "yellow",    high = "red") 
  
  p<-p+ theme(axis.text.x=element_text(angle=90))
  p<-p+geom_text(aes(size=5, label=paste(signif(affinity*100,2), "%", sep = "")), show_guide=FALSE)
  p<-p+theme(plot.title = element_text(lineheight=.8, face="bold")) + labs(title="Bliskoœæ (affinity) produktów")   
  print(p)
  
}

assoc_rules<-function(trans.set){   
  ruleCount<-30
  support.value= (max(itemFrequency(trans.set, type="relative"))/100)
  div=1; 
  repeat{
    support.value <-support.value / div;
    rules<-apriori(trans.set,  parameter=list(support=support.value, confidence=0.1, minlen=2, maxlen=2))
    rules<-subset(rules, confidence<1)
    if ((length(rules) > ruleCount)  |  (support.value<0.000001)){
      break
    } else{
      div<-div+1; 
    }  
  }
  rules
}


#konwersja regul do data frame dla regul dwuelementwoych 

assoc_rules_df<-function(rules){
    
  
  mm<-as.matrix(rules@lhs@data)
  lhs.idx<-which(mm==TRUE, arr.in=TRUE)
  items<-as.vector(rules@lhs@itemInfo$labels)
  rules.lhs<-items[lhs.idx[,1]]
  
  
  mm<-as.matrix(rules@rhs@data)
  rhs.idx<-which(mm==TRUE, arr.in=TRUE)
  items<-as.vector(rules@rhs@itemInfo$labels)
  rules.rhs<-items[rhs.idx[,1]]
  
  rules.df<-cbind(rules.lhs, rules.rhs, rules@quality)
  
  rules.df
  
  
}

assoc_graph<-function(rules.quality,frequencies.df){
  
   el<-c(as.character(rules.quality[,1]), as.character(rules.quality[,2]))  
   assoc_graph<-graph.edgelist(as.matrix(rules.quality[,1:2]))
  
  
   E(assoc_graph)$weight<-1
  
   #E(assoc_graph)$width<-rules.quality[,"confidence"]*10
   #E(assoc_graph)$arrow.width<-rules.quality[,"confidence"]*10
    
   cli<-(rules.quality[,"confidence"]-min(rules.quality[,"confidence"]))/(max(rules.quality[,"confidence"])-min(rules.quality[,"confidence"]))
    
    
   E(assoc_graph)$color<- rainbow(256,  start=0, end=0.15, alpha=1)[cli*256 + 1 ]
    
   length(rainbow(256,  start=0, end=0.15, alpha=1)[round(cli*256,0)])
    
    
    vertices<-as.vector(V(assoc_graph)$name)
    vertices.df<-data.frame(PRODUKT=vertices)
    vertices.df.weights<-merge(vertices.df, frequencies.df,  by="PRODUKT")
    
    
    vertices.df.weights[vertices.df.weights$PRODUKT=="US£UGI", "pct"]
    
    for (i in 1:length(V(assoc_graph))) {   
      V(assoc_graph)$size[i]<-vertices.df.weights[vertices.df.weights$PRODUKT==V(assoc_graph)$name[i] ,"pct"]*100
    }
   
    #(assoc_graph)$label.cex=0.5
  
    layout<-layout.fruchterman.reingold(assoc_graph,circular=T)
    plot.igraph(assoc_graph, layout=layout)
}


assoc_heatmap<-function(rules.quality){
  
  
  p<-ggplot(rules.quality, aes(rules.rhs, rules.lhs)) 
  # p<-p+ geom_tile(aes(fill=confidence, width=support*30, height=support*30)) 
  p<-p+ geom_tile(aes(fill=confidence))   
  p<-p+ scale_fill_gradient(low = "yellow",    high = "red") 
  p<-p+ theme(axis.text.x=element_text(angle=90))
  p<-p+theme(plot.title = element_text(lineheight=.8, face="bold")) + labs(title="Asocjacje produktów (macierz)")   
  p<-p+geom_text(aes(size=5, label=paste(signif(support*100,digits=2), "%", sep = "")), show_guide=FALSE)
  p<-p+ xlab("RHS - nastêpnik regu³y") + ylab("LHS (poprzednik regu³y)")  
  
  print(p)
}

assoc_visualise<-function(rules){
  
  #brak podpisow dla regula na osiach 
  p1<-plot(rules, method="scatterplot", measure=c("confidence", "support"), 
           control=list(col=rainbow(16, start=0, end=0.15)), shading="lift",  interactive=FALSE)
  
  
  print(p1)
  p2<-plot(head(sort(rules, by="support"),30), method="grouped", measure=c("support"), 
           control=list(col=rainbow(16, start=0, end=0.15, alpha=1),main="Analiza koszyka"),  
           shading="confidence",  interactive=FALSE, font=4)
  
  print(p2)
  
  p3<-plot(head(sort(rules, by="support"),30), method="graph", measure=c("confidence"), 
           control=list(main="Analiza koszyka"),  
           shading="support",  interactive=FALSE)
  print(p3)
  
  
}



