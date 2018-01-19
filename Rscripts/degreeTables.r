library(xtable)

setwd("/home/fernando/runrun/random/resume")
penalization<-c("IM", "NO0_5","NO1_0","PO2")

degree <-c(2 ,4 ,6)
ng <- 20
ntimes <- c(20, 50)

Top <- c("RANDOM")
est <- c("PPV", "F-SCORE")
Dyn<-c("BOOLEAN","PROBABILISTIC_BOOLEAN")

MetInf <- c("SA", "LG", "LS", "LM", "LSG", "LSP", "GLSFS")
MetNames <- c("Sem Agrupar", "Agrupamento Lineal", "Lineal Selectivo", "Lineal Mixto", "Lineal Selectivo Grupo", "Lineal Selectivo Parcial", "Recorrido SFS Reticulado")

data<-read.csv("degreeInference.csv", sep=" ")
dataGab<-read.csv("degreeGabarito.csv", sep=" ")
#MetInf <- c("Lineal_Selectivo_Grupo")
#MetNames <- c("xx")
#IM_50_SCALE_FREE_PROBABILISTIC_BOOLEAN_6_Sem_Agrupar
for (p in penalization) {
  for (t in Top) {
    for (d in Dyn) {
      for(nt in ntimes){
        for(g in degree){
          lineMean<-c()
          for (m in MetInf) {
            #FPFNTPTN
            datFil<-data[(data$TOPOLOGY==t) & (data$Dynamic==d) & (data$Times==nt) & (data$IdealDegree==g) & (data$Grouping==m) & (data$Penalization==p),]

            lineMean<-c(lineMean,c(mean(datFil$RealDegree),sd(datFil$RealDegree)))
          }
          fillGab<-dataGab[(dataGab$TOPOLOGY==t) & (dataGab$IdealDegree==g),]
          lineMean<-c(c(mean(fillGab$RealDegree),sd(fillGab$RealDegree)),lineMean)
          tabla<-data.frame(matrix(lineMean, ncol = 2, byrow = T),
                            row.names = gsub("_", " ",c("Gabarito", MetNames)))
          colnames(tabla)<-c("meia","desvio")
          
          #lab=paste("tab::",paste("Med",to[h],b[j],A[a], sep="_"), sep="")
          tab<-xtable(tabla,
                      caption=paste("Penalização:",p ," Topologia:",t,
                                    ", Amostras:",nt, " Grau Meio:",g),
                      align=c("|l","|c|","c|"),
                      label= paste("lab",t,g, sep = ":"))
          
          print(tab,file="tablesDegree.tex",append=T,table.placement = "h", caption.placement="bottom",
                only.contents=FALSE
          )
        }
      }
    }
  }
}

