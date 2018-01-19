setwd("/home/fernando/50run/50resume")
#penalization<-c("IM", "NO0_5","NO1_0","PO2")
penalization<-c("IM", "NO1_0")
degree <-c(2 ,4 ,6)
ntimes <- c(20, 50)

#Top <- c("RANDOM", "SCALE_FREE")
Top <- c("RANDOM")
est <- c("PPV", "F-SCORE")
Dyn<-c("BOOLEAN","PROBABILISTIC_BOOLEAN")
#suf="4__"
suf=""
dirG="boxs/"
MetInf <- c("SA", "LG", "LS", "LM", "LSG", "LSP", "GLSFS")
MetNames <- c("Sem Agr.", "Agr. Lin.", "Select.","Mixto", "Sel. Grup.", "Sel. Par.", "SFS Ret.")
data<-read.csv(paste(suf,"degreeInference.csv",sep = ""), sep=" ")
for (p in penalization) {
  for (t in Top) {
    for (d in Dyn) {
      for(nt in ntimes){
        for(g in degree){
          td <- paste(p,nt,t, d, g, sep = "_")
          ff <- c()
          ppv <- c()
          tpr<-c()
          for (m in MetInf) {
            #FPFNTPTN
            datFil<-data[(data$TOPOLOGY==t) & (data$Dynamic==d) & (data$Times==nt) & (data$IdealDegree==g) & (data$Grouping==m) & (data$Penalization==p),]
            pp <- datFil$TP / (datFil$TP + datFil$FP)
            re <- datFil$TP / (datFil$TP + datFil$FN)
            f <- 2 * (pp * re) / (pp + re)
            ppv <- c(ppv,list(m=pp))
            ff<- c(ff,list(m=f))
            tpr<-c(tpr,list(m=re))
          }
          for(e in est){
            png(paste(dirG,suf,e,"_",td, ".png", sep = ""))
            grafDat <-
              if (e == "PPV")
                ppv
            else if (e == "F-SCORE")
              ff
            else if (e == "TPR")
              tpr
            boxplot(
              grafDat,
              ylim = c(0, 1),
              names = MetNames,
              main = paste(e,nt,p, t, d, g, sep = " "), las=2) 
            dev.off()
          }
        }
      }
    }
  }
}
# data<-data.frame(Stat11=rnorm(100,mean=3,sd=2), Stat21=rnorm(100,mean=4,sd=1), Stat31=rnorm(100,mean=6,sd=0.5), Stat41=rnorm(100,mean=10,sd=0.5), Stat12=rnorm(100,mean=4,sd=2), Stat22=rnorm(100,mean=4.5,sd=2), Stat32=rnorm(100,mean=7,sd=0.5), Stat42=rnorm(100,mean=8,sd=3), Stat13=rnorm(100,mean=6,sd=0.5), Stat23=rnorm(100,mean=5,sd=3), Stat33=rnorm(100,mean=8,sd=0.2), Stat43=rnorm(100,mean=4,sd=4))
# boxplot(data,
#         las = 2,
#         col = c("red","sienna","palevioletred1","royalblue2","red","sienna","palevioletred1","royalblue2","red","sienna","palevioletred1","royalblue2"),
#         at = c(1,2,3,4, 6,7,8,9, 11,12,13,14),
#         par(mar = c(12, 5, 4, 2) + 0.1),
#         names = c("","","","","","","","","","","",""),
#         ylim=c(-6,18))
#Station labelsmtext("Station1", side=1, line=1, at=1, las=2, font=1, col="red")mtext("Station2", side=1, line=1, at=2, las=2, font=2, col="sienna")mtext("Station3", side=1, line=1, at=3, las=2, font=3, col="palevioletred1")mtext("Station4", side=1, line=1, at=4, las=2, font=4, col="royalblue2")mtext("Station1", side=1, line=1, at=6, las=2, font=1, col="red")mtext("Station2", side=1, line=1, at=7, las=2, font=2, col="sienna")mtext("Station3", side=1, line=1, at=8, las=2, font=3, col="palevioletred1")mtext("Station4", side=1, line=1, at=9, las=2, font=4, col="royalblue2")mtext("Station1", side=1, line=1, at=11, las=2, font=1, col="red")mtext("Station2", side=1, line=1, at=12, las=2, font=2, col="sienna")mtext("Station3", side=1, line=1, at=13, las=2, font=3, col="palevioletred1")mtext("Station4", side=1, line=1, at=14, las=2, font=4, col="royalblue2") #Axis labelsmtext("Time", side = 1, line = 6, cex = 2, font = 3)mtext("Oxigen (%)", side = 2, line = 3, cex = 2, font = 3)
#In-plot labelstext(1,-4,"*")text(6,-4,"*")text(11,-4,"*")text(2,9,"A",cex=0.8,font=3)text(7,11,"A",cex=0.8,font=3)text(12,15,"A",cex=0.8,font=3)
# v10=read.table("ps_P_10",header=FALSE);
# v20=read.table("ps_P_20",header=FALSE);
# v30=read.table("ps_P_30",header=FALSE);
# v40=read.table("ps_P_40",header=FALSE);
# v50=read.table("ps_P_50",header=FALSE);
# v60=read.table("ps_P_60",header=FALSE);
# v70=read.table("ps_P_70",header=FALSE);
# v80=read.table("ps_P_80",header=FALSE);
# v90=read.table("ps_P_90",header=FALSE);
# v100=read.table("ps_P_100",header=FALSE);
#
