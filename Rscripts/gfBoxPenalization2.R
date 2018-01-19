setwd("/home/fernando/runrun/")
penalization <- c("IM", "NO0_5", "NO1_0", "PO2")
MetInf <- c("SA", "LG", "LS", "LM", "LSG", "LSP", "GLSFS")
MetNames <-
  c("Sem Agr.",
    "Agr. Lin.",
    "Select.",
    "Mixto",
    "Sel. Grup.",
    "Sel. Par.",
    "SFS Ret.")


degree <- c(2 , 4 , 6)

ntimes <- c(20, 50)
Top <- c("RANDOM", "SCALE_FREE")
Dyn <- c("BOOLEAN", "PROBABILISTIC_BOOLEAN")
metr <- c('PPV', "F-SCORE")
fn<-c("5m","5le")
for (ng in fn) {
  data <- read.csv(paste("resume/", ng, "_val.csv", sep = "") , sep = " ")
  for (t in Top) {
    for (d in Dyn) {
      for (nt in ntimes) {
        for (e in metr) {
          png(paste(paste(ng,t,d,nt,e,sep="_"),".png", sep = ""),
              width = 1600,
              height = 1000)
          layout(matrix(1:12, 3, 4, byrow = T))
          for (g in degree) {
            for (p in penalization) {
              ff <- c()
              ppv <- c()
              tpr <- c()
              for (m in MetInf) {
                #FPFNTPTN
                datFil <-
                  data[(data$TOPOLOGY == t) &
                         (data$Dynamic == d) &
                         (data$Times == nt) &
                         (data$IdealDegree == g) &
                         (data$Grouping == m) &
                         (data$Penalization == p), ]
                pp <- datFil$TP / (datFil$TP + datFil$FP)
                re <- datFil$TP / (datFil$TP + datFil$FN)
                f <- 2 * (pp * re) / (pp + re)
                ppv <- c(ppv, list(m = pp))
                ff <- c(ff, list(m = f))
                tpr <- c(tpr, list(m = re))
              }
              
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
                main = paste(e, nt, p, t, d, g, sep = " "),
                las = 2
              )
            }
          }
          dev.off()
        }
      }
    }
  }
}