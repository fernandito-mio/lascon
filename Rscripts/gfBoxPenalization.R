setwd("/home/fernando/runrun/")

MetInf <- c("SA", "LG", "LS", "LM", "LSG", "LSP", "GLSFS")
MetNames <-
  c("Sem Agr.",
    "Agr. Lin.",
    "Select.",
    "Mixto",
    "Sel. Grup.",
    "Sel. Par.",
    "SFS Ret.")
data <- read.csv("resume/val_ALL.csv", sep = " ")
metr <- c('PPV', "F-SCORE")

test <-c("Penalization")
for (te in test) {
  config <- read.csv(paste(te, "csv", sep = "."), sep = " ")
  for (e in metr) {
    png(paste(te, "_20_", e, ".png", sep = ""),
        width = 1600,
        height = 1000)
    layout(matrix(1:12, 3, 4, byrow = T))
    for (ind in 1:12) {
      t <- as.character(config$to[ind])
      d <- as.character(config$di[ind])
      nt <- as.numeric(config$am[ind])
      g <- as.character(config$dg[ind])
      p <- as.character(config$gr[ind])
      
      td <- paste(p, nt, t, d, g, sep = "_")
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
                 (data$Grouping == m) & (data$Penalization == p), ]
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
    dev.off()
  }
}