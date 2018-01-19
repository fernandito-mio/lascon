setwd("/home/fernandito/Downloads/runrun_final/resume")

Topologia <- c('SCALE_FREE', 'RANDOM')
Grouping <- c('GB', 'SA', 'LG', 'LS', 'LM', 'LSG', 'LSP', 'GLSFS')
MetNames <-
  c(
    "Gabarito",
    "Sem Agr.",
    "Agr. Lin.",
    "Select.",
    "Mixto",
    "Sel. Grup.",
    "Sel. Par.",
    "SFS Ret."
  )
Penalization <- c('IM', 'NO0_5', 'NO1_0', 'PO2')
Degree <- c('2_0' , '4_0' , '6_0')
Dynamic <- c("BOOLEAN", "PROBABILISTIC_BOOLEAN")
times <- c(20, 50)

#rede<-1
for (t in Topologia) {
  for (d in Dynamic) {
    for (ti in times) {
      for (g in Degree) {
        nameFile =  paste('dgi',t, d, g, ti, sep = "__")
        png(paste("bp_",nameFile, ".png", sep = ""),
            width = 2700,
            height = 1200)
        layout(matrix(1:32, 4, 8, byrow = T))
        datos <- read.csv(paste(nameFile, '.csv', sep = ''), sep=' ')
        maxX = max(datos[, Grouping, ])
        maxY=0
        for (p in Penalization) {
          for (a in Grouping) {
            maxY=max(table(factor(datos[datos$Penalization == p, a, ])),maxY)
          }
        }
        for (p in Penalization) {
          for (a in Grouping) {
            barplot(table(factor(datos[datos$Penalization == p, a, ], levels = 0:maxX)),
                    xlab="Graus", ylab="Frequença",main=paste(a,p),ylim =c(0, maxY))
          }
        }
        dev.off()
      }
    }
  }
}