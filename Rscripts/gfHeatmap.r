setwd("/home/fernando/runrun/resume")
library(RColorBrewer)
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
        datos <- read.csv(paste(nameFile, '.csv', sep = ''), sep=' ')
        i<-0
        for (p in Penalization) {
          j<-0
          for (a in Grouping) {
            j<-j+1
            v<-if(i+j<10) paste('0',(i+j),sep = '')else i+j
            png(paste("hm_",nameFile,"_",v, ".png", sep = ""),width = 300,height = 300)
            tcalor=table(datos[datos$Penalization == p, c(a,'GB'),])
            heatmap(tcalor, Rowv=NA, Colv=NA, col = colorRampPalette(brewer.pal(8, "Blues"))(25), scale="column", xlab='Gabarito', ylab=MetNames[j],main = p)
            dev.off()
          }
          i<-i+j
        }
        
      }
    }
  }
}