options("width"=160)
library(fcm) #https://cran.r-project.org/web/packages/fcm/vignettes/vignettes.html
source('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/fcm_infer_corregida.R') # la función `fcm.infer()` tiene un error en el paquete y tiene que cargarse desde la función modificada en este archivo
library("colorout")
library(reshape2)
library(ggplot2)
library(dplyr)
# leer datos
# RED 10 NODOS
tabla_enlaces <- read.csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red10_enlaces.csv")
tabla_nodos <- read.csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red10_nodos.csv")
# RED 15 NODOS
tabla_enlaces <- read.csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red15_enlaces.csv")
tabla_nodos <- read.csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red15_nodos.csv")
# RED COMPLETA
tabla_enlaces <- read.csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red_completa_enlaces.csv")
tabla_nodos <- read.csv("/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Análisis/cytoscape_files/red_completa_nodos.csv")
# limpiar datos
lista_adj <- cbind(tabla_enlaces, do.call(rbind, strsplit(as.character(tabla_enlaces$name), " \\(-\\) ")))[ , c('1', '2', 'peso')]
colnames(lista_adj)[1:2] <- c('emisor', 'receptor')
dim_matriz <- length(tabla_nodos$name)
matriz_adj <- matrix(nrow = dim_matriz, ncol = dim_matriz, 0)
for (i in 1:length(lista_adj$emisor)) { # contador para emisores
	emisor_i <- which(tabla_nodos$name == lista_adj$emisor[i])
	receptor_i <- which(tabla_nodos$name == lista_adj$receptor[i])
	#cat('enlace ', i, ': ', emisor_i, ' y ', receptor_i, '\n')
	matriz_adj[emisor_i, receptor_i] <- lista_adj$peso[i]
}
matriz_adj <- as.data.frame(matriz_adj)
colnames(matriz_adj) <- tabla_nodos$name
vector_act <- as.data.frame(rep(0.5, dim_matriz))
# inferencia
output1 <- fcm.infer(vector_act, matriz_adj)
# graficar líneas
iterations <- as.numeric(rownames(output1$values))  # create a numeric vector named "iterations"
df <- data.frame(iterations, output1$values)   # add "iterations" in the "output1$values" dataframe
df2 <- melt(df, id="iterations")              # transform the dataframe df into long formats
p1 <- ggplot(data=df2,                              # Visualize the concepts' values 
	aes(x=iterations, y=value, group=variable, color=variable)) +
	labs(x = 'Simulación', y = 'Valor') +
	theme_bw() + geom_line(linewidth=0.7) + geom_point(size = 3)
p1
# graficar diferencias
proyecciones <- output1$values
conceptos <- c()
valores <- c()
for (i in 1:length(proyecciones)) {
	concepto_i_proy_init <- proyecciones[ ,i][1]
	concepto_i_proy_fin <- tail(proyecciones[ ,i], 1)
	conceptos <- c(conceptos, names(proyecciones[i]))
	valores <- c(valores, concepto_i_proy_fin - concepto_i_proy_init)
}
diferencias <- data.frame(conceptos = conceptos, valores = valores) %>% arrange(desc(valores))
p2 <- ggplot(data=diferencias, aes(x=conceptos, y=valores)) +
	theme_bw() +
	labs(x = 'Concepto', y = 'Valor') +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	geom_bar(stat="identity") +
	scale_x_discrete(limits = diferencias$conceptos)
p2
