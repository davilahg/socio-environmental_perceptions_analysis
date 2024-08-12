options("width"=160)
library(tidyverse)
library(modelr)
library(ggplot2)
library(gapminder)
library(broom)
library(betareg)
library(car)
library(AICcmodavg)
library(MuMIn)
library(gridExtra)
library(glm2)
library(viridis)
library(svglite)
library(ggpubr)
library("colorout")

# funciones
{
  # Función para ajustar el modelo, probar supuestos y visualizarlos
  analizar_supuesto <- function(data, response, predictor) {
    # Ajustar el modelo
    formula <- as.formula(paste(response, "~", predictor))
    model <- glm(formula, data = data, family = poisson)

    # Obtener residuos
    residuos <- residuals(model)
    valores_ajustados <- fitted(model)

    # Test de normalidad (Shapiro-Wilk)
    shapiro_test <- shapiro.test(residuos)

    # Test de homogeneidad de varianzas (Levene)
    levene_test <- leveneTest(residuos ~ data[[predictor]], data = data)

    # Crear gráfico QQ para normalidad
    qq_plot <- ggqqplot(residuos, title = paste("QQ plot de", response, "vs", predictor))

    # Crear gráfico de residuos vs valores ajustados
    resid_plot <- ggplot(data, aes(x = valores_ajustados, y = residuos)) +
      geom_point() +
      geom_smooth(method = "lm") +
      ggtitle(paste("Residuos vs Valores Ajustados de", response, "vs", predictor))

    # Combinar gráficos y resultados de tests
    combined_plot <- ggarrange(qq_plot, resid_plot, ncol = 2, nrow = 1)
    print(combined_plot)

    # Mostrar resultados de los tests
    cat("Resultados del test de Shapiro-Wilk para", response, "vs", predictor, ":\n")
    print(shapiro_test)
    cat("\n")

    cat("Resultados del test de Levene para", response, "vs", predictor, ":\n")
    print(levene_test)
    cat("\n")
  }
}

# cargar y filtrar datos
{
  datos <- read.csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/datos_con_metricas_v1.csv')
  #datos <- filter(datos, escolaridad %in% c('ninguna', 'primaria', 'secundaria', 'bachillerato'))
  propiedad_s <- c()
  escolaridad_s <-c()
  localidad_s <- c()
  educacion_basica <- c('primaria', 'secundaria')
  educacion_media_superior <- c('bachillerato', 'superior')
  costa <- c('SM', 'FV', 'EZ', 'LF')
  no_costa <- c('LFRA', 'JG', 'NC', 'RA')
  for (i in 1:nrow(datos)) {
    if (datos$escolaridad[i] %in% educacion_basica)
      escolaridad_simplificada <- 'basica'
    else if (datos$escolaridad[i] %in% educacion_media_superior)
      escolaridad_simplificada <- 'media_superior'
    else if (datos$escolaridad[i] == 'ninguna')
      escolaridad_simplificada <- 'ninguna'
    escolaridad_s <- c(escolaridad_s, escolaridad_simplificada)
    if (datos$propiedad[i] == 'ejidatario')
      propiedad_simplificada <- 'ejidatario'
    else {
      propiedad_simplificada <- 'no_ejidatario'
    }
    propiedad_s <- c(propiedad_s, propiedad_simplificada)
    if (datos$localidad[i] %in% costa)
      localidad_simplificada <- 'costa'
    else {
      localidad_simplificada <- 'no_costa'
    }
    localidad_s <- c(localidad_s, localidad_simplificada)
  }
  datos <- transform(datos, propiedad_simplificada = propiedad_s, escolaridad_simplificada = escolaridad_s, localidad_simplificada = localidad_s)
  var_ind <- c('edad', 'escolaridad_simplificada', 'extension', 'num_ganado', 'propiedad_simplificada', 'localidad_simplificada', 'redes_sociales', 'familiar_estacion', 'estacion_biologia', 'participacion_investigaciones')
  var_dep <- c('nodos', 'enlaces', 'receptores', 'transmisores', 'ordinarios', 'prop_pos_neg', 'grado', 'intermediacion', 'densidad', 'complejidad', 'jerarquia')
  datos <- subset(datos, select = c(var_ind, var_dep))
}

# seleccion automática con una o dos variables independientes
{
  # regresiones poisson y gaussian
  con_gausiana <- c('grado', 'prop_pos_neg', 'complejidad')
  con_poisson <- c('nodos', 'enlaces')
  con_beta <- c('transmisores', 'receptores', 'ordinarios', 'densidad', 'jerarquia')
  con_gamma <- c('intermediacion')
  ### solo aplica con combinaciones de una variable ### [empieza]
  combinaciones <- var_ind
  independientes_lista <- c()
  dependientes_lista <- c()
  anovas_lista <- c()
  respuesta_lista <- c()
  independiente_lista <- c()
  ### solo aplica con combinaciones de una variable ### [termina]
  #combinaciones <- combn(var_ind, 2, simplify = FALSE)
  #for (var in var_ind) {
  #  combinaciones[[length(combinaciones)+1]] <- var
  #}
  models_list <- list()
  for (respuesta in var_dep) {
    if (respuesta %in% c(con_gausiana, con_poisson, con_gamma))
      for (indep in combinaciones) {
        formula <- as.formula(paste(respuesta, "~", paste(indep, collapse = '+')))
        if (respuesta %in% con_gausiana) {
          modelo <- glm(formula, data = datos, family = 'gaussian')
        } else if (respuesta %in% con_poisson) {
          modelo <- glm(formula, data = datos, family = 'poisson')
        } else if (respuesta%in% con_gamma) {
          modelo <- glm(formula, data = datos, family = Gamma(link = 'log'))
        }
        ### solo aplica con combinaciones de una variable ### [empieza]
        aov_modelo <- Anova(modelo)
        independientes_lista <- c(independientes_lista, indep)
        dependientes_lista <- c(dependientes_lista, respuesta)
        anovas_lista <- c(anovas_lista, aov_modelo$'Pr(>Chisq)')
        variables <- paste0(respuesta, '~', indep)
        respuesta_lista <- c(respuesta_lista, respuesta)
        independiente_lista <- c(independiente_lista, indepen)
        ### solo aplica con combinaciones de una variable ### [termina]
        models_list[[paste(respuesta, paste(indep, collapse = "-"), sep = "-")]] <- modelo
      }
  }
  resultados <- list()
  for (respuesta in c(con_gausiana, con_poisson)) {
    respuesta_id <- names(models_list)[grepl(respuesta, names(models_list))]
    respuesta_models <- models_list[respuesta_id]
    resultados[[respuesta]] <- sw(respuesta_models)
  }
  # regresion beta
  models_list_b <- list()
  for (respuesta in con_beta) {
    for (indep in combinaciones) {
      formula <- as.formula(paste(respuesta, '~', paste(indep, collapse = '+')))
      modelo <- betareg(formula, data = datos)
      ### solo aplica con combinaciones de una variable ### [empieza]
      aov_modelo <- Anova(modelo)
      independientes_lista <- c(independientes_lista, indep)
      dependientes_lista <- c(dependientes_lista, respuesta)
      anovas_lista <- c(anovas_lista, aov_modelo$'Pr(>Chisq)')
      respuesta_lista <- c(respuesta_lista, respuesta)
      independiente_lista <- c(independiente_lista, indepen)
      ### solo aplica con combinaciones de una variable ### [termina]
      models_list_b[[paste(respuesta, paste(indep, collapse = '-'), sep = '-')]] <- modelo
    }
  }
  resultados_b <- list()
  for (respuesta in con_beta) {
    respuesta_id <- names(models_list_b)[grepl(respuesta, names(models_list_b))]
    respuesta_models <- models_list_b[respuesta_id]
    resultados_b[[respuesta]] <- sw(respuesta_models)
  }
}

# matriz de significancias
{
  etiquetas_x <- c('Edad', 'Escolaridad', 'Cantidad de terreno', 'Cantidad de ganado', 'Tipo de tenencia de la tierra', 'Localidad', 'Uso de redes sociales', 'Familiar en la EB', 'Participación en investigaciones', 'Visita EB')
  etiquetas_y <- c('Número de conceptos', 'Número de enlaces', 'Receptores', 'Transmisores', 'Ordinarios', 'Proporción positivas/negativas', 'Grado', 'Intermediación', 'Densidad', 'Complejidad', 'Jerarquía')
  significancias <- data.frame(caracteristica = independientes_lista, metrica = dependientes_lista, significancia = anovas_lista)
  significancias$caracteristica <- factor(significancias$caracteristica, levels = var_ind)
  significancias$metrica <- factor(significancias$metrica, levels = var_dep)
  matriz <- xtabs(significancia ~ caracteristica + metrica, significancias)
  #write.table(matriz, file = 'matriz_de_significancias.csv')
  sig_plot <- ggplot(significancias, aes(caracteristica, metrica, fill= significancia)) +
              theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) +
              geom_tile(color = 'black') +
              scale_fill_viridis(direction = -1, alpha = 0.9) +
              geom_text(aes(label = round(significancia, digits = 3)), color = 'black', size = 4) +
              labs(x = 'Característica socioeconómica', y = 'Característica del mapa cognitivo', fill = 'significancia\ndel modelo') +
              scale_x_discrete(labels=etiquetas_x) +
              scale_y_discrete(labels=etiquetas_y) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
  sig_plot
  ggsave(file="significancias.svg", plot=sig_plot, width=10, height=8)
}

# modelos finales
{
  # quitar datos de persona con 290 hectáreas
  datos_sin <- datos[datos$extension < 290,]
  # ordinarios
  ordinarios_familiar <- betareg(ordinarios~familiar_estacion, data = datos)
  # transmisores
  transmisores_localidad <- betareg(transmisores~localidad_simplificada, data = datos)
  # numero de enlaces
  enlaces_escolaridad <- glm(enlaces~escolaridad_simplificada, family = 'poisson', data = datos)
  enlaces_extension <- glm(enlaces~extension, family = 'poisson', data = datos_sin)
  enlaces_localidad <- glm(enlaces~localidad_simplificada, family = 'poisson', data = datos)
  enlaces_participacion <- glm(enlaces~participacion_investigaciones, family = 'poisson', data = datos)
  # numero de nodos
  nodos_escolaridad <- glm(nodos~escolaridad_simplificada, family = 'poisson', data = datos)
  nodos_extension <- glm(nodos~extension, family = 'poisson', data = datos_sin)
  nodos_localidad <- glm(nodos~localidad_simplificada, family = 'poisson', data = datos)
  nodos_participacion <- glm(nodos~participacion_investigaciones, family = 'poisson', data = datos)
  nodos_propiedad <- glm(nodos~propiedad_simplificada, family = 'poisson', data = datos)
}

# graficas de los modelos 1v1
{
  datos$familiar_estacion <- as.factor(as.character(datos$familiar_estacion))
  datos$localidad_simplificada <- as.factor(as.character(datos$localidad_simplificada))
  datos$escolaridad_simplificada <- as.factor(as.character(datos$escolaridad_simplificada))
  datos$participacion_investigaciones <- as.factor(as.character(datos$participacion_investigaciones))
  datos$propiedad_simplificada <- as.factor(as.character(datos$propiedad_simplificada))
  extension_pred = seq(min(datos$edad, na.rm = TRUE), max(datos$edad, na.rm = TRUE), length.out = 100)
  # ORDINARIOS - FAMILIAR
  ordinarios_familiar_plot <- ggplot(datos, aes(x = familiar_estacion, y = ordinarios)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('No', 'Sí')) +
    xlab('Con familiares en la EBCC') + ylab('Prop. nodos ordinarios') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  ordinarios_familiar_plot
  # TRANSMISORES - LOCALIDADES
  transmisores_localidad_plot <- ggplot(datos, aes(x = localidad_simplificada, y = transmisores)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Costa', 'Interior')) +
    xlab('Localidad') + ylab('Prop. de nodos transmisores') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  transmisores_localidad_plot
  # ENLACES VS COSAS
  # enlaces vs escolaridad
  enlaces_escolaridad_plot <- ggplot(datos, aes(x = escolaridad_simplificada, y = enlaces)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Básica', 'Media-Superior', 'Sin')) +
    xlab('Nivel de escolaridad') + ylab('Número de enlaces') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 2, fill = 'black') +
    theme(axis.text.x = element_text(angle = 45))
  enlaces_escolaridad_plot
  # enlaces vs extension
  extension_pred = seq(min(datos$extension, na.rm = TRUE), max(datos$extension, na.rm = TRUE), length.out = 100)
  extension_enlaces_pred = predict(enlaces_extension, newdata = data.frame(extension = extension_pred), type = 'response')
  extension_nodos_pred = predict(nodos_extension, newdata = data.frame(extension = extension_pred), type = 'response')
  extension_df <- data.frame(extension = extension_pred, nodos = extension_nodos_pred, enlaces = extension_enlaces_pred)
  enlaces_extension_plot <- ggplot(extension_df, aes(x = extension, y = enlaces)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    xlab('Cantidad de terreno') + ylab('Número de enlaces') +
    geom_line(linewidth = 1, color = 'red', alpha = 0.9) +
    geom_point(data = datos, aes(x = extension, y = enlaces), shape = 21)
  enlaces_extension_plot
  #enlaces vs localidad
  enlaces_localidad_plot <- ggplot(datos, aes(x = localidad_simplificada, y = enlaces)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Costa', 'Interior')) +
    xlab('Localidad') + ylab('Número de enlaces') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  enlaces_localidad_plot
  # enlaces vs participacion en investigaciones
  enlaces_participacion_plot <- ggplot(datos, aes(x = participacion_investigaciones, y = enlaces)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('No', 'Sí')) +
    xlab('Participación en investigaciones') + ylab('Número de enlaces') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  enlaces_participacion_plot
  # NODOS VS COSAS
  # nodos vs escolaridad
  nodos_escolaridad_plot <- ggplot(datos, aes(x = escolaridad_simplificada, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Básica', 'Media-Superior', 'Sin')) +
    xlab('Nivel de escolaridad') + ylab('Número de nodos') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 2, fill = 'black') +
    theme(axis.text.x = element_text(angle = 45))
  nodos_escolaridad_plot
  # nodos vs extension
  nodos_extension_plot <- ggplot(extension_df, aes(x = extension, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    xlab('Cantidad de terreno') + ylab('Número de nodos') +
    geom_line(linewidth = 1, color = 'red', alpha = 0.9) +
    geom_point(data = datos, aes(x = extension, y = nodos), shape = 21)
  nodos_extension_plot
  # nodos vs localidad
  nodos_localidad_plot <- ggplot(datos, aes(x = localidad_simplificada, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Costa', 'Interior')) +
    xlab('Localidad') + ylab('Número de nodos') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  nodos_localidad_plot
  # nodos vs participacion en investigaciones
  nodos_participacion_plot <- ggplot(datos, aes(x = participacion_investigaciones, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('No', 'Sí')) +
    xlab('Participación en investigaciones') + ylab('Número de nodos') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  nodos_participacion_plot
  # nodos vs propiedad
  nodos_propiedad_plot <- ggplot(datos, aes(x = propiedad_simplificada, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Ejidal', 'Otros')) +
    xlab('Propiedad de la tierra') + ylab('Número de nodos') +
    geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
    geom_boxplot(alpha = 0.5) +
    #stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize = 1, fill = 'black') +
    theme(axis.text.x = element_text(angle = 0))
  nodos_propiedad_plot

  plot <- ggarrange(enlaces_localidad_plot, enlaces_escolaridad_plot, enlaces_participacion_plot, nodos_localidad_plot, nodos_escolaridad_plot, nodos_participacion_plot, transmisores_localidad_plot, nodos_propiedad_plot, ordinarios_familiar_plot, nrow = 3, ncol = 3, labels = 'auto')
  plot
  #ggsave(file="metricas1.svg", dpi = 300, plot=plot, width=10, height=8)
  #ggsave(file="metricas1.png", dpi = 300, plot=plot, width=10, height=8)
  #
}

# gráficas de características socioambientales
{
# edad
  edad_plot <- ggplot(datos, aes(x=edad)) +
    theme_bw() +
    geom_histogram(color="black", fill="white", bins = 10) +
    geom_vline(aes(xintercept=mean(edad)), color="blue", linetype="dashed", linewidth=1) +
    xlab('Edad (años)') +
    ylab('Conteo')
  edad_plot
  #escolaridad simplificada
  esc_df <- as.data.frame(table(datos$escolaridad_simplificada))
  names(esc_df) <- c('Escolaridad', 'Conteo')
  escolaridad_plot <- ggplot(data=esc_df, aes(x=Escolaridad, y = Conteo)) +
    theme_bw() +
    scale_x_discrete(labels=c('BA', 'ME-SU', 'SIN')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  escolaridad_plot
  # terreno
  extension_plot <- ggplot(datos, aes(x=extension)) +
    theme_bw() +
    geom_histogram(color="black", fill="white", bins = 10) +
    geom_vline(aes(xintercept=mean(extension)), color="blue", linetype="dashed", linewidth=1) +
    xlab('Cantidad de terreno (hectáreas)') +
    ylab('Conteo')
  extension_plot
  # ganado
  ganado_plot <- ggplot(datos, aes(x=num_ganado)) +
    theme_bw() +
    geom_histogram(color="black", fill="white", bins = 10) +
    geom_vline(aes(xintercept=mean(num_ganado)), color="blue", linetype="dashed", linewidth=1) +
    xlab('Cantidad de ganado (animales)') +
    ylab('Conteo')
  ganado_plot
  # tipo de tenencia
  tenencia_df <- as.data.frame(table(datos$propiedad_simplificada))
  names(tenencia_df) <- c('Propiedad', 'Conteo')
  tenencia_plot <- ggplot(data=tenencia_df, aes(x=Propiedad, y = Conteo)) +
    theme_bw() +
    scale_x_discrete(labels=c('Sí', 'No')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    xlab('Propiedad ejidal') +
    ylab('Conteo')
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  tenencia_plot
  # localidad
  datos_o <- read.csv('/home/gerardo/Insync/ejemploalguien123@gmail.com/Google Drive - Shared with me/M 2022 Gerardo Dávila/Datos/metricas.csv')
  loc_df <- as.data.frame(table(datos_o$localidad))
  names(loc_df) <- c('Ejido', 'Conteo')
  loc_plot <- ggplot(data=loc_df, aes(x=Ejido, y = Conteo)) +
    theme_bw() +
    #scale_x_discrete(labels=c('Básica', 'Media-Superior', 'Sin')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  loc_plot
  # redes sociales
  redes_df <- as.data.frame(table(datos$redes_sociales))
  names(redes_df) <- c('Uso', 'Conteo')
  redes_plot <- ggplot(data=redes_df, aes(x=Uso, y = Conteo)) +
    theme_bw() +
    scale_x_discrete(labels=c('No', 'Sí')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    xlab('Uso de redes sociales') +
    ylab('Conteo')
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  redes_plot
  # familiares en la estación
  familiar_estacion_df <- as.data.frame(table(datos$familiar_estacion))
  names(familiar_estacion_df) <- c('Familiar', 'Conteo')
  familiar_estacion_plot <- ggplot(data=familiar_estacion_df, aes(x=Familiar, y = Conteo)) +
    theme_bw() +
    scale_x_discrete(labels=c('No', 'Sí')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    xlab('Con familiares en la EB') +
    ylab('Conteo')
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  familiar_estacion_plot
  # participacion en investigaciones
  inv_df <- as.data.frame(table(datos$participacion_investigaciones))
  names(inv_df) <- c('Participacion', 'Conteo')
  inv_plot <- ggplot(data=inv_df, aes(x=Participacion, y = Conteo)) +
    theme_bw() +
    scale_x_discrete(labels=c('No', 'Sí')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    xlab('Han participado en investigaciones') +
    ylab('Conteo')
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  inv_plot
  # visita a EB
  EB_df <- as.data.frame(table(datos$estacion_biologia))
  names(EB_df) <- c('Visita', 'Conteo')
  EB_plot <- ggplot(data=EB_df, aes(x=Visita, y = Conteo)) +
    theme_bw() +
    scale_x_discrete(labels=c('No', 'Sí')) +
    geom_bar(stat="identity", fill = 'white', color = 'black') +
    xlab('Han visitado la EB') +
    ylab('Conteo')
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
  EB_plot
  # panel de gráficas
  plot_soc <- ggarrange(edad_plot, escolaridad_plot, extension_plot, ganado_plot, tenencia_plot, loc_plot, redes_plot, familiar_estacion_plot, inv_plot, EB_plot, nrow = 5, ncol = 2, labels = 'auto')
  plot_soc
  #ggsave('metricas.png', plot, dpi = 300)
  #ggsave(file="caracteristicas.svg", plot=plot_soc, width=6, height=10)
}







 """ cosas viejas :


 # EDAD VS COSAS
  edad_pred = seq(min(datos$edad, na.rm = TRUE), max(datos$edad, na.rm = TRUE), length.out = 100)
  conceptos_edad_pred = predict(conceptos_edad, newdata = data.frame(edad = edad_pred), type = 'response')
  relaciones_edad_pred = predict(relaciones_edad, newdata = data.frame(edad = edad_pred), type = 'response')
  conceptos_edad_df <- data.frame(edad = edad_pred, conceptos = conceptos_edad_pred, relaciones = relaciones_edad_pred)
  # edad vs conceptos
  conceptos_edad_plot <- ggplot(conceptos_edad_df, aes(x = edad, y = conceptos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    xlab('Edad') + ylab('Número de conceptos') +
    geom_line(linewidth = 1, color = 'red', alpha = 0.9) +
    geom_point(data = datos, aes(x = edad, y = nodos), shape = 21)
  conceptos_edad_plot
  # edad vs enlaces
  relaciones_edad_plot <- ggplot(conceptos_edad_df, aes(x = edad, y = relaciones)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    xlab('Edad') + ylab('Número de relaciones') +
    geom_line(linewidth = 1, color = 'red', alpha = 0.9) +
    geom_point(data = datos, aes(x = edad, y = enlaces), shape = 21)
  relaciones_edad_plot
  # ESCOLARIDAD VS COSAS
  datos$escolaridad_simplificada <- as.factor(as.character(datos$escolaridad_simplificada))
  # escolaridad vs conceptos
  conceptos_escolaridad_plot <- ggplot(datos, aes(x = escolaridad_simplificada, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Básica', 'Media-Superior', 'Sin')) +
    xlab('Nivel de escolaridad') + ylab('Número de conceptos') +
    geom_boxplot() +
    stat_boxplot(geom = "errorbar", width = 0.15, color = 1) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
    theme(axis.text.x = element_text(angle = 45))
  conceptos_escolaridad_plot
  # escolaridad vs relaciones
  relaciones_escolaridad_plot <- ggplot(datos, aes(x = escolaridad_simplificada, y = enlaces)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Básica', 'Media-Superior', 'Sin')) +
    xlab('Nivel de escolaridad') + ylab('Número de relaciones') +
    geom_boxplot() +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
    theme(axis.text.x = element_text(angle = 45))
  relaciones_escolaridad_plot
  # REDES SOCIALES VS COSAS
  datos$redes_sociales <- as.factor(as.character(datos$redes_sociales))
  # redes sociales vs conceptos
  conceptos_redes_plot <- ggplot(datos, aes(x = redes_sociales, y = nodos)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Sí', 'No')) +
    xlab('Uso de redes sociales') + ylab('Número de conceptos') +
    geom_boxplot() +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  conceptos_redes_plot
  # redes sociales vs conceptos
  relaciones_redes_plot <- ggplot(datos, aes(x = redes_sociales, y = enlaces)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_discrete(labels=c('Sí', 'No')) +
    xlab('Uso de redes sociales') + ylab('Número de relaciones') +
    geom_boxplot() +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  relaciones_redes_plot
  # GANADO VS COSAS
  ganado_pred = seq(min(datos$num_ganado, na.rm = TRUE), max(datos$num_ganado, na.rm = TRUE), length.out = 100)
  intermediacion_ganado_pred = predict(intermediacion_ganado, newdata = data.frame(num_ganado = ganado_pred), type = 'response')
  intermediacion_ganado_df <- data.frame(ganado = ganado_pred, intermediacion = intermediacion_ganado_pred)
  intermediacion_ganado_plot <- ggplot(intermediacion_ganado_df, aes(x = ganado, y = intermediacion)) +
    theme_minimal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    xlab('Cantidad de ganado (animales)') + ylab('Centralidad de intermediación') +
    geom_line(linewidth = 1, color = 'red', alpha = 0.9) +
    geom_point(data = datos, aes(x = num_ganado, y = intermediacion), shape = 21)
  intermediacion_ganado_plot
  # PANEL DE GRÁFICAS













# YA NO SIRVE PERO LO DEJO AQUÍ POR CUALQUIER COSA

# funciones
{
  ajustar_glm <- function(variable_respuesta, variable_independiente, familia) {
    formula <- as.formula(paste(variable_respuesta, "~", variable_independiente))
    modelo <- glm(formula, data = datos, family = familia)
    modelo_anova <- Anova(modelo)
    coeficientes <- coef(modelo)
    pvalues <- coef(summary(modelo))[,4]
    return(tibble(Variable_Respuesta = variable_respuesta, Variable_Independiente = variable_independiente, Modelo = list(modelo), Anova = list(modelo_anova),
                      Intercept = coeficientes[1], Intercept_p = pvalues[1], Pendiente = coeficientes[2], Pendiente_p = pvalues[2]))
  }
  ajustar_breg <- function(variable_respuesta, variable_independiente) {
    formula <- as.formula(paste(variable_respuesta, "~", variable_independiente))
    modelo <- betareg(formula, data = datos)
    coeficientes <- summary(modelo)[1]$coefficients$mean
    pvalues <-  summary(modelo)[1]$coefficients$mean[,4]
    return(tibble(Variable_Respuesta = variable_respuesta, Variable_Independiente = variable_independiente, Modelo = list(modelo),
                      Intercept = coeficientes[1], Intercept_p = pvalues[1], Pendiente = coeficientes[2], Pendiente_p = pvalues[2]))
  }
}

# ajuste manual de modelos simples con una variable independiente
{
  # regresiones poisson y gaussian (para todos los que no son centralidad de intermediación)
  combinaciones <- expand.grid(variable_respuesta = var_dep, variable_independiente = var_ind)
  distribuciones <- c('poisson', 'poisson', 'beta', 'beta', 'beta', 'beta', 'beta', 'gaussian', 'gaussian', 'gaussian', 'gamma', 'beta', 'gaussian', 'beta')
  combinaciones <- transform(combinaciones, familia = rep(distribuciones, length(var_ind)))
  resultados_modelos <- pmap(combinaciones, ajustar_glm)
  resultados_df <- bind_rows(resultados_modelos)
  resultados_df <- resultados_df %>% mutate(Intercept_sig = Intercept_p < 0.05, Pendiente_sig = Pendiente_p < 0.05)
  anovas <- unnest(resultados_df, Anova) %>% mutate(Anova_sig = get('Pr(>Chisq)') < 0.05)
  # regresión beta (para centralidad de intermediación)
  intermediacion <- data.frame(variable_respuesta = as.factor(rep('intermediacion', length(var_ind))), variable_independiente = as.factor(var_ind))
  resultados_breg <- pmap(intermediacion, ajustar_breg)
  resultados_breg_df <- bind_rows(resultados_breg)
  resultados_breg_df <- resultados_breg_df %>% mutate(Intercept_sig = Intercept_p < 0.05, Pendiente_sig = Pendiente_p < 0.05)
  # graficas
  # para edad
  coeficientes_df <- data.frame()
  datos_edad <- filter(resultados_df, Variable_Independiente == 'edad')
  edad_predict <- seq(from = min(datos$edad, na.rm = TRUE), to = max(datos$edad, na.rm = TRUE), length.out = 100)
  edad_plt <- ggplot(datos_edad, aes(x = edad))
  for (variable in var_dep) {
    var_row <- which(datos_edad$Variable_Respuesta == variable)
    modelo <- datos_edad$Modelo[var_row][[1]]
    edad_plt <- edad_plt +
      geom_point(aes(edad, get(variable))) +
      geom_line(aes(edad, predict(modelo, newdata = data.frame(edad = edad_predict), type = 'response')))
  }
}

#neutros_plot <- ggplot(datos, aes(x = redes_sociales, y = neutros)) +
#  theme_minimal() +
#  geom_boxplot() +
#  xlab('Redes sociales') + ylab('Número de nodos neutros') +
#  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
#neutros_plot

#positivos_plot <- ggplot(datos, aes(x = redes_sociales, y = positivos)) +
#  theme_minimal() +
#  xlab('Redes sociales') + ylab('Número de enlaces positivos') +
#  geom_boxplot() +
#  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
#positivos_plot

#negativos_pred = predict(negativos_glm, newdata = data.frame(edad = edad_pred), type = "response")
#negativos_df <- data.frame(edad = edad_pred, negativos = negativos_pred)
#negativos_plot <- ggplot(negativos_df, aes(x = edad, y = negativos)) +
#  theme_minimal() +
#  xlab('Edad') + ylab('Número de enlaces negativos') +
#  geom_line(size = 1, color = 'red', alpha = 0.9) +
#  geom_point(data = datos, aes(x = edad, y = negativos), shape = 21)
#negativos_plot

nodos_pred_0 <- predict(nodos_glm, newdata = data.frame(edad = edad_pred, redes_sociales = rep(0, length(edad_pred))), type = "response")
nodos_pred_1 <- predict(nodos_glm, newdata = data.frame(edad = edad_pred, redes_sociales = rep(1, length(edad_pred))), type = "response")
nodos_df <- data.frame(nodos_sin_rs = nodos_pred_0, nodos_con_rs = nodos_pred_1, edad = edad_pred)
nodos_plot <- ggplot(datos, aes(x = edad, y = nodos)) +
  theme_minimal() +
  xlab('Edad') + ylab('Número de nodos') +
  geom_point(shape = 21) +
  geom_line(linewidth = 1, data = nodos_df, aes(x = edad, y = nodos_sin_rs), color = 'red') +
  geom_line(linewidth = 1, data = nodos_df, aes(x = edad, y = nodos_con_rs), color = 'blue')
nodos_plot

# panel de gráficas
plot <- grid.arrange(nodos_plot, enlaces_plot, transmisores_plot, nrow = 1)
ggsave('metricas.png', plot, dpi = 300)
