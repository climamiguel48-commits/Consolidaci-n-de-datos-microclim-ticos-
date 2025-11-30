
#Análisis estadístico y control de calidad

# CARGAR LIBRERÍAS NECESARIAS
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(patchwork)
library(summarytools)
library(officer)
library(flextable)
library(scales)

# CONFIGURAR OPCIONES
options(warn = -1, scipen = 999)
theme_set(theme_bw())

# =============================================================================
# FUNCIÓN PRINCIPAL COMPLETA
# =============================================================================

procesar_datos_completo <- function(archivo, prefijo = "Analisis_Parcela3", archivo_word = "Informe_Analisis_Parcela3.docx") {
  
  # Cargar datos
  cat("Cargando datos desde:", archivo, "\n")
  df <- read_excel(archivo)
  
  # ---------------------------
  # 1. PROCESAR Y RENOMBRAR
  # ---------------------------
  procesar_variables <- function(datos) {
    cat("\n1. PROCESANDO Y RENOMBRANDO VARIABLES...\n")
    
    # Convertir a numérico columnas habituales si existen
    columnas_numericas <- c(
      "SlrFD_W_Avg", "Rain_mm_Tot", "Strikes_Tot", "Dist_km_Avg", "WS_ms_Avg",
      "WindDir", "MaxWS_ms_Avg", "AirT_C_Avg", "VP_mbar_Avg", "BP_mbar_Avg",
      "ETos", "Rso", "RH", "RHT_C", "TiltNS_deg_Avg", "TiltWE_deg_Avg",
      "SlrTF_MJ_Tot", "Invalid_Wind", "TT_C_Avg", "SBT_C_Avg", "SHF_Avg",
      "NR_Wm2_Avg", "CNR_Wm2_Avg", "Temp_TC1_C_Avg", "Temp_TC2_C_Avg",
      "SWC_5CM_Avg", "SWC_40CM_Avg", "WS_ms_Avg"
    )
    
    for (col in columnas_numericas) {
      if (col %in% colnames(datos)) {
        datos[[col]] <- as.numeric(datos[[col]])
      }
    }
    
    # Renombrar variables conocidas a español (mantener otras columnas)
    nombres_espanol <- c(
      "SlrFD_W_Avg" = "Radiacion_global_Wm2",
      "Rain_mm_Tot" = "Precipitacion_mm", 
      "Strikes_Tot" = "Rayos_conteo",
      "WS_ms_Avg" = "Velocidad_viento_ms",
      "WindDir" = "Direccion_viento_grados",
      "MaxWS_ms_Avg" = "Rachas_ms",
      "AirT_C_Avg" = "Temperatura_aire_C",
      "BP_mbar_Avg" = "Presion_atmosferica_mbar",
      "RH" = "Humedad_relativa_porc",
      "SlrTF_MJ_Tot" = "Radiacion_global_MJm2",
      "SBT_C_Avg" = "Temperatura_superficie_C",
      "SHF_Avg" = "Flujo_energia_suelo_Wm2",
      "NR_Wm2_Avg" = "Radiacion_neta_Wm2",
      "Temp_TC1_C_Avg" = "Temperatura_termocupla1_C",
      "Temp_TC2_C_Avg" = "Temperatura_termocupla2_C",
      "SWC_5CM_Avg" = "Humedad_suelo_5cm_m3m3",
      "SWC_40CM_Avg" = "Humedad_suelo_40cm_m3m3"
    )
    
    for (viejo_nombre in names(nombres_espanol)) {
      if (viejo_nombre %in% colnames(datos)) {
        names(datos)[names(datos) == viejo_nombre] <- nombres_espanol[[viejo_nombre]]
      }
    }
    
    # Asegurarse de tener columna de fecha_hora (intentar detectar)
    if (!("fecha_hora" %in% names(datos))) {
      if ("fecha" %in% names(datos) & "hora" %in% names(datos)) {
        datos <- datos %>%
          mutate(fecha_hora = as.POSIXct(paste(as.character(fecha), as.character(hora)), tz = "UTC"))
      } else if ("fecha" %in% names(datos)) {
        # si 'fecha' contiene fecha+hora
        datos <- datos %>% mutate(fecha_hora = as.POSIXct(fecha))
      } else {
        warning("No se encontró columna de fecha. Asegúrate de tener 'fecha' o 'fecha_hora'.")
      }
    } else {
      datos <- datos %>% mutate(fecha_hora = as.POSIXct(fecha_hora))
    }
    
    cat("✓ Variables convertidas y renombradas\n")
    return(datos)
  }
  
  # ---------------------------
  # 2. ANÁLISIS EXPLORATORIO
  # ---------------------------
  analisis_exploratorio <- function(datos) {
    cat("\n2. REALIZANDO ANÁLISIS EXPLORATORIO...\n")
    
    resultados_ae <- list()
    
    # Información básica (proteger si fecha_hora NA)
    fecha_min <- ifelse(all(is.na(datos$fecha_hora)), NA, min(datos$fecha_hora, na.rm = TRUE))
    fecha_max <- ifelse(all(is.na(datos$fecha_hora)), NA, max(datos$fecha_hora, na.rm = TRUE))
    duracion <- ifelse(is.na(fecha_min) | is.na(fecha_max), NA, as.numeric(difftime(fecha_max, fecha_min, units = "days")))
    
    resultados_ae$info_basica <- list(
      n_filas = nrow(datos),
      n_columnas = ncol(datos),
      fecha_inicio = fecha_min,
      fecha_fin = fecha_max,
      duracion = duracion
    )
    
    # Variables numéricas disponibles
    vars_numericas <- datos %>% select(where(is.numeric)) %>% colnames()
    resultados_ae$variables_numericas <- vars_numericas
    
    # Estadísticas descriptivas (variables principales si existen)
    vars_principales <- c("Radiacion_global_Wm2",
                          "Precipitacion_mm", 
                          "Rayos_conteo",
                          "Velocidad_viento_ms",
                          "Direccion_viento_grados",
                          "Rachas_ms",
                          "Temperatura_aire_C",
                          "Presion_atmosferica_mbar",
                          "Humedad_relativa_porc",
                          "Radiacion_global_MJm2",
                          "Temperatura_superficie_C",
                          "Flujo_energia_suelo_Wm2",
                          "Radiacion_neta_Wm2",
                          "Temperatura_termocupla1_C",
                          "Temperatura_termocupla2_C",
                          "Humedad_suelo_5cm_m3m3",
                          "Humedad_suelo_40cm_m3m3")
    
    estadisticas <- list()
    for (var in vars_principales) {
      if (var %in% colnames(datos)) {
        estadisticas[[var]] <- list(
          media = mean(datos[[var]], na.rm = TRUE),
          mediana = median(datos[[var]], na.rm = TRUE),
          desviacion = sd(datos[[var]], na.rm = TRUE),
          min = min(datos[[var]], na.rm = TRUE),
          max = max(datos[[var]], na.rm = TRUE),
          nas = sum(is.na(datos[[var]]))
        )
      }
    }
    resultados_ae$estadisticas <- estadisticas
    
    # Valores faltantes
    na_count <- colSums(is.na(datos))
    resultados_ae$valores_faltantes <- na_count[na_count > 0]
    
    cat("✓ Análisis exploratorio completado\n")
    return(resultados_ae)
  }
  
  # ---------------------------
  # 3. CONTROL DE CALIDAD
  # ---------------------------
  control_calidad <- function(datos) {
    cat("\n4. REALIZANDO CONTROL DE CALIDAD...\n")
    
    resultados_cc <- list()
    
    limites <- list(
      Temperatura_aire_C = c(-10, 45),
      Humedad_relativa_porc = c(0, 100),
      Velocidad_viento_ms = c(0, 50),
      Radiacion_global_Wm2 = c(0, 1500),
      Precipitacion_mm = c(0, 100),
      Humedad_suelo_5cm_m3m3 = c(0, 0.6),
      Humedad_suelo_40cm_m3m3 = c(0, 0.6)
    )
    
    fuera_rango <- list()
    for (var in names(limites)) {
      if (var %in% colnames(datos)) {
        idx <- which(datos[[var]] < limites[[var]][1] | datos[[var]] > limites[[var]][2])
        fuera_rango[[var]] <- list(
          n_fuera_rango = length(idx),
          porcentaje = round(length(idx) / nrow(datos) * 100, 2)
        )
      }
    }
    resultados_cc$fuera_rango <- fuera_rango
    
    # Atípicos IQR
    atipicos_iqr <- list()
    variables_verificar <- c("Radiacion_global_Wm2",
                             "Precipitacion_mm", 
                             "Rayos_conteo",
                             "Velocidad_viento_ms",
                             "Direccion_viento_grados",
                             "Rachas_ms",
                             "Temperatura_aire_C",
                             "Presion_atmosferica_mbar",
                             "Humedad_relativa_porc",
                             "Radiacion_global_MJm2",
                             "Temperatura_superficie_C",
                             "Flujo_energia_suelo_Wm2",
                             "Radiacion_neta_Wm2",
                             "Temperatura_termocupla1_C",
                             "Temperatura_termocupla2_C",
                             "Humedad_suelo_5cm_m3m3",
                             "Humedad_suelo_40cm_m3m3")
    for (var in variables_verificar) {
      if (var %in% colnames(datos)) {
        Q1 <- quantile(datos[[var]], 0.25, na.rm = TRUE)
        Q3 <- quantile(datos[[var]], 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR_val
        upper_bound <- Q3 + 1.5 * IQR_val
        idx_atipicos <- which(datos[[var]] < lower_bound | datos[[var]] > upper_bound)
        atipicos_iqr[[var]] <- list(
          n_atipicos = length(idx_atipicos),
          porcentaje = round(length(idx_atipicos) / nrow(datos) * 100, 2)
        )
      }
    }
    resultados_cc$atipicos_iqr <- atipicos_iqr
    
    vars_esenciales <- c("Radiacion_global_Wm2",
                         "Precipitacion_mm", 
                         "Rayos_conteo",
                         "Velocidad_viento_ms",
                         "Direccion_viento_grados",
                         "Rachas_ms",
                         "Temperatura_aire_C",
                         "Presion_atmosferica_mbar",
                         "Humedad_relativa_porc",
                         "Radiacion_global_MJm2",
                         "Temperatura_superficie_C",
                         "Flujo_energia_suelo_Wm2",
                         "Radiacion_neta_Wm2",
                         "Temperatura_termocupla1_C",
                         "Temperatura_termocupla2_C",
                         "Humedad_suelo_5cm_m3m3",
                         "Humedad_suelo_40cm_m3m3")
    completitud <- sapply(vars_esenciales, function(x) {
      if (x %in% colnames(datos)) {
        round(sum(!is.na(datos[[x]])) / nrow(datos) * 100, 2)
      } else {
        NA
      }
    })
    resultados_cc$completitud <- completitud
    
    datos2 <- datos %>% mutate(diff_tiempo = as.numeric(difftime(fecha_hora, lag(fecha_hora), units = "hours")))
    saltos_tiempo <- which(datos2$diff_tiempo != 1 & !is.na(datos2$diff_tiempo))
    resultados_cc$saltos_temporales <- length(saltos_tiempo)
    
    cat("✓ Control de calidad completado\n")
    return(resultados_cc)
  }
  
  # ---------------------------
  # 4. GENERAR INFORME EN WORD
  # ---------------------------
  generar_informe_word <- function(datos, resultados_ae, resultados_cc, prefijo, archivo_word) {
    cat("\n5. GENERANDO INFORME EN WORD...\n")
    
    # Crear documento Word
    doc <- read_docx()
    doc <- doc %>% 
      body_add_par("Informe - Análisis de Datos Meteorológicos", style = "heading 1") %>%
      body_add_par(paste0("Fecha de generación: ", Sys.Date()), style = "Normal")
    
    # Resumen ejecutivo
    doc <- doc %>% body_add_par("Resumen ejecutivo", style = "heading 2")
    resumen_txt <- c(
      paste0("- Periodo analizado: ", as.character(resultados_ae$info_basica$fecha_inicio), " a ", as.character(resultados_ae$info_basica$fecha_fin)),
      paste0("- Duración (días): ", round(resultados_ae$info_basica$duracion, 1)),
      paste0("- Total registros: ", resultados_ae$info_basica$n_filas),
      paste0("- Variables numéricas analizadas: ", paste(resultados_ae$variables_numericas, collapse = ", "))
    )
    doc <- doc %>% body_add_par(paste(resumen_txt, collapse = "\n"), style = "Normal")
    
    # Estadísticas principales: convertir a data.frame y agregar
    if (length(resultados_ae$estadisticas) > 0) {
      stats_df <- do.call(rbind, lapply(names(resultados_ae$estadisticas), function(x) {
        s <- resultados_ae$estadisticas[[x]]
        data.frame(Variable = x, Media = round(s$media,3), Mediana = round(s$mediana,3),
                   SD = round(s$desviacion,3), Min = round(s$min,3), Max = round(s$max,3), NA_count = s$nas, stringsAsFactors = FALSE)
      }))
      stats_ft <- regulartable(stats_df)
      stats_ft <- autofit(stats_ft)
      doc <- doc %>% body_add_par("Estadísticas descriptivas (variables seleccionadas)", style = "heading 2")
      doc <- doc %>% body_add_flextable(stats_ft)
    }
    
    # Control de calidad: fuera de rango
    doc <- doc %>% body_add_par("Control de calidad - resumen", style = "heading 2")
    # fuera_rango table
    fuera_rango_df <- do.call(rbind, lapply(names(resultados_cc$fuera_rango), function(x) {
      v <- resultados_cc$fuera_rango[[x]]
      data.frame(Variable = x, N_fuera_rango = v$n_fuera_rango, Porcentaje = v$porcentaje, stringsAsFactors = FALSE)
    }))
    fr_ft <- regulartable(fuera_rango_df)
    fr_ft <- autofit(fr_ft)
    doc <- doc %>% body_add_par("Valores fuera de rango físico (conteo y %):", style = "Normal")
    doc <- doc %>% body_add_flextable(fr_ft)
    
    # atipicos table
    atipicos_df <- do.call(rbind, lapply(names(resultados_cc$atipicos_iqr), function(x) {
      v <- resultados_cc$atipicos_iqr[[x]]
      data.frame(Variable = x, N_atipicos = v$n_atipicos, Porcentaje = v$porcentaje, stringsAsFactors = FALSE)
    }))
    at_ft <- regulartable(atipicos_df)
    at_ft <- autofit(at_ft)
    doc <- doc %>% body_add_par("Valores atípicos (IQR):", style = "Normal")
    doc <- doc %>% body_add_flextable(at_ft)
    
    # completitud
    comp_df <- data.frame(Variable = names(resultados_cc$completitud), Completitud_pct = as.vector(resultados_cc$completitud))
    comp_ft <- regulartable(comp_df)
    comp_ft <- autofit(comp_ft)
    doc <- doc %>% body_add_par("Completitud de datos (%):", style = "Normal")
    doc <- doc %>% body_add_flextable(comp_ft)
    
    # Saltos temporales
    doc <- doc %>% body_add_par(paste0("Saltos en la serie temporal (intervalos != 1 hora): ", resultados_cc$saltos_temporales), style = "Normal")
    
    # Recomendaciones (breves)
    doc <- doc %>% body_add_par("Recomendaciones (generales)", style = "heading 2")
    recs <- c(
      "Revisar valores fuera de rango y su origen (sensores, errores de registro).",
      "Investigar valores atípicos y decidir imputación o descarte.",
      paste0("Mejorar completitud si alguna variable crítica < 90% (completitud mínima observada: ", min(resultados_cc$completitud, na.rm = TRUE), "%)."),
      "Verificar sincronización temporal entre sensores si hay saltos significativos."
    )
    doc <- doc %>% body_add_par(paste("- ", recs, collapse = "\n"), style = "Normal")
    
    # Guardar docx
    print(doc, target = archivo_word)
    cat("✓ Documento Word generado en:", archivo_word, "\n")
    
    return(archivo_word)
  }
  
  # ---------------------------
  # EJECUTAR PIPELINE COMPLETO
  # ---------------------------
  cat("INICIANDO PROCESAMIENTO COMPLETO DE DATOS...\n")
  datos_procesados <- procesar_variables(df)
  resultados_ae <- analisis_exploratorio(datos_procesados)
  resultados_cc <- control_calidad(datos_procesados)
  
  # imprimir informe en consola
  generar_informe <- function(datos, resultados_ae, resultados_cc) {
    cat("Resumen en consola disponible.\n")
  }
  generar_informe(datos_procesados, resultados_ae, resultados_cc)
  
  # Generar Word con todo
  archivo_word_generado <- generar_informe_word(datos_procesados, resultados_ae, resultados_cc, prefijo, archivo_word)
  
  # Guardar resultados a disco (datos, resumen txt)
  write.csv(datos_procesados, paste0(prefijo, "_Datos_Procesados.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  
  # devolver todo
  return(list(
    datos_procesados = datos_procesados,
    analisis_exploratorio = resultados_ae,
    control_calidad = resultados_cc,
    archivo_word = archivo_word_generado
  ))
}

# =============================================================================
# EJECUTAR ANÁLISIS COMPLETO (EJEMPLO)
# =============================================================================

# Sustituye "Datos_Parcela_3.xlsx" por tu archivo real.
# El informe Word se guardará como "Informe_Analisis_Parcela3.docx"
resultados_completos <- procesar_datos_completo("Datos_Parcela_3.xlsx",
                                                prefijo = "Analisis_Parcela3",
                                                archivo_word = "Informe_Analisis_Parcela3.docx")
