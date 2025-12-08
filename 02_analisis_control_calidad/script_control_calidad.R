
# =============================================================================
# Control de calidad
# =============================================================================

library(tidyverse)
library(here)

# 1. CONFIGURACIÓN
# -----------------------------------------------------------------------------
cat("CONTROL DE CALIDAD DE DATOS\n")
cat(rep("=", 50), "\n", sep = "")

# Ruta de los datos procesados
ruta_salida <- here("02_analisis_control_calidad", "datos_procesados")

# Verificar que existe la carpeta
if (!dir.exists(ruta_salida)) {
  stop("ERROR: No se encuentra la carpeta de datos procesados. Ejecuta primero el análisis exploratorio.")
}

# Buscar archivo de datos renombrados
archivo_datos <- file.path(ruta_salida, "datos_renombrados.csv")

if (!file.exists(archivo_datos)) {
  stop("ERROR: No se encuentra 'datos_renombrados.csv'. Ejecuta primero el análisis exploratorio.")
}

cat("Cargando datos procesados...\n")
datos <- read.csv(archivo_datos)

# 2. DEFINIR LÍMITES DE CALIDAD
# -----------------------------------------------------------------------------
cat("\n DEFINIENDO LÍMITES DE CALIDAD...\n")

# Límites físicos razonables para cada variable
limites <- list(
  Temperatura_aire_C = c(-10, 45),
  Humedad_relativa_porc = c(0, 100),
  Velocidad_viento_ms = c(0, 50),
  Radiacion_global_Wm2 = c(0, 1500),
  Precipitacion_mm = c(0, 100),
  Humedad_suelo_5cm_m3m3 = c(0, 0.6),
  Humedad_suelo_40cm_m3m3 = c(0, 0.6),
  Presion_atmosferica_mbar = c(800, 1100),
  Temperatura_superficie_C = c(-15, 60)
)

# 3. REALIZAR CONTROL DE CALIDAD
# -----------------------------------------------------------------------------
cat("\n🧪 REALIZANDO CONTROL DE CALIDAD...\n")

# Crear tabla de resultados de control de calidad
resultados_qc <- data.frame()

for (variable in names(limites)) {
  if (variable %in% colnames(datos)) {
    valores <- datos[[variable]]
    
    # Convertir a numérico si es necesario
    valores_numeric <- as.numeric(valores)
    
    # 3.1. Valores fuera de rango físico
    fuera_inferior <- sum(valores_numeric < limites[[variable]][1], na.rm = TRUE)
    fuera_superior <- sum(valores_numeric > limites[[variable]][2], na.rm = TRUE)
    total_fuera_rango <- fuera_inferior + fuera_superior
    porcentaje_fuera <- round(total_fuera_rango / length(valores_numeric) * 100, 2)
    
    # 3.2. Valores NA
    total_na <- sum(is.na(valores_numeric))
    porcentaje_na <- round(total_na / length(valores_numeric) * 100, 2)
    
    # 3.3. Estadísticas para contexto
    media_valor <- mean(valores_numeric, na.rm = TRUE) %>% round(2)
    
    # Agregar a resultados
    fila <- data.frame(
      Variable = variable,
      Limite_Inferior = limites[[variable]][1],
      Limite_Superior = limites[[variable]][2],
      Valores_Fuera_Rango = total_fuera_rango,
      Porcentaje_Fuera_Rango = porcentaje_fuera,
      Valores_NA = total_na,
      Porcentaje_NA = porcentaje_na,
      Media = media_valor,
      Estado = ifelse(porcentaje_fuera > 5, "PROBLEMA", 
                      ifelse(porcentaje_fuera > 0, "ALERTA", "OK")),
      stringsAsFactors = FALSE
    )
    
    resultados_qc <- rbind(resultados_qc, fila)
    
    cat("  ✓", variable, "-", 
        ifelse(porcentaje_fuera > 5, "PROBLEMA", 
               ifelse(porcentaje_fuera > 0, "ALERTA", "OK")), "\n")
  }
}

# 4. DETECCIÓN DE VALORES ATÍPICOS (IQR)
# -----------------------------------------------------------------------------
cat("\n DETECTANDO VALORES ATÍPICOS (MÉTODO IQR)...\n")

# Variables para análisis de atípicos
variables_atipicas <- c(
  "Temperatura_aire_C",
  "Humedad_relativa_porc", 
  "Velocidad_viento_ms",
  "Radiacion_global_Wm2",
  "Humedad_suelo_5cm_m3m3",
  "Humedad_suelo_40cm_m3m3"
)

resultados_atipicos <- data.frame()

for (variable in variables_atipicas) {
  if (variable %in% colnames(datos)) {
    valores <- datos[[variable]]
    valores_numeric <- as.numeric(valores)
    
    # Calcular quartiles
    Q1 <- quantile(valores_numeric, 0.25, na.rm = TRUE)
    Q3 <- quantile(valores_numeric, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    
    # Límites para atípicos
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    # Contar atípicos
    atipicos_inferiores <- sum(valores_numeric < lower_bound, na.rm = TRUE)
    atipicos_superiores <- sum(valores_numeric > upper_bound, na.rm = TRUE)
    total_atipicos <- atipicos_inferiores + atipicos_superiores
    porcentaje_atipicos <- round(total_atipicos / length(valores_numeric) * 100, 2)
    
    # Agregar a resultados
    if (total_atipicos > 0) {
      fila_atipico <- data.frame(
        Variable = variable,
        Atipicos_Totales = total_atipicos,
        Porcentaje_Atipicos = porcentaje_atipicos,
        Limite_Inferior_IQR = round(lower_bound, 2),
        Limite_Superior_IQR = round(upper_bound, 2),
        Estado_Atipicos = ifelse(porcentaje_atipicos > 5, "MUCHOS", 
                                 ifelse(porcentaje_atipicos > 0, "POCOS", "NINGUNO")),
        stringsAsFactors = FALSE
      )
      
      resultados_atipicos <- rbind(resultados_atipicos, fila_atipico)
    }
  }
}

# 5. GUARDAR RESULTADOS
# -----------------------------------------------------------------------------
cat("\n GUARDANDO RESULTADOS DE CONTROL DE CALIDAD...\n")

# Guardar resultados principales de QC
archivo_qc <- file.path(ruta_salida, "control_calidad.csv")
write.csv(resultados_qc, archivo_qc, row.names = FALSE)
cat("  ✓ Control de calidad guardado en: control_calidad.csv\n")

# Guardar resultados de atípicos (si hay)
if (nrow(resultados_atipicos) > 0) {
  archivo_atipicos <- file.path(ruta_salida, "valores_atipicos.csv")
  write.csv(resultados_atipicos, archivo_atipicos, row.names = FALSE)
  cat("  ✓ Valores atípicos guardados en: valores_atipicos.csv\n")
}

