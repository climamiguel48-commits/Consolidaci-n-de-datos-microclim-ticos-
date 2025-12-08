
# =============================================================================
# Análisis exploratorio 
# =============================================================================

library(tidyverse)
library(readxl)
library(here)

# 1. CONFIGURACIÓN
# -----------------------------------------------------------------------------
cat("ANÁLISIS EXPLORATORIO\n")
cat(rep("=", 50), "\n", sep = "")

# Ruta de los datos
archivo <- here("01_importar_datos", "datos_consolidados", "Datos_Parcela_3.xlsx")

# Verificar que el archivo existe
if (!file.exists(archivo)) {
  stop("RROR: No se encuentra el archivo: ", archivo)
}

cat("Cargando datos desde:", basename(archivo), "\n")
datos <- read_excel(archivo)

# 2. PROCESAR Y RENOMBRAR VARIABLES
# -----------------------------------------------------------------------------
cat("\n PROCESANDO Y RENOMBRANDO VARIABLES...\n")

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

# Renombrar variables conocidas a español
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
    cat("  ✓", viejo_nombre, "->", nombres_espanol[[viejo_nombre]], "\n")
  }
}

cat("Variables convertidas y renombradas\n")

# 3. ANÁLISIS ESTADÍSTICO
# -----------------------------------------------------------------------------
cat("\n ANÁLISIS ESTADÍSTICO \n")

# Lista de variables renombradas para analizar
variables_analizar <- c(
  "Radiacion_global_Wm2", "Precipitacion_mm", "Rayos_conteo",
  "Velocidad_viento_ms", "Direccion_viento_grados", "Rachas_ms",
  "Temperatura_aire_C", "Presion_atmosferica_mbar", "Humedad_relativa_porc",
  "Radiacion_global_MJm2", "Temperatura_superficie_C", "Flujo_energia_suelo_Wm2",
  "Radiacion_neta_Wm2", "Temperatura_termocupla1_C", "Temperatura_termocupla2_C",
  "Humedad_suelo_5cm_m3m3", "Humedad_suelo_40cm_m3m3"
)

# Crear tabla de resultados
resultados <- data.frame()

for (var in variables_analizar) {
  if (var %in% names(datos)) {
    valores <- datos[[var]]
    
    # Calcular estadísticas básicas
    fila <- data.frame(
      Variable = var,
      Media = mean(valores, na.rm = TRUE) %>% round(3),
      Mediana = median(valores, na.rm = TRUE) %>% round(3),
      Desviacion = sd(valores, na.rm = TRUE) %>% round(3),
      Minimo = min(valores, na.rm = TRUE) %>% round(3),
      Maximo = max(valores, na.rm = TRUE) %>% round(3),
      N_NA = sum(is.na(valores)),
      N_Total = length(valores),
      Completitud = round((1 - sum(is.na(valores))/length(valores)) * 100, 1),
      stringsAsFactors = FALSE
    )
    
    resultados <- rbind(resultados, fila)
  }
}

# 4. GUARDAR RESULTADOS
# -----------------------------------------------------------------------------
cat("\n GUARDANDO RESULTADOS...\n")

# Crear carpeta si no existe
ruta_salida <- here("02_analisis_control_calidad", "datos_procesados")
if (!dir.exists(ruta_salida)) {
  dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)
  cat("  ✓ Carpeta creada:", ruta_salida, "\n")
}

# Guardar tabla de estadísticas
archivo_estadisticas <- file.path(ruta_salida, "estadisticas_basicas.csv")
write.csv(resultados, archivo_estadisticas, row.names = FALSE)
cat("  ✓ Estadísticas guardadas en: estadisticas_basicas.csv\n")

# Guardar datos renombrados (opcional)
archivo_datos <- file.path(ruta_salida, "datos_renombrados.csv")
write.csv(datos, archivo_datos, row.names = FALSE)
cat("  ✓ Datos renombrados guardados en: datos_renombrados.csv\n")

