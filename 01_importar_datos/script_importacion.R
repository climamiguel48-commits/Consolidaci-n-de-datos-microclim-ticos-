
# SCRIPT PARA IMPORTAR Y CONSOLIDAR DATOS DE DATALOGGER
# =====================================================

# 1. CARGAR PAQUETES NECESARIOS
# =============================

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readr")) install.packages("readr")
if (!require("writexl")) install.packages("writexl")
if (!require("here")) install.packages("here")

library(tidyverse)
library(lubridate)
library(readr)
library(writexl)
library(here)

# 2. CONFIGURACIÓN INICIAL
# ========================

Parcela1_datos <- here("01_importar_datos", "datos_crudos")

# Definir el patrón de nombres de archivo
patron_archivos <- "\\.dat$"

# Número de filas a eliminar al inicio de cada archivo
filas_a_eliminar <- 4

# Nombres de las columnas
nombres_columnas <- c("TIMESTAMP", "RECORD","SlrFD_W_Avg", "Rain_mm_Tot", "Strikes_Tot",
                      "Dist_km_Avg", "WS_ms_Avg", "WindDir", "MaxWS_ms_Avg", "AirT_C_Avg", "VP_mbar_Avg",
                      "BP_mbar_Avg", "ETos", "Rso", "RH", "RHT_C", "TiltNS_deg_Avg", "TiltWE_deg_Avg", "SlrTF_MJ_Tot",
                      "CVMeta", "Invalid_Wind", "TT_C_Avg", "SBT_C_Avg", "SHF_Avg", "NR_Wm2_Avg", "CNR_Wm2_Avg",
                      "Temp_TC1_C_Avg", "Temp_TC2_C_Avg", "SWC_5CM_Avg", "SWC_40CM_Avg")


# 3. FUNCIÓN PARA IMPORTAR UN ARCHIVO 
# ================================================
importar_archivo <- function(ruta_archivo) {
  tryCatch({
    datos <- read_csv(ruta_archivo,
                      skip = filas_a_eliminar,
                      col_names = nombres_columnas,
                      na = c("", "NA", "NAN"),
                      col_types = cols(.default = col_character()),  
                      locale = locale(encoding = "UTF-8"),
                      show_col_types = FALSE) 

    # Verifica que el archivo no esté vacío después de eliminar las filas
    if (nrow(datos) == 0) {
      message("Archivo vacío después de eliminar filas: ", ruta_archivo)
      return(NULL)
    }

    # Añade columna con el nombre del archivo para trazabilidad
    datos$archivo_origen <- basename(ruta_archivo)

    cat("Procesado:", basename(ruta_archivo), "- Filas:", nrow(datos), "\n")

    return(datos)

  }, error = function(e) {
    message("Error leyendo ", ruta_archivo, ": ", e$message)
    return(NULL)
  })
}

# 4. IDENTIFICAR Y LEER TODOS LOS ARCHIVOS
# ========================================
# Lista todos los archivos que coincidan con el patrón
archivos <- list.files(Parcela1_datos,
                       pattern = patron_archivos,
                       full.names = TRUE,
                       recursive = FALSE)

cat("Se encontraron", length(archivos), "archivos\n")

# 5. IMPORTAR Y COMBINAR TODOS LOS DATOS
# ======================================
# Lee y combina todos los archivos
datos_completos <- map_df(archivos, importar_archivo)

print(datos_completos)

# 6. PROCESAR FECHAS Y ORDENAR 
# ================================================
if (!is.null(datos_completos) && nrow(datos_completos) > 0) {
  datos_completos <- datos_completos %>%
    mutate(
      fecha_hora = parse_date_time(TIMESTAMP,
                                   orders = c("Y-m-d H:M:S")),
      fecha = as_date(fecha_hora)
    ) %>%
    # Verifica que las conversiones fueron exitosas
    filter(!is.na(fecha_hora)) %>%
    # Ordena cronológicamente por fecha_hora
    arrange(fecha_hora) %>%
    # Elimina duplicados por fecha/hora
    distinct(fecha_hora, .keep_all = TRUE) %>%
    # Reordena columnas
    select(fecha_hora, fecha, everything(), -TIMESTAMP)  
  
  # 7. VERIFICACIÓN Y RESULTADOS
  # ============================
  cat("\nRESUMEN DEL DATASET COMBINADO:\n")
  cat("================================\n")
  cat("Número total de archivos procesados:", length(archivos), "\n")
  cat("Número total de filas:", nrow(datos_completos), "\n")
  cat("Número total de columnas:", ncol(datos_completos), "\n")
  cat("Nombres de columnas:", names(datos_completos), "\n")
  cat("Rango temporal cubierto:\n")
  cat("  Desde:", as.character(min(datos_completos$fecha_hora, na.rm = TRUE)), "\n")
  cat("  Hasta:", as.character(max(datos_completos$fecha_hora, na.rm = TRUE)), "\n")
  
  # Verifica si hay huecos temporales
  diferencias <- diff(datos_completos$fecha_hora)
  if (length(unique(diferencias)) == 1) {
    cat("Frecuencia de datos:", unique(diferencias), "\n")
  } else {
    cat("¡ADVERTENCIA: Frecuencia de datos irregular!\n")
    cat("Diferencias encontradas:", paste(unique(diferencias), collapse = ", "), "\n")
  }
}

# 11. LIMPIAR VARIABLES TEMPORALES
# ================================
rm(archivos, importar_archivo, filas_a_eliminar, nombres_columnas)

# Luego exportar

write_xlsx(datos_completos,
           here("01_importar_datos", "datos_consolidados", "Datos_Parcela_3.xlsx"))
