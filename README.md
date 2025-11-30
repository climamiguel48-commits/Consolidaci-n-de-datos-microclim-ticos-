# Ordenamiento y estructuración de datos meteorológicos descargados de dataloggers

Para un proyecto de investigación se instaló una estación meteorológica automática compacta y una serie de sensores que miden los flujos de energia dentro de una parcela de 1000 m-2 en donde se registran las variables cada 60 minutos dentro de un datalogger CR5000. Por cuestiones de seguridad, los datos se descargan semanalmente y estos se van almacenado. El formato de los archivos es ".dat". Para garantizar la integridad y aplicabilidad de los datos, es necesario adjuntarlos de forma secuencial y posteriormente, verificar la calidad; realizar una exploración que integre estadística descriptiva y visualización.

## *Hipótesis*

Asi entonces, se plantea la siguiente hipótesis:

La implementación sistemática de un pipeline integral de procesamiento, visualización y control de calidad de datos es un requisito fundamental para transformar datos meteorológicos crudos en información confiable y aplicable, permitiendo la detección de patrones climáticos significativos y apoyando la toma de decisiones informadas en el manejo agrícola de la parcela.

# Metodología

## **Zona de estudio**

La zona de estudio del proyecto en donde se instaló la estación meteorológica, se encuentra ubicada en Carr. Gral. San Martín 7021, 8, Huechuraba, entre las coordenadas -33.354 de latitud y -70.691 de longitud, a una altitud de 490.87 msnm, Santiago de Chile, Chile.

![](area.png)

## Procesamiento de los datos

### *Variables meteorológicas:*

-   Temperatura (°C)

-   Humedad relativa (%)

-   Velocidad del viento (m/s)

-   Dirección del viento (°)

-   Presión atmosférica (mbar)

-   Radiación global (Wm-2)

-   Precipitación (mm)

### *Variables de flujos de energía:*

-   Temperatura superficial (°C)

-   Flujo de energía en el suelo (Wm-2)

-   Radiación Neta (Wm-2)

-   Temperatura termocupla 1 (°C)

-   Temperatura termocupla 2 (°C)

-   Humedad del suelo a los 5 cm (m3/m3)

-   Humedad del suelo a los 40 cm (m3/m3)

## Consolidar datos

Este paso consiste en importar a R y agrupar los datos en forma secuencial. Para ello se diseñó el siguiente script:

```
# SCRIPT PARA IMPORTAR Y CONSOLIDAR DATOS DE DATALOGGER \# =====================================================

# 1. CARGAR PAQUETES NECESARIOS

# =============================

library(tidyverse) 
library(lubridate) 
library(readr) 
library(writexl)

# 2. CONFIGURACIÓN INICIAL

# ========================

# Define la ruta de la carpeta donde están los archivos

Parcela1_datos \<- "\~/MEGA/MEGAsync Imports/Doctorado en Chile/Proyecto de doctorado/Instalacion de sensores/Datos y procesamiento/Parcela 3"

# Definir el patrón de nombres de archivo (ajusta según tu caso)

patron_archivos \<- "\\.dat\$" \# Para archivos CSV, cambiar si son .txt, .dat, etc.

# Número de filas a eliminar al inicio de cada archivo

filas_a_eliminar \<- 4

# Nombres de las columnas (AJUSTA SEGÚN TUS DATOS)

nombres_columnas \<- c("TIMESTAMP", "RECORD","SlrFD_W_Avg", "Rain_mm_Tot", "Strikes_Tot", "Dist_km_Avg", "WS_ms_Avg", "WindDir", "MaxWS_ms_Avg", "AirT_C_Avg", "VP_mbar_Avg", "BP_mbar_Avg", "ETos", "Rso", "RH", "RHT_C", "TiltNS_deg_Avg", "TiltWE_deg_Avg", "SlrTF_MJ_Tot", "CVMeta", "Invalid_Wind", "TT_C_Avg", "SBT_C_Avg", "SHF_Avg", "NR_Wm2_Avg", "CNR_Wm2_Avg", "Temp_TC1_C_Avg", "Temp_TC2_C_Avg", "SWC_5CM_Avg", "SWC_40CM_Avg")

# 3. FUNCIÓN PARA IMPORTAR UN ARCHIVO

# ================================================

importar_archivo \<- function(ruta_archivo) { tryCatch({ \# Lee el archivo saltando las primeras 4 filas y ASIGNANDO NOMBRES \# Especificamos que todas las columnas se lean como carácter inicialmente datos \<- read_csv(ruta_archivo, skip = filas_a_eliminar, col_names = nombres_columnas, na = c("", "NA", "NAN"), col_types = cols(.default = col_character()), \# FORZAR todo como carácter locale = locale(encoding = "UTF-8"), show_col_types = FALSE) \# Suprimir mensajes

# Verifica que el archivo no esté vacío después de eliminar las filas
if (nrow(datos) == 0) {
  message("Archivo vacío después de eliminar filas: ", ruta_archivo)
  return(NULL)
}

# Añade columna con el nombre del archivo para trazabilidad
datos$archivo_origen <- basename(ruta_archivo)

cat("Procesado:", basename(ruta_archivo), "- Filas:", nrow(datos), "\n")

return(datos)

}, error = function(e) { message("Error leyendo ", ruta_archivo, ": ", e\$message) return(NULL) }) }

# 4. IDENTIFICAR Y LEER TODOS LOS ARCHIVOS

# ========================================

# Lista todos los archivos que coincidan con el patrón

archivos \<- list.files(Parcela1_datos, pattern = patron_archivos, full.names = TRUE, recursive = FALSE)

cat("Se encontraron", length(archivos), "archivos\n")

# 5. IMPORTAR Y COMBINAR TODOS LOS DATOS

# ======================================

# Lee y combina todos los archivos

datos_completos \<- map_df(archivos, importar_archivo)

print(datos_completos)

# 6. PROCESAR FECHAS Y ORDENAR (SOLO SI HAY DATOS)

# ================================================

if (!is.null(datos_completos) && nrow(datos_completos) \> 0) {

\# Procesar la columna Date que ya existe en los datos datos_completos \<- datos_completos %\>% \# Convertir la columna Date a formato datetime (ajusta el formato según necesites) mutate( fecha_hora = parse_date_time(TIMESTAMP, orders = c("Y-m-d H:M:S")), fecha = as_date(fecha_hora) ) %\>% \# Verificar que las conversiones fueron exitosas filter(!is.na(fecha_hora)) %\>% \# Ordenar cronológicamente por fecha_hora arrange(fecha_hora) %\>% \# Eliminar duplicados por fecha/hora distinct(fecha_hora, .keep_all = TRUE) %\>% \# Reordenar columnas (opcional) select(fecha_hora, fecha, everything(), -TIMESTAMP) \# Elimina la columna Date original si prefieres

\# 7. VERIFICACIÓN Y RESULTADOS \# ============================ cat("\nRESUMEN DEL DATASET COMBINADO:\n") cat("================================\n") cat("Número total de archivos procesados:", length(archivos), "\n") cat("Número total de filas:", nrow(datos_completos), "\n") cat("Número total de columnas:", ncol(datos_completos), "\n") cat("Nombres de columnas:", names(datos_completos), "\n") cat("Rango temporal cubierto:\n") cat(" Desde:", as.character(min(datos_completos$fecha_hora, na.rm = TRUE)), "\n")
  cat("  Hasta:", as.character(max(datos_completos$fecha_hora, na.rm = TRUE)), "\n")

\# Verificar si hay huecos temporales diferencias \<- diff(datos_completos\$fecha_hora) if (length(unique(diferencias)) == 1) { cat("Frecuencia de datos:", unique(diferencias), "\n") } else { cat("¡ADVERTENCIA: Frecuencia de datos irregular!\n") cat("Diferencias encontradas:", paste(unique(diferencias), collapse = ", "), "\n") } }

# 11. LIMPIAR VARIABLES TEMPORALES

# ================================

rm(archivos, importar_archivo, filas_a_eliminar, nombres_columnas)

#Exportamos los datos#

write_xlsx(datos_completos, "Datos_Parcela_3.xlsx") 
```


## **Análisis Estadístico**

El análisis estadístico que se realizó consistió en un proceso exploratorio integral que comenzó con la carga y transformación de los datos, convirtiendo las variables a formato numérico y renombrando las columnas a un esquema en español para mayor claridad. Se calcularon estadísticas descriptivas fundamentales —como la media, mediana, desviación estándar, valores mínimo y máximo, y el conteo de valores faltantes— para las variables meteorológicas principales, entre las que se incluyeron la temperatura del aire, la humedad relativa, la velocidad del viento y la radiación global. Este análisis inicial proporcionó un resumen ejecutivo del período de datos, incluyendo las fechas de inicio y fin, la duración total en días y el número total de registros, sentando las bases para una comprensión cuantitativa del conjunto de datos antes de proceder con evaluaciones más profundas.

## **Visualización**

La visualización de datos se llevó a cabo mediante la generación de una serie de gráficos estáticos diseñados para revelar patrones temporales y distribucionales. Se utilizaron `ggplot2` y `patchwork` para crear paneles de series de tiempo que mostraron la evolución de variables atmosféricas (como radiación, viento y temperatura) y variables del suelo (como temperatura superficial y humedad a diferentes profundidades) a lo largo del tiempo. Además, se generó un boxplot para comparar la distribución de las diferentes temperaturas medidas, resaltando valores atípicos, y un gráfico circular o de rosa de los vientos para visualizar la dirección predominante del viento, agrupando los datos en sectores (N, NE, E, etc.). Estas visualizaciones se guardaron en archivos de imagen de alta calidad y se integraron automáticamente en el informe final.

## **Control de Calidad**

El control de calidad fue un proceso riguroso y automatizado que se aplicó para garantizar la integridad de los datos. Este proceso se compuso de varias verificaciones explícitas: 

**1) Verificación de rangos físicos plausibles:** Se definieron y evaluaron límites físicos para cada variable (por ejemplo, la humedad relativa debía estar entre 0% y 100%, y la radiación global entre 0 y 1500 W/m²), contabilizando todos los valores que se encontraron fuera de estos intervalos. 

**2) Detección de valores atípicos estadísticos:** Se utilizó el método del rango intercuartílico (IQR) para identificar observaciones atípicas en variables clave, calculando el número y porcentaje de estos valores que se desviaron significativamente de la distribución central. 

**3) Evaluación de la completitud:** Se calculó el porcentaje de datos no faltantes para las variables esenciales, lo que permitió identificar cuáles pudieron tener problemas de disponibilidad. 

**4) Control de la continuidad temporal:** Se detectaron saltos en la serie temporal identificando intervalos entre registros consecutivos que no fueron de una hora, lo que ayudó a encontrar huecos o inconsistencias en la frecuencia del muestreo. Los resultados de todas estas pruebas se reportaron detalladamente en tablas dentro del informe.
