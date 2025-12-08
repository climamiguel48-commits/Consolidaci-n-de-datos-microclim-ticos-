
#Script para visualización grafica#

library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(openair)
library(ggpmisc)
library(viridis)

# Leer el archivo Excel
datos_renombrado <- read.csv("02_analisis_control_calidad/datos_procesados/datos_renombrados.csv")

# Convertir fecha_hora a formato datetime
datos_renombrado$fecha_hora <- as.POSIXct(datos_renombrado$fecha_hora, format = "%Y-%m-%d %H:%M:%S")

# Extraer componentes de fecha y hora
datos_renombrado <- datos_renombrado %>%
  mutate(
    fecha = as.Date(fecha_hora),
    hora = hour(fecha_hora),
    dia = day(fecha_hora),
    mes = month(fecha_hora)
  )

# 1. GRÁFICO DE SERIES TEMPORALES
# Crear un dataset largo para facetas
datos_largo <- datos_renombrado %>%
  select(fecha_hora, 
         Radiacion_global_Wm2, 
         Temperatura_aire_C, 
         Humedad_relativa_porc,
         Velocidad_viento_ms) %>%
  pivot_longer(cols = -fecha_hora, names_to = "Variable", values_to = "Valor")

# Gráfico de series temporales múltiples
# Crear versión diaria (promedio o máximo según variable)
# Agregar una columna que identifique segmentos continuos de tiempo
datos_largo_diarios <- datos_diarios %>%
  arrange(Variable, fecha) %>%
  group_by(Variable) %>%
  mutate(
    # Calcular diferencia entre registros consecutivos (en días, no horas)
    dif_dias = as.numeric(difftime(fecha, lag(fecha), units = "days")),
    # Crear un nuevo grupo cuando haya un hueco > 1 día
    grupo_continuo = cumsum(is.na(dif_dias) | dif_dias > 1 | dif_dias < 0)
  ) %>%
  ungroup()

# Etiquetas para las variables
etiquetas_1 <- c(
  "Radiacion_global_Wm2" = "Radiacion global (Wm-2)",
  "Temperatura_aire_C" = "Temperatura del aire (°C)", 
  "Humedad_relativa_porc" = "Humedad relativa (%)",
  "Velocidad_viento_ms" = "Velocidad del viento (m/s)"
)

# Aplicar etiquetas
datos_largo_diarios$Variable_etiqueta <- factor(datos_largo_diarios$Variable,
                                                levels = names(etiquetas_1),
                                                labels = etiquetas_1)

# Ahora graficar usando el grupo continuo
series_temporales <- ggplot(datos_largo_diarios, aes(x = fecha, y = Valor_diario, color = Variable_etiqueta)) +
  geom_line(linewidth = 0.4, alpha = 0.8, aes(group = interaction(Variable_etiqueta, grupo_continuo))) +
  facet_wrap(~ Variable_etiqueta, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(
    "Humedad relativa (%)" = "blue",
    "Temperatura del aire (°C)" = "red",
    "Radiacion global (Wm-2)" = "orange",
    "Velocidad del viento (m/s)" = "purple"
  )) +
  scale_x_date(
    date_breaks = "15 days",     
    date_labels = "%d/%m",        
    minor_breaks = "1 week",      
    expand = c(0.02, 0)
  ) +
  
  labs(
    title = "Comportamiento diario de las variables meteorológicas",
    x = "Fecha",
    y = ""
  ) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
    plot.caption = element_text(size = 8, hjust = 1, color = "gray50"),
    panel.grid.minor = element_line(color = "gray93", linewidth = 0.15),
    panel.grid.major = element_line(color = "gray88", linewidth = 0.25),
    strip.background = element_rect(fill = "gray95"),
    legend.position = "none"
  )

series_temporales

# 3. GRÁFICO DE HUMEDAD DEL SUELO EN EL TIEMPO
datos_suelo <- datos_renombrado %>%
  select(fecha_hora, Humedad_suelo_5cm_m3m3, Humedad_suelo_40cm_m3m3) %>%
  pivot_longer(cols = -fecha_hora, names_to = "Profundidad", values_to = "Humedad")

Humedad_Suelo<-ggplot(datos_suelo, aes(x = fecha_hora, y = Humedad, color = Profundidad)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Humedad del suelo a diferentes profundidades",
       x = "Fecha y Hora", y = "Humedad (m³/m³)",
       color = "Profundidad") +
  theme_bw() +
  theme(legend.position = "top")

Humedad_Suelo

# 4. GRÁFICO DE PRECIPITACIÓN ACUMULADA
Precipitacion<-datos_renombrado %>%
  mutate(fecha = as.Date(fecha_hora),
         Precipitacion_mm = as.numeric(Precipitacion_mm)) %>%
  group_by(fecha) %>%
  summarise(Precipitacion_diaria = sum(Precipitacion_mm, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = Precipitacion_diaria)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(title = "Precipitación diaria",
       x = "Fecha", y = "Precipitación (mm)") +
  theme_bw()

Precipitacion

# 5. ROSA DE VIENTOS (Wind Rose)
# Asegurarse de que openair esté instalado y cargado
rosas_viento<-if(require(openair)) {
  # Preparar datos para rosa de vientos
  wind_data <- datos_renombrado %>%
    select(ws = Velocidad_viento_ms, wd = Direccion_viento_grados) %>%
    filter(!is.na(ws), !is.na(wd))
  
  # Crear rosa de vientos
  windRose(wind_data, 
           ws = "ws", 
           wd = "wd",
           main = "Rosa de Vientos",
           paddle = FALSE,
           key.header = "Velocidad (m/s)")
}

# 6. GRÁFICO DE DISTRIBUCION DE TEMPERATURAS 
datos_temp_2 <- datos_renombrado %>%
  select(Temperatura_aire_C, Temperatura_superficie_C, 
         Temperatura_termocupla1_C, Temperatura_termocupla2_C) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear etiquetas más cortas
etiquetas <- c(
  "Temperatura_aire_C" = "Temperatura del aire",
  "Temperatura_superficie_C" = "Temperatura de la superficie del suelo", 
  "Temperatura_termocupla1_C" = "Termocupla 1 a los 5 cm",
  "Temperatura_termocupla2_C" = "Termocupla 2 a los 5 cm"
)

datos_temp_2$Variable_etiqueta <- factor(datos_temp_2$Variable,
                                       levels = names(etiquetas),
                                       labels = etiquetas)


# Boxplot de temperaturas
distribucion_temperaturas_2 <- ggplot(datos_temp_2, aes(x = Variable_etiqueta, y = as.numeric(Valor), fill = Variable_etiqueta)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 1.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c(
    "Temperatura del aire" = "#FF6A6A",        
    "Temperatura de la superficie del suelo" =  "#8B2323",  
    "Termocupla 1 a los 5 cm" = "#CD3333", 
    "Termocupla 2 a los 5 cm" = "#EE3B3B"  
  )) +
  labs(title = "Distribución de temperaturas", 
       x = "", y = "°C") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text())

distribucion_temperaturas_2

# Guardar todos los gráficos en archivos PNG
ggsave("03_visualizacion_grafica/graficos/series_temporales.png", plot = series_temporales, width = 12, height = 6, dpi = 500)
ggsave("03_visualizacion_grafica/graficos/humedad_suelo.png", plot = Humedad_Suelo, width = 10, height = 6, dpi = 500)
ggsave("03_visualizacion_grafica/graficos/Distribucion_temperaturas.png", plot = distribucion_temperaturas_2 , width = 10, height = 6, dpi = 500)
ggsave("03_visualizacion_grafica/graficos/precipitacion_diaria.png", plot = Precipitacion, width = 10, height = 6, dpi = 500)
png("03_visualizacion_grafica/graficos/rosa_viento.png", width = 10, height = 6, units = "in", res = 500)
print(rosas_viento)  # Usar print() explícitamente
dev.off()

