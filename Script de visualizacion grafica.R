
#Script para visualización grafica#

# Cargar librerías necesarias
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(openair)
library(ggpmisc)

# Leer el archivo Excel
datos <- read_excel("Datos_Parcela_3.xlsx")

# Renombrar variables según especificaciones
datos_renombrado <- datos %>%
  rename(
    Radiacion_global_Wm2 = SlrFD_W_Avg,
    Precipitacion_mm = Rain_mm_Tot,
    Rayos_conteo = Strikes_Tot,
    Velocidad_viento_ms = WS_ms_Avg,
    Direccion_viento_grados = WindDir,
    Rachas_ms = MaxWS_ms_Avg,
    Temperatura_aire_C = AirT_C_Avg,
    Presion_atmosferica_mbar = BP_mbar_Avg,
    Humedad_relativa_porc = RH,
    Radiacion_global_MJm2 = SlrTF_MJ_Tot,
    Temperatura_superficie_C = SBT_C_Avg,
    Flujo_energia_suelo_Wm2 = SHF_Avg,
    Radiacion_neta_Wm2 = NR_Wm2_Avg,
    Temperatura_termocupla1_C = Temp_TC1_C_Avg,
    Temperatura_termocupla2_C = Temp_TC2_C_Avg,
    Humedad_suelo_5cm_m3m3 = SWC_5CM_Avg,
    Humedad_suelo_40cm_m3m3 = SWC_40CM_Avg
  )

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

# 1. GRÁFICO DE SERIES TEMPORALES - VARIABLES PRINCIPALES
# Crear un dataset largo para facetas
datos_largo <- datos_renombrado %>%
  select(fecha_hora, 
         Radiacion_global_Wm2, 
         Temperatura_aire_C, 
         Humedad_relativa_porc,
         Precipitacion_mm,
         Velocidad_viento_ms,
         Presion_atmosferica_mbar) %>%
  pivot_longer(cols = -fecha_hora, names_to = "Variable", values_to = "Valor")

datos_largo$Valor<-as.numeric(datos_largo$Valor)

# Gráfico de series temporales múltiples
series_temporales<-ggplot(datos_largo, aes(x = fecha_hora, y = Valor, color = Variable)) +
  geom_line(linewidth = 0.3, alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(
    "Humedad_relativa_porc" = "blue",
    "Presion_atmosferica_mbar" = "darkgreen", 
    "Temperatura_aire_C" = "red",
    "Precipitacion_mm" = "cyan",
    "Radiacion_global_Wm2" = "orange",
    "Velocidad_viento_ms" = "purple",
    "Direccion_viento" = "brown"
  )) +
  labs(title = "Series Temporales de Variables Meteorológicas-Huechuraba",
       x = "Fecha y Hora", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = "none")

# 2. GRÁFICO DE RADIACIÓN vs TEMPERATURA

RADIACIÓN_TEMPERATURA<-ggplot(datos_renombrado, aes(x = as.numeric(Radiacion_global_Wm2), y = as.numeric(Temperatura_aire_C))) +
  geom_point(alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), 
                                 after_stat(rr.label), 
                                 after_stat(p.value.label), 
                                 sep = "~~~")),
               formula = y ~ x, 
               parse = TRUE,
               size = 4,
               label.x = 0.05, label.y = 0.95) +
  labs(title = "Radiación Global vs Temperatura del Aire",
       x = "Radiación Global (W/m²)", y = "Temperatura del Aire (°C)") +
  theme_bw()

# 3. GRÁFICO DE HUMEDAD DEL SUELO EN EL TIEMPO
datos_suelo <- datos_renombrado %>%
  select(fecha_hora, Humedad_suelo_5cm_m3m3, Humedad_suelo_40cm_m3m3) %>%
  pivot_longer(cols = -fecha_hora, names_to = "Profundidad", values_to = "Humedad")

datos_suelo$Humedad<-as.numeric(datos_suelo$Humedad)

Humedad_Suelo<-ggplot(datos_suelo, aes(x = fecha_hora, y = Humedad, color = Profundidad)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Humedad del Suelo a Diferentes Profundidades",
       x = "Fecha y Hora", y = "Humedad (m³/m³)",
       color = "Profundidad") +
  theme_bw() +
  theme(legend.position = "top")

# 4. GRÁFICO DE PRECIPITACIÓN ACUMULADA
Precipitacion<-datos_renombrado %>%
  mutate(fecha = as.Date(fecha_hora),
         Precipitacion_mm = as.numeric(Precipitacion_mm)) %>%
  group_by(fecha) %>%
  summarise(Precipitacion_diaria = sum(Precipitacion_mm, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha, y = Precipitacion_diaria)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(title = "Precipitación Diaria Acumulada",
       x = "Fecha", y = "Precipitación (mm)") +
  theme_bw()

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

# 6. GRÁFICO DE CORRELACIONES ENTRE VARIABLES
# Seleccionar variables numéricas para matriz de correlación
variables_cor <- datos_renombrado %>%
  select(Radiacion_global_Wm2, Temperatura_aire_C, Humedad_relativa_porc,
         Velocidad_viento_ms, Presion_atmosferica_mbar,
         Humedad_suelo_5cm_m3m3, Humedad_suelo_40cm_m3m3)

variables_cor_numeric <- variables_cor %>%
  mutate(across(everything(), as.numeric))

# Calcular matriz de correlación
cor_matrix <- cor(variables_cor_numeric, use = "complete.obs")

# Convertir a formato largo para ggplot
cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Var1 = Var1, Var2 = Var2, Correlacion = Freq)

correlaciones<-ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlacion)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlación") +
  geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 3) +
  labs(title = "Matriz de Correlación entre Variables",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. GRÁFICO DE DISTRIBUCION DE TEMPERATURAS 
datos_temp_2 <- datos_renombrado %>%
  select(Temperatura_aire_C, Temperatura_superficie_C, 
         Temperatura_termocupla1_C, Temperatura_termocupla2_C) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear etiquetas más cortas
etiquetas <- c(
  "Temperatura_aire_C" = "Temp Aire",
  "Temperatura_superficie_C" = "Temp Superficie", 
  "Temperatura_termocupla1_C" = "Termocupla 1",
  "Temperatura_termocupla2_C" = "Termocupla 2"
)

datos_temp_2$Variable_etiqueta <- factor(datos_temp_2$Variable,
                                       levels = names(etiquetas),
                                       labels = etiquetas)

# Crear el boxplot
distribucion_temperaturas_2<-ggplot(datos_temp_2, aes(x = Variable_etiqueta, y = as.numeric(Valor), fill = Variable_etiqueta)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 1.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "yellow") +
  labs(title = "Distribución de Temperaturas - Boxplot", 
       x = "Variables", y = "°C") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar todos los gráficos en archivos PNG
ggsave("series_temporales.png", plot = series_temporales, width = 12, height = 8, dpi = 500)
ggsave("radiacion_vs_temperatura.png", plot = RADIACIÓN_TEMPERATURA, width = 10, height = 6, dpi = 500)
ggsave("humedad_suelo.png", plot = Humedad_Suelo, width = 10, height = 6, dpi = 500)
ggsave("Distribucion_temperaturas.png", plot = distribucion_temperaturas_2 , width = 10, height = 6, dpi = 500)
ggsave("precipitacion_diaria.png", plot = Precipitacion, width = 10, height = 6, dpi = 500)
ggsave("boxplot_temperatura_hora.png", plot = Distribución_Temperatura, width = 10, height = 6, dpi = 500)
ggsave("matriz_correlacion.png", plot = correlaciones,  width = 10, height = 8, dpi = 500)
png("rosa_viento.png", width = 10, height = 6, units = "in", res = 500)
print(rosas_viento)  # Usar print() explícitamente
dev.off()

