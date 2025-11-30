# Ordenamiento y estructuración de datos meteorológicos descargados de dataloggers

Para el proyecto de investigación sobre determinación del consumo hídrico de plantas ornamentales se instaló una estación meteorológica automática compacta. Esta estación registra variables meteorológicas cada 30 minutos, estas son; temperatura, humedad relativa, velocidad del viento, dirección del viento, presión atmosférica, radiación global y precipitación.

Por cuestiones de seguridad, los datos se descargan semanalmente y estos se van almacenado. El formato de los archivos es ".dat". Para garantizar la integridad y aplicabilidad de los datos, es necesario adjuntarlos de forma secuencial y posteriormente, verificar la calidad; realizar una exploración que integre estadística descriptiva y visualización.

## Hipotesis

Asi entonces, se plantea la siguiente hipótesis:

La implementación sistemática de un pipeline integral de procesamiento, visualización y control de calidad de datos es un requisito fundamental para transformar datos meteorológicos crudos en información confiable y aplicable, permitiendo la detección de patrones climáticos significativos y apoyando la toma de decisiones informadas en el manejo agrícola de la parcela.

# Metodología

## **Zona de estudio**

La zona de estudio del proyecto en donde se instaló la estación meteorológica, se encuentra ubicada en Carr. Gral. San Martín 7021, 8, Huechuraba, entre las coordenadas -33.354 de latitud y -70.691 de longitud, a una altitud de 490.87 msnm, Santiago de Chile, Chile.

![](area.png)

## **Análisis Estadístico de los Datos**

El análisis estadístico que se realizó consistió en un proceso exploratorio integral que comenzó con la carga y transformación de los datos, convirtiendo las variables a formato numérico y renombrando las columnas a un esquema en español para mayor claridad. Se calcularon estadísticas descriptivas fundamentales —como la media, mediana, desviación estándar, valores mínimo y máximo, y el conteo de valores faltantes— para las variables meteorológicas principales, entre las que se incluyeron la temperatura del aire, la humedad relativa, la velocidad del viento y la radiación global. Este análisis inicial proporcionó un resumen ejecutivo del período de datos, incluyendo las fechas de inicio y fin, la duración total en días y el número total de registros, sentando las bases para una comprensión cuantitativa del conjunto de datos antes de proceder con evaluaciones más profundas

## **Visualización de los Datos**

La visualización de datos se llevó a cabo mediante la generación de una serie de gráficos estáticos diseñados para revelar patrones temporales y distribucionales. Se utilizaron `ggplot2` y `patchwork` para crear paneles de series de tiempo que mostraron la evolución de variables atmosféricas (como radiación, viento y temperatura) y variables del suelo (como temperatura superficial y humedad a diferentes profundidades) a lo largo del tiempo. Además, se generó un boxplot para comparar la distribución de las diferentes temperaturas medidas, resaltando valores atípicos, y un gráfico circular o de rosa de los vientos para visualizar la dirección predominante del viento, agrupando los datos en sectores (N, NE, E, etc.). Estas visualizaciones se guardaron en archivos de imagen de alta calidad y se integraron automáticamente en el informe final

## **Control de Calidad de los Datos**

El control de calidad fue un proceso riguroso y automatizado que se aplicó para garantizar la integridad de los datos. Este proceso se compuso de varias verificaciones explícitas: **1) Verificación de rangos físicos plausibles:** Se definieron y evaluaron límites físicos para cada variable (por ejemplo, la humedad relativa debía estar entre 0% y 100%, y la radiación global entre 0 y 1500 W/m²), contabilizando todos los valores que se encontraron fuera de estos intervalos. **2) Detección de valores atípicos estadísticos:** Se utilizó el método del rango intercuartílico (IQR) para identificar observaciones atípicas en variables clave, calculando el número y porcentaje de estos valores que se desviaron significativamente de la distribución central. **3) Evaluación de la completitud:** Se calculó el porcentaje de datos no faltantes para las variables esenciales, lo que permitió identificar cuáles pudieron tener problemas de disponibilidad. **4) Control de la continuidad temporal:** Se detectaron saltos en la serie temporal identificando intervalos entre registros consecutivos que no fueron de una hora, lo que ayudó a encontrar huecos o inconsistencias en la frecuencia del muestreo. Los resultados de todas estas pruebas se reportaron detalladamente en tablas dentro del informe.
