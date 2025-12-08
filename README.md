# Consolidación de datos agrometeorológicos

Para un proyecto de investigación se instaló una estación meteorológica automática compacta y una serie de sensores que miden los flujos de energia dentro de una parcela de 1000 m-2, en donde se registran las variables cada 60 minutos dentro de un datalogger CR5000. Por cuestiones de seguridad, los datos se descargan semanalmente y estos se van almacenado. El formato de los archivos es ".dat". Para garantizar la integridad y aplicabilidad de los datos, es necesario adjuntarlos de forma secuencial y posteriormente, verificar la calidad.

En este repositorio se encuentran los procedimientos llevados a cabo para procesar los datos meteorologicos registrados; una exploración que integra estadística descriptiva y visualización gráfica, además de aplicar principios metodologicos para detectar la calidad de los mismos.

![](Sensores.jpg)

***Objetivo***

Implementar un conjunto de procesamientos, visualización y control de calidad para transformar datos meteorológicos crudos en información confiable y aplicable.

## Procesamiento de los datos

### Paqueterías utilizadas

Se usa el entorno de RStudio y los siguientes paquetes: `readxl`, `dplyr`, `ggplot2`, `tidyr`, `lubridate`, `patchwork`, `summarytools`, `officer`, `flextable`, `scales`, `tidyverse`, `lubridate`, `readr` y `writexl`.

### Variables agrometeorológicas registradas

-   Temperatura (°C)

-   Humedad relativa (%)

-   Velocidad del viento (m/s)

-   Dirección del viento (°)

-   Presión atmosférica (mbar)

-   Radiación global (Wm-2)

-   Precipitación (mm)

-   Temperatura superficial (°C)

-   Flujo de energía en el suelo (Wm-2)

-   Radiación Neta (Wm-2)

-   Temperatura termocupla 1 (°C)

-   Temperatura termocupla 2 (°C)

-   Humedad del suelo a los 5 cm (m3/m3)

-   Humedad del suelo a los 40 cm (m3/m3)

## Estructura del repositorio

``` text
├── README.md
├── 01_importar_datos/
│   ├── datos_crudos/        
│   ├── script_importacion.R  
│   └── datos_consolidados/    
├── 02_analisis_control_calidad/
│   ├── script_analisis_exploratorio.R   
│   ├── script_control_calidad.R
│   ├── datos_procesados/   
├── 03_visualizacion_grafica/
│   ├── script_visualizacion.R 
    └── graficos/ 
```

## Pasos para utilizar el repositorio

```         
1. Ejecutar 01_importar_datos/script_importacion.R
2. Ejecutar 02_analisis_control_calidad/script_analisis_exploratorio.R
            02_analisis_control_calidad/script_control_calidad.R
3. Ejecutar 03_visualizacion_grafica/script_visualizacion.R
```
