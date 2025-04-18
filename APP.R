#Paquetes requeridos
# ------------------------------
#install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "lubridate", "forecast","shinydashboardPlus"))

library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(forecast)
library(knitr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
  
# Cargar datos
Ruta_datos <- X5_online_retail_II %>%
  filter(Quantity > 0, Price > 0) %>%
  mutate(Ventas = Quantity * Price)

Ruta_datos <- as.data.table(Ruta_datos)

# Interfaz UI
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Ventas",
                  dropdownMenu(type = "messages",
                               messageItem(from = "Grupo 4: Juan/Maria", 
                                           message = "¡Bienvenidos al dashboard en Shiny!")
                  )),
  dashboardSidebar(
    sidebarSearchForm("searchText", "buttonSearch", "Buscar", icon = icon("search")),
    sidebarMenu(
      menuItem("Estadísticas", tabName = "estadisticas", icon = icon("chart-bar")),
      menuItem("Regresión Lineal", tabName = "regresion", icon = icon("chart-line")),
      menuItem("ARIMA y Forecast", tabName = "arima", icon = icon("project-diagram")),
      menuItem("Informe Analítico", tabName = "informe", icon = icon("clipboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "estadisticas",
              
              fluidRow(
                valueBoxOutput("periodo_datos"),
                valueBoxOutput("total_registros"),
                valueBoxOutput("monto_total")
              ),
              
              fluidRow(
                box(title = "Análisis exploratorio del conjunto de datos", 
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("histograma"), width = 12)
              ),
              
              fluidRow(
                box(title = "Resumen de ventas", width = 6, solidHeader = TRUE, status = "primary",
                    verbatimTextOutput("resumenVentas")),
                box(title = "Descripción de la distribución", width = 6, solidHeader = TRUE,
                    uiOutput("Desc_destri"))
              ),
              
              fluidRow(
                box(title = "Top productos vendidos", width = 6, solidHeader = TRUE, status = "info",
                    plotlyOutput("topProductos")),
                box(title = "Distribución por país", width = 6, solidHeader = TRUE, status = "info",
                    plotlyOutput("ventasPorPais"))
              
              ),
              # 7: Histograma de ventas
              fluidRow(
                box(title = "Distribucion de facturas según su monto total de venta", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("histogramaVentas"))
              )
              
      ),
      tabItem(tabName = "regresion",
              fluidRow(
                box(title = "Regresión Lineal sobre Ventas Diarias", status = "warning", solidHeader = TRUE,
                    plotlyOutput("graficoRegresion"), width = 12),
                box(title = "Resumen del Modelo", uiOutput("modeloRegresion"), width = 12)
              )
      ),
      tabItem(tabName = "arima",
               
              fluidRow(
                valueBoxOutput("rmse_box"),
                valueBoxOutput("mae_box")
              ),
              
              fluidRow(
                box(title = "Pronóstico con ARIMA", status = "success", solidHeader = TRUE,
                    plotOutput("graficoForecast"), width = 12)
              ),

              fluidRow(
                box(title = "Detalles Pronóstico", width = 12, status = "success", solidHeader = TRUE,
                    uiOutput("metricas"))
              )
      ),
      tabItem(tabName = "informe",
              fluidRow(
                box(title = "Resumen General del Dataset", status = "info", solidHeader = TRUE, width = 12,
                    htmlOutput("resumen_general")),
                box(title = "Descripción de Variables", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput("tabla_variables")),
                                    box(title = "Tabla de Pronóstico de Ventas (30 días)", status = "success", solidHeader = TRUE, width = 12,
                    dataTableOutput("tabla_forecast")),
                box(title = "Mejora del modelo ARIMA", status = "warning", solidHeader = TRUE, width = 12,
                    htmlOutput("mejora_modelo")),
box(
  title = "Auditoría del dataset", 
  status = "success", 
  solidHeader = TRUE, 
  width = 12,
  downloadButton("descargar_auditoria", "Descargar Auditoría ARIMA")
)
              )
      )
    )
  )
)

# Creacion del servidor
server <- function(input, output) {
  
  datos_limpios <- reactive({
    Ruta_datos
  })
  
  output$histograma <- renderPlotly({
    p <- ggplot(datos_limpios(), aes(x = Ventas)) +
      geom_histogram(bins = 50, fill = "skyblue", color = "black") +
      scale_x_log10() +
      labs(title = "Distribución de Ventas (log)", x = "Ventas", y = "Frecuencia") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$resumenVentas <- renderPrint({
    summary(datos_limpios()$Ventas)
  })
  
output$Desc_destri <- renderUI({
  HTML("
      <p>Se evidencia que la distribución temporal abarca desde diciembre de 2009 hasta diciembre de 2010, lo que permite realizar análisis de series de tiempo y estacionalidad.</p>  
    <p>Podemos ver que las ventas diarias siguen una distribución altamente sesgada, con la mayoría de las transacciones por debajo de 20.</p>
    <p>Existen outliers extremos que influyen fuertemente en la media y en el modelo de predicción, razón por la cual es importante hacer transformaciones para modelar los datos.</p>
  ")
})
  
  output$graficoRegresion <- renderPlotly({
    ventas_diarias <- datos_limpios() %>%
      mutate(Fecha = as.Date(InvoiceDate)) %>%
      group_by(Fecha) %>%
      summarise(VentasTotales = sum(Ventas)) %>%
      mutate(Dias = as.numeric(Fecha - min(Fecha)))
    
    p <- ggplot(ventas_diarias, aes(x = Fecha, y = VentasTotales)) +
      geom_line(color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Regresión Lineal sobre Ventas Diarias", x = "Fecha", y = "Ventas") +
      theme_minimal()
    
    ggplotly(p)
  })
  
output$modeloRegresion <- renderUI({

  HTML("
    <p><strong>Los resultados del modelo indican una relación positiva y estadísticamente significativa entre el tiempo y las ventas:</strong></p>
    <ul>
      <li>El intercepto estimado es de <strong>23,123</strong>, lo que representa la venta diaria promedio estimada al inicio del periodo de análisis.</li>
      <li>El coeficiente de la variable <strong>Días</strong> es de <strong>54.67</strong>, lo que implica que, en promedio, las ventas diarias aumentan en 54.67 unidades monetarias por día a lo largo del tiempo.</li>
    </ul>
    <p>Esta tendencia ascendente está respaldada por un valor p menor a 0.001 (<em>p = 6.94e-10</em>), lo que indica una alta significancia estadística del modelo.</p>
    <p>Sin embargo, el coeficiente de determinación (<strong>R² = 0.1174</strong>) es relativamente bajo, lo que sugiere que el modelo solo explica el 11.74% de la variabilidad de las ventas diarias. Esto implica que, si bien existe una tendencia creciente en las ventas, otros factores no incluidos en el modelo (como promociones, estacionalidad, eventos externos o comportamiento del consumidor) podrían estar influyendo significativamente en los volúmenes de venta.</p>
    <p>Además, los residuos presentan una alta dispersión, con un error estándar residual de <strong>16,130</strong>, y extremos que varían entre <strong>-27,404</strong> y <strong>79,385</strong>. Esto refuerza la idea de que la línea de tendencia no captura completamente las fluctuaciones diarias, pero sí proporciona una dirección general del crecimiento de ventas en el tiempo.</p>
  ")
})
  
  output$graficoForecast <- renderPlot({
    ventas_diarias <- datos_limpios() %>%
      mutate(Fecha = as.Date(InvoiceDate)) %>%
      group_by(Fecha) %>%
      summarise(VentasTotales = sum(Ventas))
    
    ts_ventas <- ts(ventas_diarias$VentasTotales, frequency = 7)
    modelo_arima <- auto.arima(ts_ventas)
    forecast_arima <- forecast(modelo_arima, h = 30)
    
    autoplot(forecast_arima) +
      labs(title = "Pronóstico de Ventas (ARIMA)", x = "Tiempo", y = "Ventas")
  })
  
output$metricas <- renderUI({
  HTML("
    <p><strong>Interpretación del gráfico de pronóstico:</strong></p>
    <ul>
      <li><strong>Serie histórica</strong> (línea azul): Representa las ventas reales por día que has registrado hasta la fecha.</li>
      <li><strong>Línea de pronóstico</strong> (línea azul claro o gris oscuro hacia adelante): Esta línea muestra el pronóstico de ventas para los próximos 30 días.</li>
      <li><strong>Bandas de confianza</strong> (áreas sombreadas): Las bandas más oscuras representan un intervalo de confianza del 80%.</li>
      <li><strong>Bandas más claras</strong>: Representan un intervalo del 95%, más amplio, lo que indica mayor incertidumbre.</li>
    </ul>
  ")
})

output$mejora_modelo <- renderUI({
  HTML('
    <p>Debido a los altos valores del MAE y RMSE, se optó por mejorar el modelo de ARIMA para asegurar que los resultados sean adecuados. Para corregir el modelo se implementaron dos estrategias:</p>
    <ul>
      <li>Se ajustó un modelo ARIMA con <strong>estacionalidad</strong> para capturar patrones que se repiten en el tiempo (por ejemplo, semanalmente).</li>
      <li>Se aplicó una <strong>transformación logarítmica</strong> a las ventas, con el objetivo de estabilizar la varianza (útil si hay días con ventas muy altas en comparación con otros).</li>
    </ul>
    
    <h4>Comparación de modelos</h4>
    <table border="1" cellpadding="6" cellspacing="0">
      <thead style="background-color:#f0f0f0;">
        <tr>
          <th>Modelo</th>
          <th>RMSE</th>
          <th>MAE</th>
          <th>Notas</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>ARIMA Estacional</td>
          <td>26,241.42</td>
          <td>19,077.31</td>
          <td>El modelo estacional no mejora con relación al modelo anterior.</td>
        </tr>
        <tr>
          <td>ARIMA con log(ventas)</td>
          <td>0.6631</td>
          <td>0.5680</td>
          <td>Menores errores en escala logarítmica. Mejor ajuste relativo a variaciones proporcionales, no absolutas.</td>
        </tr>
      </tbody>
    </table>
    
    <p><strong>Interpretación y elección del modelo:</strong></p>
    <ul>
      <li>Si se quiere <strong>predecir el valor real</strong> de ventas, se recomienda mantener el modelo ARIMA original.</li>
      <li>Si el interés está en <strong>entender tendencias relativas o proporcionales</strong>, se recomienda el modelo logarítmico.</li>
    </ul>
    <p>Importante: aunque los errores del modelo logarítmico parecen mucho menores, los datos están en <strong>escala logarítmica</strong>, por lo que para comparar directamente se deben transformar a la escala real.</p>
  ')
})

  
output$resumen_general <- renderUI({
  HTML("
    <p>El conjunto de datos analizado corresponde a transacciones de una tienda en línea, con un total de <strong>525,461 registros</strong> y <strong>8 variables</strong> que describen detalles como productos vendidos, precios, cantidades, fechas de facturación, clientes y países de origen. Su objetivo es representar el comportamiento de compra de los clientes para facilitar análisis predictivos y de consumo.</p>
    
    <p><strong>A destacar las inconsistencias de los datos:</strong></p>
    <ul>
      <li>El dataset contiene <strong>110,855 valores nulos</strong>, concentrados en las variables <strong>Customer ID (107,927)</strong> y <strong>Description (2,928)</strong>.</li>
      <li><strong>Quantity</strong> incluye cantidades negativas, lo cual podría indicar devoluciones o errores de entrada de datos.</li>
      <li><strong>Price</strong> también contiene valores negativos, con un mínimo de <strong>-53,594.36</strong>, lo que puede representar descuentos, devoluciones o inconsistencias.</li>
    </ul>
    
    <p>Para los fines del análisis, los datos con valores nulos serán <strong>omitidos</strong>, mientras que los datos en blanco de <strong>Customer ID</strong> y <strong>Description</strong> no afectan al análisis, por lo cual <strong>no serán tratados</strong>.</p>
  ")
})
  
  output$resumen_general <- renderUI({
  HTML("
    <p>El conjunto de datos analizado corresponde a transacciones de una tienda en línea, con un total de <strong>525,461 registros</strong> y <strong>8 variables</strong> que describen detalles como productos vendidos, precios, cantidades, fechas de facturación, clientes y países de origen. Su objetivo es representar el comportamiento de compra de los clientes para facilitar análisis predictivos y de consumo.</p>
    
    <p><strong>A destacar las inconsistencias de los datos:</strong></p>
    <ul>
      <li>El dataset contiene <strong>110,855 valores nulos</strong>, concentrados en las variables <strong>Customer ID (107,927)</strong> y <strong>Description (2,928)</strong>.</li>
      <li><strong>Quantity</strong> incluye cantidades negativas, lo cual podría indicar devoluciones o errores de entrada de datos.</li>
      <li><strong>Price</strong> también contiene valores negativos, con un mínimo de <strong>-53,594.36</strong>, lo que puede representar descuentos, devoluciones o inconsistencias.</li>
    </ul>
    
    <p>Para los fines del análisis, los datos con valores nulos serán <strong>omitidos</strong>, mientras que los datos en blanco de <strong>Customer ID</strong> y <strong>Description</strong> no afectan al análisis, por lo cual <strong>no serán tratados</strong>.</p>
  ")
})
  
output$tabla_variables <- renderDataTable({
  data.frame(
    Variable = c("Invoice", "StockCode", "Description", "Quantity", "InvoiceDate", 
                 "Price", "Customer ID", "Country"),
    Tipo = c("Categórica", "Categórica", "Categórica", "Numérico", "Temporal", 
             "Numérico", "Numérico", "Categórica"),
    Descripción = c(
      "Identificador del número de factura.",
      "Código único del producto.",
      "Descripción del producto vendido.",
      "Cantidad de artículos comprados por línea.",
      "Fecha y hora de la transacción.",
      "Precio unitario del producto.",
      "Identificador anónimo del cliente.",
      "País desde donde se realizó la compra."
    ),
    Estadísticas = c(
      "525,461 entradas (sin valores nulos)",
      "525,461 entradas (sin valores nulos)",
      "2,928 valores nulos",
      "Mín: -9600, Mediana: 3, Máx: 19,152, Media: 10.34",
      "Rango: 01-12-2009 a 09-12-2010 (sin valores nulos)",
      "Mín: -53,594.36, Mediana: 2.10, Máx: 25,111.09, Media: 4.69",
      "107,927 valores nulos",
      "Solo contiene dos países"
    ),
    check.names = FALSE
  )
}, options = list(pageLength = 8, autoWidth = TRUE))

  
  output$descargar_auditoria <- downloadHandler(
  filename = function() {
    paste("auditoria_datos_limpios", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    datos <- datos_limpios()
    write.csv(datos, file, row.names = FALSE)
  }
)
  
output$tabla_forecast <- renderDataTable({
    ventas_diarias <- datos_limpios() %>%
      mutate(Fecha = as.Date(InvoiceDate)) %>%
      group_by(Fecha) %>%
      summarise(VentasTotales = sum(Ventas))
    ts_ventas <- ts(ventas_diarias$VentasTotales, frequency = 7)
    modelo_arima <- auto.arima(ts_ventas)
    forecast_arima <- forecast(modelo_arima, h = 30)
    data.frame(Día = 1:30, 
    `Pronóstico de Ventas` = round(as.numeric(forecast_arima$mean), 2),
    `Límite Inferior 80%` = round(forecast_arima$lower[, "80%"], 2),
    `Límite Superior 80%` = round(forecast_arima$upper[, "80%"], 2),
    `Límite Inferior 95%` = round(forecast_arima$lower[, "95%"], 2),
    `Límite Superior 95%` = round(forecast_arima$upper[, "95%"], 2))
  })
  
output$periodo_datos <- renderValueBox({
  datos <- datos_limpios()
  fecha_min <- min(datos$InvoiceDate)
  fecha_max <- max(datos$InvoiceDate)
  
  valueBox(
    paste(format(as.Date(fecha_min), "%b %Y"), " - ", format(as.Date(fecha_max), "%b %Y")),
    subtitle = "Periodo de los datos",
    icon = icon("calendar-alt"),
    color = "purple"
  )
})

output$total_registros <- renderValueBox({
  total <- nrow(datos_limpios())
  valueBox(
    formatC(total, format = "d", big.mark = ","),
    subtitle = "Total de registros",
    icon = icon("database"),
    color = "blue"
  )
})

output$monto_total <- renderValueBox({
  total_ventas <- sum(datos_limpios()$Ventas, na.rm = TRUE)
  valueBox(
    paste0("$", formatC(total_ventas, format = "f", big.mark = ",", digits = 2)),
    subtitle = "Monto total de ventas",
    icon = icon("dollar-sign"),
    color = "green"
  )
})

# Top productos vendidos
output$topProductos <- renderPlotly({
  top_productos <- datos_limpios() %>%
    group_by(Description) %>%
    summarise(total_ventas = sum(Quantity * Price, na.rm = TRUE)) %>%
    arrange(desc(total_ventas)) %>%
    slice_head(n = 10)
  
  plot_ly(top_productos,
          x = ~reorder(Description, total_ventas),
          y = ~total_ventas,
          type = "bar",
          marker = list(color = "#1f77b4")) %>%
    layout(title = "Top 10 productos vendidos",
           xaxis = list(title = "Producto"),
           yaxis = list(title = "Total de Ventas"),
           margin = list(b = 100))  # Espacio para etiquetas largas
})

# Distribución por país
output$ventasPorPais <- renderPlotly({
  ventas_pais <- datos_limpios() %>%
    group_by(Country) %>%
    summarise(total_ventas = sum(Quantity * Price, na.rm = TRUE)) %>%
    arrange(desc(total_ventas)) %>%
    slice_head(n = 10)
  

  plot_ly(ventas_pais,
          labels = ~Country,
          values = ~total_ventas,
          type = "pie",
          textinfo = "label+percent",
          insidetextorientation = "radial",
          rotation = -90) %>%  
    layout(title = "Distribución de ventas por país",
           showlegend = TRUE,
           margin = list(l = 20, r = 20, b = 50, t = 50))
  
})

# Histograma de ventas
output$histogramaVentas <- renderPlotly({
  ventas_por_factura <- datos_limpios() %>%
    group_by(Invoice) %>%
    summarise(total_venta = sum(Quantity * Price, na.rm = TRUE))
  
  plot_ly(x = ventas_por_factura$total_venta,
          type = "histogram",
          marker = list(color = "#2ca02c")) %>%
    layout(title = "Histograma de ventas por factura",
           xaxis = list(title = "Monto de Venta"),
           yaxis = list(title = "Frecuencia"))
})

# RMSE
output$rmse_box <- renderValueBox({
  ventas_diarias <- datos_limpios() %>%
    mutate(Fecha = as.Date(InvoiceDate)) %>%
    group_by(Fecha) %>%
    summarise(VentasTotales = sum(Ventas))
  
  train_size <- floor(0.8 * nrow(ventas_diarias))
  train <- ventas_diarias[1:train_size, ]
  test <- ventas_diarias[(train_size + 1):nrow(ventas_diarias), ]
  
  modelo_arima_train <- auto.arima(train$VentasTotales)
  forecast_test <- forecast(modelo_arima_train, h = nrow(test))
  
  rmse <- sqrt(mean((forecast_test$mean - test$VentasTotales)^2))
  
  valueBox(
    value = format(round(rmse, 2), big.mark = ","),
    subtitle = "RMSE (Error cuadrático medio)",
    icon = icon("chart-line"),
    color = "olive"
  )
})

# MAE
output$mae_box <- renderValueBox({
  ventas_diarias <- datos_limpios() %>%
    mutate(Fecha = as.Date(InvoiceDate)) %>%
    group_by(Fecha) %>%
    summarise(VentasTotales = sum(Ventas))
  
  train_size <- floor(0.8 * nrow(ventas_diarias))
  train <- ventas_diarias[1:train_size, ]
  test <- ventas_diarias[(train_size + 1):nrow(ventas_diarias), ]
  
  modelo_arima_train <- auto.arima(train$VentasTotales)
  forecast_test <- forecast(modelo_arima_train, h = nrow(test))
  
  mae <- mean(abs(forecast_test$mean - test$VentasTotales))
  
  valueBox(
    value = format(round(mae, 2), big.mark = ","),
    subtitle = "MAE (Error absoluto medio)",
    icon = icon("calculator"),
    color = "olive"
  )
})
}

# Correr APP
shinyApp(ui , server)