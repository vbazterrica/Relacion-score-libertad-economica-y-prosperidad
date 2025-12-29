library(DT)
library(tidyr)        
library(dplyr)        
library(tidyverse)    
library(reshape2)     
library(forcats)      
library(purrr)     
library(ggplot2)      
library(corrplot)     
library(plotly)       
library(viridis)      
library(paletteer)
library(caret)        
library(randomForest) 
library(rpart)        
library(rpart.plot)   
library(e1071)         
library(class) 
library(shiny)        
library(shinydashboard)
library(knitr)        
library(kableExtra)   
library(collapsibleTree)
library(shinycssloaders)
library(httr)         
library(readtext)     
library(pdftools)  
library(psych)
library(glmnet)
library(Matrix)
library(rmarkdown)
library(writexl)


#--------------------------


#Parte previa
#Online data
url <- "https://www.heritage.org/index/pages/all-country-scores"
archivo_local <- tempfile(fileext = ".xlsx")
GET(url, write_disk(archivo_local))


# Leer el archivo Excel sin nonbres de columnas
datos <- readxl::read_excel("2024_indexofeconomicfreedom_data.xlsx", col_names =FALSE)

#Establecer la segunda fila como los nombres de la columnas
colnames(datos) <- datos[2,] #Se usa la segunda fila como nombre de columnas
#Limpiar las primeras dos filas (encabezados y datos no útiles)
datos <- datos[-(1:2),]


# Filtrado de datos - 

# Convertir los datos de las  columnas a los formatos numéricos
datos_limpios <- datos %>%
  dplyr::mutate(
    Year = as.integer(Year),  
    
    # Convertir las columnas numéricas, primero se manejan los "N/A" con na_if
    `Overall Score` = as.numeric(dplyr::na_if(`Overall Score`, "N/A")), 
    `Property Rights` = as.numeric(dplyr::na_if(`Property Rights`, "N/A")),
    `Government Integrity` = as.numeric(dplyr::na_if(`Government Integrity`, "N/A")),
    `Judicial Effectiveness` = as.numeric(dplyr::na_if(`Judicial Effectiveness`, "N/A")),
    `Tax Burden` = as.numeric(dplyr::na_if(`Tax Burden`, "N/A")),
    `Government Spending` = as.numeric(dplyr::na_if(`Government Spending`, "N/A")),
    `Fiscal Health` = as.numeric(dplyr::na_if(`Fiscal Health`, "N/A")),
    `Business Freedom` = as.numeric(dplyr::na_if(`Business Freedom`, "N/A")),
    `Labor Freedom` = as.numeric(dplyr::na_if(`Labor Freedom`, "N/A")),
    `Monetary Freedom` = as.numeric(dplyr::na_if(`Monetary Freedom`, "N/A")),
    `Trade Freedom` = as.numeric(dplyr::na_if(`Trade Freedom`, "N/A")),
    `Investment Freedom` = as.numeric(dplyr::na_if(`Investment Freedom`, "N/A")),
    `Financial Freedom` = as.numeric(dplyr::na_if(`Financial Freedom`, "N/A"))
  )

# Verificar el resultado con glimpse
glimpse(datos_limpios)



#Deteccion de valores nulos 
nulos_por_variable <- datos_limpios %>%
  summarise_all(~ sum(is.na(.)))

print(nulos_por_variable)

#Datos limpios sin NA
datos_limpios1 <- datos_limpios%>%
  drop_na()
#Verificar
glimpse(datos_limpios1)

#Comprobación
nulos_por_variable <- datos_limpios1 %>%
  summarise_all(~ sum(is.na(.)))

print(nulos_por_variable)


#Convertir los datos de  tible a un data frame
datos_limpios1_df <- as.data.frame(datos_limpios1)

head(datos_limpios1_df)

# Eliminar la columna 'Year'
datos_limpios1_df <- datos_limpios1_df %>%
  select(-Year, -Region)

# Verifica que la columna haya sido eliminada
colnames(datos_limpios1_df)

str(datos_limpios1_df)

#convertir datos a una tabla
datatable(datos_limpios1_df)

# ---------------------------------------  analisis -------------------------------


# ---------------------------------------  interactivo-------------------------------

# Lógica del servidor
server <- function(input, output) {
  
  # Crear un evento reactivo para cuando el botón "Calcular" es presionado
  observeEvent(input$calcular, {
    # Calcular el valor neto para ambos clusters
    resultado_cluster_1 <- calcular_valor_neto_ajustado(
      input$costo_inicial,
      input$costo_recurrente,
      beneficio_pib_cluster_1,
      input$tasa_descuento,
      input$anos,
      fiscalidad_cluster_1,
      gobernanza_cluster_1,
      input$ingreso_anual_bruto  # Ingresos netos desde el slider
    )
    
    resultado_cluster_2 <- calcular_valor_neto_ajustado(
      input$costo_inicial,
      input$costo_recurrente,
      beneficio_pib_cluster_2,
      input$tasa_descuento,
      input$anos,
      fiscalidad_cluster_2,
      gobernanza_cluster_2,
      input$ingreso_anual_bruto  # Ingresos netos desde el slider
    )
    
    # Mostrar los resultados de valor neto ajustado
        output$valor_neto_cluster_1 <- renderText({
          paste("El Valor Neto Ajustado para el Cluster 1 es: $", round(resultado_cluster_1$valor_neto, 2))
        })
        
        output$valor_neto_cluster_2 <- renderText({
          paste("El Valor Neto Ajustado para el Cluster 2 es: $", round(resultado_cluster_2$valor_neto, 2))
        })
        
        # Mostrar el ingreso esperado para el Cluster 1
        output$ingreso_esperado_cluster_1 <- renderText({
          paste("El Ingreso Esperado para el Cluster 1 es: $", round(resultado_cluster_1$ingreso_esperado, 2))
        })
        
        # Mostrar el ingreso esperado para el Cluster 2
        output$ingreso_esperado_cluster_2 <- renderText({
          paste("El Ingreso Esperado para el Cluster 2 es: $", round(resultado_cluster_2$ingreso_esperado, 2))
        })
        
        # Gráfico interactivo de Plotly para el Cluster 1
        output$grafico_cluster_1 <- renderPlotly({
          plot_ly(
            x = 1:input$anos, 
            y = resultado_cluster_1$beneficios_desc, 
            type = "scatter", 
            mode = "lines", 
            name = "Beneficios Cluster 1", 
            line = list(color = '#d6496c')
          ) %>%
            add_trace(y = resultado_cluster_1$costos_desc, name = "Costos Cluster 1", line = list(color = '#d6496c')) %>%
            layout(title = "Beneficios y Costos Descontados para Cluster 1")
        })
        
        # Gráfico interactivo de Plotly para el Cluster 2
        output$grafico_cluster_2 <- renderPlotly({
          plot_ly(
            x = 1:input$anos, 
            y = resultado_cluster_2$beneficios_desc, 
            type = "scatter", 
            mode = "lines", 
            name = "Beneficios Cluster 2", 
            line = list(color = '#2ca02c')
          ) %>%
            add_trace(y = resultado_cluster_2$costos_desc, name = "Costos Cluster 2", line = list(color = '#2ca02c')) %>%
            layout(title = "Beneficios y Costos Descontados para Cluster 2")
        })
      })
}



# --------------------------------------- UI ----------------------------------
# --------------------------------------- UI ----------------------------------
# --------------------------------------- UI ----------------------------------





ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title =tags$span(
    style = "font-size: 15px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
    "Indice de Libertad Económica")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio"),
      menuItem("Informacion", tabName = "info"),
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("EDA", tabName = "eda"),
      menuItem("Modelos", tabName = "modelos",
               menuSubItem("Cluster", tabName = "cluster"),
               menuSubItem("Análisis", tabName = "analisis"),
               menuSubItem("Nuevas Variables", tabName = "nuevas_variables", icon = icon("database")),
               menuSubItem("Regresion", tabName = "regresion"),
               menuSubItem("Modelo de Inversion", tabName = "inversion")
      ),
      menuItem("Interactivo", tabName = "interactivo")
    )
  ),
  
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-green .main-header {
            background-color: #00796b;
            color: white;
        }
        .skin-green .main-header .navbar {
            background-color: #004d40;
        }
        .btn-primary {
            background-color: #0288d1;
            border-color: #0288d1;
        }
        .btn-primary:hover {
            background-color: #0277bd;
            border-color: #0277bd;
        }
        .content-wrapper {
            background-color: #f0f0f0;
        }
        .box {
            background-color: #ffffff;
            border-radius: 10px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "inicio",
              h2("Inicio"),
              fluidRow(
                box(width = 12, height = "500px", 
                    img(src = "inicio.jpg", style = "width: 100%; height: 100%; object-fit: contain;"))
              )
      ),
      
      tabItem(tabName = "info",
              h2("Informacion"),
              fluidRow(
                box(width = 12, height = "500px", 
                    img(src = "t1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "te2.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "t3.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "te4.jpg", style = "width: 100%; height: 100%; object-fit: contain;"))
              )
      ),
      
      tabItem(tabName = "dataset",
              h2("Dataset"),
              fluidRow(
                box(title = "Forma de los Datos", status = "success", solidHeader = TRUE, width = 12, tableOutput("datos")),
                box(title = "Datos limpios", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("datos_limpios1_df_str")),
                box(title = "Datos Faltantes", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("colSum_dat")),
                box(title = "Estadísticas Descriptivas", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("datos_limpios1_df_summary")),
                box(title = "Tabla de datos", status = "success", width = 12, solidHeader = TRUE, DTOutput("table_data")),
                box(title = "Acciones",status = "primary", width = 12, solidHeader = TRUE,
                    downloadButton("download_excel", "Descargar como Excel"),
                    downloadButton("download_pdf", "Descargar como PDF")
                )
              )
      ),
      
      tabItem(tabName = "eda",
              h2("EDA"),
              fluidRow(
                box(title = "Distribución del Overall Score", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("graf1")),
                box(title = "Identificación de Outliers en Overall Score", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("graf3")),
                box(title = "Boxplot de Variables del Score", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("graf4")),
                box(title = "Matriz de Correlación Interactiva", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("graf_corr")),
                box(title = "Top 10 Países con Mejor Score", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("grafico_mejor_score")),
                box(title = "Grafico Países con Peor Score", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("grafico_peor_score")),
                box(title = "Relación entre Business Freedom y Overall Score", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("graf5")),
                box(title = "Relación entre el overall score e integridad gubernamental", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("grafico_dispersion1")),
                box(title = "Relación entre el overall score y la Efectividad de la Justicia", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("grafico_dispersion2")),
                box(title = "Relación entre el overall score y el nivel de gasto público", status = "info", solidHeader = TRUE, width = 12, plotlyOutput("grafico_dispersion3"))
                
              )
      ),
      
      tabItem(tabName = "cluster",
              h2("Cluster"),
              fluidRow(
                box(width = 12, height = "500px", 
                    img(src = "codo1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "cluster1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Gráfico codo", width = 12, plotlyOutput("codo_interactivo")),
                box(width = 12, height = "500px", 
                    img(src = "pca.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Varianza explicada", width = 12, plotlyOutput("grafico_varianza")),
                box(title = "Grafico carga de las variables", width = 12, plotlyOutput("fig_loadings")),
                box(title = "Cluster pais", width = 12, plotlyOutput("fig_cluster_pais")),
                box(width = 12, height = "500px", 
                    img(src = "cluster.jpg", style = "width: 100%; height: 100%; object-fit: contain;"))
                
                
                
              )
      ),
      
      tabItem(tabName = "analisis",
              h2("Análisis"),
              fluidRow(
                box(width = 12, height = "500px", 
                    img(src = "rf.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Resultado RF", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("rf")),
                box(title = "Importancia de las variables", width = 12, plotlyOutput("graf_importancia")),
                box(width = 12, height = "500px", 
                    img(src = "msv.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "msv1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Resultado Validación cruzada máquina MSV", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("val_msv")),
                box(title = "Matrices de RF total", width = 3, plotlyOutput("graf_matriz_rf")),
                box(title = "Matrices de RF selección variables", width = 3, plotlyOutput("graf_matriz_rf2")),
                box(title = "Matrices de RF balanceado", width = 3, plotlyOutput("graf_matriz_rf3")),
                box(title = "Matrices de MSV", width = 3, plotlyOutput("graf_matriz_msv")),
               
                box(title = "Comparación de métricas", width = 12, plotlyOutput("grafico_metricas")),
                box(title = "Métricas de cruzamiento de datos", width = 12, plotlyOutput("grafico_cruzamiento")),
                
                
                
                
              )
      ),
      
      tabItem(tabName = "nuevas_variables",
              h2("Nuevas Variables"),
              fluidRow(
                box(title = "Datatet con nuevos datos ingresados", status = "success", solidHeader = TRUE, width = 12, tableOutput("datos_clustering_nuevo")),
                box(title = "Tabla con nuevos datos", status = "success", width = 12, solidHeader = TRUE, DTOutput("table_datos_clustering_nuevo")),
                box(title = "Acciones", status = "primary", width = 12, solidHeader = TRUE,
                    downloadButton("download_excel", "Descargar como Excel"),
                    downloadButton("download_pdf", "Descargar como PDF"),
                    downloadButton("download_rmarkdown", "Descargar el trabajo completo en PDF")),
                box(title = "Nuevos datos", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("datos_clustering_nuevo")),
                box(title = "caracteristicas datos", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("datos_clustering_nuevo_str")),
                box(title = "Estadísticas Descriptivas", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("summary_data")),
                box(title = "Distribución de frecuencia", width = 12, plotlyOutput("graf_frec_nuevas_var")),
                box(title = "Correlacion", width = 12, plotlyOutput("graf_cor1")),
                box(title = "Variables más correlacionadas", status = "success", width = 12, solidHeader = TRUE, verbatimTextOutput("alta_correlation_sorted")),
                box(title = "Comparación cluster", width = 12, plotlyOutput("fig_comp_cluster")),
                box(title = "Comparación score y pib per capita", width = 12, plotlyOutput("fig_cluster_2"))
                
                
              )
      ),
      
      tabItem(tabName = "regresion",
              h2("Regresión"),
              fluidRow(
                box(width = 12, height = "500px", 
                    img(src = "reg.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Modelo de regresion lasso Cluster1", status = "success", width = 6, solidHeader = TRUE, verbatimTextOutput("modelo1")),
                box(title = "Modelo de regresion lasso Cluster2", status = "success", width = 6, solidHeader = TRUE, verbatimTextOutput("modelo2")),
                box(title = "Ecuación regresión cluster del  Cluster1", status = "success", width = 6, solidHeader = TRUE, verbatimTextOutput("ecuacion1")),
                box(title = "Ecuación regresión cluster del  Cluster2", status = "success", width = 6, solidHeader = TRUE, verbatimTextOutput("ecuacion2")),
                box(title = "Comparación de métricas mse", width = 12, plotlyOutput("graf_pred_mse")),
                box(title = "Comparación de métricas R2 y Lambda", width = 12, plotlyOutput("graf_metricas")),
                box(title = "Comparación de residuos", width = 12, plotlyOutput("graf_residuos")),
                box(width = 12, height = "500px", 
                    img(src = "prueba.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Resultado de comparación de modelo", width = 12, plotlyOutput("grafico_comparativo"))
                
              )
      ),
      
      tabItem(tabName = "inversion",
              h2("Modelo de Inversión"),
              fluidRow(
                box(width = 12, height = "500px", 
                    img(src = "inve1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "modelo.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Valor neto", width = 12,status = "success", plotlyOutput("grafico_cluster_inv")),
                box(width = 12, height = "500px", 
                    img(src = "modelo1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Valor neto con  modelo ajustado", width = 12,status = "success", plotlyOutput("grafico_cluster_ajustado")),
                box(title = "Grafico 3d", width = 12, plotlyOutput("grafico_3d")),
                box(width = 12, height = "500px", 
                    img(src = "costos.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Grafico cambio de costos", width = 12, plotlyOutput("grafico_costos")),
                box(width = 12, height = "500px", 
                    img(src = "tir.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "tir1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Resultado Tasa interna de retorno por cluster", width = 12, plotlyOutput("resultados_tir")),
                box(width = 12, height = "500px", 
                    img(src = "tasa.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(width = 12, height = "500px", 
                    img(src = "tasa1.jpg", style = "width: 100%; height: 100%; object-fit: contain;")),
                box(title = "Análisis del Valor Presente ante cambios en la tasa de descuento", width = 12, plotlyOutput("grafico_tasas_rentabilidad"))
                
                
              )
      ),
      
      tabItem(tabName = "interactivo",
              h2("Interactivo"),
              fluidRow(
                box(title = "Parámetros de Cálculo", width = 6,
                    sliderInput("costo_inicial", "Costo Inicial:", min = 5000, max = 100000, value = 10000, step = 1000),
                    sliderInput("costo_recurrente", "Costo Recurrente:", min = 5000, max = 500000, value = 50000, step = 100),
                    sliderInput("ingreso_esperado", "Ingreso Esperado:", min = 0, max = 1000000, value = 50000, step = 100),
                    sliderInput("tasa_descuento", "Tasa de Descuento:", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
                    selectInput("anos", "Número de Años:", choices = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), selected = 10),
                    actionButton("calcular", "Calcular"),
                ),
                box(title = "Resultados Cluster 1", width = 6, textOutput("valor_neto_cluster_1")),
                box(title = "Resultados Cluster 2", width = 6, textOutput("valor_neto_cluster_2")),
                
              ),
              fluidRow(
                # Gráfico Cluster 1
                box(title = "Gráfico Cluster 1", width = 6, plotlyOutput("grafico_cluster_1")),
                
                # Gráfico Cluster 2
                box(title = "Gráfico Cluster 2", width = 6, plotlyOutput("grafico_cluster_2"))
              )
      )
    )
  )
)


server <- function(input, output) {
  
  # Mostrar la estructura del dataframe
  output$datos <- renderPrint({
    glimpse(datos)  # Muestra la estructura de los datos
  })
  
  # Mostrar la estructura del dataframe
  output$datos_limpios1_df_str <- renderPrint({
    str(datos_limpios1_df)  # Muestra la estructura de los datos
  })
  
  # Mostrar datos faltantes por columna
  output$colSum_dat <- renderPrint({
    nulos_por_variable <- datos_limpios %>%
      summarise_all(~ sum(is.na(.)))
    print(nulos_por_variable)
  })
  
  # Mostrar estadísticas descriptivas (summary básico)
  output$datos_limpios1_df_summary <- renderPrint({
    summary(datos_limpios1_df)
  })
  
  # Tabla de datos
  output$table_data <- renderDT({
    datatable(datos_limpios1_df)  # Muestra la tabla interactiva
  })
  
  # Función para exportar los datos a Excel
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("datos_limpios1_df_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(datos_limpios1_df, file)  # Exporta el dataframe a un archivo Excel
    }
  )
  
  # Función para exportar los datos a PDF
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("datos_limpios1_df_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(
        c(
          "---",
          "title: 'Resumen de Datos'",
          "output: pdf_document",
          "---",
          "",
          "```{r}",
          "summary(datos_limpios1_df)",
          "```"
        ),
        temp_rmd
      )
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  
  #=======================================================
  #==========================EDA================
  #1er grafico
  output$graf1 <- renderPlotly({
    graf1 <- plot_ly(datos_limpios1_df, x = ~`Overall Score`, type = 'histogram', nbinsx = 30, marker = list(color = '#5cacc4', line = list(color = '#14888b', width = 2))) %>%
      layout(title = "Distribución del Overall Score", 
             xaxis = list(title = "Overall Score"),
             yaxis = list(title = "Frecuencia"))
  })
  
  #2do grafico
  output$graf3 <- renderPlotly({
    graf3 <- plot_ly(datos_limpios1_df, y = ~`Overall Score`, type = 'box', 
                     boxmean = "sd",  # Esto incluye una línea para la media y el rango de la caja.
                     marker = list(color = '#5cacc4',  # Cambiar el color de los outliers
                                   line = list(color = 'black', width = 1))) %>%
      layout(title = "Identificación de Outliers en Overall Score",
             yaxis = list(title = "Overall Score"),
             boxplot = list(marker = list(color = '#4eb3de')))
    graf3
  })
  
  #3r grafico
  output$graf4 <- renderPlotly({
    # Definir las columnas de interés
    columnas <- c("Judicial Effectiveness", "Fiscal Health", "Monetary Freedom", 
                  "Financial Freedom", "Property Rights", "Tax Burden", 
                  "Business Freedom", "Trade Freedom", "Government Integrity", 
                  "Government Spending", "Labor Freedom", "Investment Freedom")
    
    # Filtrar el dataframe solo con las columnas seleccionadas
    datos_boxplot <- datos_limpios1_df[, columnas]
    
    # Convertir los datos a formato largo (long format) para plotly
    datos_largos <- datos_boxplot %>%
      pivot_longer(cols = everything(), 
                   names_to = "Variable", 
                   values_to = "Valor")
    
    # Crear el gráfico de cajas con un solo color
    
    
    graf4 <- plot_ly(data = datos_largos, 
                     x = ~Variable, 
                     y = ~Valor, 
                     type = 'box', 
                     boxmean = TRUE,  # Incluir la línea de la media
                     marker = list(color = '#5cacc4'),  
                     line = list(color = 'black')) %>%
      layout(title = "Boxplot de Variables del Score",
             yaxis = list(title = "Valor"),
             xaxis = list(title = "Variables"),
             boxmode = "group")  # Mostrar las cajas de forma agrupada
    
    ggplotly(graf4)
  })
  
  
  #4t  graf 
  
  output$graf_corr <- renderPlotly({
    correlation_long <- as.data.frame(as.table(correlation_matrix))
    
    # Crear el gráfico interactivo con plotly
    graf_corr <- plot_ly(data = correlation_long, 
                         x = ~Var1, y = ~Var2, z = ~Freq, 
                         type = "heatmap", 
                         colors = "viridis",  # Cambiar la paleta de colores a viridis en minúsculas
                         colorbar = list(
                           title = "Correlación", 
                           tickvals = c(-1, 0, 1),
                           ticktext = c("-1", "0", "1"),
                           tickcolor = "black"
                         ),
                         hoverinfo = "text", 
                         # Mostrar las variables y su correlación en el hover
                         text = ~paste("Variables: ", Var1, " vs ", Var2, "<br>Correlación: ", round(Freq, 2))) %>%
      layout(
        title = "Matriz de Correlación Interactiva",
        xaxis = list(
          title = "Variables", 
          tickangle = 45,  # Rotación de las etiquetas del eje X a 45° para facilitar la visualización
          tickmode = "array",  # Asegurarse de que se muestren todas las variables
          showticklabels = TRUE,
          tickfont = list(size = 8, family = "Arial", weight = "bold"),  # Negrita y tamaño de las etiquetas X
          tickvals = correlation_long$Var1  # Asegurarse de que todas las etiquetas se muestren
        ),
        yaxis = list(
          title = "Variables",
          tickangle = 0,  # Mantener las etiquetas del eje Y sin rotar
          showticklabels = TRUE,
          tickfont = list(size = 8, family = "Arial", weight = "bold")  # Negrita y tamaño de las etiquetas Y
        ),
        showlegend = FALSE,  # Ocultar la leyenda
        width = 700,  # Ajustar el tamaño del gráfico
        height = 550,  # Ajustar el tamaño del gráfico
        margin = list(t = 80, b = 150, l = 100, r = 100),  # Ajuste de márgenes
        xaxis = list(showgrid = TRUE),  # Activar líneas de cuadrícula en el eje X
        yaxis = list(showgrid = TRUE)   # Activar líneas de cuadrícula en el eje Y
      )
    graf_corr
  })
  
  
  #5to graf
  output$grafico_mejor_score <- renderPlotly({
    # Ordenar los top 10 países con el mejor score de mayor a menor
    top_10_mejor_score <- top_10_mejor_score %>%
      arrange(desc(`Overall Score`))
    
    # Ordenar los top 10 países con el peor score de menor a mayor
    bottom_10_peor_score <- bottom_10_peor_score %>%
      arrange(`Overall Score`)
    
    # Asegurarnos de que las barras se ordenen correctamente en el gráfico
    top_10_mejor_score$Country <- factor(top_10_mejor_score$Country, levels = top_10_mejor_score$Country)
    bottom_10_peor_score$Country <- factor(bottom_10_peor_score$Country, levels = bottom_10_peor_score$Country)
    
    # Crear el gráfico para los top 10 países con el mejor score (de mayor a menor)
    grafico_mejor_score <- plot_ly(data = top_10_mejor_score, 
                                   x = ~Country, y = ~`Overall Score`, 
                                   type = 'bar', 
                                   marker = list(color = '#ef6771'),  
                                   text = ~paste('Score: ', `Overall Score`),  # Mostrar el score en el hover
                                   hoverinfo = 'text') %>%
      layout(
        title = 'Top 10 Países con Mejor Score',
        xaxis = list(title = 'Países', tickangle = 45),
        yaxis = list(title = 'Overall Score'),
        showlegend = FALSE
      )
    
    
    # Mostrar ambos gráficos
    grafico_mejor_score
  })
  
  #6to graf
  
  output$grafico_peor_score <- renderPlotly({
    # Crear el gráfico para los bottom 10 países con el peor score (de menor a mayor)
    grafico_peor_score <- plot_ly(data = bottom_10_peor_score, 
                                  x = ~Country, y = ~`Overall Score`, 
                                  type = 'bar', 
                                  marker = list(color = '#5cacc4'),  
                                  text = ~paste('Score: ', `Overall Score`),  # Mostrar el score en el hover
                                  hoverinfo = 'text') %>%
      layout(
        title = 'Grafico Países con Peor Score',
        xaxis = list(title = 'Países', tickangle = 45),
        yaxis = list(title = 'Overall Score'),
        showlegend = FALSE
      )
    grafico_peor_score
  })
  
  #graf score y libertad
  output$graf5 <- renderPlotly({
    graf5<- plot_ly(datos_limpios1_df, x = ~`Business Freedom`, y = ~`Overall Score`, type = 'scatter', mode = 'markers',marker = list(color = '#ef6771', line = list(color = '#c95c7a', width = 2)) ) %>%
      layout(title = "Relación entre Business Freedom y Overall Score",
             xaxis = list(title = "Business Freedom"),
             yaxis = list(title = "Overall Score"))
    
    ggplotly(graf5)
  })
  
  
  #graf score e integridd gubernamental
  output$grafico_dispersion1  <- renderPlotly({
    grafico_dispersion <- plot_ly(datos_limpios1, x = ~`Government Integrity`, y = ~`Overall Score`,
                                  type = "scatter", mode = "markers", 
                                  marker = list(color = '#ef6771', line = list(color = '#c95c7a', 
                                                                               width = 2))) %>%
      layout(title = "Relación entre el overall score y la integridad gubernamental",
             xaxis = list(title = "Government Integrity"),
             yaxis = list(title = "Overall Score"))
    
    # Mostrar el gráfico
    grafico_dispersion
  })
  
  
  #graf score y efectividad de la justicia
  output$grafico_dispersion2 <- renderPlotly({
    grafico_dispersion2 <- plot_ly(datos_limpios1_df, x = ~`Judicial Effectiveness`, y = ~`Overall Score`,
                                   type = "scatter", mode = "markers", 
                                   marker = list(color = '#ef6771', line = list(color = '#c95c7a', 
                                                                                width = 2))) %>%
      layout(title = "Relación entre el overall score y la Efectividad de la Justicia",
             xaxis = list(title = "Judicial Effectiveness"),
             yaxis = list(title = "Overall Score"))
    
    # Mostrar el gráfico
    grafico_dispersion2
  })
  #graf overal score y gasto publico
  output$grafico_dispersion3 <- renderPlotly({
    grafico_dispersion3 <- plot_ly(datos_limpios1_df, x = ~`Government Spending`, y = ~`Overall Score`,
                                   type = "scatter", mode = "markers", 
                                   marker = list(color = '#ef6771', line = list(color = '#c95c7a', 
                                                                                width = 2))) %>%
      layout(title = "Relación entre el overall score y el nivel de gasto público",
             xaxis = list(title = "Government Spending"),
             yaxis = list(title = "Overall Score"))
    
    # Mostrar el gráfico
    grafico_dispersion3
  })
  
  #========================================================================
  #================================CLUSTER===========================
  
    output$codo_interactivo <- renderPlotly({
    
    # Crear el gráfico con ggplot2 y personalizar colores
    # Crear el gráfico con ggplot2 y personalizar colores
    gg <- ggplot(data.frame(K = 1:10, Inercia = inercia), aes(x = K, y = Inercia)) +
      geom_line(color = "#7db8a2", size = 1.2) +  # Línea de color azul
      geom_point(color = "#ef6771", size = 4) +    # Puntos de color naranja
      labs(title = "Método del Codo para Determinar el Número de Clústeres",
           x = "Número de Clústeres",
           y = "Inercia") +
      theme_minimal() +                           # Estilo minimalista
      theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
    
    # Convertir el gráfico ggplot en interactivo con plotly
    codo_interactivo <- ggplotly(gg)
    
    # Mostrar el gráfico interactivo
    codo_interactivo
  })
  output$table_pca  <- renderPrint({
    datatable(pca_df)
  })
  
  #tabla con pca
  output$pca_datatable  <- renderPrint({
    datatable(pca_df)
  })
  
  
  #Varianza explicada
  output$grafico_varianza <- renderPlotly({
    # Obtener la proporción de varianza y la varianza acumulada
    varianza_explicada <- summary(pca_resultado)$importance[2, ]  # Proporción de varianza
    varianza_acumulada <- summary(pca_resultado)$importance[3, ]  # Varianza acumulada
    
    # Crear el gráfico de barras para la varianza explicada
    grafico_varianza <- plot_ly(
      x = 1:length(varianza_explicada),  # Componentes principales
      y = varianza_explicada,  # Proporción de varianza explicada
      type = 'bar',
      name = 'Varianza explicada',
      marker = list(color = "#7db8a2")
    )
    
    # Agregar la varianza acumulada en el mismo gráfico
    grafico_varianza <- grafico_varianza %>%
      add_trace(
        x = 1:length(varianza_acumulada),
        y = varianza_acumulada,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Varianza acumulada',
        line = list(color = '#ef6771', shape = 'linear'),
        marker = list(color = '#ef6771')
      )
    
    # Personalizar el gráfico
    grafico_varianza <- grafico_varianza %>%
      layout(
        title = "Explicación de la Varianza por Componentes Principales",
        xaxis = list(
          title = "Componentes Principales",
          tickmode = "linear",
          dtick = 1
        ),
        yaxis = list(
          title = "Proporción de Varianza",
          rangemode = "tozero"
        ),
        barmode = 'group'
      )
    
    # Mostrar el gráfico
    grafico_varianza
  })
  
  #carga de las variables
  output$fig_loadings   <- renderPlotly({
    
    # Convertir las cargas en un data frame
    cargas_2_componentes_df <- data.frame(
      variable = rownames(cargas_2_componentes),
      PC1 = cargas_2_componentes[, 1],
      PC2 = cargas_2_componentes[, 2]
    )
    
    # Crear un gráfico de barras interactivo
    fig_loadings <- plot_ly(cargas_2_componentes_df, 
                            x = ~variable, 
                            y = ~PC1, 
                            type = 'bar', 
                            name = 'Componente Principal 1',
                            marker = list(color = '#d6496c')) %>%
      add_trace(y = ~PC2, 
                name = 'Componente Principal 2', 
                marker = list(color = '#7db8a2')) %>%
      layout(
        title = 'Cargas de las Variables en los Componentes Principales',
        xaxis = list(title = 'Variables', tickangle = 45),
        yaxis = list(title = 'Carga'),
        barmode = 'group',
        hovermode = 'closest',
        showlegend = TRUE,
        margin = list(b = 150)
      )
    
    # Mostrar el gráfico
    fig_loadings
  })
  
  
  output$fig_cluster_pais  <-  renderPlotly({
    # Crear el gráfico interactivo con Plotly incorporandolos paises
    fig_cluster_pais <- plot_ly(pca_df, 
                                x = ~PC1, 
                                y = ~PC2, 
                                type = 'scatter', 
                                mode = 'markers', 
                                color = ~Cluster, 
                                colors = RColorBrewer::brewer.pal(length(unique(pca_df$Cluster)), "Set1"),  # Paleta de colores
                                text = ~paste("Country: ", Country, "<br>Cluster: ", Cluster),  # Agregar país y clúster al texto emergente
                                hoverinfo = 'text',  # Mostrar información cuando se pase el mouse
                                marker = list(size = 10, opacity = 0.6, line = list(width = 1, color = 'rgb(40, 40, 40)'))) %>%
      layout(title = "Clustering K-means usando PCA y paises",
             xaxis = list(title = "Componente Principal 1"),
             yaxis = list(title = "Componente Principal 2"),
             showlegend = TRUE)
    
    # Mostrar el gráfico
    fig_cluster_pais
  })
  output$rf  <- renderPrint({
    #RANDOM FOREST TOTAL VARIABLES
    # Particionar los datos en conjuntos de entrenamiento y prueba
    set.seed(42)
    seleccion1 <- sample(x = 1:nrow(datos_clustering), nrow(datos_clustering) * 0.7, replace = FALSE)
    training1 <- datos_clustering[seleccion1, ]
    testing1 <- datos_clustering[-seleccion1, ]
    
    # Crear modelo random forest
    rf_model1 <- randomForest(Cluster ~ ., data = training1)
    
    
    # Visualizar el resumen del modelo
    #print(rf_model1)
    predictions1 <- predict(rf_model1, newdata = testing1)
    confusion1<-   confusionMatrix(predictions1, testing1$Cluster)
    print(confusion1)
  })
  #Importancia de las variables
  output$graf_importancia <- renderPlotly({
    # Crear gráfico interactivo con plotly
    colors <- c("#8de0a6", "#FEE08B", "#FDAE61", "#f27c7c", "#de528c")
    graf_importancia <- plot_ly(top_variables, x = ~variable, y = ~Importance, type = 'bar',textposition = 'auto',
                                marker = list(color = colors)) %>%
      layout(title = 'Importancia de las Variables',
             xaxis = list(title = 'Variables',categoryorder = 'total descending'),
             yaxis = list(title = 'Importancia')
      )
    
    graf_importancia
  })
    
    output$msv <- renderPrint({
      
      # Dividir los datos en características (X) y la variable objetivo (y)
      X <- datos_clustering[, -ncol(datos_clustering)] # Todas las columnas excepto la última (Cluster)
      y <- datos_clustering$Cluster # La última columna (Cluster)
      
      # Definir la semilla para la reproducibilidad
      set.seed(123)
      
      # Separar los datos en conjuntos de entrenamiento y prueba (por ejemplo, 70% para entrenamiento y 30% para prueba)
      indices_entrenamiento <- sample(1:nrow(datos_clustering), 0.7 * nrow(datos_clustering))
      X_entrenamiento <- X[indices_entrenamiento, ]
      y_entrenamiento <- y[indices_entrenamiento]
      X_prueba <- X[-indices_entrenamiento, ]
      y_prueba <- y[-indices_entrenamiento]
      
      
      #MAQUINA DE SOPORTE VECTORIAL
      
      # Entrenar el modelo de Máquinas de Soporte Vectorial (MSV) utilizando el conjunto de entrenamiento
      modelo_svm <- svm(x = X_entrenamiento, y = y_entrenamiento)
      
      # Realizar predicciones en el conjunto de prueba
      predicciones <- predict(modelo_svm, newdata = X_prueba)
      # Definir el control de validación cruzada
      control <- trainControl(method = "cv",   # Método de validación cruzada: "cv" para validación cruzada
                              number = 5)      # Número de folds en la validación cruzada
      
      # Entrenar el modelo utilizando la validación cruzada
      modelo <- train(Cluster ~ .,             # Fórmula de la variable objetivo y características
                      data = datos_clustering,         # Conjunto de datos
                      method = "svmRadial",         # Algoritmo de clasificación / Máquinas de vectores de soporte con kernel radial
                      trControl = control)    # Especifica el control de validación cruzada
      
      # Imprimir el resumen del modelo
      print(modelo)
      
      # Imprimir los resultados de la validación cruzada
      print(modelo$results)
      
    })
    
    output$val_msv  <- renderPrint({
      # Definir el modelo optimo a través de validación cruzada
      control <- trainControl(method = "cv", number = 5)
      
      # Entrenar el modelo utilizando la validación cruzada
      modelo <- train(Cluster ~ ., data = datos_clustering, method = "svmRadial", trControl = control)
      
      # Imprimir el resumen del modelo
      print(modelo)
      
      # Imprimir los resultados de la validación cruzada
      print(modelo$results)
      
      # Entrenar el modelo SVM con kernel RBF utilizando el mejor parámetro C
      modelo_optimo <- train(Cluster ~ ., data = datos_clustering, method = "svmRadial", trControl = control,
                             tuneGrid = expand.grid(C = 1.00, sigma = 0.07276369	))
      
      # Hacer predicciones en el conjunto de datos de entrenamiento
      predicciones_entrenamiento_modelo_optimo <- predict(modelo_optimo, datos_clustering)
      
      # Calcular la precisión en el conjunto de datos de entrenamiento
      precision_entrenamiento_modelo_optimo <- mean(predicciones_entrenamiento_modelo_optimo == datos_clustering$Cluster)
      
      # Imprimir la precisión en el conjunto de datos de entrenamiento
      cat("Precisión en el conjunto de datos de entrenamiento:", precision_entrenamiento_modelo_optimo, "\n")
      
      # Calcular la matriz de confusión en el conjunto de datos de entrenamiento
      matriz_confusion_modelo_optimo <- confusionMatrix(predicciones_entrenamiento_modelo_optimo, datos_clustering$Cluster)
      
      # Imprimir la matriz de confusión
      print(matriz_confusion_modelo_optimo)
      
      # Calcular Kappa
      kappa_value_modelo_optimo <- matriz_confusion_modelo_optimo$overall["Kappa"]
      cat("Índice Kappa:", round(kappa_value_modelo_optimo, 2), "\n")
      
      # Calcular el Error de Precisión (obb_error_precision)
      error_precision_modelo_optimo <- 1 - precision_entrenamiento_modelo_optimo
      cat("Error de Precisión (obb_error_precision):", round(error_precision_modelo_optimo * 100, 2), "%\n")
      
      # Convertir la matriz de confusión en un dataframe
      df_confusion <- as.data.frame(as.table(matriz_confusion_modelo_optimo))
      
    })
    
    #Matrices de correlacion
    #RFtotal
    output$graf_matriz_rf <- renderPlotly({
      #Gráfico interactivo de matriz
      grafico_confusion_rf <- ggplot(data = confusion1_df, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "#5cacc4", high = "#9467bd") +
        geom_text(aes(label = Freq), color = "black", vjust = 1) +
        labs(title = paste("Matriz de Confusión para RF\nAccuracy:", round(accuracy_rf_total * 100, 2), "%"),
             x = "Actual",
             y = "Predicted") +
        theme_minimal()
      
      # Convertir el gráfico a interactivo con plotly
      grafico_interactivo_rf <- ggplotly(grafico_confusion_rf)
      
      # Mostrar el gráfico interactivo
      grafico_interactivo_rf
    })
    #RF Variables seleecionadas
    output$graf_matriz_rf2  <- renderPlotly({
      
      confusion_df <- as.data.frame(confusion2$table)
      colnames(confusion_df) <- c("Prediction", "Reference", "Frequency")
      
      
      graf_matriz_rf2 <- ggplot(data = confusion_df, aes(x = Reference, y = Prediction, fill = Frequency)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "#5cacc4", high = "#9467bd") +
        geom_text(aes(label = Frequency), color = "black", vjust = 1) +
        labs(title = paste("Matriz de Confusión para RF sobre variables seleccionadas \nAccuracy: ", round(accuracy_rf_selec, 2), "%"),
             x = "Actual",
             y = "Predicted") +
        theme_minimal()
      
      ggplotly(graf_matriz_rf2)
    })
    
    #Gráfico RF modelo optimizado
    output$graf_matriz_rf3  <- renderPlotly({
      #Gráfico interactivo de matriz
      
      
      confusion3 <- as.data.frame(confusion_matrix3)
      colnames(confusion3) <- c("Prediction", "Reference", "Frequency")
      
      
      graf_matriz_rf3 <- ggplot(data = confusion3, aes(x = Reference, y = Prediction, fill = Frequency)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "#5cacc4", high = "#9467bd") +
        geom_text(aes(label = Frequency), color = "black", vjust = 1) +
        labs(title = paste("Matriz de Confusion para RF sobre todas las variables \nAccuracy: ", round(accuracy_rf_balanced, 2), "%"),
             x = "Actual",
             y = "Predicted") +
        theme_minimal()
      
      ggplotly(graf_matriz_rf3)
    })
    
    #Grafico de matri<zde soporte vectorial
    output$graf_matriz_msv <- renderPlotly({
      df_confusion_msv <- as.data.frame(confusion2$table)
      colnames(df_confusion_msv) <- c("Prediction", "Reference", "Freq")
      
      grafico_confusion_msv <- ggplot(data = df_confusion_msv, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile(color = "black", size = 0.3) +  # Ajuste del borde de las celdas
        geom_text(aes(label = Freq), color = "white", fontface = "bold", vjust = 0.5) +  # Etiquetas más legibles
        scale_fill_gradient(low = "#5cacc4", high = "#9467bd") +
        labs(title = paste("Matriz de Confusión SVM\nPrecisión:", round(precision_cv * 100, 2), "%"),
             x = "Real",
             y = "Predicho") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas de los ejes si son largas
      
      # Convertir el gráfico en interactivo con plotly
      graf_matriz_msv <- ggplotly(grafico_confusion_msv)
      
      # Mostrar el gráfico interactivo
      graf_matriz_msv
     
    })
   
    
    
    
    
    #Gr¿afico comparación de metrica
    output$grafico_metricas   <- renderPlotly({
      
      # Crear el dataframe con las métricas
      metrics <- data.frame(
        Model = c("RF_total", "RF_Seleccion", "Modelo_RF_balanceado",  "MSV_optimizado"),
        Accuracy = c(accuracy_rf_total, accuracy_rf_selec, accuracy_rf_balanced,  precision_entrenamiento_modelo_optimo * 100),  
        Kappa = c(kappa_rf_total*100, kappa_rf_selec*100, kappa_rf_balanceado*100,  kappa_value_modelo_optimo*100),
        OBB_Error = c(rf_model1$err.rate[5, 1]*100, rf_model2$err.rate[5, 1]*100, rf_model3$err.rate[5, 1]*100 
                      , error_precision_modelo_optimo*100) 
      )
      
      # Verifica si los datos se ven correctos
      print(metrics)
      
      # Crear un gráfico de barras con plotly
      grafico_metricas <- plot_ly(metrics, x = ~Model, y = ~Accuracy, type = 'bar', name = 'Accuracy', marker = list(color = '#de528c')) %>%
        add_trace(y = ~Kappa, name = 'Kappa', type = 'bar', marker = list(color = '#8de0a6')) %>%
        add_trace(y = ~OBB_Error, name = 'OBB Error', type = 'bar', marker = list(color = '#FEE08B')) %>%
        layout(
          title = 'Comparación de Métricas por Modelo',
          barmode = 'group',
          xaxis = list(title = 'Modelos'),
          yaxis = list(title = 'Porcentaje (%)'),
          legend = list(title = list(text = 'Métricas'))
        )
      
      # Mostrar el gráfico
      grafico_metricas
      
    })
    output$grafico_cruzamiento   <- renderPlotly({
      #Validación cru<ada de todos los modelos
      # Configuración de la validación cruzada (10 pliegues)
      ctrl <- trainControl(method = "cv", number = 10)
      
      # 1. Modelo RF_total
      rf_total_model <- train(Cluster ~ ., data = datos_clustering, 
                              method = "rf", trControl = ctrl)
      
      # 2. Modelo RF_Seleccion
      rf_seleccion_model <- train(Cluster ~ ., data = datos_seleccionados, 
                                  method = "rf", trControl = ctrl)
      
      # 3. Modelo Modelo_RF_balanceado
      rf_balanceado_model <- train(Cluster ~ ., data = datos_balanceados_clustering, 
                                   method = "rf", trControl = ctrl)
      
      
      # 4. Modelo MSV optimizado (con parámetros ajustados si es necesario)
      msv_optimizado_model <- train(Cluster ~ ., data = datos_clustering, 
                                    method = "svmRadial", trControl = ctrl, 
                                    tuneGrid = expand.grid(C = 1, sigma = 0.07452337))  # Ajusta si es necesario
      
      # Resumen de resultados de validación cruzada
      cat("Resultados de la validación cruzada para los modelos:\n")
      
      # Mostrar resultados de cada modelo
      cat("\nResultados para RF_total:\n")
      print(rf_total_model$results)
      
      cat("\nResultados para RF_Seleccion:\n")
      print(rf_seleccion_model$results)
      
      cat("\nResultados para RF_balanceado:\n")
      print(rf_balanceado_model$results)
      
      cat("\nResultados para MSV optimizado:\n")
      print(msv_optimizado_model$results)
      
      # Obtener los resultados de validación cruzada para cada modelo
      rf_total_results <- rf_total_model$results
      rf_seleccion_results <- rf_seleccion_model$results
      rf_balanceado_results <- rf_balanceado_model$results
      msv_optimizado_results <- msv_optimizado_model$results
      
      # Crear un dataframe para consolidar los resultados
      resultados_completos <- data.frame(
        Modelo = c("RF_total", "RF_Seleccion", "RF_Balanceado",  "MSV_Optimizado"),
        Accuracy = c(
          max(rf_total_results$Accuracy), 
          max(rf_seleccion_results$Accuracy), 
          max(rf_balanceado_results$Accuracy), 
          max(msv_optimizado_results$Accuracy)
        ),
        Kappa = c(
          max(rf_total_results$Kappa), 
          max(rf_seleccion_results$Kappa), 
          max(rf_balanceado_results$Kappa), 
          max(msv_optimizado_results$Kappa)
        ),
        AccuracySD = c(
          min(rf_total_results$AccuracySD), 
          min(rf_seleccion_results$AccuracySD), 
          min(rf_balanceado_results$AccuracySD), 
          min(msv_optimizado_results$AccuracySD)
        )
      )
      
      # Imprimir los resultados consolidados
      print(resultados_completos)
      
      # Crear el dataframe para los resultados consolidados y multiplicar todo por 100
      resultados_completos <- data.frame(
        Modelo = c("RF_total", "RF_Seleccion", "RF_Balanceado",  "MSV_Optimizado"),
        Accuracy = c(
          max(rf_total_results$Accuracy) * 100, 
          max(rf_seleccion_results$Accuracy) * 100, 
          max(rf_balanceado_results$Accuracy) * 100, 
          max(msv_optimizado_results$Accuracy) * 100
        ),
        Kappa = c(
          max(rf_total_results$Kappa) * 100, 
          max(rf_seleccion_results$Kappa) * 100, 
          max(rf_balanceado_results$Kappa) * 100, 
          max(msv_optimizado_results$Kappa) * 100
        ),
        AccuracySD = c(
          min(rf_total_results$AccuracySD) * 100, 
          min(rf_seleccion_results$AccuracySD) * 100, 
          min(rf_balanceado_results$AccuracySD) * 100, 
          min(msv_optimizado_results$AccuracySD) * 100
        )
      )
      
      # Imprimir los resultados consolidados para verificar
      print(resultados_completos)
      
      # Crear el gráfico de barras con plotly
      grafico_cruzamiento <- plot_ly(resultados_completos, x = ~Modelo, y = ~Accuracy, type = 'bar', name = 'Accuracy', marker = list(color = '#de528c')) %>%
        add_trace(y = ~Kappa, name = 'Kappa', type = 'bar', marker = list(color = '#8de0a6')) %>%
        add_trace(y = ~AccuracySD, name = 'Accuracy SD', type = 'bar', marker = list(color = '#FEE08B')) %>%
        layout(
          title = 'Comparación de Métricas de validación cruzada por Modelo',
          barmode = 'group',
          xaxis = list(title = 'Modelos'),
          yaxis = list(title = 'Porcentaje (%)'),
          legend = list(title = list(text = 'Métricas')),
          plot_bgcolor = "white" # Color de fondo del gráfico
        )
      
      # Mostrar el gráfico
      grafico_cruzamiento
    })
    #=========================================================================================
    #===========================================NUEVAS VARIABLES
    
    #Nuevas variables
    # Mostrar la estructura del dataframe
    
    output$datos_clustering_nuevo <- renderPrint({
      head(datos_clustering_nuevo)
    })
    
    output$table_datos_clustering_nuevo  <- renderDT({
      datatable(datos_clustering_nuevo) 
    })
    
    # Función para exportar los datos a Excel
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("datos_clustering_nuevo_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write_xlsx(datos_clustering_nuevo, file)  # Exporta el dataframe a un archivo Excel
      }
    )
    #Función para exportar datos a pdf
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("datos_clustering_nuevo_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Creamos un archivo RMarkdown temporal para renderizarlo en PDF
        
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(
          c(
            "---",
            "title: 'Resumen de Datos'",
            "output: pdf_document",
            "---",
            "",
            "```{r}",
            "summary(datos_clustering_nuevo)",
            "```"
          ),
          temp_rmd
        )
        #Renderizamos el archivo PDF usando rmarkdown
      }
    )
    
    # Función para descargar el trabajo completo en PDF (incluyendo código y resultados)
    output$download_rmarkdown <- downloadHandler(
      filename = function() {
        paste("trabajo_completo_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Creamos un archivo RMarkdown temporal para renderizarlo en PDF
        temp_rmd <- tempfile(fileext = ".Rmd")
        
        # Escribimos el contenido de RMarkdown, incluyendo el código y los resultados
        writeLines(
          c(
            "---",
            "title: 'Indice de libertad económica'",
            "author: 'Tu Nombre'",
            "output: pdf_document",
            "---",
            "",
            "## Resumen de los Datos",
            "",
            "```{r}",
            "summary(datos_clustering_nuevof)",
            "```",
            "",
            "## Estructura de los Datos",
            "```{r}",
            "str(datos_clustering_nuevo)",
            "```",
            "",
            "## Datos Faltantes",
            "```{r}",
            "datos_clustering_nuevo %>% summarise_all(~ sum(is.na(.)))",
            "```",
            "",
            "## Estadísticas Descriptivas",
            "```{r}",
            "summary(datos_clustering_nuevo)",
            "```"
          ),
          temp_rmd
        )
        
        # Renderizamos el archivo PDF usando rmarkdown
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }
    )
    
    output$datos_clustering_nuevo_str <- renderPrint({
      str(datos_clustering_nuevo)
    })
    # Mostrar estadísticas descriptivas
    output$summary_data <- renderPrint({
      summary(datos_clustering_nuevo)  # Muestra estadísticas descriptivas
    })
    
    
    #Distribución de frecuencia de nuevas variables
    output$graf_frec_nuevas_var<- renderPlotly({
      # Definir las columnas de interés
      columnas <- c("Promedio_10_Años_GDP_PerCap", "Promedio_10_Años_Crec_pib", "Efectividad.gobierno", "Control.corrupcion", "Voz_y_Rendicion_cuentas")
      
      # Filtrar el dataframe solo con las columnas seleccionadas
      datos_boxplot1 <- datos_clustering_nuevo[, columnas]
      
      # Convertir los datos a formato largo (long format) para ggplot
      datos_largos1 <- datos_boxplot1 %>%
        pivot_longer(cols = everything(), 
                     names_to = "Variable", 
                     values_to = "Valor")
      
      # Crear gráfico con ggplot
      graf_frec_nuevas_var <- ggplot(datos_largos1, aes(x = Valor, fill = Variable)) +
        geom_histogram(bins = 30, color = "#d2fae2", alpha = 0.7) +  # Ajustamos la transparencia con alpha
        facet_wrap(~ Variable, scales = 'free', ncol = 3) +  # Ajuste de facetas
        scale_fill_viridis_d() +  # Colores suaves con viridis
        theme_minimal() + 
        theme(
          strip.text = element_text(size = 12, face = "bold"),  # Tamaño y estilo de las etiquetas de las facetas
          axis.text.x = element_text(angle = 45, hjust = 1),  # Rotación de las etiquetas del eje X
          axis.text.y = element_text(size = 10),  # Tamaño de las etiquetas del eje Y
          axis.title = element_text(size = 12, face = "bold"),  # Títulos más grandes
          panel.grid = element_blank()  # Eliminar las líneas de la cuadrícula
        ) + 
        labs(
          title = "Distribución de las Características",
          x = "Valor de la Característica",
          y = "Frecuencia"
        )
      
      # Hacer gráfico interactivo con ggplotly
      ggplotly(graf_frec_nuevas_var, tooltip = 'y') %>%
        layout(
          title = "Distribución de las Características",
          #xaxis = list(title = "Valor de la Característica"),
          yaxis = list(title = "Frecuencia"),
          showlegend = FALSE  # Ocultar leyenda, si no es necesaria
        )
      
      graf_frec_nuevas_var
      
      
    })
    #correlación
    output$graf_cor1  <- renderPlotly({
      
      #Previamente correlación
      # Seleccionar solo las variables numéricas
      datos_numericos <- datos_clustering_nuevo[, sapply(datos_clustering_nuevo, is.numeric)]
      
      # Calcular la matriz de correlación
      correlation_matrix1 <- cor(datos_numericos)
      # Convertir la matriz de correlación en un dataframe largo
      correlation_df1 <- as.data.frame(as.table(correlation_matrix1))
      
      # Crear el heatmap con Plotly
      graf_cor1 <- plot_ly(
        data = correlation_df1,
        x = ~Var1,  # Las filas de la matriz de correlación (variables)
        y = ~Var2,  # Las columnas de la matriz de correlación (variables)
        z = ~Freq,  # Los valores de correlación
        type = "heatmap",
        colors = "viridis",  # Colores usando la paleta Viridis
        colorbar = list(title = "Correlación"),  # Etiqueta de la barra de color
        showscale = TRUE  # Mostrar la escala de colores
      ) %>%
        layout(
          title = "Matriz de Correlación de las Variables Numéricas",
          xaxis = list(title = "Variable 1"),
          yaxis = list(title = "Variable 2")
        )
      graf_cor1
      
    })
    
    #principales variables con correlacion
    output$alta_correlation_sorted  <- renderPrint({
      correlation_long <- melt(correlation_matrix1)
      
      # Filtrar las correlaciones entre diferentes variables (no consigo misma)
      correlation_long <- correlation_long[correlation_long$Var1 != correlation_long$Var2, ]
      
      # Filtrar las correlaciones mayores a 0.8 (ajusta este valor según lo que necesites)
      alta_correlation <- correlation_long[abs(correlation_long$value) > 0.8, ]
      
      # Ordenar por la correlación más alta a la más baja
      alta_correlation_sorted <- alta_correlation[order(-abs(alta_correlation$value)), ]
      
      # Imprimir las primeras 5 correlaciones más altas
      head(alta_correlation_sorted, 10)
    })
    
    #Grafico compacion cluster
    
    output$fig_comp_cluster  <- renderPlotly({
      # Convertir los datos a formato largo (long format) para facilitar la visualización
      cluster_comparison_long <- cluster_comparison_porcentajes %>%
        pivot_longer(cols = starts_with("Porcentaje"), 
                     names_to = "Indicador", 
                     values_to = "Porcentaje")
      
      # Crear el gráfico de barras con Plotly
      fig_comp_cluster <- plot_ly(cluster_comparison_long, 
                                  x = ~Cluster, 
                                  y = ~Porcentaje, 
                                  color = ~Indicador,  # Cambié aquí a 'Indicador' para que cada barra sea diferenciada por tipo de indicador
                                  colors = c("Porcentaje_PIB_perCapita" = "#d6496c", 
                                             "Porcentaje_Crecimiento" = "#7db8a2", 
                                             "Porcentaje_Integridad_gubernamental" = "#f1c232"),   
                                  type = 'bar', 
                                  text = ~paste(Indicador, ": ", round(Porcentaje, 2), "%"),
                                  hoverinfo = 'text') %>%
        layout(title = "Porcentaje de PIB per Capita, Crecimiento e Integridad Gubernamental por Cluster",
               xaxis = list(title = "Cluster"),
               yaxis = list(title = "Porcentaje (%)"),
               barmode = 'group')
      
      # Mostrar el gráfico
      fig_comp_cluster
    })
    
    #Grafico comp y pib
    output$fig_cluster_2  <- renderPlotly({
      # Convertir los datos a formato largo (long format) para facilitar la visualización
      cluster_comparison_long1 <- cluster_comparison_porcentajes1 %>%
        pivot_longer(cols = starts_with("Porcentaje"), 
                     names_to = "Indicador", 
                     values_to = "Porcentaje")
      
      fig_cluster_2 <- plot_ly(cluster_comparison_long1, 
                               x = ~Cluster, 
                               y = ~Porcentaje, 
                               color = ~Indicador,  
                               colors = c("Porcentaje_PIB_perCapita" = "#d6496c", 
                                          "Porcentaje_score" = "#7db8a2"),   
                               type = 'bar', 
                               text = ~paste(Indicador, ": ", round(Porcentaje, 2), "%"),
                               hoverinfo = 'text') %>%
        layout(title = "Porcentaje de PIB per Capita, Porcentaje score por Cluster",
               xaxis = list(title = "Cluster"),
               yaxis = list(title = "Porcentaje (%)"),
               barmode = 'group',   # Esto asegura que las barras estén agrupadas por Cluster
               xaxis = list(type = "category")  # Esto asegura que Cluster sea tratado como una categoría
        )
      
      fig_cluster_2
    })
    #===================================================================================
    #===========================REGRESIÖN================================
    
    # Modelo re regresion1 con lasso
    
    output$modelo1  <- renderPrint({
      # Convertir la columna cluster a caracteres y luego filtrar
      cluster_1_data <- subset(datos_clustering_nuevo, as.character(Cluster) == "1")
      cluster_2_data <- subset(datos_clustering_nuevo, as.character(Cluster) == "2")
      
      
      # Filtrar datos para el Cluster 1
      df_cluster1 <- subset(datos_clustering_nuevo, Cluster == "1")
      # Filtrar datos para el Cluster 2
      df_cluster2 <- subset(datos_clustering_nuevo, Cluster == "2")
      
      
      
      #Regresión lineal multiple con lasso
      
      X_1 <- as.matrix(df_cluster1[, c("Investment_Freedom", 
                                       "Fiscal_Health", 
                                       "Promedio_10_Años_Crec_pib", 
                                       "Judicial_Effectiveness", 
                                       "Efectividad.gobierno", 
                                       "Cal.Regulacion", 
                                       "Property_Rights", 
                                       "Tax_Burden", 
                                       "Government_Integrity", 
                                       "Labor_Freedom", 
                                       "Business_Freedom")])
      
      # Variable dependiente
      y_1 <- df_cluster1$Promedio_10_Años_GDP_PerCap
      
      # Ajustar el modelo Lasso con validación cruzada
      
      modelo_lasso_1 <- cv.glmnet(X_1, y_1, alpha = 1)
      
      # Imprimir el valor de lambda que minimiza el error de validación cruzada
      cat("Valor óptimo de lambda Cluster 1:", modelo_lasso_1$lambda.min, "\n")
      
      # Obtener los coeficientes del modelo con el mejor lambda
      coeficientes_lasso_1 <- coef(modelo_lasso_1, s = "lambda.min")
      cat("Coeficientes del modelo Lasso con el valor óptimo de lambda:\n")
      print(coeficientes_lasso_1)
      
      # Predicciones con el modelo Lasso
      predicciones_lasso_1 <- predict(modelo_lasso_1, newx = X_1, s = "lambda.min")
      
      # Calcular el MSE (Error Cuadrático Medio)
      mse_lasso_1 <- mean((predicciones_lasso_1 - y_1)^2)
      cat("MSE del modelo Lasso:", mse_lasso_1, "\n")
      
      # Calcular R-cuadrado (R^2)
      r2_lasso_1 <- 1 - sum((predicciones_lasso_1 - y_1)^2) / sum((y_1 - mean(y_1))^2)
      cat("R-cuadrado del modelo Lasso:", r2_lasso_1, "\n")
      
      
      
      
    })
    
    #Modelo 2
    output$modelo2 <- renderPrint({
      # Filtrar las variables predictoras (asegúrate de excluir la variable dependiente)
      X_2 <- as.matrix(df_cluster2[, c("Investment_Freedom", 
                                       "Fiscal_Health", 
                                       "Promedio_10_Años_Crec_pib", 
                                       "Judicial_Effectiveness", 
                                       "Efectividad.gobierno", 
                                       "Cal.Regulacion", 
                                       "Property_Rights", 
                                       "Tax_Burden", 
                                       "Government_Integrity", 
                                       "Labor_Freedom", 
                                       "Business_Freedom")])
      
      # Variable dependiente
      y_2 <- df_cluster2$Promedio_10_Años_GDP_PerCap
      
      # Ajustar el modelo Lasso con validación cruzada
      
      modelo_lasso_2 <- cv.glmnet(X_2, y_2, alpha = 1)
      
      # Imprimir el valor de lambda que minimiza el error de validación cruzada
      cat("Valor óptimo de lambda Cluster 2:", modelo_lasso_2$lambda.min, "\n")
      
      # Obtener los coeficientes del modelo con el mejor lambda
      coeficientes_lasso_2 <- coef(modelo_lasso_2, s = "lambda.min")
      cat("Coeficientes del modelo Lasso con el valor óptimo de lambda:\n")
      print(coeficientes_lasso_2)
      
      # Predicciones con el modelo Lasso
      predicciones_lasso_2 <- predict(modelo_lasso_2, newx = X_2, s = "lambda.min")
      
      # Calcular el MSE (Error Cuadrático Medio)
      mse_lasso_2 <- mean((predicciones_lasso_2 - y_2)^2)
      cat("MSE del modelo Lasso:", mse_lasso_2, "\n")
      
      # Calcular R-cuadrado (R^2)
      r2_lasso_2 <- 1 - sum((predicciones_lasso_2 - y_2)^2) / sum((y_2 - mean(y_2))^2)
      cat("R-cuadrado del modelo Lasso:", r2_lasso_2, "\n")
      
    })
    
    #Ecuación modelo1
    
    output$ecuacion1 <- renderPrint({
      #formula para la ecuacion
      # Obtener los coeficientes del modelo con el mejor lambda
      coeficientes_lasso_1 <- coef(modelo_lasso_1, s = "lambda.min")
      
      # Convertir los coeficientes a un data.frame
      coeficientes_lasso_1_df <- as.data.frame(as.matrix(coeficientes_lasso_1))
      coeficientes_lasso_1_df$Variable <- rownames(coeficientes_lasso_1_df)
      
      # Renombramos la columna de coeficientes para asegurarnos de que sea numérica
      colnames(coeficientes_lasso_1_df) <- c("Coeficiente", "Variable")
      
      # Asegurarnos de que los coeficientes sean numéricos (podría estar en un formato extraño)
      coeficientes_lasso_1_df$Coeficiente <- as.numeric(coeficientes_lasso_1_df$Coeficiente)
      
      # Filtrar las variables con coeficientes no nulos
      coeficientes_lasso_1_df <- coeficientes_lasso_1_df[coeficientes_lasso_1_df$Coeficiente != 0, ]
      
      cat("Ecuación de la regresión múltiple del Cluster 1:\n")
      
      # Imprimir el intercepto (si es distinto de cero)
      cat("Promedio_10_Años_GDP_PerCap =", round(coeficientes_lasso_1_df$Coeficiente[1], 2))
      
      # Imprimir el resto de la fórmula
      for (i in 2:nrow(coeficientes_lasso_1_df)) {
        cat(" +", round(coeficientes_lasso_1_df$Coeficiente[i], 2), "*", coeficientes_lasso_1_df$Variable[i], "\n")
      }
      
    })
    #Ecuación modelo2
    output$ecuacion2 <- renderPrint({
      #formula
      # Obtener los coeficientes del modelo con el mejor lambda
      coeficientes_lasso_2 <- coef(modelo_lasso_2, s = "lambda.min")
      
      # Convertir los coeficientes a un data.frame
      coeficientes_lasso_2_df <- as.data.frame(as.matrix(coeficientes_lasso_2))
      coeficientes_lasso_2_df$Variable <- rownames(coeficientes_lasso_2_df)
      
      # Renombramos la columna de coeficientes para asegurarnos de que sea numérica
      colnames(coeficientes_lasso_2_df) <- c("Coeficiente", "Variable")
      
      # Asegurarnos de que los coeficientes sean numéricos (podría estar en un formato extraño)
      coeficientes_lasso_2_df$Coeficiente <- as.numeric(coeficientes_lasso_2_df$Coeficiente)
      
      # Filtrar las variables con coeficientes no nulos
      coeficientes_lasso_2_df <- coeficientes_lasso_2_df[coeficientes_lasso_2_df$Coeficiente != 0, ]
      
      cat("Ecuación de la regresión múltiple del Cluster 2:\n")
      
      # Imprimir el intercepto (si es distinto de cero)
      cat("Promedio_10_Años_GDP_PerCap =", round(coeficientes_lasso_1_df$Coeficiente[1], 2))
      
      # Imprimir el resto de la fórmula
      for (i in 2:nrow(coeficientes_lasso_2_df)) {
        cat(" +", round(coeficientes_lasso_2_df$Coeficiente[i], 2), "*", coeficientes_lasso_2_df$Variable[i], "\n")
      }
    })
    #Comparación de métricass de los modelos
    output$graf_pred_mse  <- renderPlotly({
      # Filtrar las variables predictoras para Cluster 1
      X_1 <- as.matrix(df_cluster1[, c("Investment_Freedom", 
                                       "Fiscal_Health", 
                                       "Promedio_10_Años_Crec_pib", 
                                       "Judicial_Effectiveness", 
                                       "Efectividad.gobierno", 
                                       "Cal.Regulacion", 
                                       "Property_Rights", 
                                       "Tax_Burden", 
                                       "Government_Integrity", 
                                       "Labor_Freedom", 
                                       "Business_Freedom")])
      
      # Variable dependiente Cluster 1
      y_1 <- df_cluster1$Promedio_10_Años_GDP_PerCap
      
      # Ajustar el modelo Lasso para Cluster 1
      modelo_lasso_1 <- cv.glmnet(X_1, y_1, alpha = 1)
      
      # Imprimir el valor de lambda que minimiza el error de validación cruzada
      cat("Valor óptimo de lambda Cluster 1:", modelo_lasso_1$lambda.min, "\n")
      
      # Obtener los coeficientes del modelo con el mejor lambda para Cluster 1
      coeficientes_lasso_1 <- coef(modelo_lasso_1, s = "lambda.min")
      cat("Coeficientes del modelo Lasso con el valor óptimo de lambda para Cluster 1:\n")
      print(coeficientes_lasso_1)
      
      # Predicciones con el modelo Lasso para Cluster 1
      predicciones_lasso_1 <- predict(modelo_lasso_1, newx = X_1, s = "lambda.min")
      
      # Calcular el MSE (Error Cuadrático Medio) para Cluster 1
      mse_lasso_1 <- mean((predicciones_lasso_1 - y_1)^2)
      cat("MSE del modelo Lasso para Cluster 1:", mse_lasso_1, "\n")
      
      # Calcular R-cuadrado (R²) para Cluster 1
      r2_lasso_1 <- 1 - sum((predicciones_lasso_1 - y_1)^2) / sum((y_1 - mean(y_1))^2)
      cat("R-cuadrado del modelo Lasso para Cluster 1:", r2_lasso_1, "\n")
      
      
      # Filtrar las variables predictoras para Cluster 2
      X_2 <- as.matrix(df_cluster2[, c("Investment_Freedom", 
                                       "Fiscal_Health", 
                                       "Promedio_10_Años_Crec_pib", 
                                       "Judicial_Effectiveness", 
                                       "Efectividad.gobierno", 
                                       "Cal.Regulacion", 
                                       "Property_Rights", 
                                       "Tax_Burden", 
                                       "Government_Integrity", 
                                       "Labor_Freedom", 
                                       "Business_Freedom")])
      
      # Variable dependiente Cluster 2
      y_2 <- df_cluster2$Promedio_10_Años_GDP_PerCap
      
      # Ajustar el modelo Lasso para Cluster 2
      modelo_lasso_2 <- cv.glmnet(X_2, y_2, alpha = 1)
      
      # Imprimir el valor de lambda que minimiza el error de validación cruzada para Cluster 2
      cat("Valor óptimo de lambda Cluster 2:", modelo_lasso_2$lambda.min, "\n")
      
      # Obtener los coeficientes del modelo con el mejor lambda para Cluster 2
      coeficientes_lasso_2 <- coef(modelo_lasso_2, s = "lambda.min")
      cat("Coeficientes del modelo Lasso con el valor óptimo de lambda para Cluster 2:\n")
      print(coeficientes_lasso_2)
      
      # Predicciones con el modelo Lasso para Cluster 2
      predicciones_lasso_2 <- predict(modelo_lasso_2, newx = X_2, s = "lambda.min")
      
      # Calcular el MSE (Error Cuadrático Medio) para Cluster 2
      mse_lasso_2 <- mean((predicciones_lasso_2 - y_2)^2)
      cat("MSE del modelo Lasso para Cluster 2:", mse_lasso_2, "\n")
      
      # Calcular R-cuadrado (R²) para Cluster 2
      r2_lasso_2 <- 1 - sum((predicciones_lasso_2 - y_2)^2) / sum((y_2 - mean(y_2))^2)
      cat("R-cuadrado del modelo Lasso para Cluster 2:", r2_lasso_2, "\n")
      
      
      
      # Filtrar y almacenar los resultados de lambda, R² y MSE por Cluster 1 y Cluster 2
      
      # Resultados para Cluster 1
      lambda_1 <- modelo_lasso_1$lambda.min
      mse_1 <- mse_lasso_1
      r2_1 <- r2_lasso_1*100
      
      # Resultados para Cluster 2
      lambda_2 <- modelo_lasso_2$lambda.min
      mse_2 <- mse_lasso_2
      r2_2 <- r2_lasso_2*100
      
      # Crear un dataframe con los resultados
      resultados_completos <- data.frame(
        Cluster = c("Cluster 1", "Cluster 2"),
        Lambda = c(lambda_1, lambda_2),
        MSE = c(mse_1, mse_2),
        R2 = c(r2_1, r2_2)
      )
      
      # Mostrar el dataframe de resultados
      print(resultados_completos)
      
      
      #Grafico
      
      # Crear un dataframe para MSE
      resultados_mse <- data.frame(
        Cluster = c("Cluster 1", "Cluster 2"),
        MSE = c(mse_1, mse_2)
      )
      
      # Crear el gráfico solo para MSE con un color específico
      fig_mse <- plot_ly(resultados_mse, x = ~Cluster, y = ~MSE, type = 'bar', name = 'MSE',
                         marker = list(color = '#de528c')) %>%
        layout(
          title = "Comparación de MSE por Cluster",
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "MSE"),
          showlegend = FALSE
        )
      
      # Crear un dataframe para Lambda y R²
      resultados_lambda_r2 <- data.frame(
        Cluster = c("Cluster 1", "Cluster 2"),
        Lambda = c(lambda_1, lambda_2),
        R2 = c(r2_1, r2_2)
      )
      
      # Reshape para Lambda y R²
      resultados_long_lambda_r2 <- reshape(resultados_lambda_r2, 
                                           varying = c("Lambda", "R2"),
                                           v.names = "Valor",
                                           timevar = "Metrica",
                                           times = c("Lambda", "R2"),
                                           direction = "long")
      
      # Crear el gráfico para Lambda y R² con colores específicos
      fig_lambda_r2 <- plot_ly(resultados_long_lambda_r2, x = ~Cluster, y = ~Valor, color = ~Metrica, type = 'bar', 
                               text = ~paste(Metrica, ": ", round(Valor, 2)),
                               hoverinfo = 'text', 
                               colors = c("Lambda" = "#8de0a6", "R2" = "#dc6378")) %>%
        layout(
          title = "Comparación de Lambda y R² por Cluster",
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "Valor"),
          barmode = 'group',  # Modo de agrupación de barras
          showlegend = TRUE
        )
      
      # Mostrar ambos gráficos
      fig_mse
      
      
      
    })
    
      
    
    output$graf_metricas  <- renderPlotly({
      # Crear un dataframe para MSE
      resultados_mse <- data.frame(
        Cluster = c("Cluster 1", "Cluster 2"),
        MSE = c(mse_1, mse_2)
      )
      
      # Crear el gráfico solo para MSE con un color específico
      fig_mse <- plot_ly(resultados_mse, x = ~Cluster, y = ~MSE, type = 'bar', name = 'MSE',
                         marker = list(color = '#de528c')) %>%
        layout(
          title = "Comparación de MSE por Cluster",
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "MSE"),
          showlegend = FALSE
        )
      
      # Crear un dataframe para Lambda y R²
      resultados_lambda_r2 <- data.frame(
        Cluster = c("Cluster 1", "Cluster 2"),
        Lambda = c(lambda_1, lambda_2),
        R2 = c(r2_1, r2_2)
      )
      
      # Reshape para Lambda y R²
      resultados_long_lambda_r2 <- reshape(resultados_lambda_r2, 
                                           varying = c("Lambda", "R2"),
                                           v.names = "Valor",
                                           timevar = "Metrica",
                                           times = c("Lambda", "R2"),
                                           direction = "long")
      
      # Crear el gráfico para Lambda y R² con colores específicos
      fig_lambda_r2 <- plot_ly(resultados_long_lambda_r2, x = ~Cluster, y = ~Valor, color = ~Metrica, type = 'bar', 
                               text = ~paste(Metrica, ": ", round(Valor, 2)),
                               hoverinfo = 'text', 
                               colors = c("Lambda" = "#8de0a6", "R2" = "#dc6378")) %>%
        layout(
          title = "Comparación de Lambda y R² por Cluster",
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "Valor"),
          barmode = 'group',  # Modo de agrupación de barras
          showlegend = TRUE
        )
      
      # Mostrar ambos gráficos
      
      fig_lambda_r2
    })
    
    output$graf_metricas_lasso   <- renderPlotly({
      
    })
    #grafico residuos
    output$graf_residuos  <- renderPlotly({
      # Residuos para Cluster 1
      residuos_lasso_1 <- y_1 - predicciones_lasso_1
      cat("Residuos Cluster 1:\n")
      print(residuos_lasso_1)
      
      # Graficar los residuos de Cluster 1 usando Plotly
      grafico_residuos_1 <- plot_ly(x = seq_along(residuos_lasso_1), y = residuos_lasso_1, 
                                    type = 'scatter', mode = 'markers', 
                                    marker = list(color = '#8359ed', size = 6),  # Cambiar a color azul
                                    name = "Residuos Cluster 1") %>%
        add_trace(x = seq_along(residuos_lasso_1), y = rep(0, length(residuos_lasso_1)),
                  type = 'scatter', mode = 'lines', 
                  line = list(color = '#f90050', width = 4),  # Cambiar a línea roja
                  name = "Línea de referencia 0")
      
      # Residuos para Cluster 2
      residuos_lasso_2 <- y_2 - predicciones_lasso_2
      cat("Residuos Cluster 2:\n")
      print(residuos_lasso_2)
      
      # Graficar los residuos de Cluster 2 usando Plotly
      grafico_residuos_2 <- plot_ly(x = seq_along(residuos_lasso_2), y = residuos_lasso_2, 
                                    type = 'scatter', mode = 'markers', 
                                    marker = list(color = '#4ab5e5', size = 6),  
                                    name = "Residuos Cluster 2") %>%
        add_trace(x = seq_along(residuos_lasso_2), y = rep(0, length(residuos_lasso_2)),
                  type = 'scatter', mode = 'lines', 
                  line = list(color = '#d6496c', width = 4),  # Línea roja
                  name = "Línea de referencia 0")
      
      # Combinamos ambos gráficos usando subplot
      grafico_residuos_combinado <- subplot(grafico_residuos_1, grafico_residuos_2, 
                                            nrows = 1, shareX = TRUE, shareY = TRUE) %>%
        layout(title = "Comparación de Residuos de los Modelos Lasso por Cluster",
               showlegend = TRUE)
      
      # Mostrar el gráfico combinado
      grafico_residuos_combinado
    })
    #Gráfico comperativo
    output$grafico_comparativo  <- renderPlotly({
      # Predicción del pib per capita con cambios en los valores de los indicadores
      
      # Datos para hacer las predicciones (Escenarios)
      datos_varios <- data.frame(
        Fiscal_Health = c(40, 50, 65, 80),
        Judicial_Effectiveness = c(20, 40, 60, 75),
        Monetary_Freedom = c(30, 40, 70, 60),
        Financial_Freedom = c(45, 75, 85, 50),
        Property_Rights = c(40, 55, 65, 75),
        Tax_Burden = c(15, 20, 30, 45),
        Business_Freedom = c(30, 40, 50, 75),
        Trade_Freedom = c(40, 50, 60, 75),
        Government_Integrity = c(40, 50, 60, 80),
        Government_Spending = c(35, 45, 60, 75),
        Labor_Freedom = c(30, 50, 65, 80),
        Investment_Freedom = c(40, 50, 60, 85),
        Efectividad.gobierno = c(0.3, 1.1, 1.5, 3),
        Cal.Regulacion = c(-2, 1, 1.5, 2.5),
        Promedio_10_Años_Crec_pib = c(1, 1.5, 2, 3.5),
        Overall_score = c(20, 40, 60, 79)
      )
      
      # Asegúrarse de que las columnas estén en el mismo orden que en el modelo Lasso
      orden_columnas <- c("Investment_Freedom", "Fiscal_Health", "Promedio_10_Años_Crec_pib", 
                          "Judicial_Effectiveness", "Efectividad.gobierno", "Cal.Regulacion", 
                          "Property_Rights", "Tax_Burden", "Government_Integrity", 
                          "Labor_Freedom", "Business_Freedom")
      
      # Reorganizar las columnas para que coincidan con el orden que espera el modelo
      datos_varios <- datos_varios[, orden_columnas]
      
      # Convertir los datos a una matriz (asegúrate de excluir la variable dependiente)
      X_varios <- as.matrix(datos_varios)
      
      # Realizar las predicciones con el modelo Lasso ajustado para Cluster 1 y Cluster 2
      predicciones_lasso_cluster1 <- predict(modelo_lasso_1, newx = X_varios, s = "lambda.min")
      predicciones_lasso_cluster2 <- predict(modelo_lasso_2, newx = X_varios, s = "lambda.min")
      
      # Crear un data frame con las predicciones y los escenarios
      predicciones_comb <- data.frame(
        Escenario = c("Escenario 1", "Escenario 2", "Escenario 3", "Escenario 4"),
        Predicciones_Lasso_Cluster1 = as.vector(predicciones_lasso_cluster1), # Convertir a vector para evitar problemas de tipo
        Predicciones_Lasso_Cluster2 = as.vector(predicciones_lasso_cluster2)  # Convertir a vector para evitar problemas de tipo
      )
      
      # Imprimir las predicciones combinadas para observar cómo el PIB per cápita cambia
      print(predicciones_comb)
      
      # Crear gráfico interactivo con Plotly
      grafico_comparativo <- plot_ly(data = predicciones_comb, x = ~Escenario) %>%
        add_trace(
          y = ~Predicciones_Lasso_Cluster1,
          name = "Cluster 1",
          type = 'bar',
          marker = list(color = '#d6496c')
        ) %>%
        add_trace(
          y = ~Predicciones_Lasso_Cluster2,
          name = "Cluster 2",
          type = 'bar',
          marker = list(color = '#2ca02c')
        ) %>%
        layout(
          title = "Comparación de Predicciones de PIB per Cápita por Cluster",
          xaxis = list(title = "Escenarios"),
          yaxis = list(title = "Predicción de PIB per Cápita"),
          barmode = 'group',  # Pone las barras lado a lado
          legend = list(x = 0.1, y = 0.9)
        )
      
      # Mostrar el gráfico
      grafico_comparativo
      
    })
    #===================================================================================
    #=============================INVERSION============================================
    #comparcación vp
    output$grafico_cluster_inv <- renderPlotly({
      # Definir los parámetros constantes para el modelo
      costo_inicial <- 5000  # En millones
      tasa_descuento <- 0.03  # Tasa de descuento
      costo_recurrente <- 300  # Costo recurrente
      ingreso_anual_bruto <- 500  # Millones agregado recientemente
      anos <- 5  # Proyecto de 5 años
      
      # Función para calcular el valor neto  por cluster para ver si se recupera la inversión
      calcular_valor_neto <- function(costo_inicial, costo_recurrente, ingreso_anual_bruto, 
                                      beneficio_incremento_pib, tasa_descuento, anos) {
        # Ajustar los costos y beneficios con base en la fiscalidad y gobernanza
        # (si no es necesario un ajuste, puedes omitir esta parte)
        costo_ajustado <- costo_recurrente  # Si hay ajustes fiscales u otros, se pueden agregar aquí.
        
        # Ajuste de beneficio en función del crecimiento del PIB
        beneficio_ajustado <- ingreso_anual_bruto * beneficio_incremento_pib
        
        # Calcular los costos y beneficios descontados
        costos_desc <- costo_ajustado * sum(1 / (1 + tasa_descuento)^(1:anos))  # Costos descontados
        beneficios_desc <- sum(beneficio_ajustado / (1 + tasa_descuento)^(1:anos))  # Beneficios descontados
        
        # Calcular el valor neto
        valor_neto <- beneficios_desc - costos_desc
        return(valor_neto)
      }
      
      # Agrupar los datos por cluster y calcular el valor neto ajustado por cada uno
      resultados_cluster <- datos_clustering_nuevo %>%
        group_by(Cluster) %>%
        summarise(
          valor_neto_cluster = calcular_valor_neto(
            costo_inicial = costo_inicial,
            ingreso_anual_bruto = ingreso_anual_bruto,
            costo_recurrente = costo_recurrente,
            beneficio_incremento_pib = mean(Promedio_10_Años_Crec_pib, na.rm = TRUE),  # Ajuste del crecimiento del PIB
            tasa_descuento = tasa_descuento,
            anos = anos
          )
        )
      
      # Imprimir resultados
      print(resultados_cluster)
      # Crear el gráfico con plotly
      grafico_cluster <- resultados_cluster %>%
        plot_ly(
          x = ~Cluster, 
          y = ~valor_neto_cluster, 
          type = 'bar', 
          name = 'Valor Neto',
          marker = list(color = c('#d6496c', '#2ca02c'))  # Corregido "mmarker" a "marker"
        ) %>%
        layout(
          title = "Valor Neto por Cluster",
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "Valor Neto (millones)"),
          barmode = 'group'
        )
      
      # Mostrar el gráfico
      grafico_cluster
    })
    #grafico ajustado
    output$grafico_cluster_ajustado  <- renderPlotly({
      # Definir los parámetros constantes para el modelo
      costo_inicial <- 5000  # En millones
      tasa_descuento <- 0.03  # Tasa de descuento
      costo_recurrente <- 300  # Costo recurrente
      ingreso_anual_bruto <- 500 # Millones agregado recientemente
      anos <- 5  # Proyecto de 5 años
      
      # Función para calcular el valor neto ajustado por cluster para recuperar la inversión
      calcular_valor_neto_ajustado <- function(costo_inicial, costo_recurrente, ingreso_anual_bruto, 
                                               beneficio_incremento_pib, tasa_descuento, anos, fiscal_salud, gobernanza) {
        # Ajustar los costos y beneficios con base en la fiscalidad y gobernanza
        costo_ajustado <- costo_recurrente * (1 + fiscal_salud * 0.1)  # Ajuste del costo según la salud fiscal
        beneficio_ajustado <- ingreso_anual_bruto * beneficio_incremento_pib * (1 + gobernanza * 0.1)  # Ajuste del beneficio según la efectividad del gobierno
        
        # Calcular los costos y beneficios descontados, manejando NA's si existen
        costos_desc <- costo_inicial + sum(costo_ajustado / (1 + tasa_descuento)^(1:anos), na.rm = TRUE)
        beneficios_desc <- sum(beneficio_ajustado / (1 + tasa_descuento)^(1:anos), na.rm = TRUE)
        
        # Calcular el valor neto
        valor_neto_ajustado <- beneficios_desc - costos_desc
        return(valor_neto_ajustado)
      }
      
      # Agrupar los datos por cluster y calcular el valor neto ajustado por cada uno
      resultados_cluster_ajustado <- datos_clustering_nuevo %>%
        group_by(Cluster) %>%
        summarise(
          valor_neto_cluster = calcular_valor_neto_ajustado(
            costo_inicial = costo_inicial,
            ingreso_anual_bruto = ingreso_anual_bruto,
            costo_recurrente = mean(Tax_Burden),  # Usamos el Tax_Burden para ajustar el costo
            beneficio_incremento_pib = mean(Promedio_10_Años_Crec_pib ),  # Convertimos a porcentaje
            tasa_descuento = tasa_descuento,
            anos = anos,
            fiscal_salud = mean(Fiscal_Health),  # Ajustamos con la salud fiscal
            gobernanza = mean(Government_Integrity)  # Ajustamos con la efectividad del gobierno
          )
        )
      
      # Imprimir resultados
      print(resultados_cluster_ajustado)
      grafico_cluster_ajustado <- plot_ly(
        data = resultados_cluster_ajustado,
        x = ~Cluster,
        y = ~valor_neto_cluster,
        type = "bar",
        marker = list(color = c('#d6496c', '#2ca02c'))  # Personaliza los colores
      ) %>%
        layout(
          title = "Valor Neto Ajustado por Cluster",
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "Valor Neto Ajustado")
        )
      
      grafico_cluster_ajustado
      
      
    })
    #Grafico 3d
    output$grafico_3d <-  renderPlotly({
      # Datos para el gráfico 3d
      datos_grafico <- resultados_cluster_ajustado %>%
        left_join(datos_clustering_nuevo, by = "Cluster") %>%  # Unir para obtener todas las variables relevantes
        select(Cluster, valor_neto_cluster, Tax_Burden, Fiscal_Health)  # Seleccionar las columnas necesarias
      
      # Crear el gráfico 3D usando plotly
      grafico_3d <- plot_ly(
        data = datos_grafico,
        x = ~Tax_Burden,  # Eje X: Tax Burden
        y = ~Fiscal_Health,  # Eje Y: Salud Fiscal
        z = ~valor_neto_cluster,  # Eje Z: Valor Neto Ajustado
        color = ~Cluster,  # Colorear por Cluster
        colors = c('#d6496c', '#2ca02c'),  # Colores para los clusters
        type = "scatter3d",  # Tipo de gráfico 3D
        mode = "markers",  # Mostrar los puntos
        marker = list(size = 5)  # Tamaño de los puntos
      ) %>%
        layout(
          title = "Análisis 3D por Cluster",
          scene = list(
            xaxis = list(title = "Tax Burden"),
            yaxis = list(title = "Fiscal Health"),
            zaxis = list(title = "Valor Neto Ajustado")
          )
        )
      
      # Mostrar el gráfico 3D
      grafico_3d
      
    })
    #Grafico costos
    output$grafico_costos <- renderPlotly({
      # Función para calcular el valor neto ajustado por cluster
      calcular_valor_neto_ajustado <- function(costo_inicial, costo_recurrente, beneficio_incremento_pib, tasa_descuento, anos, fiscal_salud, gobernanza, ingreso_anual_bruto) {
        # Ajuste del costo y beneficio según fiscalidad y gobernanza
        costo_ajustado <- costo_recurrente * (1 + fiscal_salud * 0.03)  # Ajuste del costo a un valor moderado
        beneficio_ajustado <- ingreso_anual_bruto * beneficio_incremento_pib * (1 + gobernanza * 0.03)  # Ajuste del beneficio a un valor moderado
        
        # Calcular los costos y beneficios descontados usando la fórmula de valor presente
        costos_desc <- costo_inicial + sum(costo_ajustado / (1 + tasa_descuento)^(1:anos))
        beneficios_desc <- sum(beneficio_ajustado / (1 + tasa_descuento)^(1:anos))
        
        # Calcular el valor neto ajustado
        valor_neto_ajustado <- beneficios_desc - costos_desc
        return(valor_neto_ajustado)
      }
      
      # Probar tres escenarios con diferentes costos recurrentes (reducidos, originales y aumentados)
      escenarios_costos <- data.frame(
        escenario = c("Costo +10%", "Costo +20%", "Costo +40%"),
        costo_recurrente_modificado = c(1.1, 1.2, 1.4),  # Reducir, mantener o aumentar el costo recurrente
        stringsAsFactors = FALSE
      )
      
      # Inicializar un dataframe para los resultados
      resultados_costos <- data.frame(
        escenario = character(),
        valor_neto_ajustado = numeric(),
        cluster = integer(),
        stringsAsFactors = FALSE
      )
      
      # Recorrer los clusters y escenarios con los ajustes de costos
      for (cluster_id in unique(datos_clustering_nuevo$Cluster)) {
        cluster_data <- datos_clustering_nuevo[datos_clustering_nuevo$Cluster == cluster_id, ]
        
        for (i in 1:nrow(escenarios_costos)) {
          # Ajustar el costo recurrente según el escenario
          costo_recurrente_ajustado <- mean(cluster_data$Tax_Burden) * escenarios_costos$costo_recurrente_modificado[i]
          
          # Calcular el valor neto ajustado para el escenario
          valor_neto <- calcular_valor_neto_ajustado(
            costo_inicial = 5000 ,  # Costo inicial fijo
            costo_recurrente = costo_recurrente_ajustado, 
            ingreso_anual_bruto =  ingreso_anual_bruto,
            beneficio_incremento_pib = mean(cluster_data$Promedio_10_Años_Crec_pib),  # Crecimiento PIB
            tasa_descuento = 0.03,  # Tasa de descuento fija
            anos = 5,
            fiscal_salud = mean(cluster_data$Fiscal_Health),
            gobernanza = mean(cluster_data$Government_Integrity)
          )
          
          # Almacenar los resultados
          resultados_costos <- rbind(resultados_costos, data.frame(
            escenario = escenarios_costos$escenario[i],
            valor_neto_ajustado = valor_neto,
            cluster = cluster_id
          ))
        }
      }
      
      # Asegurarse de que la columna Cluster sea un factor para el gráfico
      resultados_costos$cluster <- factor(resultados_costos$cluster)
      
      # Visualizar los resultados ajustados por escenario y cluster
      grafico_costos <- plot_ly(
        data = resultados_costos,
        x = ~escenario,  # Eje X: Escenarios
        y = ~valor_neto_ajustado,  # Eje Y: Valor Neto Ajustado
        color = ~cluster,  # Colorear por Cluster
        colors = c('#d6496c', '#2ca02c'),  # Usar más colores si tienes más de dos clusters
        type = "bar",  # Tipo de gráfico (barras)
        text = ~paste("Valor Neto: ", round(valor_neto_ajustado, 2)),
        hoverinfo = "text"  # Mostrar el valor neto al pasar el ratón
      ) %>%
        layout(
          title = "Valor Neto Ajustado por Escenario de Costo y Cluster",
          xaxis = list(title = "Escenario de Costo"),
          yaxis = list(title = "Valor Neto Ajustado"),
          legend = list(title = list(text = 'Cluster')),
          barmode = "group",  # Agrupar las barras por escenario y cluster
          showlegend = TRUE
        )
      
      # Mostrar el gráfico
      grafico_costos
    })
    #Grafico tir
    output$resultados_tir  <- renderPlotly({
      # Ajuste de los parámetros de entrada, usando escalas más pequeñas si es necesario
      calcular_vpn <- function(tasa, costo_inicial, costo_recurrente, beneficio_incremento_pib, anos, fiscal_salud, gobernanza, ingreso_anual_bruto) {
        # Ajuste de los costos y beneficios
        costo_ajustado <- costo_recurrente * (1 + fiscal_salud * 0.1)
        beneficio_ajustado <- ingreso_anual_bruto * beneficio_incremento_pib * (1 + gobernanza * 0.1)
        
        # Verificación de los costos y beneficios
        print(paste("Costo ajustado: ", costo_ajustado))
        print(paste("Beneficio ajustado: ", beneficio_ajustado))
        
        # Calcular los costos y beneficios descontados para cada año
        costos_desc <- costo_inicial + sum(sapply(1:anos, function(a) costo_ajustado / (1 + tasa)^a))
        beneficios_desc <- sum(sapply(1:anos, function(a) beneficio_ajustado / (1 + tasa)^a))
        
        # Calcular el VPN
        vpn <- beneficios_desc - costos_desc
        return(vpn)
      }
      
      # Función para calcular la TIR por cluster
      calcular_tir <- function(costo_inicial, costo_recurrente, beneficio_incremento_pib, fiscal_salud, gobernanza, ingreso_anual_bruto) {
        tir_funcion <- function(tasa) {
          return(calcular_vpn(tasa, costo_inicial, costo_recurrente, beneficio_incremento_pib, anos = 5, fiscal_salud, gobernanza, ingreso_anual_bruto))
        }
        
        # Comprobación de valores en los extremos del intervalo
        vpn_inicial <- tir_funcion(0)  # VPN en tasa = 0
        vpn_final <- tir_funcion(100)  # Ampliamos el intervalo hasta 1000
        
        # Verificar si hay cruce de signos
        print(paste("VPN en tasa 0: ", vpn_inicial))
        print(paste("VPN en tasa 1000: ", vpn_final))
        
        if (sign(vpn_inicial) == sign(vpn_final)) {
          message("No hay cruce de signos en el intervalo de 0 a 1000.")
          return(NA)  # Si no hay cruce de signos, no tiene sentido calcular la TIR
        }
        
        # Intentar calcular la TIR
        resultado <- tryCatch({
          uniroot(tir_funcion, c(0, 1000))  # Intervalo entre 0 y 1000
        }, error = function(e) {
          message(paste("Error calculando TIR:", e$message))
          return(NA)  # Si hay error, devolver NA
        })
        
        if (!is.null(resultado)) {
          return(resultado$root)  # Retornar la TIR
        } else {
          return(NA)  # En caso de error
        }
      }
      
      # Función para calcular TIR por cluster
      calcular_tir_por_cluster <- function(data) {
        # Verificar que el dataframe contiene las columnas necesarias
        if (!all(c("Cluster", "Tax_Burden", "Promedio_10_Años_Crec_pib", "Fiscal_Health", "Government_Integrity", "Overall_score") %in% colnames(data))) {
          stop("El dataframe 'data' no tiene todas las columnas necesarias.")
        }
        
        # Calcular la TIR para cada cluster
        resultados_cluster <- data %>%
          group_by(Cluster) %>%
          mutate(
            tir_cluster = calcular_tir(
              costo_inicial = 5000,  # Costo inicial
              costo_recurrente = mean(Tax_Burden, na.rm = TRUE),  # Promedio de carga tributaria
              beneficio_incremento_pib = mean(Promedio_10_Años_Crec_pib * 10, na.rm = TRUE),  # Promedio de crecimiento del PIB
              fiscal_salud = mean(Fiscal_Health, na.rm = TRUE),  # Promedio de salud fiscal
              gobernanza = mean(Government_Integrity, na.rm = TRUE),  # Promedio de gobernanza
              ingreso_anual_bruto = mean(Overall_score, na.rm = TRUE)  # Promedio de ingresos anuales
            )
          ) %>%
          summarise(tir_cluster = mean(tir_cluster, na.rm = TRUE))  # Resumir el resultado por cluster
        
        return(resultados_cluster)
      }
      
      # Verifica el contenido de los resultados
      resultados_cluster <- calcular_tir_por_cluster(datos_clustering_nuevo)
      print(resultados_cluster)
      
      # Colores definidos
      colores <- c('#d6496c', '#2ca02c')
      
      # Crear el gráfico de barras para visualizar la TIR por Cluster
      plot_tir <- ggplot(resultados_cluster, aes(x = Cluster, y = tir_cluster, fill = Cluster)) +
        geom_bar(stat = "identity") +  # Usamos barras para visualizar las TIR
        scale_fill_manual(values = colores) +  # Aplicamos los colores definidos
        theme_minimal() +  # Estilo limpio
        labs(
          title = "TIR por Cluster", 
          x = "Cluster", 
          y = "TIR"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Giramos las etiquetas del eje X si es necesario
      
      # Convertir a gráfico interactivo con plotly
      ggplotly(plot_tir)
      
      
    })
    #Grafico cambios en la tasa de interes
    output$grafico_tasas_rentabilidad   <- renderPlotly({
      # Definir las tasas de descuento
      # Parámetros constantes para el modelo
      costo_inicial <- 5000  # En millones
      costo_recurrente <- 300  # Costo recurrente
      ingreso_anual_bruto <- 500  # Millones agregado recientemente
      anos <- 5  # Proyecto de 5 años
      
      tasas_descuento <- c(0.01, 0.03, 0.05, 0.07, 0.09, 0.1)
      
      # Definir el número de años para el análisis
      anos <- 5
      
      # Definir el costo inicial
      costo_inicial <- 5000  # Asegúrate de definirlo o adaptarlo a tu caso
      
      # Crear un data frame vacío para almacenar los resultados
      resultados <- data.frame(Cluster = integer(),
                               Tasa_Descuento = numeric(),
                               Beneficio_Descontado = numeric(),
                               Costo_Descontado = numeric(),
                               Valor_Neto = numeric())
      
      # Realizar el análisis de sensibilidad por cluster
      for (cluster_id in unique(datos_clustering_nuevo$Cluster)) {
        
        # Filtrar los datos para el cluster específico
        cluster_data <- datos_clustering_nuevo[datos_clustering_nuevo$Cluster == cluster_id, ]
        
        # Promedio de las variables para el cluster
        beneficio_incremento_pib <- mean(cluster_data$Promedio_10_Años_Crec_pib)  # Crecimiento PIB (ya en porcentaje)
        costo_recurrente <- mean(cluster_data$Tax_Burden)
        
        # Calcular los valores netos por tasas de descuento
        for (tasa in tasas_descuento) {
          
          # Calcular el beneficio descontado (ajustado por tasa de descuento)
          beneficios_desc <- sum(ingreso_anual_bruto * beneficio_incremento_pib / (1 + tasa)^(1:anos))
          
          # Calcular los costos descontados (ajustados por tasa de descuento)
          costos_desc <- costo_inicial + sum(costo_recurrente / (1 + tasa)^(1:anos))
          
          # Calcular el valor neto
          valor_neto <- beneficios_desc - costos_desc
          
          # Almacenar los resultados en el data frame
          resultados <- rbind(resultados, data.frame(Cluster = cluster_id, 
                                                     Tasa_Descuento = tasa, 
                                                     Beneficio_Descontado = beneficios_desc, 
                                                     Costo_Descontado = costos_desc, 
                                                     Valor_Neto = valor_neto))
        }
      }
      
      # Ver los resultados organizados por cluster y tasa de descuento
      print(resultados)
      # Crear un gráfico interactivo con plotly
      grafico_tasa_descuento <- plot_ly(data = resultados, 
                                        x = ~Tasa_Descuento,  # Usar el nombre correcto de la columna
                                        y = ~Valor_Neto,      # Usar el nombre correcto de la columna
                                        color = ~factor(Cluster),  # Usar el nombre correcto de la columna
                                        type = 'scatter', 
                                        mode = 'lines+markers',
                                        line = list(width = 2),
                                        marker = list(size = 8),
                                        colors = c('#d6496c', '#2ca02c')) %>%  # Colores explícitos por cluster
        layout(title = "Análisis de Sensibilidad por Cluster",
               xaxis = list(title = "Tasa de Descuento"),
               yaxis = list(title = "Valor Presente Neto"),
               showlegend = TRUE)
      
      # Mostrar el gráfico interactivo
      grafico_tasa_descuento
      
    })
    
    #==================================================================================
    #===============================INTERACTIVO======================================
    # Crear un evento reactivo para cuando el botón "Calcular" es presionado
    observeEvent(input$calcular, {
      
      # Calcular el valor neto para ambos clusters
      resultado_cluster_1 <- calcular_valor_neto_ajustado(
        input$costo_inicial,
        input$costo_recurrente,
        beneficio_pib_cluster_1,
        input$tasa_descuento,
        input$anos,
        fiscalidad_cluster_1,
        gobernanza_cluster_1,
        input$ingreso_esperado  # Usamos ingreso esperado desde el slider
      )
      
      resultado_cluster_2 <- calcular_valor_neto_ajustado(
        input$costo_inicial,
        input$costo_recurrente,
        beneficio_pib_cluster_2,
        input$tasa_descuento,
        input$anos,
        fiscalidad_cluster_2,
        gobernanza_cluster_2,
        input$ingreso_esperado  # Usamos ingreso esperado desde el slider
      )
      
      # Mostrar los resultados de valor neto ajustado
      output$valor_neto_cluster_1 <- renderText({
        paste("El Valor Neto Ajustado para el Cluster 1 es: $", round(resultado_cluster_1$valor_neto, 2))
      })
      
      output$valor_neto_cluster_2 <- renderText({
        paste("El Valor Neto Ajustado para el Cluster 2 es: $", round(resultado_cluster_2$valor_neto, 2))
      })
      
      # Gráfico interactivo de Plotly para el Cluster 1
      output$grafico_cluster_1 <- renderPlotly({
        plot_ly(
          x = 1:input$anos, 
          y = resultado_cluster_1$beneficios_desc, 
          type = "scatter", 
          mode = "lines+markers", 
          name = "Beneficios Cluster 1", 
          line = list(color = '#d6496c', width = 3),
          marker = list(size = 8, color = '#d6496c', opacity = 0.7)
        ) %>%
          add_trace(
            y = resultado_cluster_1$costos_desc, 
            name = "Costos Cluster 1", 
            line = list(color = '#7c4d6f', width = 3, dash = 'dash'),
            marker = list(size = 8, color = '#7c4d6f', opacity = 0.7)
          ) %>%
          layout(
            title = "Beneficios y Costos Descontados para Cluster 1",
            xaxis = list(title = "Años"),
            yaxis = list(title = "Monto ($)", rangemode = "tozero"),
            legend = list(x = 0.8, y = 0.1)
          )
      })
      
      # Gráfico interactivo de Plotly para el Cluster 2
      output$grafico_cluster_2 <- renderPlotly({
        plot_ly(
          x = 1:input$anos, 
          y = resultado_cluster_2$beneficios_desc, 
          type = "scatter", 
          mode = "lines+markers", 
          name = "Beneficios Cluster 2", 
          line = list(color = '#2ca02c', width = 3),
          marker = list(size = 8, color = '#2ca02c', opacity = 0.7)
        ) %>%
          add_trace(
            y = resultado_cluster_2$costos_desc, 
            name = "Costos Cluster 2", 
            line = list(color = '#4c9a2a', width = 3, dash = 'dash'),
            marker = list(size = 8, color = '#4c9a2a', opacity = 0.7)
          ) %>%
          layout(
            title = "Beneficios y Costos Descontados para Cluster 2",
            xaxis = list(title = "Años"),
            yaxis = list(title = "Monto ($)", rangemode = "tozero"),
            legend = list(x = 0.8, y = 0.1)
          )
      })
      
    })
}

shinyApp(ui = ui, server = server)

  
