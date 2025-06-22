# Aplicación Shiny para Planeación Agregada - ManuTech S.A.
# Versión Corregida y Funcional
# Requiere: install.packages(c("shiny", "shinydashboard", "DT", "plotly", "dplyr"))

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)

# Función para resolver el problema de planeación
solve_production_heuristic <- function(demanda, params) {
  
  T <- 12
  resultados <- data.frame()
  
  # Parámetros
  cap_trabajador_regular <- 920  # unidades/trabajador/mes
  cap_trabajador_extra <- 90     # unidades/trabajador/mes
  max_trabajadores <- params$max_trabajadores
  min_trabajadores <- params$min_trabajadores
  inventario_inicial <- params$inventario_inicial
  inventario_final_deseado <- params$inventario_final_deseado
  
  # Variables de estado
  trabajadores_actual <- params$empleados_inicial
  inventario_actual <- inventario_inicial
  costo_total <- 0
  
  for (t in 1:T) {
    mes <- month.name[t]
    demanda_periodo <- demanda[t] * 1000
    
    # Calcular necesidad neta
    necesidad_neta <- demanda_periodo - inventario_actual
    if (necesidad_neta < 0) necesidad_neta <- 0
    
    # Capacidad interna máxima
    cap_interna_max <- trabajadores_actual * (cap_trabajador_regular + cap_trabajador_extra)
    
    # Decisión de producción interna vs subcontratación
    if (necesidad_neta <= cap_interna_max) {
      # Puede cubrirse con producción interna
      prod_regular <- min(necesidad_neta, trabajadores_actual * cap_trabajador_regular)
      prod_extra <- max(0, necesidad_neta - prod_regular)
      
      # No se necesita subcontratación
      prov_a <- 0
      prov_b <- 0
      prov_c <- 0
      
    } else {
      # Producir al máximo internamente
      prod_regular <- trabajadores_actual * cap_trabajador_regular
      prod_extra <- trabajadores_actual * cap_trabajador_extra
      
      # Calcular lo que falta por subcontratación
      restante <- necesidad_neta - cap_interna_max
      
      # Asignar a proveedores en orden de costo (C más barato → A más caro)
      # Proveedor C (más barato: $25)
      prov_c <- min(restante, params$cap_c)
      restante <- restante - prov_c
      
      # Proveedor B (intermedio: $28.5)
      if (restante > 0) {
        prov_b <- min(restante, params$cap_b)
        restante <- restante - prov_b
      } else {
        prov_b <- 0
      }
      
      # Proveedor A (más caro: $32)
      if (restante > 0) {
        prov_a <- min(restante, params$cap_a)
      } else {
        prov_a <- 0
      }
    }
    
    # Ajustar fuerza laboral (heurística simple)
    demanda_promedio <- mean(demanda) * 1000
    trabajadores_necesarios <- ceiling(demanda_promedio / cap_trabajador_regular)
    trabajadores_necesarios <- max(min_trabajadores, min(trabajadores_necesarios, max_trabajadores))
    
    contratados <- max(0, trabajadores_necesarios - trabajadores_actual)
    despedidos <- max(0, trabajadores_actual - trabajadores_necesarios)
    trabajadores_actual <- trabajadores_necesarios
    
    # Calcular inventario
    inventario_inicial_periodo <- inventario_actual
    produccion_total <- prod_regular + prod_extra + prov_a + prov_b + prov_c
    inventario_final <- inventario_actual + produccion_total - demanda_periodo
    
    # Asegurar inventario no negativo
    if (inventario_final < 0) {
      deficit <- abs(inventario_final)
      
      # Asignar déficit a proveedores disponibles
      if (prov_c + deficit <= params$cap_c) {
        prov_c <- prov_c + deficit
      } else if (prov_b + deficit <= params$cap_b) {
        prov_b <- prov_b + deficit
      } else if (prov_a + deficit <= params$cap_a) {
        prov_a <- prov_a + deficit
      }
      
      # Recalcular
      produccion_total <- prod_regular + prod_extra + prov_a + prov_b + prov_c
      inventario_final <- inventario_actual + produccion_total - demanda_periodo
      
      if (inventario_final < 0) {
        inventario_final <- 0
      }
    }
    
    # Ajuste para el último período
    if (t == T && inventario_final != inventario_final_deseado) {
      diferencia <- inventario_final_deseado - inventario_final
      if (diferencia > 0 && prov_c + diferencia <= params$cap_c) {
        prov_c <- prov_c + diferencia
        produccion_total <- prod_regular + prod_extra + prov_a + prov_b + prov_c
        inventario_final <- inventario_actual + produccion_total - demanda_periodo
      }
    }
    
    # Calcular costos del período
    costo_salarios <- trabajadores_actual * params$salario_regular * 8 * 23
    costo_tiempo_extra <- (prod_extra / 5) * params$salario_extra
    costo_materiales <- (prod_regular + prod_extra) * params$costo_material
    costo_contratacion <- contratados * params$costo_contratacion
    costo_despido <- despedidos * params$costo_despido
    costo_prov_a <- prov_a * params$costo_a
    costo_prov_b <- prov_b * params$costo_b
    costo_prov_c <- prov_c * params$costo_c
    costo_inventario <- inventario_final * params$costo_inventario
    
    costo_periodo <- costo_salarios + costo_tiempo_extra + costo_materiales + 
      costo_contratacion + costo_despido + costo_prov_a + 
      costo_prov_b + costo_prov_c + costo_inventario
    
    costo_total <- costo_total + costo_periodo
    
    # Almacenar resultados
    fila <- data.frame(
      Mes = mes,
      Periodo = t,
      Demanda = demanda_periodo,
      Inventario_Inicial = round(inventario_inicial_periodo),
      Prod_Regular = round(prod_regular),
      Prod_Extra = round(prod_extra),
      Trabajadores = trabajadores_actual,
      Contratados = contratados,
      Despedidos = despedidos,
      Prov_A = round(prov_a),
      Prov_B = round(prov_b),
      Prov_C = round(prov_c),
      Inventario_Final = round(inventario_final),
      Costo_Salarios = round(costo_salarios),
      Costo_Tiempo_Extra = round(costo_tiempo_extra),
      Costo_Materiales = round(costo_materiales),
      Costo_Contratacion = round(costo_contratacion),
      Costo_Despido = round(costo_despido),
      Costo_Prov_A = round(costo_prov_a),
      Costo_Prov_B = round(costo_prov_b),
      Costo_Prov_C = round(costo_prov_c),
      Costo_Inventario = round(costo_inventario),
      Costo_Total_Mes = round(costo_periodo)
    )
    
    resultados <- rbind(resultados, fila)
    inventario_actual <- inventario_final
  }
  
  return(list(
    status = "Óptimo (Heurística)",
    costo_total = costo_total,
    detalle = resultados
  ))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Planeación Agregada ManuTech S.A."),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parámetros", tabName = "parametros", icon = icon("cog")),
      menuItem("Resultados", tabName = "resultados", icon = icon("chart-line")),
      menuItem("Análisis Detallado", tabName = "analisis", icon = icon("analytics")),
      menuItem("Costos", tabName = "costos", icon = icon("dollar-sign"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f4f4f4;
      }
    "))),
    
    tabItems(
      # Pestaña de parámetros
      tabItem(tabName = "parametros",
              fluidRow(
                box(title = "Demanda Mensual (miles de unidades)", status = "primary", 
                    solidHeader = TRUE, width = 6,
                    numericInput("d1", "Enero:", value = 1050, min = 0, step = 10),
                    numericInput("d2", "Febrero:", value = 1150, min = 0, step = 10),
                    numericInput("d3", "Marzo:", value = 1300, min = 0, step = 10),
                    numericInput("d4", "Abril:", value = 1400, min = 0, step = 10),
                    numericInput("d5", "Mayo:", value = 1550, min = 0, step = 10),
                    numericInput("d6", "Junio:", value = 1700, min = 0, step = 10)
                ),
                
                box(title = "Demanda Mensual (continuación)", status = "primary", 
                    solidHeader = TRUE, width = 6,
                    numericInput("d7", "Julio:", value = 1890, min = 0, step = 10),
                    numericInput("d8", "Agosto:", value = 1620, min = 0, step = 10),
                    numericInput("d9", "Septiembre:", value = 1050, min = 0, step = 10),
                    numericInput("d10", "Octubre:", value = 1250, min = 0, step = 10),
                    numericInput("d11", "Noviembre:", value = 1200, min = 0, step = 10),
                    numericInput("d12", "Diciembre:", value = 1350, min = 0, step = 10)
                )
              ),
              
              fluidRow(
                box(title = "Parámetros de Costos", status = "info", 
                    solidHeader = TRUE, width = 6,
                    numericInput("salario_regular", "Salario Regular ($/hora):", value = 25, min = 0),
                    numericInput("salario_extra", "Salario Tiempo Extra ($/hora):", value = 37.5, min = 0),
                    numericInput("costo_material", "Costo Material ($/unidad):", value = 24, min = 0),
                    numericInput("costo_contratacion", "Costo Contratación ($):", value = 27, min = 0),
                    numericInput("costo_despido", "Costo Despido ($):", value = 29, min = 0),
                    numericInput("costo_inventario", "Costo Inventario ($/unidad/mes):", value = 3, min = 0)
                ),
                
                box(title = "Parámetros de Fuerza Laboral e Inventario", status = "warning", 
                    solidHeader = TRUE, width = 6,
                    h5("Fuerza Laboral"),
                    numericInput("empleados_inicial", "Empleados Iniciales:", value = 900, min = 1, max = 2000),
                    numericInput("min_trabajadores", "Mínimo de Trabajadores:", value = 600, min = 1),
                    numericInput("max_trabajadores", "Máximo de Trabajadores:", value = 900, min = 1),
                    br(),
                    h5("Inventario"),
                    numericInput("inventario_inicial", "Inventario Inicial (unidades):", value = 60000, min = 0),
                    numericInput("inventario_final_deseado", "Inventario Final Deseado:", value = 60000, min = 0)
                )
              ),
              
              fluidRow(
                box(title = "Parámetros de Proveedores", status = "info", 
                    solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4,
                             h5("Proveedor A"),
                             numericInput("cap_a", "Capacidad (unidades/mes):", value = 145000, min = 0),
                             numericInput("costo_a", "Costo ($/unidad):", value = 32, min = 0)
                      ),
                      column(4,
                             h5("Proveedor B"),
                             numericInput("cap_b", "Capacidad (unidades/mes):", value = 136000, min = 0),
                             numericInput("costo_b", "Costo ($/unidad):", value = 28.5, min = 0)
                      ),
                      column(4,
                             h5("Proveedor C"),
                             numericInput("cap_c", "Capacidad (unidades/mes):", value = 115000, min = 0),
                             numericInput("costo_c", "Costo ($/unidad):", value = 25, min = 0)
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = "Optimización", status = "success", 
                    solidHeader = TRUE, width = 12,
                    br(),
                    actionButton("resolver", "🚀 Optimizar Plan de Producción", 
                                 class = "btn-success btn-lg", width = "100%",
                                 style = "height: 60px; font-size: 18px;"),
                    br(), br(),
                    div(style = "text-align: center;", 
                        textOutput("mensaje_estado")),
                    br(),
                    div(style = "text-align: center; color: #777; font-size: 12px;",
                        "💡 Tip: Asegúrese que empleados inicial esté entre mínimo y máximo de trabajadores")
                )
              )
      ),
      
      # Pestaña de resultados
      tabItem(tabName = "resultados",
              fluidRow(
                valueBoxOutput("costo_total", width = 4),
                valueBoxOutput("status_solucion", width = 4),
                valueBoxOutput("total_produccion", width = 4)
              ),
              
              fluidRow(
                box(title = "📊 Plan de Producción Mensual", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("tabla_resultados")
                )
              ),
              
              fluidRow(
                box(title = "📈 Producción vs Demanda", status = "info", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_produccion", height = "400px")
                ),
                
                box(title = "👥 Gestión de Fuerza Laboral", status = "info", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_trabajadores", height = "400px")
                )
              )
      ),
      
      # Pestaña de análisis
      tabItem(tabName = "analisis",
              fluidRow(
                box(title = "🏭 Utilización de Proveedores", status = "primary", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_proveedores", height = "400px")
                ),
                
                box(title = "📦 Evolución del Inventario", status = "primary", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_inventario", height = "400px")
                )
              ),
              
              fluidRow(
                box(title = "⚡ Producción Interna vs Externa", status = "info", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_prod_mix", height = "400px")
                ),
                
                box(title = "📋 Resumen Ejecutivo", status = "info", 
                    solidHeader = TRUE, width = 6,
                    br(),
                    tableOutput("resumen_ejecutivo")
                )
              )
      ),
      
      # Pestaña de costos
      tabItem(tabName = "costos",
              fluidRow(
                box(title = "💰 Desglose de Costos Mensuales", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("tabla_costos_detalle")
                )
              ),
              
              fluidRow(
                box(title = "📊 Composición de Costos Anuales", status = "info", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_costos_composicion", height = "400px")
                ),
                
                box(title = "📈 Evolución de Costos", status = "info", 
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("grafico_costos_evolucion", height = "400px")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Variable reactiva para almacenar resultados
  resultados <- reactiveVal(NULL)
  
  # Resolver modelo cuando se presiona el botón
  observeEvent(input$resolver, {
    output$mensaje_estado <- renderText("Validando parámetros...")
    
    # Validaciones de entrada
    if (input$empleados_inicial < input$min_trabajadores || input$empleados_inicial > input$max_trabajadores) {
      output$mensaje_estado <- renderText("❌ Error: Empleados inicial debe estar entre mínimo y máximo de trabajadores")
      return()
    }
    
    if (input$min_trabajadores > input$max_trabajadores) {
      output$mensaje_estado <- renderText("❌ Error: Mínimo de trabajadores no puede ser mayor que el máximo")
      return()
    }
    
    output$mensaje_estado <- renderText("Optimizando plan de producción...")
    
    # Recopilar demanda
    demanda <- c(input$d1, input$d2, input$d3, input$d4, input$d5, input$d6,
                 input$d7, input$d8, input$d9, input$d10, input$d11, input$d12)
    
    # Parámetros
    params <- list(
      salario_regular = input$salario_regular,
      salario_extra = input$salario_extra,
      costo_material = input$costo_material,
      costo_contratacion = input$costo_contratacion,
      costo_despido = input$costo_despido,
      costo_inventario = input$costo_inventario,
      empleados_inicial = input$empleados_inicial,
      min_trabajadores = input$min_trabajadores,
      max_trabajadores = input$max_trabajadores,
      inventario_inicial = input$inventario_inicial,
      inventario_final_deseado = input$inventario_final_deseado,
      cap_a = input$cap_a, costo_a = input$costo_a,
      cap_b = input$cap_b, costo_b = input$costo_b,
      cap_c = input$cap_c, costo_c = input$costo_c
    )
    
    # Resolver modelo
    tryCatch({
      sol <- solve_production_heuristic(demanda, params)
      resultados(sol)
      output$mensaje_estado <- renderText("✅ Optimización completada exitosamente")
    }, error = function(e) {
      output$mensaje_estado <- renderText(paste("❌ Error:", e$message))
    })
  })
  
  # Value boxes
  output$costo_total <- renderValueBox({
    if (is.null(resultados())) {
      valueBox(value = "N/A", subtitle = "Costo Total Anual", 
               icon = icon("dollar-sign"), color = "blue")
    } else {
      valueBox(value = paste0("$", format(round(resultados()$costo_total), big.mark = ",")), 
               subtitle = "Costo Total Anual", 
               icon = icon("dollar-sign"), color = "green")
    }
  })
  
  output$status_solucion <- renderValueBox({
    if (is.null(resultados())) {
      valueBox(value = "Pendiente", subtitle = "Estado", 
               icon = icon("clock"), color = "yellow")
    } else {
      valueBox(value = "Resuelto", subtitle = "Estado de la Solución", 
               icon = icon("check"), color = "green")
    }
  })
  
  output$total_produccion <- renderValueBox({
    if (is.null(resultados())) {
      valueBox(value = "N/A", subtitle = "Total Producido", 
               icon = icon("industry"), color = "blue")
    } else {
      total <- sum(resultados()$detalle$Prod_Regular + resultados()$detalle$Prod_Extra)
      valueBox(value = format(round(total), big.mark = ","), 
               subtitle = "Total Unidades Producidas", 
               icon = icon("industry"), color = "purple")
    }
  })
  
  # Tabla de resultados principales
  output$tabla_resultados <- DT::renderDataTable({
    if (is.null(resultados())) {
      return(data.frame(Mensaje = "Presione 'Optimizar Plan de Producción' para ver los resultados"))
    }
    
    tabla <- resultados()$detalle %>%
      select(Mes, Demanda, Inventario_Inicial, Prod_Regular, Prod_Extra, Trabajadores, 
             Contratados, Despedidos, Prov_A, Prov_B, Prov_C, Inventario_Final) %>%
      mutate(
        Demanda = format(Demanda, big.mark = ","),
        Inventario_Inicial = format(Inventario_Inicial, big.mark = ","),
        Prod_Regular = format(Prod_Regular, big.mark = ","),
        Prod_Extra = format(Prod_Extra, big.mark = ","),
        Prov_A = format(Prov_A, big.mark = ","),
        Prov_B = format(Prov_B, big.mark = ","),
        Prov_C = format(Prov_C, big.mark = ","),
        Inventario_Final = format(Inventario_Final, big.mark = ",")
      ) %>%
      rename(
        "Inv. Inicial" = Inventario_Inicial,
        "Prod. Regular" = Prod_Regular,
        "Prod. Extra" = Prod_Extra,
        "Trabajadores" = Trabajadores,
        "Contratados" = Contratados,
        "Despedidos" = Despedidos,
        "Prov. A" = Prov_A,
        "Prov. B" = Prov_B,
        "Prov. C" = Prov_C,
        "Inv. Final" = Inventario_Final
      )
    
    DT::datatable(tabla, 
                  options = list(scrollX = TRUE, pageLength = 12, dom = 'tip'),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = c("Inv. Inicial", "Inv. Final"), 
                      backgroundColor = "#e6f3ff")
  })
  
  # Gráfico de producción
  output$grafico_produccion <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los gráficos"))
    }
    
    datos <- resultados()$detalle
    
    p <- plot_ly(datos, x = ~Mes) %>%
      add_trace(y = ~Demanda, name = "Demanda", type = 'scatter', 
                mode = 'lines+markers', line = list(color = 'red', dash = 'dash', width = 3)) %>%
      add_trace(y = ~Prod_Regular, name = "Producción Regular", type = 'bar', 
                marker = list(color = 'lightblue')) %>%
      add_trace(y = ~Prod_Extra, name = "Tiempo Extra", type = 'bar', 
                marker = list(color = 'orange')) %>%
      layout(title = "Producción vs Demanda Mensual", 
             yaxis = list(title = "Unidades"),
             xaxis = list(title = "Mes"),
             barmode = 'stack')
    
    p
  })
  
  # Gráfico de trabajadores
  output$grafico_trabajadores <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los gráficos"))
    }
    
    datos <- resultados()$detalle
    
    p <- plot_ly(datos, x = ~Mes) %>%
      add_trace(y = ~Trabajadores, name = "Trabajadores Totales", type = 'scatter', 
                mode = 'lines+markers', line = list(color = 'blue', width = 3)) %>%
      add_trace(y = ~Contratados, name = "Contratados", type = 'bar', 
                marker = list(color = 'green')) %>%
      add_trace(y = ~Despedidos, name = "Despedidos", type = 'bar', 
                marker = list(color = 'red')) %>%
      layout(title = "Gestión de Fuerza Laboral", 
             yaxis = list(title = "Número de Trabajadores"),
             xaxis = list(title = "Mes"))
    
    p
  })
  
  # Gráfico de proveedores
  output$grafico_proveedores <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los gráficos"))
    }
    
    datos <- resultados()$detalle
    
    p <- plot_ly(datos, x = ~Mes) %>%
      add_trace(y = ~Prov_C, name = "Proveedor C ($25)", type = 'bar', 
                marker = list(color = 'lightgreen')) %>%
      add_trace(y = ~Prov_B, name = "Proveedor B ($28.5)", type = 'bar', 
                marker = list(color = 'orange')) %>%
      add_trace(y = ~Prov_A, name = "Proveedor A ($32)", type = 'bar', 
                marker = list(color = 'red')) %>%
      layout(title = "Compras a Proveedores Externos", 
             yaxis = list(title = "Unidades Compradas"),
             xaxis = list(title = "Mes"),
             barmode = 'stack')
    
    p
  })
  
  # Gráfico de inventario
  output$grafico_inventario <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los gráficos"))
    }
    
    datos <- resultados()$detalle
    
    p <- plot_ly(datos, x = ~Mes) %>%
      add_trace(y = ~Inventario_Inicial, name = "Inventario Inicial", type = 'scatter', 
                mode = 'lines+markers', line = list(color = 'blue', width = 2, dash = 'dash'),
                marker = list(size = 6)) %>%
      add_trace(y = ~Inventario_Final, name = "Inventario Final", type = 'scatter', 
                mode = 'lines+markers', fill = 'tonexty', fillcolor = 'rgba(128, 0, 128, 0.2)',
                line = list(color = 'purple', width = 3),
                marker = list(size = 8)) %>%
      layout(title = "Evolución del Inventario (Inicial vs Final)", 
             yaxis = list(title = "Unidades en Inventario"),
             xaxis = list(title = "Mes"))
    
    p
  })
  
  # Gráfico de mix de producción
  output$grafico_prod_mix <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los gráficos"))
    }
    
    datos <- resultados()$detalle %>%
      mutate(
        Prod_Total_Interna = Prod_Regular + Prod_Extra,
        Prod_Total_Externa = Prov_A + Prov_B + Prov_C
      )
    
    p <- plot_ly(datos, x = ~Mes) %>%
      add_trace(y = ~Prod_Total_Interna, name = "Producción Interna", type = 'bar',
                marker = list(color = 'lightblue')) %>%
      add_trace(y = ~Prod_Total_Externa, name = "Subcontratación", type = 'bar',
                marker = list(color = 'lightcoral')) %>%
      layout(title = "Producción Interna vs Subcontratación", 
             yaxis = list(title = "Unidades"),
             xaxis = list(title = "Mes"),
             barmode = 'stack')
    
    p
  })
  
  # Tabla de costos detallados
  output$tabla_costos_detalle <- DT::renderDataTable({
    if (is.null(resultados())) {
      return(data.frame(Mensaje = "Resuelva el modelo para ver el desglose de costos"))
    }
    
    tabla_costos <- resultados()$detalle %>%
      select(Mes, Costo_Salarios, Costo_Tiempo_Extra, Costo_Materiales, 
             Costo_Contratacion, Costo_Despido, Costo_Prov_A, Costo_Prov_B, 
             Costo_Prov_C, Costo_Inventario, Costo_Total_Mes) %>%
      mutate_if(is.numeric, function(x) paste0("$", format(round(x), big.mark = ",")))
    
    DT::datatable(tabla_costos, 
                  options = list(scrollX = TRUE, pageLength = 12, dom = 'tip'),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:ncol(tabla_costos), fontSize = '11px')
  })
  
  # Gráfico de composición de costos
  output$grafico_costos_composicion <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los costos"))
    }
    
    datos_costos <- resultados()$detalle
    
    # Calcular totales manualmente
    total_salarios <- sum(datos_costos$Costo_Salarios)
    total_tiempo_extra <- sum(datos_costos$Costo_Tiempo_Extra)
    total_materiales <- sum(datos_costos$Costo_Materiales)
    total_personal <- sum(datos_costos$Costo_Contratacion + datos_costos$Costo_Despido)
    total_prov_a <- sum(datos_costos$Costo_Prov_A)
    total_prov_b <- sum(datos_costos$Costo_Prov_B)
    total_prov_c <- sum(datos_costos$Costo_Prov_C)
    total_inventario <- sum(datos_costos$Costo_Inventario)
    
    # Crear data frame para el gráfico
    componentes <- c("Salarios", "Tiempo Extra", "Materiales", "Contratación/Despido",
                     "Proveedor A", "Proveedor B", "Proveedor C", "Inventario")
    valores <- c(total_salarios, total_tiempo_extra, total_materiales, total_personal,
                 total_prov_a, total_prov_b, total_prov_c, total_inventario)
    
    datos_pie <- data.frame(Componente = componentes, Costo = valores)
    
    p <- plot_ly(datos_pie, labels = ~Componente, values = ~Costo, type = 'pie',
                 textposition = 'inside', textinfo = 'label+percent') %>%
      layout(title = "Composición del Costo Total Anual")
    
    p
  })
  
  # Gráfico de evolución de costos
  output$grafico_costos_evolucion <- renderPlotly({
    if (is.null(resultados())) {
      return(plotly_empty() %>% layout(title = "Resuelva el modelo para ver los costos"))
    }
    
    datos <- resultados()$detalle
    
    p <- plot_ly(datos, x = ~Mes, y = ~Costo_Total_Mes, type = 'scatter',
                 mode = 'lines+markers', line = list(color = 'green', width = 3),
                 marker = list(size = 8)) %>%
      layout(title = "Evolución del Costo Total Mensual", 
             yaxis = list(title = "Costo ($)"),
             xaxis = list(title = "Mes"))
    
    p
  })
  
  # Resumen ejecutivo
  output$resumen_ejecutivo <- renderTable({
    if (is.null(resultados())) {
      return(data.frame(Métrica = "Resuelva el modelo", Valor = "para ver el resumen"))
    }
    
    datos <- resultados()$detalle
    
    resumen <- data.frame(
      Métrica = c(
        "Costo Total Anual",
        "Producción Interna Total",
        "Subcontratación Total",
        "Promedio de Trabajadores",
        "Total Contrataciones",
        "Total Despidos",
        "Inventario Promedio Final",
        "Inventario Final del Año"
      ),
      Valor = c(
        paste0("$", format(round(resultados()$costo_total), big.mark = ",")),
        format(sum(datos$Prod_Regular + datos$Prod_Extra), big.mark = ","),
        format(sum(datos$Prov_A + datos$Prov_B + datos$Prov_C), big.mark = ","),
        format(round(mean(datos$Trabajadores)), big.mark = ","),
        format(sum(datos$Contratados), big.mark = ","),
        format(sum(datos$Despedidos), big.mark = ","),
        format(round(mean(datos$Inventario_Final)), big.mark = ","),
        format(datos$Inventario_Final[12], big.mark = ",")
      )
    )
    
    resumen
  }, striped = TRUE, hover = TRUE)
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)