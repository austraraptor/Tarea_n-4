
library(shiny)
library(ggplot2)
library(dplyr)
library(nortest)
library(tseries)
library(moments)
library(report)
library(readr)

ui <- fluidPage(
  titlePanel("Análisis Estadístico y Visualización Inteligente"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Sube tu archivo CSV", accept = ".csv"),
      uiOutput("varselect"),
      selectInput("metodo", "Método estadístico",
                  choices = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Lilliefors", "Jarque-Bera")),
      actionButton("analizar", "Ejecutar análisis"),
      conditionalPanel(
        condition = "input.analizar > 0",
        selectInput("grafico", "Tipo de gráfico",
                    choices = c("Barras", "Circular", "Líneas", "Dispersión", "Histograma", "Caja y bigotes", "Pictograma"))
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.analizar > 0",
        h4("Resultado del análisis"),
        verbatimTextOutput("resultado"),
        
        h4("Gráfico seleccionado"),
        uiOutput("graficoUI"),
        
        h4("Reporte automático"),
        verbatimTextOutput("reporte")
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$datafile)
    read_csv(input$datafile$datapath)
  })
  
  output$varselect <- renderUI({
    req(datos())
    selectInput("variable", "Selecciona una variable", choices = names(datos()))
  })
  
  output$resultado <- renderPrint({
    req(input$analizar)
    isolate({
      df <- datos()
      var <- input$variable
      x <- na.omit(df[[var]])
      
      if (!is.numeric(x)) {
        cat("La variable seleccionada no es numérica. El test no se puede aplicar.")
      } else {
        switch(input$metodo,
               "Shapiro-Wilk" = print(shapiro.test(x)),
               "Kolmogorov-Smirnov" = print(ks.test(x, "pnorm", mean(x), sd(x))),
               "Lilliefors" = print(lillie.test(x)),
               "Jarque-Bera" = print(jarque.bera.test(x))
        )
      }
    })
  })
  
  output$graficoUI <- renderUI({
    req(input$grafico)
    plotOutput("grafico")
  })
  
  output$grafico <- renderPlot({
    req(input$analizar, input$grafico)
    isolate({
      df <- datos()
      var <- input$variable
      x <- df[[var]]
      
      tipo <- ifelse(is.numeric(x), "cuantitativa", "cualitativa")
      
      
      if (input$grafico == "Histograma" && tipo == "cuantitativa") {
        ggplot(df, aes(x = .data[[var]])) +
          geom_histogram(fill = "skyblue", bins = 30, color = "black") +
          theme_minimal()
        
      } else if (input$grafico == "Caja y bigotes" && tipo == "cuantitativa") {
        ggplot(df, aes(y = .data[[var]])) +
          geom_boxplot(fill = "orange") +
          theme_minimal()
        
      } else if (input$grafico == "Líneas" && tipo == "cuantitativa") {
        ggplot(df, aes(x = seq_along(.data[[var]]), y = .data[[var]])) +
          geom_line(color = "blue") +
          theme_minimal()
        
      } else if (input$grafico == "Dispersión" && tipo == "cuantitativa") {
        ggplot(df, aes(x = seq_along(.data[[var]]), y = .data[[var]])) +
          geom_point(color = "darkred") +
          theme_minimal()
        
      } else if (input$grafico == "Barras" && tipo == "cualitativa") {
        ggplot(df, aes(x = .data[[var]])) +
          geom_bar(fill = "steelblue") +
          theme_minimal()
        
      } else if (input$grafico == "Circular" && tipo == "cualitativa") {
        ggplot(df, aes(x = "", fill = .data[[var]])) +
          geom_bar(width = 1) +
          coord_polar("y") +
          theme_void()
        
      } else if (input$grafico == "Pictograma" && tipo == "cualitativa") {
        ggplot(df, aes(x = .data[[var]])) +
          geom_point(stat = "count", size = 10, shape = 21, fill = "purple") +
          theme_minimal()
        
      } else {
        
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No compatible", size = 8, color = "red") +
          theme_void()
      }
    })
  })
  
  output$reporte <- renderPrint({
    req(input$analizar)
    isolate({
      df <- datos()
      var <- input$variable
      x <- na.omit(df[[var]])
      
      if (is.numeric(x)) {
        print(report(x))
      } else {
        cat("Solo se generan reportes para variables numéricas.")
      }
    })
  })
}

shinyApp(ui, server)
