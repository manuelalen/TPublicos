library(shiny)
library(plotly)

# Leer datos desde GitHub
data_url <- "https://raw.githubusercontent.com/manuelalen/TPublicos/main/Funcionarios.csv"
sheet <- read.csv(data_url, dec = ",")

# Convertir la columna "Año" a formato numérico
sheet$Año <- as.numeric(sheet$Año)

# Convertir la columna "Porcentaje" a formato numérico
sheet$Porcentaje <- gsub("[^0-9.-]", "", sheet$Porcentaje)
sheet$Porcentaje <- as.numeric(sheet$Porcentaje)

# UI
ui <- fluidPage(
  titlePanel("Gráfico de Líneas Interactivo con Slicer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_slider", "Selecciona el año:",
                  min = min(sheet$Año), max = max(sheet$Año),
                  value = min(sheet$Año), step = 1)
    ),
    
    mainPanel(
      plotlyOutput("line_plot")
    )
  )
)

# Server
server <- function(input, output) {
  # Función para generar el gráfico
  generate_plot <- reactive({
    filtered_df <- sheet[sheet$Año >= input$year_slider, ]
    
    plot_ly(filtered_df, x = ~Año) %>%
      add_trace(y = ~Porcentaje, name = 'Públicos', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~LIM_INF, name = 'LIM_INF', line = list(color = "green", dash = 'dash', opacity = 0.7), type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~LIM_SUP, name = 'LIM_SUP', line = list(color = "red", dash = 'dot', opacity = 0.7), type = 'scatter', mode = 'lines') %>%
      layout(title = "Porcentaje de trabajadores públicos por año",
             xaxis = list(title = "Año", color = 'black', showline = TRUE),
             yaxis = list(title = "Porcentaje trabajadores públicos", color = 'black', showline = TRUE),
             showlegend = TRUE,
             showgrid = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  # Renderizar el gráfico
  output$line_plot <- renderPlotly({
    generate_plot()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
