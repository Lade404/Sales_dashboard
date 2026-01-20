library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(readxl)
library(plotly)


# -----------------------------
# DASHBOARD UI
# -----------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "📊 Sales Dashboard for Beginners"),
  
  dashboardSidebar(
    sidebarMenu(
      fileInput("upload","Upload your document", accept = c(".xlsx", ".xls")),
      hr(),
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Table", tabName = "data", icon = icon("table")),
      menuItem("Visuals", tabName = "visuals", icon = icon("chart-bar"))
    ),
    br(),
   uiOutput("region_ui"),
   uiOutput("product_ui")
  ),
  
  dashboardBody(
    tabItems(
      
      # ---- Overview Tab ----
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales", width = 4),
                valueBoxOutput("avg_profit", width = 4),
                valueBoxOutput("top_region", width = 4)
              ),
              fluidRow(
                box(plotlyOutput("line_plot", height = "300px"),width = 12, title = "📈 Monthly Average Sales Trend", status = "primary", solidHeader = TRUE)
              )
      ),
      
      # ---- Data Table Tab ----
      tabItem(tabName = "data",
              box(width = 12, title = "🗂️ Sales Data Table", status = "info", solidHeader = TRUE,
                  DTOutput("sales_table"))
      ),
      
      # ---- Visuals Tab ----
      tabItem(tabName = "visuals",
              fluidRow(
                box(width = 6, title = "💰 Sales by Region", status = "success", solidHeader = TRUE,
                    plotOutput("bar_region", height = "300px")),
                box(width = 6, title = "📦 Profit by Product", status = "warning", solidHeader = TRUE,
                    plotOutput("bar_product", height = "300px"))
              )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output,session) {
  
  # --- Load Uploaded Data ---
 # To make this table be displayed by default
 data_reactive <- reactive({
   if (is.null(input$upload)) {
     read_excel("sales_data.xlsx")  # Default fallback file
   } else {
     read_excel(input$upload$datapath)
   }
 })
 
 # --- Dynamic Dropdowns ---
  output$region_ui <- renderUI({
    req(data_reactive())
    data <- data_reactive()
    selectInput("region", "Select Region:", 
                choices = c("All", unique(data$Region)), selected = "All")
  })
  output$product_ui <- renderUI({
    req(data_reactive())
    data <- data_reactive()
    selectInput("product", "Select Product:", 
                choices = c("All", unique(data$Product)), selected = "All")
  })
  
  # --- Filter Data ---
  filtered_data <- reactive({
    req(data_reactive())
    data <- data_reactive()
    if (input$region != "All") {
      data <- data[data$Region == input$region, ]
    }
    if (input$product != "All") {
      data <- data[data$Product == input$product, ]
    }
    data
  })
  
  # Value boxes
  output$total_sales <- renderValueBox({
    req(filtered_data())
    total <- sum(filtered_data()$Sales)
    valueBox(paste0("$", format(total, big.mark=",")), "Total Sales", icon = icon("dollar-sign"), color = "blue")
  })
  
  output$avg_profit <- renderValueBox({
    req(filtered_data())
    avg <- round(mean(filtered_data()$Profit), 2)
    valueBox(paste0("$", avg), "Average Profit", icon = icon("chart-line"), color = "green")
  })
  
  output$top_region <- renderValueBox({
    req(filtered_data())
    top <- filtered_data()|> 
      group_by(Region) |>
      summarise(TotalSales = sum(Sales)) |>
      arrange(desc(TotalSales)) |>
      slice(1)
    valueBox(top$Region, "Top Region", icon = icon("trophy"), color = "yellow")
  })
  
  # Table output
  output$sales_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 8))
  })
  
  # Line Plot - Monthly Sales Trend
  #output$line_plot <- renderPlot({
  # ggplot(filtered_data(), aes(x = Month, y = Sales, group = 1)) +
  #  geom_line(color = "steelblue", size = 1.2) +
  # geom_point(color = "darkblue", size = 3) +
  #labs(title = "Monthly Sales Trend", x = "Month", y = "Sales") +
  #theme_minimal(base_size = 13)
  #})
  
  # Line Plot - Monthly Sales Trend
  output$line_plot <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
   
    # Optional: ensure Month is ordered correctly
    if ("Month" %in% names(data)) {
      data$Month <- factor(data$Month,
                           levels = c("January", "February", "March", "April", "May"))}
    DATA <- data|>
      group_by(Month)|>
      summarise(SALES = mean(Sales))
    p<- ggplot(DATA, aes(x = Month, y = SALES, group = 1,
                         text = paste("Month", Month, "<br>Sales:", paste0("$", format(round(SALES, 2), big.mark=",")))))+
      geom_point(color = "darkblue", size = 3)+
      geom_line(color = "steelblue", size = 1.2)+
      labs(title = "Average Sales Per Month", x = "Month", y = "Sales") +
      #geom_smooth()+
      theme_classic(base_size = 13)
    ggplotly(p, tooltip = c("text"))
       })
  
  #   ggplot(data, aes(x = Month, y = Sales, group = 1)) +
  #     geom_line(color = "steelblue", size = 1.2) +
  #     geom_point(color = "darkblue", size = 3) +
  #     labs(title = "Monthly Sales Trend", x = "Month", y = "Sales") +
  #     theme_minimal(base_size = 13)
  # })
  
  # Bar Plot - Sales by Region
  output$bar_region <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Region, y = Sales, fill = Region)) +
      geom_bar(stat = "summary", fun = "sum", width = 0.7) +
      labs(title = "Total Sales by Region", x = "Region", y = "Total Sales") +
      theme_classic(base_size = 13) +
      theme(legend.position = "none")
  })
  
  # Bar Plot - Profit by Product
  output$bar_product <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Product, y = Profit, fill = Product)) +
      geom_bar(stat = "summary", fun = "mean", width = 0.7) +
      labs(title = "Average Profit by Product", x = "Product", y = "Average Profit") +
      theme_classic(base_size = 13) +
      theme(legend.position = "none")
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui = ui, server = server)
