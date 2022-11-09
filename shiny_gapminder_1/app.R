library(ggplot2)
library(shiny)
# 加载数据集gapminder
library(gapminder)
# 加载颜色选择库
library(colourpicker)
# 加载交互式画图包
library(plotly)
#加载dataTable库，分页展示表格
library(DT)

# Define UI for the application
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      
      # Add radio buttons for colour 单选
      # radioButtons("color", "Point color", choices =c("blue", "red", "green", "black")),
      # 颜色选择器
      colourInput('color', 'select a color', value ='orange'),
      
      # Add a continent dropdown selector 下拉选择
      selectInput('continents', 'please choose the continents', choices = levels(gapminder$continent),selected = 'Europe', multiple = TRUE),
      
      # Add a slider selector for years to filter 滑块
      sliderInput('years', 'Years', value = c(1977,2002), min = min(gapminder$year), max = max(gapminder$year)),
      
      # 下拉选择表格数据, 如果只有10个国家可供选择：choices = levels(gapminder$country)[1:10]
      # 如果想要添加额外的选项： choices =c('any', levels(gapminder$country))
      selectInput('country','choose the countries displayed in the table', choices = c('All', levels(gapminder$country)),selected = 'All')
    
    ),
     
    mainPanel(
      # 常规画图输出
      # plotOutput(outputId = "plot", width=400, height =400)
      
      # plotly交互式画图输出
      plotlyOutput(outputId = "plot", width=800, height =400),
      
      # 下载按钮
      downloadButton(outputId = 'download_data', label='Download the data that correspond to the filtered graphs'),
      downloadButton('download_data2', label ='Download the data corresponding to the filtered table'),
      
      # 表格输出
      #tableOutput('table1')
      
      # DT表格输出
      DT::dataTableOutput('table1')
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # 创建反应变量
  # Subset the gapminder dataset by the chosen continents
  data1 <- reactive({
    data <- subset(gapminder, continent %in% input$continents &
                     year >= input$years[1] &
                     year <= input$years[2])
    data
  })
  
  # 渲染函数，输出图形
  # renderPlot({ ggplot(data) }) 常规渲染平面图
  # renderPlotly({ ggplotly({ ggplot(data) }) }) 交互式渲染
  output$plot <- renderPlotly({
    ggplotly({
          
          # 画图
          p <- ggplot(data1(), aes(gdpPercap, lifeExp)) +
            # Use the value of the color input as the point colour
            geom_point(size = input$size, col = input$color) +
            scale_x_log10() +
            ggtitle(input$title)
          
          if (input$fit) {
            p <- p + geom_smooth(method = "lm")
          }
          p
    })
  })
  
  # 创建反应变量
  data2 <- reactive({
    # Don't subset the data if "All" country are chosen
    data <- gapminder
    if (input$country != 'All') {
      data <- subset(
        data,
        country == input$country
      )
    }
    data
  })
  
  # 渲染函数，输出表格
  #output$table1 <- renderTable({
  
  # DT渲染数据表格（分页展示）
  output$table1 <- DT::renderDataTable({
    data2()
  })
  
  # 下载处理程序（data1）
  output$download_data <- downloadHandler(
    filename = 'data1.csv',
    content = function(file){
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  
  # 下载(data2)
  output$download_data2 <- downloadHandler(
    filename = 'data2.csv',
    content = function(file){
      data <- data2()
      write.csv(data, file, row.names = TRUE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)