library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(colourpicker)
library(gapminder)
library(wordcloud2)
library(tm)
#library(readxl) #excel

# daten aufrufen
# arbeitslos <- read.csv('./data/01_arbeitslos.csv')
# phone <- read_excel('./data/03_iphone.xlsx')

css <- "
  #title{
    color: red;
  }
  
  #table1{
  /* background: skyblue; */
  font-size : 12px;
  color : black;
  }
  
  #download_data{
  background-color: skyblue;
  margin-left: 50px;
  margin-top: 10px;
  border: 1px solid #4CAF50;
  font-size : 10px;
  }
  #download_data2{
  background-color: lightblue;
  #margin-left: 50px;
  margin-bottom: 10px;
  border: 1px solid #4CAF50;
  font-size : 9px;
  }
"


ui <- fluidPage(
  
  # 标签页
  tabsetPanel(
    tabPanel(title='plotpage', h1('Benutzerdefinierte Grafiken'), 
             sidebarLayout(
               sidebarPanel(
                 textInput("title", "Title", "GDP vs life exp"),
                 numericInput("size", "Point size", 1, 1),
                 colourInput('color', 'select a color', value ='orange'),
                 
                 # Add a slider selector for years to filter 滑块
                 sliderInput('years', 'Years', value = c(1977,2002), min = min(gapminder$year), max = max(gapminder$year)),
                 
                 # sliderInput(inputId = "life", label = "Life expectancy",value = c(30, 80), min = 0, max = 120 ),
                 # Add a continent dropdown selector 下拉选择
                 selectInput('continent', 'please choose the continents', choices = levels(gapminder$continent),selected = 'Europe', multiple = TRUE),
                 checkboxInput("fit", "Add line of best fit", FALSE)
               ),
               mainPanel(
                 # plotly交互式画图输出
                 plotlyOutput(outputId = "plot", width=800, height =400),
                 downloadButton(outputId = 'download_data', label='Download the data that correspond to the filtered graphs')
               )
             )
    ),
                 
    tabPanel(title ='table', h1('Filtern und Herunterladen von Datensatz'), 
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "country_continent",
                   label = "country / continent",
                   choices = c( 'country' ,'continent')),
                 
                 conditionalPanel(
                   condition = "input.country_continent == 'country'",
                   selectInput('country','choose the countries', choices = c('All', levels(gapminder$country)),selected = 'All')
                 ),
                 conditionalPanel(
                   condition = "input.country_continent == 'continent'",
                   # Add a continent dropdown selector 下拉选择
                   selectInput('continent2', 'please choose the continents', choices = levels(gapminder$continent),selected = 'Europe', multiple = TRUE)
                 ),

                 # Add a slider selector for years to filter 滑块
                 sliderInput('yearsb', 'Years', value = c(1977,2002), min = min(gapminder$year), max = max(gapminder$year)),
                 sliderInput(inputId = "life", label = "Life expectancy",
                             min = 0, max = 120,
                             value = c(30, 50))
               ),
               
               mainPanel(
                 downloadButton('download_data2', label ='Download the data corresponding to the filtered table'),
                 DT::dataTableOutput('table1')
               )
             )
    ),
             
    
    #词云自定义
    tabPanel(title='wordcloud', h1("Word Cloud"),
             # Add a sidebar layout to the UI
             sidebarLayout(
               # Define a sidebar panel around the inputs
               sidebarPanel(
                 # 单选输入
                 radioButtons(
                   inputId = "source",
                   label = "Word source",
                   choices = c(
                     # First choice is "book", with "Art of War" displaying
                     "Template" = "book",
                     # Second choice is "own", with "Use your own words" displaying
                     "Use your own words" = "own",
                     # Third choice is "file", with "Upload a file" displaying
                     "Upload a file" = "file"
                   )
                 ),
                 # 条件面板：文本框输入
                 conditionalPanel(
                   condition = "input.source == 'own'",
                   textAreaInput("text", "Enter text", rows = 7)
                 ),
                 # 条件面板：文件上传输入
                 conditionalPanel(
                   # The condition should be that the user selects
                   # "file" from the radio buttons
                   condition = "input.source == 'file'",
                   fileInput("file", "Select a file")
                 ),
                 
                 #numericInput("num", "Maximum number of words",value = 100, min = 5),
                 colourInput("col", "Background color", value = "white"),
                 
                 actionButton('draw','Draw!')
                 
               ),
               # Define a main panel around the output
               mainPanel(
                 wordcloud2Output("cloud")
               )
             ) )
  ),
  
  # 网页样式
  tags$style(css)
)

server <- function(input, output){
  
  data1 <- reactive({
    data <- subset(gapminder, continent %in% input$continent &
                     year >= input$years[1] &
                     year <= input$years[2])
    data
  })
 
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(data1(), aes(gdpPercap, lifeExp))+
        geom_point(size = input$size, col = input$color) +
        scale_x_log10()+
        ggtitle(input$title)
      if (input$fit) {
        p <- p + geom_smooth(method = "lm")
      }
      p
    })
  })
  
  # 下载处理程序（data1）
  output$download_data <- downloadHandler(
    filename = 'data1.csv',
    content = function(file){
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  
  data2 <- reactive({
    data <- subset(gapminder, 
                     year >= input$yearsb[1] &
                     year <= input$yearsb[2] &
                     lifeExp >= input$life[1] & lifeExp <= input$life[2])
    if (input$country_continent == 'country' & input$country != 'All'){
        data <- subset(data, country == input$country )
    } else if (input$country_continent == 'continent'){
        data <- subset(data, continent %in% input$continent2 )
    }
    return(data)
  })
  
  output$download_data2 <- downloadHandler(
    filename = 'data2.csv',
    content = function(file){
      write.csv(data2(), file, row.names = FALSE)
    }
  )
  
  output$table1 <- DT::renderDataTable({
    data <- data2()
    data
  })
  
  # 创建数据来源反应变量
  # Create a "data_source" reactive variable
  data_source <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$source == "book") {
      #data <- readLines("file.txt") 
      data <- "hello world hello how are you data analyst data scienece"
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    # Read the text in the uploaded file
    readLines(input$file$datapath)
  })
  
  data_list <- reactive({
    text <- data_source()
    docs <- Corpus(VectorSource(text))
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))  #"german"
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    df
  })
  
  output$cloud <- renderWordcloud2({
    
    #将按钮添加为词云渲染函数中的依赖项，以便在按下按钮时重新运行词云
    input$draw
    isolate({
      #figPath = system.file("examples/t.png",package = "wordcloud2")
      # figPath = figPath, color = "skyblue"
      wordcloud2(data_list(), size = 1,color = "random-light", backgroundColor =input$col,shape = 'diamond' )
      #letterCloud(data_list(), word = "R", size = 2)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)