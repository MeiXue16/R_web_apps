library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(colourpicker)
library(gapminder)
library(wordcloud2)
library(tm)

css <- "
  #text{
    color: red;
  }
  
  #table{
  /* background: yellow; */
  font-size : 14px;
  color : blue;
  }
"


ui <- fluidPage(
  
  # 标签页
  tabsetPanel(
    tabPanel(title ='table', 'first tab', DT::dataTableOutput('table')),
    tabPanel(title='plot', 'second tab', plotlyOutput(outputId = 'plot')),
    tabPanel(title='title', textInput(inputId = 'text', label = 'Name',value='' )),
    
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
    data <- gapminder
    data
  })
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(data1(), aes(gdpPercap, lifeExp))+
        geom_point() +
        scale_x_log10()+
        ggtitle(input$text)
      p
    })
  })
  
  output$table <- DT::renderDataTable({
    data <- data1()
    data
  })
  
  # 创建数据来源反应变量
  # Create a "data_source" reactive variable
  data_source <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$source == "book") {
      #data <- readLines("file.txt") 
      data <- "hello world hello enemy"
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