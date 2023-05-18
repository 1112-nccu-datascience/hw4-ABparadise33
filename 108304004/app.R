# 載入所需的套件
library(shiny)
library(factoextra)
library(FactoMineR)
library(ggbiplot)
library(ggplot2)

# 定義PCA的函式
performPCA <- function() {
  data(iris)
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  
  return(ir.pca)
}

# 定義CA的函式
performCA <- function(species) {
  data(iris)
  filtered_data <- iris[iris$Species == species, ]
  ca_results <- CA(filtered_data[, 1:4])
  
  return(ca_results)
}

# 定義Shiny UI
ui <- fluidPage(
  navbarPage(
    title = "108304004 統計四 張翊鞍",
    tabPanel(
      "PCA",
      fluidRow(
        column(
          width = 12
        )
      ),
      fluidRow(
        column(
          width = 12,
          tabsetPanel(
            id = "subTabs",
            tabPanel(
              "PCA Plot",
              sidebarLayout(
                sidebarPanel(
                  radioButtons(
                    "xaxis", "X軸主成分",
                    c("PC1" = "PC1", "PC2" = "PC2", "PC3" = "PC3"),
                    selected = "PC1"
                  ),
                  radioButtons(
                    "yaxis", "y軸主成分",
                    c("PC1" = "PC1", "PC2" = "PC2", "PC3" = "PC3"),
                    selected = "PC2"
                  )
                ),
                mainPanel(plotOutput("pcaPlot"))
              )
            ),
            tabPanel("Result Data",
                     mainPanel(
                       tableOutput("resultTable")
                     )
            ),
            tabPanel("Input Data (log)",
                     sidebarLayout(
                       sidebarPanel(
                         numericInput(
                           "numRows", "顯示行數：",
                           value = 10, min = 1, max = nrow(iris)
                         )
                       ),
                       mainPanel(
                         verbatimTextOutput("dataSummary"),
                         tableOutput("logData")
                       )
                     )
            ),
            tabPanel("Extended Result",
                     mainPanel(
                       textOutput("extendedResult")
                     )
            )
          )
        ),
        column(
          width = 9,
          mainPanel(
            # mainPanel content goes here
          )
        )
      )
    ),
    tabPanel(
      "CA",
      fluidRow(
        column(
          width = 12,
          tabsetPanel(
            id = "caTabs",
            tabPanel(
              "CA Plot",
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    "species", "選擇要顯示的物種",
                    choices = unique(iris$Species)
                  )
                ),
                mainPanel(plotOutput("caPlot"))
              )
            ),
            tabPanel("Result Data",
                     mainPanel(
                       tableOutput("caResultTable")
                     )
            )
          )
        )
      )
    )
  )
)

# 定義Shiny伺服器邏輯
server <- function(input, output, session) {
  # PCA
  ir.pca <- performPCA()
  
  output$pcaPlot <- renderPlot({
    x_axis <- input$xaxis
    y_axis <- input$yaxis
    
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = iris$Species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g <- g + coord_cartesian(
      xlim = c(min(ir.pca$x[, x_axis]), max(ir.pca$x[, x_axis])),
      ylim = c(min(ir.pca$x[, y_axis]), max(ir.pca$x[, y_axis]))
    )
    g <- g + scale_x_continuous(
      name = paste0(
        x_axis, " (", round(summary(ir.pca)$importance[2, x_axis] * 100, 1), "% explained var.)"
      )
    )
    g <- g + scale_y_continuous(
      name = paste0(
        y_axis, " (", round(summary(ir.pca)$importance[2, y_axis] * 100, 1), "% explained var.)"
      )
    )
    
    print(g)
  })
  
  # Result Data
  output$resultTable <- renderTable({
    summary(ir.pca)$importance
  })
  
  
  output$dataSummary <- renderPrint({
    summary(log(iris[,1:4]))
  })
  
  # Input Data (log)
  output$logData <- renderTable({
    log(iris[1:input$numRows, 1:4])
  })
  
  # Extended Result
  output$extendedResult <- renderText({
    paste("PCA擁有的主成分數量：", length(ir.pca$sdev))
  })
  
  # CA
  output$caPlot <- renderPlot({
    species <- input$species
    
    ca_results <- performCA(species)
    fviz_ca_biplot(ca_results, axes = 1:2)
  })
  
  # CA Result Data
  output$caResultTable <- renderTable({
    species <- input$species
    
    ca_results <- performCA(species)
    ca_results$eig
  })
}

# 執行Shiny應用程式
shinyApp(ui, server)
