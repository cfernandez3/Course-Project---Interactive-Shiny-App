

# Team Members:
# Eduardo Santiago
# Jimmy Rosier
# Daniel Ortigoza
# Casiana Fernandez-Bango


library(dplyr)
library(shiny)
library(stringr)
library(ggplot2)
library(scales)
library(lubridate)


##########
# PART 1 #
##########

# Reading in un-cleaned CarSalesData here, please read in via your personal directory

CarSalesData = read.csv('CarSalesData.csv')


# Cleaning the CarSalesData here in 1 dplyr chain

CarSalesData = read.csv('CarSalesData.csv') %>% 
  mutate(
    Sale.date = mdy(Sale.date), # using Lubridate to transform Sale.date column from chr to numeric
    Year = year(Sale.date), # using the transformation above to create a new column named Year
    Sale = as.numeric(str_replace_all(Sale, "[$,]", "")), # transforming the Sale col. to numeric by removing '$', ',' to be numeric
    Average.MPG = as.numeric(str_replace_all(Average.MPG, " Miles per Gallon", "")), # removing string ' Miles per Gallon' to make values numeric
    Average.Weight.in.Pounds = as.numeric(str_replace_all(Average.Weight.in.Pounds, " lbs", "")) # removing string ' lbs' to make values numeric
  )


##########
# PART 2 #
##########


# ui here
ui = fluidPage(
  titlePanel("Car Sales Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      # showing scatterplot options when the scatterplot tab is active
      conditionalPanel(
        condition = "input.tabs == 'Scatterplot'",
        selectInput(
          inputId = "scatterX",
          label = "Scatterplot X-axis:",
          choices = c("Average.MPG", "Average.Weight.in.Pounds"),
          selected = "Average.MPG"
        ),
        selectInput(
          inputId = "scatterY",
          label = "Scatterplot Y-axis:",
          choices = c("Sale", "Average.Weight.in.Pounds"),
          selected = "Sale"
        )
      ),
      
      # showing barplot options when the bar plot tab is active
      conditionalPanel(
        condition = "input.tabs == 'Bar Plot'",
        selectInput(
          inputId = "barX",
          label = "Bar Plot X-axis:",
          choices = c("Manufacturer", "Year"),
          selected = "Manufacturer"
        )
      ),
      
      # showing pie chart options when the pie chart tab is active
      conditionalPanel(
        condition = "input.tabs == 'Pie Chart'",
        selectInput(
          inputId = "pieVar",
          label = "Pie Chart Variable:",
          choices = c("Manufacturer", "Region"),
          selected = "Region"
        )
      )
    ),
    
    mainPanel(
      # adding tabs for each plot
      tabsetPanel(
        id = "tabs",  # Add an ID to track which tab is active
        tabPanel("Bar Plot", plotOutput("barPlot")),
        tabPanel("Scatterplot", plotOutput("scatterPlot")),
        tabPanel("Pie Chart", plotOutput("pieChart")),
        tabPanel("Manufacturer vs Region Bar Plot", plotOutput("manufacturerRegionPlot"))  
      )
    )
  )
)

# server here
server = function(input, output) {
  # rendering bar plot
  output$barPlot = renderPlot({
    data = CarSalesData %>%
      group_by(.data[[input$barX]]) %>%
      summarise(Total_Sale = sum(Sale, na.rm = TRUE))
    
    ggplot(data, aes_string(x = input$barX, y = "Total_Sale", fill = input$barX)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        title = paste("Total Sales by", input$barX),
        x = input$barX,
        y = "Total Sales"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      scale_y_continuous(labels = function(x) paste0("$", x / 1e6, "M"))
  })
  
  # rendering scatterplot
  output$scatterPlot = renderPlot({
    ggplot(CarSalesData, aes_string(x = input$scatterX, y = input$scatterY)) +
      geom_point(color = "peru") +
      theme_gray() +
      labs(
        title = paste("Scatterplot of", input$scatterY, "vs", input$scatterX),
        x = input$scatterX,
        y = input$scatterY
      )
  })
  
  # rendering pie chart
  output$pieChart = renderPlot({
    data = CarSalesData %>%
      group_by(.data[[input$pieVar]]) %>%
      summarise(Count = n()) %>%
      mutate(Percent = Count / sum(Count) * 100)
    
    ggplot(data, aes(x = "", y = Count, fill = .data[[input$pieVar]])) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      theme_void() +
      labs(
        title = paste("Distribution of", input$pieVar),
        fill = input$pieVar
      ) +
      geom_text(aes(label = paste0(round(Percent, 1), "%")), 
                position = position_stack(vjust = 0.5))
  })
  
  # rendering Manufacturer vs Region bar plot
  output$manufacturerRegionPlot = renderPlot({
    data = CarSalesData %>%
      group_by(Region, Manufacturer) %>%
      summarise(Total_Sale = sum(Sale, na.rm = TRUE))
    
    ggplot(data, aes(x = Region, y = Total_Sale, fill = Manufacturer)) +
      geom_bar(stat = "identity", position = "dodge") +  # Dodge to have side-by-side bars
      theme_minimal() +
      labs(
        title = "Total Sales by Manufacturer and Region",
        x = "Region",
        y = "Total Sales"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_y_continuous(labels = function(x) paste0("$", x / 1e6, "M"))
  })
}

# shinyapp
shinyApp(ui, server)
