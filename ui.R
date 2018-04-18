
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  navbarPage("Water tool network",
    tabPanel("Network", id="tab1", icon = icon("share-alt"),
             fluidRow(
               column(3,
                      helpText("This app shows the network of computational tools that could be used to better understand water flow in the soil-plant-atmosphere system. "),
                      helpText("Hoover over the different nodes of the network to get more informations and links to the available tools."),
                      tags$hr(),
                      h4("Refine network"),
                      selectInput("scales_to_plot", label="Scales to plot", choices = c("Load datafile"), selected = NULL, multiple = TRUE, width="100%"),
                      selectInput("group_to_plot", label="Types to plot", choices = c("Load datafile"), selected = NULL, multiple = TRUE, width="100%"),
                      selectInput("color_to_plot", label="Color by:", choices = c("type", "scale", "scope"), selected = 1, multiple = F, width="100%"),
                      checkboxInput('scale_nodes', "Scale nodes sizes by number of tools", value = T, width = NULL),
                      selectInput("selection_degree", label="Selection degree", choices = c(1:3), selected = 3, multiple = F, width="50%"),
                      helpText("When you click on a node, to which degree should we highlight the connected nodes?"),
                      
                      tags$hr(),
                      tags$strong("Connecting numerical tools to analyse soil-plant water relations."),
                      helpText("Sixtine Passot, Valentin Couvreur, Xavier Draye, Mathieu Javaux, Daniel Leitner, Félicien Meunier, Loïc Pagès, Andrea Schnepf, Jan Vanderborght and Guillaume Lobet"),
                      # actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('#', '_blank')"),                                              
                      tags$hr(),
                      img(src='logo.png', align = "left", width="100%")
               ),
               column(8, 
                      visNetworkOutput("network", height="800px")
               )
             )
    ),
    tabPanel("Your data", id="tab1", icon = icon("flask"),
             fluidRow(
               column(3,
                      h4("Refine network"),
                      selectInput("scales_to_plot_2", label="Scales to plot", choices = c("Load datafile"), selected = NULL, multiple = TRUE, width="100%"),
                      h4("Select the data you have"),
                      selectInput("input_data", label="Input data", choices = c("Load datafile"), selected = NULL, multiple = TRUE, width="100%"),
                      selectInput("input_method", label="Input method", choices = c("Load datafile"), selected = NULL, multiple = TRUE, width="100%"),
                      tags$hr(),
                      helpText("On this side, select the data, or methods that you have. As you select them, they will be highlighted in the network (in green), together with the potential newly available data (in yellow). ")
               ),
               column(6, 
                      visNetworkOutput("network_2", height="800px")
               ),
               column(3,
                      h4("Select the data you want"),
                      selectInput("output_data", label="Output data", choices = c("Load datafile"), selected = NULL, multiple = TRUE, width="100%"),
                      tags$hr(),
                      helpText("On this side, select the data you wish to have. I will be highlighted in blue in the network.")
                      
               )              
             )
    ),    
    tabPanel("Tables", id="tab1", icon = icon("download"),
             fluidRow(
               column(3,
                      helpText("This is the table containing the raw data used for the network"),
                      tags$hr(),
                      tags$strong("Connecting numerical tools to analyse soil-plant water relations."),
                      helpText("Sixtine Passot, Valentin Couvreur, Xavier Draye, Mathieu Javaux, Daniel Leitner, Félicien Meunier, Loïc Pagès, Andrea Schnepf, Jan Vanderborght and Guillaume Lobet"),
                      # actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('#', '_blank')"),                                              
                      tags$hr(),
                      img(src='logo.png', align = "left", width="100%")
               ),
               column(8, 
                      tabsetPanel(
                        
                        tabPanel("Tools data",
                                 helpText("This table contains data about the different tools in the network. It can be updated here:"),
                                 tags$a("https://docs.google.com/spreadsheets/d/1guaKkfeVxDfKXE0GXN1oBBw_A2uQMo2OPxnBJ9J3P8Y/edit?usp=sharing"),
                                 downloadButton('download_tools', 'Download full table'),
                                 tags$hr(),
                                 DT::dataTableOutput('tools_results'),
                                 value=2
                        ),
                        
                        tabPanel("Nodes data",
                                 helpText("This table contains data for the nodes of the network"),
                                 downloadButton('download_nodes', 'Download full table'),
                                 tags$hr(),
                                 DT::dataTableOutput('nodes_results'),
                                 value=2
                        ),
                        
                        tabPanel("Edges table",
                                 helpText("This table contains data for the connections (edges) between nodes of the network"),
                                 downloadButton('download_edges', 'Download full table'),
                                 tags$hr(),
                                 DT::dataTableOutput('edges_results'),
                                 value=2
                        )
                      )
                        
               )
             )
    )    
  )

))
