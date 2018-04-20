
# Copyright © 2018, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2018 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.

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
