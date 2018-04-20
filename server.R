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

shinyServer(function(input, output, clientData, session) {

  rs <- reactiveValues(nodes = NULL,
                       edges = NULL,
                       tools = NULL)
  
  
  ## Load the data -----
  observe({
    
    tools <- read_excel("www/nodes.xlsx", sheet = 3)
    nodes <- read_excel("www/nodes.xlsx", sheet = 1)
    edges <- read_excel("www/nodes.xlsx", sheet = 2)
    
    if(input$scale_nodes) nodes$value <- 1
    
    for(i in c(1:nrow(nodes))){
      temp <- tools %>% 
        filter(type == nodes$label[i])
      if(nrow(temp) > 0){
        temp$a <- paste0("<a href='",temp$link,"'>",temp$name,"</a>")
        nodes$title[i] <- paste0(temp$a, collapse = "</br>")
        if(input$scale_nodes) nodes$value[i] <- nrow(temp)
      }
    }
    
    rs$tools <- tools
    rs$nodes <- nodes
    rs$edges <- edges
  })
  
  
  ## Update the UI -----
  observe({
    if(is.null(rs$nodes)){return()}
    vars <- unique(rs$nodes$scale)
    ct_options <- list()
    sel <- input$scales_to_plot
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "scales_to_plot", choices = ct_options, selected=sel) 
  })  
  
  observe({
    if(is.null(rs$nodes)){return()}
    vars <- unique(rs$nodes$scale)
    ct_options <- list()
    sel <- input$scales_to_plot_2
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "scales_to_plot_2", choices = ct_options, selected=sel) 
  })  
  
  observe({
    if(is.null(rs$nodes)){return()}
    nodes <- rs$nodes %>%
      filter(scale %in% input$scales_to_plot_2)
    vars <- nodes$label[nodes$type == "data"]
    ct_options <- list()
    sel <- input$input_data
    if(length(sel) == 0) sel = NULL
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "input_data", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$nodes)){return()}
    nodes <- rs$nodes %>%
      filter(scale %in% input$scales_to_plot_2)
    vars <- nodes$label[nodes$type == "method"]
    ct_options <- list()
    sel <- input$input_method
    if(length(sel) == 0) sel = NULL
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "input_method", choices = ct_options, selected=sel) 
  })   
  
  observe({
    if(is.null(rs$nodes)){return()}
    nodes <- rs$nodes %>%
      filter(scale %in% input$scales_to_plot_2)
    vars <- nodes$label[nodes$type == "data"]
    ct_options <- list()
    sel <- input$output_data
    if(length(sel) == 0) sel = NULL
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "output_data", choices = ct_options, selected=sel) 
  }) 
  
  observe({
    if(is.null(rs$nodes)){return()}
    vars <- unique(rs$nodes$type)
    ct_options <- list()
    sel <- input$group_to_plot
    if(length(sel) == 0) sel = vars
    for(ct in vars) ct_options[[ct]] <- ct
    # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    updateSelectInput(session, "group_to_plot", choices = ct_options, selected=sel) 
  })   
  
  
  ## Network visualisation ----
  
  output$network <- renderVisNetwork({
    # minimal example
    nodes <- rs$nodes
    nodes <- nodes %>%
      filter(scale %in% input$scales_to_plot) %>%
      filter(type %in% input$group_to_plot)
    nodes$group <- nodes[[input$color_to_plot]]
    edges <- rs$edges
    
    visNetwork(nodes, edges) %>% 
      visLegend() %>% 
      visOptions(highlightNearest = list(enabled =TRUE, degree = input$selection_degree), 
                 selectedBy = "scale",
                 nodesIdSelection = TRUE)%>% 
      visEdges(arrows = 'to') %>%
      visLayout(randomSeed=12) %>%
      visEdges(shadow = F,
               arrows =list(to = list(scaleFactor = 2)))
  })
  
  
  ## Network visualisation perso ----
  
  output$network_2 <- renderVisNetwork({
    # minimal example
    nodes <- rs$nodes %>%
      filter(scale %in% input$scales_to_plot_2)
    
    edges <- rs$edges %>%
      filter(from %in% nodes$id)
    
    nodes$group <- "null"
    nodes$group[nodes$label %in% input$output_data] <- "output"
    nodes$group[nodes$label %in% input$input_data] <- "input"
    nodes$group[nodes$label %in% input$input_method] <- "input"
    
    for(k in c(1:4)){
      for(n in c(1:nrow(nodes))){
        if(nodes$group[n] == "null"){
          ids <- edges$from[edges$to == nodes$id[n]]
          j <- 0
          print(nodes$label[n])
          print(ids)
          for(i in ids){
            if(i %in% nodes$id){
              if(nodes$group[nodes$id == i] != "null") j <- j+1
            }
          }
          if(j > 0){
            for(i in ids){
              if(i %in% nodes$id){
                if(nodes$type[nodes$id == i] == "method") j <- j+1
              }
            }  
          }
          if(j >= length(ids) & length(ids) > 0){
            nodes$group[n] <- "tool"
          }
        }
      }
      print(k)
    }
  
    visNetwork(nodes, edges) %>% 
      visGroups(groupname = "null", color = "lightgrey") %>%
      visGroups(groupname = "input", color = "#7edf4d") %>%
      visGroups(groupname = "output", color = "#99c3fa") %>%
      visGroups(groupname = "tool", color = "#fffd38") %>%
      visEdges(arrows = 'to') %>%
      visLayout(randomSeed=12) %>%
      visEdges(shadow = F,
               arrows =list(to = list(scaleFactor = 2)))
  })
  
  
  
  ## nodes table ################################
  
  output$nodes_results <- DT::renderDataTable({
    if(is.null(rs$nodes)){return()}
    temp <- rs$nodes
    
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  })    
  output$download_nodes <- downloadHandler(
    filename = function() {"nodes_results.csv"},
    content = function(file) {
      write.csv(rs$nodes, file)
    }
  )
  
  ## tools table ################################
  
  output$tools_results <- DT::renderDataTable({
    if(is.null(rs$tools)){return()}
    temp <- rs$tools
    
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  })    
  output$download_tools <- downloadHandler(
    filename = function() {"tools_results.csv"},
    content = function(file) {
      write.csv(rs$tools, file)
    }
  )  
  
  ## edges table ################################
  
  output$edges_results <- DT::renderDataTable({
    if(is.null(rs$edges)){return()}
    temp <- rs$edges
    
    DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 10))
  })    
  output$download_edges <- downloadHandler(
    filename = function() {"edges_results.csv"},
    content = function(file) {
      write.csv(rs$edges, file)
    }
  )  
  
  

})
