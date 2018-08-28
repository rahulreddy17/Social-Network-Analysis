#install.packages("visNetwork")
#install.packages("igraph")
#install.packages("igraphdata")
library(visNetwork)
library(igraph)
library(igraphdata)
library(data.table)
library(shiny)

#UI
ui <- fluidPage(
                headerPanel(title = "Rahul Reddy Muppidi_Social Network Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file1","Attach users file"),
                    fileInput("file2","Attach Department file"),
                    numericInput("numeric","Enter Connections",20)
                  ),
                  mainPanel(
                    tableOutput("network")
                    
                    
                  )
                )
)
#Server
server <- function(input,output){
  
  data <- reactive({
    file1 <- input$file1
    read.table(file=file1$datapath,col.names = c('from','to'))    
    
  })
  
  data2 <- reactive({
    file2 <- input$file2
    read.table(file=file2$datapath,col.names = c('id','department'))     
    
  })
  
  output$table1 <- renderTable({                                   
    data()
    
  })
  
  output$table2 <- renderTable({
    data2()
    
  })
  
  output$random <- renderTable({
    x <- input$numeric
    random <- head(data(),x)
    random
  })
  
  output$mailsent <- renderTable({
    users<-data.frame(data())
    mailsent<-as.data.frame(table(users$from))
    names(mailsent) <- c("from","frequency")
    mailsent
  })
  
  output$mailreceived <- renderTable({
    users<-data.frame(data())
    mailreceived<-as.data.frame(table(users$to))
    names(mailreceived) <- c("Received","frequency")
    mailreceived
  })
  
  output$two_hop_Q6a <- renderTable({
    users<-data.frame(data())
    mailsent<-as.data.frame(table(users$from))
    names(mailsent) <- c("from","frequency")
    g <- graph.data.frame(users)
    sent_sorted_mails<-mailsent[order(-mailsent$frequency),]
    v<-as.data.frame(sent_sorted_mails[1:10,1])
    xx<-ego(g, 2, nodes = v$`sent_sorted_mails[1:10, 1]`, mode = "all",
            mindist = 0)
    two_hop_Q6a<-as.data.frame(unique(unlist(xx)))
    names(two_hop_Q6a)<-c("List of 2hop neighbors of top10 mails sent users")
    two_hop_Q6a
  })
  
  output$two_hop_Q6b <- renderTable({
    users<-data.frame(data())
    mailreceived<-as.data.frame(table(users$to))
    names(mailreceived) <- c("Received","frequency")
    g1 <- graph.data.frame(users)
    received_sorted_mails<-mailreceived[order(-mailreceived$frequency),]
    u<-as.data.frame(received_sorted_mails[1:10,1])
    yy<-ego(g1, 2, nodes = u$`received_sorted_mails[1:10, 1]`, mode = "all",
            mindist = 0)
    two_hop_Q6b<-as.data.frame(unique(unlist(yy)))
    names(two_hop_Q6b)<-c("List of 2hop neighbors of top10 mails received users")
    two_hop_Q6b
  })
  
  output$Top10_deg <- renderTable({
    users<-data.frame(data())
    graph <- graph.data.frame(users, directed=T)
    graph <- simplify(graph)
    
    V(graph)$degree <- centr_degree(graph, mode = "total")$res
    
    nodes1 <- get.data.frame(graph, what="vertices")
    sol1<-nodes1[order(-nodes1$degree),] #sol1 displays the user and degree centrality from high to low
    Top10_deg<-as.data.frame(sol1[1:10,])
    Top10_deg
  })
  
  output$plotQ7 <- renderVisNetwork({
    users<-data.frame(data())
    graph <- graph.data.frame(users, directed=T)
    graph <- simplify(graph)
    
    V(graph)$degree <- centr_degree(graph, mode = "total")$res
    
    nodes1 <- get.data.frame(graph, what="vertices")
    sol1<-nodes1[order(-nodes1$degree),] #sol1 displays the user and degree centrality from high to low
    Top10_deg<-as.data.frame(sol1[1:10,])
    zz<-ego(graph, 2, nodes = Top10_deg$name, mode = "all",
            mindist = 0)
    two_hop_Q7<-as.data.frame(unique(unlist(zz)))
    names(two_hop_Q7)<-c("id")
    total<-merge(users,two_hop_Q7,by.y = "id",by.x = "from") #Edges for the 2hop nieghbors of top 10 degree centrality
    EdgesQ7<-as.data.frame(total[1:5000,])
    Vert<-as.data.frame(sol1[1:10,1])
    names(Vert)<-c("id")
    plotQ7<-visNetwork(Vert, EdgesQ7, width = "100%") %>% visOptions(highlightNearest = TRUE)
    plotQ7
  })
  
  output$Top10_bet <- renderTable({
    users<-data.frame(data())
    graph2 <- graph.data.frame(users, directed=T)
    graph2 <- simplify(graph2)
    V(graph2)$betweenness<-as.numeric(betweenness(graph2, v = V(graph2), directed = TRUE, weights = NULL,
                                                  nobigint = TRUE, normalized = FALSE))
    nodes2 <- get.data.frame(graph2, what="vertices")
    nodes2[,2] <- round(nodes2$betweenness,digits = 0)
    sol2<-nodes2[order(-nodes2$betweenness),]
    Top10_bet<-as.data.frame(sol2[1:10,])
  })
  
  output$plotQ8 <- renderVisNetwork({
    users<-data.frame(data())
    graph2 <- graph.data.frame(users, directed=T)
    graph2 <- simplify(graph2)
    V(graph2)$betweenness<-as.numeric(betweenness(graph2, v = V(graph2), directed = TRUE, weights = NULL,
                                                  nobigint = TRUE, normalized = FALSE))
    nodes2 <- get.data.frame(graph2, what="vertices")
    nodes2[,2] <- round(nodes2$betweenness,digits = 0)
    sol2<-nodes2[order(-nodes2$betweenness),]
    Top10_bet<-as.data.frame(sol2[1:10,])
    aa<-ego(graph2, 2, nodes = Top10_bet$name, mode = "all",
            mindist = 0)
    two_hop_Q8<-as.data.frame(unique(unlist(aa)))
    names(two_hop_Q8)<-c("id")
    total2<-merge(users,two_hop_Q8,by.y = "id",by.x = "from") #Edges for the 2hop nieghbors of top 10 betweenness
    EdgesQ8<-as.data.frame(total2[1:5000,])
    Vert2<-as.data.frame(sol2[1:10,1])
    names(Vert2)<-c("id")
    plotQ8<-visNetwork(Vert2, EdgesQ8, width = "100%") %>% visOptions(highlightNearest = TRUE)
    plotQ8
  })
  
  output$Top10_indeg <- renderTable({
    users<-data.frame(data())
    graph3 <- graph.data.frame(users, directed=T)
    graph3 <- simplify(graph3)
    V(graph3)$indegree <- centr_degree(graph3, mode = "in")$res
    
    nodes3 <- get.data.frame(graph3, what="vertices")
    sol3<-nodes3[order(-nodes3$indegree),]
    Top10_indeg<-as.data.frame(sol3[1:10,])
  })
  
  output$plotQ9 <- renderVisNetwork({
    users<-data.frame(data())
    graph3 <- graph.data.frame(users, directed=T)
    graph3 <- simplify(graph3)
    V(graph3)$indegree <- centr_degree(graph3, mode = "in")$res
    
    nodes3 <- get.data.frame(graph3, what="vertices")
    sol3<-nodes3[order(-nodes3$indegree),]
    Top10_indeg<-as.data.frame(sol3[1:10,])
    bb<-ego(graph3, 2, nodes = Top10_indeg$name, mode = "all",
            mindist = 0)
    two_hop_Q9<-as.data.frame(unique(unlist(bb)))
    names(two_hop_Q9)<-c("id")
    total3<-merge(users,two_hop_Q9,by.y = "id",by.x = "from") #Edges for the 2hop nieghbors of top 10 indegree centrality
    EdgesQ9<-as.data.frame(total3[1:5000,])
    Vert3<-as.data.frame(sol3[1:10,1])
    names(Vert3)<-c("id")
    plotQ9<-visNetwork(Vert3, EdgesQ9, width = "100%") %>% visOptions(highlightNearest = TRUE)
    plotQ9
    
    
  })
  
  output$final <- renderTable({
    users<-data.frame(data())
    names(users)<-c("from","to")
    department<-data.frame(data2())
    names(department)<-c("person","dept")
    test1 <- merge(x = users,y = department, by.x = "from", by.y = "person")
    names(test1) <- c("from","to","from.dep")
    test2 <- merge(x = test1,y = department,by.x = "to",by.y = "person")
    test2 <- test2[,c(3,4)]
    names(test2) <- c("from.dep","to.dep")
    test2$count <- 1
    final <- as.data.frame(aggregate(count ~ from.dep + to.dep, data = test2, sum))
    final
  })
  
  output$plotQ10 <- renderVisNetwork({
    users<-data.frame(data())
    names(users)<-c("from","to")
    department<-data.frame(data2())
    names(department)<-c("person","dept")
    test1 <- merge(x = users,y = department, by.x = "from", by.y = "person")
    names(test1) <- c("from","to","from.dep")
    test2 <- merge(x = test1,y = department,by.x = "to",by.y = "person")
    test2 <- test2[,c(3,4)]
    names(test2) <- c("from.dep","to.dep")
    test2$count <- 1
    final <- as.data.frame(aggregate(count ~ from.dep + to.dep, data = test2, sum))
    Vert4<-as.data.frame(unique(department$dept))
    names(Vert4)<-c("id")
    EdgesQ10<-as.data.frame(final[,1:2])
    names(EdgesQ10)<-c("from","to")
    plotQ10<-visNetwork(Vert4, EdgesQ10, width = "100%") %>% visOptions(highlightNearest = TRUE)
    
    
  })
  
  output$network <- renderUI({
    tabsetPanel(tabPanel("Users Data", tableOutput("table1")),
                tabPanel("Department Data", tableOutput("table2")),
                tabPanel("Random Connections", tableOutput("random")),
                tabPanel("No:of E-mails sent", tableOutput("mailsent")),
                tabPanel("No:of E-mails received", tableOutput("mailreceived")),
                tabPanel("2Hop neighbors of Top 10 emails sent users", tableOutput("two_hop_Q6a")),
                tabPanel("2Hop neighbors of Top 10 emails received users", tableOutput("two_hop_Q6b")),
                tabPanel("Users with top 10 degree centrality", tableOutput("Top10_deg")),
                tabPanel("Visualize_q7", visNetworkOutput("plotQ7", width = "100%", height = "100%")),
                tabPanel("Users with top 10 betweenness", tableOutput("Top10_bet")),
                tabPanel("Visualize_q8", visNetworkOutput("plotQ8", width = "100%", height = "100%")),
                tabPanel("Users with top 10 indegree", tableOutput("Top10_indeg")),
                tabPanel("Visualize_q9", visNetworkOutput("plotQ9", width = "100%", height = "100%")),
                tabPanel("Department_summary_q10", tableOutput("final")),
                tabPanel("Visualize_q10", visNetworkOutput("plotQ10", width = "100%", height = "100%")),
                tabPanel("Insights", 
                         h6("From the final results in 7,8 and 9 we could see a set of frequent users in the top 10 list"),
                         h6("Even on different metrics like degree centrality and betweenness, most of the users(but not all) in top 10 list occurs repetitively. So these users are most important people in our email network "),
                         h6("Note: For the visualization in 7,8 &9 subset of edges has been considered from 2hop neighbors of respective top10 lists since R isnt able to respond for all the edges.")
                         )
                )
  })
}
shinyApp(ui,server)