#Sankey plot tuto

# Library
library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))


test <- df_message_status %>% select(date, status_sender, status_recipient) %>%
  filter(!is.na(status_sender) & !is.na(status_recipient)) %>%
  mutate(year = format(date,"%Y")) %>%
  group_by(date,status_sender, status_recipient) %>%
  mutate(number_exchange = n()) %>% ungroup() %>%
  distinct(date, status_sender, status_recipient, number_exchange, year)

test_2000 <- as.data.frame(test %>% filter(year == 2000) %>%
  group_by(status_sender, status_recipient) %>%
  mutate(sum = sum(number_exchange)) %>% ungroup() %>%
  distinct(status_sender, status_recipient, sum) %>%
    filter(status_sender != status_recipient) %>%
    arrange(status_sender, status_recipient)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes_test <- data.frame(
  name=c(as.character(test_2000$status_sender), 
         as.character(test_2000$status_recipient)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
test_2000$IDsource <- match(test_2000$status_sender, nodes_test$name)-1 
test_2000$IDtarget <- match(test_2000$status_recipient, nodes_test$name)-1

# Make the Network
p <- sankeyNetwork(Links = test_2000, Nodes = nodes_test,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "sum", NodeID = "name", 
                   sinksRight=FALSE)
p

test_2001 <- as.data.frame(test %>% filter(year == 2001) %>%
                             group_by(status_sender, status_recipient) %>%
                             mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                             distinct(status_sender, status_recipient, sum) %>%
                             filter(status_sender != status_recipient) %>%
                             arrange(sum)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes_test_01 <- data.frame(
  name=c(as.character(test_2001$status_sender), 
         as.character(test_2001$status_recipient)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
test_2001$IDsource <- match(test_2001$status_sender, nodes_test_01$name)-1 
test_2001$IDtarget <- match(test_2001$status_recipient, nodes_test_01$name)-1

# Make the Network
p01 <- sankeyNetwork(Links = test_2001, Nodes = nodes_test_01,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "sum", NodeID = "name", 
                   sinksRight=FALSE)
p01


test_hand <- data.frame(
  source=c("CEO","CEO", "CEO", "CEO", "CEO", "CEO", "CEO"), 
  target=c("VP","Trader", "President", "Managing director", "Manager", "Employee", "Director"), 
  value=c(7,6, 5, 4, 3, 2, 1)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(test_hand$source), 
         as.character(test_hand$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
test_hand$IDsource <- match(test_hand$source, nodes$name)-1 
test_hand$IDtarget <- match(test_hand$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = test_hand, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

test_2000_rank <- as.data.frame(test_2000 %>%
  group_by(status_sender)%>%
  mutate(
    rank = rank(sum, ties.method = "first")
  ))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes_test <- data.frame(
  name=c(as.character(test_2000_rank$status_sender), 
         as.character(test_2000_rank$status_recipient)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
test_2000_rank$IDsource <- match(test_2000_rank$status_sender, nodes_test$name)-1 
test_2000_rank$IDtarget <- match(test_2000_rank$status_recipient, nodes_test$name)-1

# Make the Network
p <- sankeyNetwork(Links = test_2000_rank, Nodes = nodes_test,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "rank", NodeID = "name", 
                   sinksRight=FALSE)
p

#circular diagram

library(circlize)

status_color <- c(
  "Employee" = "pink",
  "Director" = "yellowgreen",
  "CEO" = "orange",
  "Vice President" = "tomato4",
  "Trader" = "springgreen3",
  "President" = "snow4",
  "Managing Director" = "violetred4",
  "Manager" = "skyblue3",
  "In House Lawyer" = "purple4"
)

adjacencyData <-with(test_2000, table(status_sender, status_recipient))


diag_2000<-chordDiagram(adjacencyData, transparency = 0.5, grid.col = status_color)


adjacencyData_01 <-with(test_2001, table(status_sender, status_recipient))


diag_2001<-chordDiagram(adjacencyData_01, transparency = 0.5, grid.col = status_color)

test_2002 <- as.data.frame(test %>% filter(year == 2002) %>%
                             group_by(status_sender, status_recipient) %>%
                             mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                             distinct(status_sender, status_recipient, sum) %>%
                             filter(status_sender != status_recipient) %>%
                             arrange(sum)
)

adjacencyData_02 <-with(test_2002, table(status_sender, status_recipient))


diag_2002<- chordDiagram(adjacencyData_02, transparency = 0.5, grid.col = status_color)

test_1999 <- as.data.frame(test %>% filter(year == 1999) %>%
                             group_by(status_sender, status_recipient) %>%
                             mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                             distinct(status_sender, status_recipient, sum) %>%
                             filter(status_sender != status_recipient) %>%
                             arrange(sum)
)

adjacencyData_99 <-with(test_1999, table(status_sender, status_recipient))


diag_1999<-chordDiagram(adjacencyData_99, transparency = 0.5, grid.col = status_color)

library(circlize)
library(grid)

# Example matrix
mat <- matrix(c(0, 2, 3,
                2, 0, 1,
                3, 1, 0),
              nrow = 3,
              dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

# Clear before starting
circos.clear()

# Create the layout: 2 rows (title + diagram)
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1,
                                           heights = unit(c(1, 9), "null"))))  # 1 part title, 9 parts plot

# Title at the top
pushViewport(viewport(layout.pos.row = 1))
grid.text("My Awesome Chord Diagram", gp = gpar(fontsize = 16, fontface = "bold"))
popViewport()

# Chord diagram in the second row
pushViewport(viewport(layout.pos.row = 2))
circos.par(start.degree = 90, gap.degree = 10)
chordDiagram(mat, grid.col = c("A" = "red", "B" = "blue", "C" = "green"))
popViewport()

