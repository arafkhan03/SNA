library(skimr)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)
library(sjmisc)
library(igraph)
library(sna)
library(linkprediction)
library(xlsx)
library(readxl)

##people <- read_excel("")

remove(vardesc)

people_NA <- as.data.frame(
        round(colMeans(is.na(people)), 2)
)
people_WNA <- people[, people_NA$`round(colMeans(is.na(people)), 2)` < 0.75]

df <- people_WNA[people_WNA$`Primary Organization` == 'Facebook',]
df <- df[complete.cases(df[ , 1]),]
df <- df %>% select('Full Name', 'Current Organizations')
df <- tidyr::separate_rows(df, `Current Organizations`, sep = ", ", convert = FALSE)
colnames(df) <- c("X1", "X2")

gg_social <- graph_from_data_frame(df, directed = FALSE)
## Error: gg_social <- simplify(graph.edgelist(as.matrix(df[c(1:5, 25:30),]), directed=FALSE))

tkplot(gg_social)
get.adjacency(gg_social)

par(mar=c(0,0,2,0))
plot(gg_social,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20",
     vertex.label.cex = 1,
     main = "Companies and People (Currently or Previously Associated") # change edge color to grey

check_df <- unique(df$X1)
check_df <- append(check_df, unique(df$X2))

s <- investors[investors$`Organization/Person Name` %in% check_df,]
s <- s[, c(1, 64)]
s <- na.omit(s)
s <- tidyr::separate_rows(s, `Top 5 Investors`, sep = ", ", convert = FALSE)
s[, c(2,1)] <- s[, c(1,2)]

ss <- simplify(graph.edgelist(as.matrix(s[c(2:6, 29:30), ]), directed=T))

par(mar=c(0,0,2,0))
plot(ss,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20",
     vertex.label.cex = 1,
     main = "Investment Graph") # change edge color to grey

gg_social_linkPred <- proxfun(gg_social, method = "aa", value = "graph")
plot(gg_social_linkPred,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20",
     vertex.label.cex = 1,
     main = "Future Link Predictions") # change edge color to grey)



colnames(df) <- c("Peron Name", "Company Associated With or Was")
colnames(s) <- c("Organizaton/Person Name", "Investor")


edge_list <- tibble(from = c("A", "B", "C", "D"),
                    to = c("B", "C", "D", "A"), weight = c(1,2,3,4))



