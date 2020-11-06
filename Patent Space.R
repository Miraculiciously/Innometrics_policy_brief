getwd()
setwd("C:/Users/Max Kooistra/OneDrive/UU Innovation Sciences/Innometrics/Tutorial 6")

library(dplyr)
library(igraph)
library(EconGeo)
library(Matrix)
library(treemap)
library(ggplot2)

load("tutorial 6 materials/EP_IPC_2015.RDA")
load("EP_IPC.RDA")
load("EP_Y02.RDA")
load("EP_INV_Y02_2005.RDA")
load("EP_APP_clean_Y02_2005.RDA")



# Join IPC dataframe to Y02 dataframe
Y02_IPC <- inner_join(EP_IPC, EP_Y02)
Y02_IPC <- filter(Y02_IPC, CPC_Class != "")
# Select only patents with Y02W as CPC class
Y02W  <- Y02_IPC %>%
  filter(grepl('Y02W', Y02_IPC$CPC_Class))



Y02W <- Y02W %>%
  filter(IPC != "") %>%
  mutate(IPC3 = substr(IPC, 1,4)) %>%
  filter(Prio_Year <=2015) %>%
  select(Appln_id, IPC3) %>%
  distinct()

Count_Y02W <- count(Y02W, IPC3)

Count_Y02W <- Count_Y02W %>%
  mutate(section = substr(IPC3, 1, 1)) 

colors <- c("#E41A1C", "#377EB8" , "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")

treemap(Count_Y02W, index = c("section", "IPC3"), vSize = "n", type = "index",
        palette = colors)

#two mode into one mode

M <- table(Y02W)
A <- t(M) %*% M
diag(A) <- 0
A[1:10, 1:10]

G_IPC <- graph_from_adjacency_matrix(A, mode = c("undirected"), weighted = TRUE)

section <- LETTERS[1:8]
df_colors <- data.frame(section, colors)


Count_Y02W <- left_join(Count_Y02W, df_colors)


colors_IPC3 <- as.vector(Count_Y02W$colors)

graph_layout <- layout.fruchterman.reingold(G_IPC)  

legend.name <- c("A: Human Necessities", "B: Performing Operations, Transporting", "C: Chemistry, Metallurgy", 
                 "D: Textiles, Paper", "E: Fixed Constructions", "F: Mechanical Engineering, Lighting, Heating, Weapons, Blasting", 
                 "G: Physics", "H: Electricity")
degree <- degree(G_IPC)


Isolated <- which(degree(G_IPC)==0)
G_IPC <- delete.vertices(G_IPC, Isolated)
LO2 <- graph_layout[-Isolated,]



plot(G_IPC,
     layout = LO2,
     vertex.label = NA,                           
     vertex.color = colors_IPC3,                  
     vertex.size = degree^0.3,                        
     edge.width = E(G_IPC)$weight/max(E(G_IPC)$weight), 
     edge.color = rgb(.5,.5,.5,.5)
     )

 
legend(x = -1, y = -1.0,legend.name, pch = 21,    
       col = "#777777", pt.bg = colors, pt.cex = 1.5, cex = 0.9, bty = "n", ncol = 2)


