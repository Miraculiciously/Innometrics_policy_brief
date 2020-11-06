getwd()
setwd("C:/Users/Max Kooistra/OneDrive/UU Innovation Sciences/Innometrics/Tutorial 6")

library(devtools)
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

# Join Y02 to the dataframe containing regional codes for each patents
app_country <- select(EP_APP_clean_Y02_2005, c(Appln_id, Reg_code))
Y02_NL <- inner_join(Y02W, app_country)

Y02_NL <- Y02_NL %>%
  filter(grepl('NL', Y02_NL$Reg_code))


# Unique patents in NL
totalpatents <- select(Y02_NL, Appln_id)
totalpatents <- distinct(totalpatents)

total_names <- inner_join(totalpatents, EP_APP_clean_Y02_2005) %>% select(1,2,3)
total_names <-  total_names[grepl("UNIVERSIT", total_names$Clean_name),]

# Prepare for joining
totalpatents <- mutate(totalpatents, region = "NL")

# Join IPC codes to country NL
IPC_NL <- left_join(Y02W, totalpatents)

# FREQUENCY OF IPC CODES IN NL
IPC_NL_Count <- count(IPC_NL, IPC_NL$IPC3)
quantile(IPC_NL_Count$n)

IPC_NL_top <- IPC_NL_Count[order(IPC_NL_Count$n, decreasing = T),]
IPC_NL_top <- head(IPC_NL_top, 30)

g <- ggplot(IPC_NL_top, aes(x=reorder(`IPC_NL$IPC3`, -n), y=n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Total occurences", x = "IPC code") +
  ggtitle("IPC code occurrences in the Dutch Y02W patent portfolio")

g



# LIST WITH UNIQUE IPC CODES IN NL
IPC_NL_unique <- IPC_NL %>%
  na.omit() %>%
  select(IPC3, region) %>%
  distinct()
  
region_full <- select(Y02W, IPC3) 
region_full <- distinct(region_full)

IPC_region <- left_join(region_full, IPC_NL_unique)
IPC_region[is.na(IPC_region)] <- "foreign"



#two mode into one mode

M <- table(Y02W)
A <- t(M) %*% M
diag(A) <- 0
A[1:10, 1:10]

G_IPC <- graph_from_adjacency_matrix(A, mode = c("undirected"), weighted = TRUE)

degree <- degree(G_IPC)

graph_layout <- layout.fruchterman.reingold(G_IPC) 


edges <- E(G_IPC)
weights <- E(G_IPC)$weight
edges_weights <- data.frame(as_ids(edges), weights)
edges_weights <- edges_weights %>%
  mutate(node1 = substr(as_ids.edges., 1, 4)) %>%
  mutate(node2 = substr(as_ids.edges., 6, 9)) %>%
  select(node1, node2, weights)

dutch_1 <- edges_weights[which(edges_weights$node1 %in% IPC_NL_unique$IPC3),]
dutch_2 <- edges_weights[which(edges_weights$node2 %in% IPC_NL_unique$IPC3),]
dutch <- rbind(dutch_1, dutch_2)
dutch <- distinct(dutch)

double_dutch <- dutch[which(dutch$node1 %in% IPC_NL_unique$IPC3 & dutch$node2 %in% IPC_NL_unique$IPC3),]
no_double_dutch <- anti_join(dutch, double_dutch)

no_double_dutch$temp <- no_double_dutch$node1
no_double_dutch$temp[which(no_double_dutch$node1 %in% IPC_NL_unique$IPC3)] <-
  no_double_dutch$node2[which(no_double_dutch$node1 %in% IPC_NL_unique$IPC3)]

no_double_dutch <- no_double_dutch %>% select(-c(node1, node2))

library(plyr)
no_double_dutch <- ddply(no_double_dutch,.(temp),summarize,sum=sum(weights))
quantile(no_double_dutch$sum)

diversify <- filter(no_double_dutch, sum >= 15)

g <- ggplot(diversify, aes(x=reorder(temp, -sum), y=sum)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(y = "Total co-occurences", x = "IPC code not in Dutch patent portfolio") +
  ggtitle("Number of co-occurences of IPC codes not in the Dutch patent portfolio with \nIPC codes in the Dutch patent portfolio")
g

region <- as.vector(IPC_region$region)
V(G_IPC)$region <- region


Isolated <- which(degree(G_IPC)==0)
G_IPC <- delete.vertices(G_IPC, Isolated)
LO2 <- graph_layout[-Isolated,]

colors1 <- c("red", "grey")

legend.name1 <- c("NL", "Other")

plot(G_IPC,
     layout = LO2,
     vertex.label = NA,                            
     vertex.color = c( "grey", "red")[1 + (V(G_IPC)$region == "NL")],                  
     vertex.size = degree^0.3,                                         
     edge.width = E(G_IPC)$weight/max(E(G_IPC)$weight),  
     edge.color = rgb(.5,.5,.5,.5)
)


legend(x = -1, y = -1.0,legend.name1, pch = 21,    
       col = "#777777", pt.bg = colors1, pt.cex = 1.5, cex = 0.9, bty = "n", ncol = 2)



