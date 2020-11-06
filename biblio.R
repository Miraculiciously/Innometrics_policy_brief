setwd("~/Desktop/Uni Utrecht/Innometrics 2020/Policy brief")

library(bibliometrix)
library(igraph)
library(plyr)
library(treemap)
library(dplyr)
library(RColorBrewer)


rm(list=ls())

M <- convert2df(c("data/savedrecs_5/savedrecs_1.txt", "data/savedrecs_5/savedrecs_2.txt",
                  "data/savedrecs_5/savedrecs_3.txt", "data/savedrecs_5/savedrecs_4.txt",
                  "data/savedrecs_5/savedrecs_5.txt", "data/savedrecs_5/savedrecs_6.txt",
                  "data/savedrecs_5/savedrecs_7.txt", "data/savedrecs_5/savedrecs_8.txt",
                  "data/savedrecs_5/savedrecs_9.txt"),
                dbsource = "wos", format = "plaintext")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";") # get authors countries

# Join Wageningen Research Centre and University to one
M$AU_UN <- gsub("UNIV WAGENINGEN AND RES CTR", "WAGENINGEN UNIV", M$AU_UN)

results <- biblioAnalysis(M)

################################################################
################################################################
####################### ACTOR  MATCHING ########################
################################################################
################################################################

# In this part, we will check for matches between patent applicants
# and authors/universities. For this, we first load the patent data
load("data/EP_INV_2005.RDA")
load("data/EP_APP_clean_2005.RDA")
load("data/EP_Y02.RDA")
load("data/EP_IPC.RDA")

# Extract list of universities, authors, and inventors. Standardise those
# to lower case and identify unique ones.
authors <- unlist(strsplit(as.character(rbind(M$AU, M$AF)),split = ";")) %>%
  tolower() %>% unique()
unis <- unlist(strsplit(as.character(M$AU_UN),split = ";")) %>%
  tolower() %>% unique()
inv <- EP_INV_2005$Inv_name[which(EP_INV_2005$Ctry_code == "NL")] %>%
  tolower() %>% unique()

# Now look for any matches of patent applicant substring in the lists
# of authors and universities. For this, create an empty index list and take any
# index for which the inventor substring is a match. Since the loop takes some
# time, we have added a progress bar.
index_list <- c()
width <- options()$width
for (k in 1:length(inv)) {
  cat(paste0(rep('=', (k-1) / length(inv) * (width-2)), collapse = ''))
  
  if(any(unlist(strsplit(inv[k], split = ", ")) %in% authors))
    index_list[length(index_list) + 1] <- k
  if(any(unlist(strsplit(inv[k], split = " ")) %in% unis))
    index_list[length(index_list) + 1] <- k
  
  if (k == length(inv)) cat(' Done!\n')
  else cat('\014')
}

# Now print the results - and manually check matches the matches
ifelse (length(inv[index_list]) > 0, print(inv[index_list]), "No matches found")



# Now, we perform a more thorough patent seach. We filter the patents as we do 
# for the technology space.
# First, join IPC dataframe to Y02 dataframe
Y02_IPC <- inner_join(EP_IPC, EP_Y02)
Y02_IPC <- filter(Y02_IPC, CPC_Class != "")
# Select only patents with Y02W as CPC class
Y02W  <- Y02_IPC %>%
  filter(grepl('Y02W', Y02_IPC$CPC_Class))
# Select the Y02 classes
Y02W <- Y02W %>%
  filter(IPC != "") %>%
  mutate(IPC3 = substr(IPC, 1,4)) %>%
  filter(Prio_Year <=2015) %>%
  select(Appln_id, IPC3) %>%
  distinct()

# Join Y02 to the dataframe containing regional codes for each patents
app_country <- select(EP_APP_clean_2005, c(Appln_id, Reg_code))
Y02_NL <- inner_join(Y02W, app_country)

Y02_NL <- Y02_NL %>%
  filter(grepl('NL', Y02_NL$Reg_code))

# Unique patents in NL
totalpatents <- select(Y02_NL, Appln_id)
totalpatents <- distinct(totalpatents)

total_names <- inner_join(totalpatents, EP_APP_clean_2005) %>% select(1,2,3)
total_names <-  total_names[grepl("UNIVERSIT", total_names$Clean_name),]
print(total_names$App_name)

################################################################
################################################################
######################### COOPERATION ##########################
################################################################
################################################################

###############
# look at country collaboration of most important collaborators for NL
###############

rm(list=setdiff(ls(), c("M", "results")))

temp <- as.data.frame(table(results$CO)) # frequency table of country collaboration
quantile(temp$Freq, probs = seq(0.6, 1.0, 0.05))
temp <- temp[which(temp$Freq > 74),]  # choose upper 10th percentile
temp <- as.character(droplevels(temp$Var1)) # get most important collaboration countries

# now find the papers that only contain authors from these countries
index_list <- c()
for (k in c(1:nrow(M))) {
  string <- unlist(strsplit(as.character(M$AU_CO[k]),split = ";")) # split string of countries
  # get index of paper if all countries amongst most important collaboration countries?
  if (all(string %in% temp)) index_list[length(index_list) + 1] <- k
  rm(string)
}
N <- M[index_list,] # choose only those papers

# Create collaboration network of only these countries
NetMatrix <- biblioNetwork(N, analysis = "collaboration", network = "countries", sep = ";")
# get countries ordered by relevance
diag(as.matrix(NetMatrix))[order(diag(as.matrix(NetMatrix)), decreasing = T)]
net=networkPlot(NetMatrix,
                n = dim(NetMatrix)[1],
                Title = "Top 10% Collaboration Countries for the Netherlands",
                type = "circle",
                size=TRUE,
                remove.multiple=FALSE,
                labelsize=1,
                alpha = 0.9,
                cluster="none")

# Create collaboration network of universities in only these countries
NetMatrix <- biblioNetwork(N, analysis = "collaboration", network = "universities", sep = ";")

# Due for standardizing errors, France is listed as a university.
# Thus, we remove this entry manually:
#NetMatrix[which(dimnames(NetMatrix)[[1]] == "FRANCE."),] <- 0
#NetMatrix[,which(dimnames(NetMatrix)[[1]] == "FRANCE.")] <- 0

net=networkPlot(NetMatrix,
                n = dim(NetMatrix)[1],
                Title = "University Collaboration amongst top 10% Countries for the Netherlands",
                type = "auto",
                size=TRUE,
                remove.multiple=FALSE,
                label = NULL,
                alpha = 0.9,
                cluster="louvain")

# This is not very helpful, so we transform the matrix to a network and work with that
net <- graph_from_adjacency_matrix( as.matrix(NetMatrix),
                                    mode = "undirected",
                                    diag = F,
                                    weighted = T)

# Simplifying the graph does not have any effect but we leave it in
# to show that we considered this.
# net <- simplify(net, remove.multiple = T, remove.loops = T)

# Now obtain top 10 central universities by different measures
sort(degree(net, mode = "all"), decreasing = T, na.last = T)[1:10]
sort(betweenness(net), decreasing = T, na.last = T)[1:10]

##################################
# For closenes, reduce network to its biggest component since
# otherwise, the measure is not well-defined anymore.
##################################
comp = components(net)
main_net = induced_subgraph(net, 
                            which(comp$membership == which.max(comp$csize)))
sort(closeness(main_net , mode = "all"), decreasing = T, na.last = T)[1:10]


sort(eigen_centrality(net)$vector, decreasing = T, na.last = T)[1:11]
sort(subgraph.centrality(net, diag = T), decreasing = T, na.last = T)[1:10]

#Average clustering coefficient
transitivity(net, type="global", isolates = "nan")

# Network density
n <- length(V(net))
all_possible_edges <- n*(n+1)/2
length(E(net)) / all_possible_edges*100

# Giant component
length(which(components(net)$membership == which.max(components(net)$csize))) / length(V(net))

###########################################
############### Intra-Dutch ###############
###########################################
rm(list=setdiff(ls(), c("M", "results")))

# collect all Dutch universities for a colouring vector
index_list <- c()
for (k in c(1:nrow(M))) {
  string <- unlist(strsplit(as.character(M$AU_CO[k]),split = ";")) # split string of countries
  # get index of paper if all countries amongst most important collaboration countries?
  if (all(string %in% "NETHERLANDS")) index_list[length(index_list) + 1] <- k
  rm(string)
}
N <- M[index_list,]

NetMatrix <- biblioNetwork(N, analysis = "collaboration", network = "universities", sep = ";")
net <- graph_from_adjacency_matrix( as.matrix(NetMatrix), mode = "undirected", diag = F, weighted = T)

quantile(degree(net), probs = seq(0.9, 1.0, 0.005))


index_list <- c()
for (k in 1:length(degree(net))) {
  if (degree(net)[k] > 4.99) index_list[length(index_list) + 1] <- k
}
net2 <- delete_vertices(net, V(net)[-index_list])

my_levels <- cut(degree(net2), breaks=c(quantile(degree(net2), probs = seq(0, 1, by = 0.20))), 
    labels=c("0-20","20-40","40-60","60-80","80-100"), include.lowest=TRUE)

color_level <- as.data.frame(levels(my_levels))
names(color_level) <- "lvl"
color_level$col <- brewer.pal(n = 5, name = "RdYlBu")

verti_level <- as.data.frame(as.vector(V(net2)))
names(verti_level) <- "names"
verti_level$lvl <- my_levels

verti_color <- inner_join(verti_level, color_level)

plot.igraph(net2,
     layout = layout.fruchterman.reingold(net2),
     vertex.size = degree(net2)^0.8,
     vertex.color = verti_color$col,
     vertex.frame.color = "white",
     edge.width = E(net2)$weight/max(E(net2)$weight),
     edge.color = "black", #rgb(.5,.5,.5,.5), 
     vertex.label= abbreviate(V(net2)$name),
     vertex.label.cex=0.5,
     vertex.label.color="black",
     vertex.label.dist=0.1
)
text(0,1.1,"Collaboration among top 10% of Dutch universities",col="black", cex=1)
legend(x=1.1, y=1.1, 
       legend=c("90-92%", "92-94%", "94-96%", "96-98%", "98-100%"), 
       col = color_level$col , 
       bty = "n", pch=20 , pt.cex = 2, cex = 1,
       text.col="black" , horiz = F)

################################################################
################################################################
###################### KEYWORD ANALYSIS ########################
################################################################
################################################################

rm(list=setdiff(ls(), "M"))

keywords_2005 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2005"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2005"),]))$ID))
keywords_2005$Tab <-  gsub("[[:punct:]]", "", keywords_2005$Tab)
keywords_2005 <- ddply(keywords_2005,~Tab, summarise, "2005"=sum(Freq))

keywords_2006 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2006"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2006"),]))$ID))
keywords_2006$Tab <-  gsub("[[:punct:]]", "", keywords_2006$Tab)
keywords_2006 <- ddply(keywords_2006,~Tab, summarise, "2006"=sum(Freq))

keywords_2007 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2007"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2007"),]))$ID))
keywords_2007$Tab <-  gsub("[[:punct:]]", "", keywords_2007$Tab)
keywords_2007 <- ddply(keywords_2007,~Tab, summarise, "2007"=sum(Freq))

keywords_2008 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2008"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2008"),]))$ID))
keywords_2008$Tab <-  gsub("[[:punct:]]", "", keywords_2008$Tab)
keywords_2008 <- ddply(keywords_2008,~Tab, summarise, "2008"=sum(Freq))

keywords_2009 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2009"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2009"),]))$ID))
keywords_2009$Tab <-  gsub("[[:punct:]]", "", keywords_2009$Tab)
keywords_2009 <- ddply(keywords_2009,~Tab, summarise, "2009"=sum(Freq))

keywords_2010 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2010"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2010"),]))$ID))
keywords_2010$Tab <-  gsub("[[:punct:]]", "", keywords_2010$Tab)
keywords_2010 <- ddply(keywords_2010,~Tab, summarise, "2010"=sum(Freq))

keywords_2011 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2011"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2011"),]))$ID))
keywords_2011$Tab <-  gsub("[[:punct:]]", "", keywords_2011$Tab)
keywords_2011 <- ddply(keywords_2011,~Tab, summarise, "2011"=sum(Freq))

keywords_2012 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2012"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2012"),]))$ID))
keywords_2012$Tab <-  gsub("[[:punct:]]", "", keywords_2012$Tab)
keywords_2012 <- ddply(keywords_2012,~Tab, summarise, "2012"=sum(Freq))

keywords_2013 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2013"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2013"),]))$ID))
keywords_2013$Tab <-  gsub("[[:punct:]]", "", keywords_2013$Tab)
keywords_2013 <- ddply(keywords_2013,~Tab, summarise, "2013"=sum(Freq))

keywords_2014 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2014"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2014"),]))$ID))
keywords_2014$Tab <-  gsub("[[:punct:]]", "", keywords_2014$Tab)
keywords_2014 <- ddply(keywords_2014,~Tab, summarise, "2014"=sum(Freq))

keywords_2015 <- rbind(as.data.frame((biblioAnalysis(M[which(M$PY == "2015"),]))$DE),
                       as.data.frame((biblioAnalysis(M[which(M$PY == "2015"),]))$ID))
keywords_2015$Tab <-  gsub("[[:punct:]]", "", keywords_2015$Tab)
keywords_2015 <- ddply(keywords_2015,~Tab, summarise, "2015"=sum(Freq))

keywords <- merge(keywords_2005, keywords_2006, all = TRUE)
keywords <- merge(keywords, keywords_2007, all = TRUE)
keywords <- merge(keywords, keywords_2008, all = TRUE)
keywords <- merge(keywords, keywords_2009, all = TRUE)
keywords <- merge(keywords, keywords_2010, all = TRUE)
keywords <- merge(keywords, keywords_2011, all = TRUE)
keywords <- merge(keywords, keywords_2012, all = TRUE)
keywords <- merge(keywords, keywords_2013, all = TRUE)
keywords <- merge(keywords, keywords_2014, all = TRUE)
keywords <- merge(keywords, keywords_2015, all = TRUE)

keywords[is.na(keywords)] <- 0

  ## Create lm for each row, get Growth coefficient to see if growing or now
keywords$Growth <- lm.fit(cbind(1,2005:2015), t(keywords[,c(2:12)]))$coefficients[2,]

keywords <- keywords %>% mutate(per_year = rowSums(.[2:12])/11) # normalise to average per year

# hot topics by keywords
quantile(keywords$per_year, probs = seq(0.995, 1.0, 0.0005))
temp <- keywords[which(keywords$per_year >= 3.751136),]  # choose 99.75% quantile
treemap(temp,
        index="Tab",
        vSize="per_year",
        vColor="Growth",
        type="manual",
        palette="RdYlBu",
        title= "Treemap of hot topics by keywords 2005-2015\n"
        )

# fastest growing topics over the past 5 years
temp <- keywords %>%
  select(Tab, "2010", "2011", "2012", "2013", "2014", "2015")
temp$Growth <- lm.fit(cbind(1,2010:2015), t(temp[,c(2:7)]))$coefficients[2,]
temp <- temp %>% mutate(per_year = rowSums(.[2:7])/5) # normalise to average per year

quantile(temp$Growth, probs = seq(0.99, 1.0, 0.0005)) # get quantiles
temp <- temp[which(temp$Growth >= 0.8646429),]  # choose 99.75% quantile

treemap(temp,
        index="Tab",
        vSize="per_year",
        vColor="Growth",
        type="manual",
        palette="RdYlBu",
        title= "Treemap of fastest growing keywords 2010-2015\n"
        )

# fastest declining topics over the past 5 years
temp <- keywords %>%
  select(Tab, "2010", "2011", "2012", "2013", "2014", "2015")
temp$Growth <- lm.fit(cbind(1,2010:2015), t(temp[,c(2:7)]))$coefficients[2,]
temp <- temp %>% mutate(per_year = rowSums(.[2:7])/5) # normalise to average per year

quantile(temp$Growth, probs = seq(0.0, 0.02, 0.0005)) # get quantiles
temp <- temp[which(temp$Growth < -0.3142857),]  # choose 0.25% quantile

treemap(temp,
        index="Tab",
        vSize="per_year",
        vColor="Growth",
        type="manual",
        palette="RdYlBu",
        title= "Treemap of fastest decreasing keywords 2010-2015\n"
        )
