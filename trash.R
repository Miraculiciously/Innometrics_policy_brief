setwd("~/Desktop/Uni Utrecht/Innometrics 2020/Policy brief")

library(dplyr)
library(ggplot2)
library(readr)

df <- read_delim("data/trash_provinces/netherlands.csv", ";")
colnames(df) <- c("Topic", "Mode", c(2005:2015), "Province")
df <- df %>% select(-Mode)

nl <- df[grepl("Processing", df$Topic),]
nl <- nl[grepl("Total municipal waste", nl$Topic),]
nl$Topic <- substr(nl$Topic, 34, nchar(nl$Topic))


nl <- data.frame(activity = nl$Topic,
                   province = nl$Province,
                   value = c(nl$`2005`,nl$`2006`, nl$`2007`,
                             nl$`2008`,nl$`2009`, nl$`2010`,
                             nl$`2011`, nl$`2012`, nl$`2013`,
                             nl$`2014`, nl$`2015`),
                   year = c(rep("2005", nrow(nl)),
                            rep("2006", nrow(nl)),
                            rep("2007", nrow(nl)),
                            rep("2008", nrow(nl)),
                            rep("2009", nrow(nl)),
                            rep("2010", nrow(nl)),
                            rep("2011", nrow(nl)),
                            rep("2012", nrow(nl)),
                            rep("2013", nrow(nl)),
                            rep("2014", nrow(nl)),
                            rep("2015", nrow(nl))
                   )
)

ggplot(nl, aes(x=year, y=value, fill=activity)) +
  geom_bar(stat='identity', position='stack') +
  theme_minimal() +
  facet_wrap(~province) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(fill = "Processing type",
       x = "Year",
       y = "Percentage",
       title = "Processing of municipal trash for each province, 2005-2015"
  )
