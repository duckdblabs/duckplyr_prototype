
library(ggplot2)
library(ggthemes)
library(dplyr)


options(scipen=10000)

our_theme <- theme_few(base_size = 24) + 
theme(axis.title.y=element_text(vjust=0.9), 
  axis.title.x=element_text(vjust=-0.1),
  axis.ticks.x=element_blank(),
  text=element_text(family="serif"), legend.title = element_blank(),
  legend.position = c(0.60, 0.70),
    legend.justification = c("left", "bottom"),
    legend.box.just = "left",
    legend.margin = margin(0, 0, 0, 0), legend.background= element_blank())


results <- rbind(read.csv("res-dplyr.csv"), read.csv("res-duckplyr.csv"), read.csv("res-data.table.csv"), read.csv("res-arrow.csv")) 

aggr_results <- results %>% mutate(file=pkg, query=sprintf("%2d", q), time_seconds=time) %>% group_by(file, query) %>% summarize(median_time_seconds = median(time_seconds))

pdf("prelim-tpch-results.pdf", height=5, width=10)
print(ggplot(aggr_results, aes(x=query, y=median_time_seconds, fill=file)) + geom_bar(position="dodge", stat="identity") + our_theme + ylab("Time (s)") + ggtitle("TPC-H SF1 Q1-10"))
dev.off()


print(aggr_results |> group_by(file) |> summarise(sum(median_time_seconds)))

