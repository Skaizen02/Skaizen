data <- tags[order(tags$avg_pop, decreasing = T),]
par(las = 2, mar =c(2.5, 10, 1, 1))
barplot(height = data$avg_pop[15:0], names.arg = data$tag[15:0], cex.names = 0.5, horiz = T)
grid(col = "gray")

data <- tags[order(tags$occurences, decreasing = T),]
par(las = 2, mar =c(2.5, 10, 1, 1))
barplot(height = data$occurences[15:0], names.arg = data$tag[15:0], cex.names = 0.5, horiz = T)
grid(col = "gray")