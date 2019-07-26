### Data Analysis of Terraforming Mars

library(readxl)
library(ggplot2)
library(dplyr)

mars <- read_excel("\\\\jefferson\\KARAS_JO$\\Documents\\Terraforming Mars Model.xlsx", sheet = "Card(2)")
mars.c <- mars %>% select(-card, -notes) # prep data for regression

ggplot(mars, aes(x = cost)) + geom_histogram(bins = 15, fill = "gray", color = "black")

# regression
model <- lm(cost ~ 0 + ., data = mars.c) # through the origin
summary(model)

# predition
p <- predict(model, mars.c)
mars.p <- cbind(mars, p)

ggplot(mars.p, aes(x = p)) + geom_histogram(bins = 15, fill = "gray", color = "black")

# Long format for cost data
mars.cost <- rbind(data.frame(type = "True Cost", cost = mars$cost),
                   data.frame(type = "Modeled Cost", cost = mars.p$p))

ggplot(mars.cost, aes(x = cost)) + geom_histogram(bins = 15, fill = "gray", color = "black") + facet_grid(~type)


# card efficiency
mars.p <- mars.p %>% mutate(p.diff = p - cost)

ggplot(mars.p, aes(x = p.diff)) + geom_histogram(bins = 15, fill = "gray", color = "black")

# top 10 worst cards
mars.p %>% select(card, cost, p, p.diff) %>% arrange(p.diff) %>% head(n = 10)

# top 10 best cards
mars.p %>% select(card, cost, p, p.diff) %>% arrange(desc(p.diff)) %>% head(n = 10)

