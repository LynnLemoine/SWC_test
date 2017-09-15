df <- read.csv("Metadata.csv")

# which variables?
str(df)

# start plotting
library(ggplot2)

# make first plot
ggplot(data = df, aes(x = Timepoint, y = ph, fill = Reactor.cycle)) +
  geom_point(shape = 21, size = 4)

# change structure of column reactor.cycle
df$Reactor.cycle <- factor(df$Reactor.cycle)

# NA in plot because of empty rows in dataset -> common when making csv from excel files

ggplot(data = df, aes(x = Timepoint, y = temp, fill = Reactor.cycle)) +
  geom_point(shape = 21, size = 4)
ggplot(data = df, aes(x = Timepoint, y = Reactor.phase, fill = Reactor.cycle)) +
  geom_point(shape = 21, size = 4)

# make ggplot object
p1 <- ggplot(data = df, aes(x = Timepoint, y = ph, fill = Reactor.cycle))
p1 <- p1 + geom_point(shape=21, size=4, alpha=0.5)
p2 <- p1 + theme_bw() + geom_line()

# lets facet this
p3 <- p2 + facet_grid(~Reactor.cycle)
p3

# How do I know
# what's inside reactor phase
df$Reactor.phase
levels(df$Reactor.phase) #show only levels of factor variable

p4 <- p2 + facet_grid(Reactor.phase~Reactor.cycle)
p4

p5 <- ggplot(data = df, aes(x = Timepoint, y = temp, fill = Reactor.phase)) + geom_point(shape = 21, size = 4, alpha=0.5) + theme_bw() + geom_line()
p6 <- p5 + facet_grid(Reactor.phase~Reactor.cycle)
p6

# alternative way to change "color by"
p5 <- ggplot(data = df, aes(x = Timepoint, y = temp, fill = Reactor.phase)) + geom_point(shape = 21, size = 4, alpha=0.5) + theme_bw() + geom_line(aes(color=Reactor.phase))
p6 <- p5 + facet_grid(Reactor.phase~Reactor.cycle)
p6

# ggplot Diversity DO




