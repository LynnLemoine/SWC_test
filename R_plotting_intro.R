#### load data ####
df <- read.csv("Metadata.csv")

# which variables?
str(df)

#### load necessary packages ####
library(ggplot2)
library(dplyr)

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

plot_div <- ggplot(data = df, aes(x = Timepoint, y = Diversity...D0, fill = Reactor.cycle)) + geom_point(shape=21, size=4, alpha=0.5) + theme_bw()
plot_div

p5<-ggplot(data=df, aes(x=Timepoint , y=Diversity...D0, fill=Reactor.phase ))+
  geom_point(shape=21, size=5)
P6 <- p5 + facet_grid(~Reactor.phase)+theme_dark()
P6

pp1 <- ggplot(data=df,aes(x= Timepoint,y=Conductivity,fill=Reactor.phase))
pp1 <- pp1 + geom_point(shape=21,size=4,alpha = 0.5) + theme_bw() + geom_line(aes(color=Reactor.cycle)) # Facet it 
pp3 <- pp1 + facet_grid(~Reactor.cycle) 
pp4 <- pp1 + facet_grid(Reactor.phase~Reactor.cycle) 
pp4


#### dplyr intro####

mean(df[df$Reactor.phase == "Control", "ph"]) #calc mean ph for all rows where reactor.phase variable = "control"
levels(df$Reactor.phase)


#### select ####
physicochem <- select(df, ph, temp, Conductivity)
head(physicochem)
# %>% #alternative for pipe in linux, advantage is that is suggests names from columns, and can be used to combine pipes eg filter, select, mean...

physicochem <- df %>% select(ph,temp,Conductivity)

physicochem.control <- df %>% 
  filter(Reactor.phase == "Control") %>% 
  select(ph,temp,Conductivity)

# to select only diversity parameters
# for reactor phase "Startup"
grep("Diversity", names(df), value=TRUE)

diversity <- df %>% 
  filter(Reactor.phase == "Startup") %>% 
  select(contains("Diversity"))

#### group & summarize ####
meanph <- df %>%  group_by(Reactor.phase) %>% 
                  summarise(mean.ph = mean(ph), 
                            mean.d2 = mean(Diversity...D2),
                            sd.ph = sd(ph))
meanph

# summary for reactor phase 2 & add sdv of d2 and log10 transf cell count

std2 <- df %>%  filter(Reactor.cycle == 2) %>% 
                mutate(condratio = Conductivity/temp) %>% 
                group_by(Reactor.phase) %>% 
              summarise(mean.ph = mean(ph), 
            mean.d2 = mean(Diversity...D2),
            sd.ph = sd(ph),
            sd.rc2 = sd(Diversity...D2),
                          meanlogcell = mean(log10(Cell.density..cells.mL.),
                          meanratio = mean(condratio)))
std2


#### join datasets ####
physicochem <- df %>% select(sample_title, temp, ph, Conductivity)
diversity <- df %>% select(sample_title, contains("Diversity"))
View(physicochem)
View(diversity)

physicodiv <- full_join(physicochem, diversity, by="sample_title")
View(physicodiv)

# partial joining to prevent inserting NA data in table
# semi join -> useful to filter for matching rows in two tables

# remove rows that are empty
df_noNA <- na.exclude(df)
df_noNA
rowSums(is.na(df))
is.na(df_noNA)

#### combining dplyr and ggplot2 ####

p1 <- ggplot(data = df, aes(x = Timepoint, y = ph, fill = Reactor.cycle))
p1 <- p1 + geom_point(shape=21, size=4, alpha=0.5)
p2 <- p1 + theme_bw() + geom_line()

df.2 <- df %>%  filter(Reactor.cycle == 2)
p2 <- df %>%  filter(Reactor.cycle == 2) %>% 
          ggplot(aes(x=Timepoint, y=Cell.density..cells.mL., fill=Cell.density..cells.mL.)) + geom_point(shape=21, size=4))

p2 <- df %>% filter(Reactor.cycle==2) %>% 
  ggplot(aes(x = Timepoint,y = Cell.density..cells.mL., fill = Cell.density..cells.mL.)) +
  geom_point(shape = 21, size = 4)
p2
p2 + scale_y_log10()

p2.2 <- df %>% ggplot(aes(x = Timepoint,y = Cell.density..cells.mL., fill = Cell.density..cells.mL.)) +
  geom_point(shape = 21, size = 4)

