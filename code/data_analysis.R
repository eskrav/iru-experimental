rm(list=ls(all=TRUE))

library(lmerTest)
library(tidyverse)
# Results may be slightly different; original code was run with lme version 1.1-17
library(lme4)
library(Hmisc)

set.seed(42)

#### Setup ####


data <- read_csv("../data/anonymized_data.csv") %>% 
  mutate_if(is.character, funs(factor(.))) %>%
  mutate(workerid = factor(workerid))

data <- filter(data, world %in% c("ordinary","wonky"), eligible=="True")
data <- droplevels(data)

data$cWorld <- ifelse(data$world=="ordinary",0.5,-0.5)
data$cCondition <- ifelse(data$condition=="after",0.5,-0.5)

data$experiment <- factor(data$experiment,
                          levels=c("exclamation","ohyeah","period"))
contrast <- matrix(c(-0.5,0.5,0,-1/3,-1/3,2/3),nrow=3)
contrasts(data$experiment) <- contrast

habitual.data <- filter(data,(activity=="habitual"|is.na(activity)),
                        Qtype=="habitual")
nonhabitual.data <- filter(data,(activity=="non-habitual"|is.na(activity)),
                           Qtype=="non-habitual")

exc.habitual <- filter(habitual.data, experiment=="exclamation")
exc.nonhabitual <- filter(nonhabitual.data, experiment=="exclamation")
oh.habitual <- filter(habitual.data, experiment=="ohyeah")
oh.nonhabitual <- filter(nonhabitual.data, experiment=="ohyeah")
period.habitual <- filter(habitual.data, experiment=="period")
period.nonhabitual <- filter(nonhabitual.data, experiment=="period")

#### Exclamation ####

## Habitual model
habitual_exc <- lmer(rating ~ cWorld + cCondition + cWorld*cCondition + 
                       (cWorld+cCondition|workerid) + 
                       (cWorld*cCondition|story), 
                     exc.habitual)
summary(habitual_exc)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(exc.habitual, world=="ordinary")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(exc.habitual, world=="wonky")))

## Non-habitual model
nonhabitual_exc <- lmer(rating ~ cWorld + cCondition + cWorld*cCondition + 
                          (cWorld+cCondition|workerid) + 
                          (cWorld*cCondition|story), 
                        exc.nonhabitual)
summary(nonhabitual_exc)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(exc.nonhabitual, world=="ordinary")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(exc.nonhabitual, world=="wonky")))

## Plot Habitual ##
exc.habitual.data_plot <- exc.habitual
exc.habitual.data_plot$world <- factor(exc.habitual.data_plot$world, 
                                       levels=c("wonky", "ordinary"))
exc.habitual.data_plot$condition <- factor(exc.habitual.data_plot$condition,
                                           levels=c("after","before"))

dodge <- position_dodge(width = 0.85)

exc.habitual.summary <- exc.habitual.data_plot %>%
  group_by(world, condition) %>%
  summarise(val = mean(rating))

ggplot(exc.habitual.data_plot, aes(x = world, y = rating, fill = condition)) + 
  geom_violin(position = dodge, scale="count") +
  geom_boxplot(width=.175, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = exc.habitual.summary, aes(y = val), position = dodge, 
             shape=21) +
  geom_line(data = filter(exc.habitual.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches")))+
  geom_line(data = filter(exc.habitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) + 
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="right", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary")) + 
  coord_flip()

## Plot Non-habitual ##
exc.nonhabitual.data_plot <- exc.nonhabitual
exc.nonhabitual.data_plot$world <- factor(exc.nonhabitual.data_plot$world, 
                                          levels=c("wonky", "ordinary"))
exc.nonhabitual.data_plot$condition <- 
  factor(exc.nonhabitual.data_plot$condition, levels=c("after","before"))

dodge <- position_dodge(width = 0.85)

exc.nonhabitual.summary <- exc.nonhabitual.data_plot %>%
  group_by(world, condition) %>%
  summarise(val = mean(rating))

ggplot(exc.nonhabitual.data_plot, aes(x = world, y = rating, fill=condition)) + 
  geom_violin(position = dodge, width=0.5) +
  geom_boxplot(width=.175, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = exc.nonhabitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(exc.nonhabitual.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85))+
  geom_line(data = filter(exc.nonhabitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) + 
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="right", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary")) + 
  coord_flip()

#### Oh Yeah ####

## Habitual model
habitual_oh <- lmer(rating ~ cWorld + cCondition + cWorld*cCondition + 
                      (cWorld+cCondition|workerid) + 
                      (cWorld*cCondition|story), 
                    oh.habitual)
summary(habitual_oh)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(oh.habitual, world=="ordinary")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(oh.habitual, world=="wonky")))

## Non-habitual model
nonhabitual_oh <- lmer(rating ~ cWorld + cCondition + cWorld*cCondition + 
                         (cWorld+cCondition|workerid) + 
                         (cWorld*cCondition|story), 
                       oh.nonhabitual)
summary(nonhabitual_oh)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(oh.nonhabitual, world=="ordinary")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(oh.nonhabitual, world=="wonky")))

## Plot Habitual ##
oh.habitual.data_plot <- oh.habitual
oh.habitual.data_plot$world <- factor(oh.habitual.data_plot$world, 
                                      levels=c("wonky", "ordinary"))
oh.habitual.data_plot$condition <- factor(oh.habitual.data_plot$condition,
                                          levels=c("after","before"))

dodge <- position_dodge(width = 0.85)

oh.habitual.summary <- oh.habitual.data_plot %>%
  group_by(world, condition) %>%
  summarise(val = mean(rating))

ggplot(oh.habitual.data_plot, aes(x = world, y = rating, fill = condition)) + 
  geom_violin(position = dodge) +
  geom_boxplot(width=.175, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = oh.habitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(oh.habitual.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches")))+
  geom_line(data = filter(oh.habitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) +
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="right", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary")) + 
  coord_flip()


## Plot Non-habitual ##
oh.nonhabitual.data_plot <- oh.nonhabitual
oh.nonhabitual.data_plot$world <- factor(oh.nonhabitual.data_plot$world, 
                                         levels=c("wonky", "ordinary"))
oh.nonhabitual.data_plot$condition <- factor(oh.nonhabitual.data_plot$condition,
                                             levels=c("after","before"))

dodge <- position_dodge(width = 0.85)

oh.nonhabitual.summary <- oh.nonhabitual.data_plot %>%
  group_by(world, condition) %>%
  summarise(val = mean(rating))

ggplot(oh.nonhabitual.data_plot, aes(x = world, y = rating, fill = condition)) + 
  geom_violin(position = dodge, width=0.5) +
  geom_boxplot(width=.175, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = oh.nonhabitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(oh.nonhabitual.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches")))+
  geom_line(data = filter(oh.nonhabitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches"))) + 
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="right", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary")) + 
  coord_flip()

#### Period ####

## Habitual model
habitual_per <- lmer(rating ~ cWorld + cCondition + cWorld*cCondition + 
                       (cWorld+cCondition|workerid) + 
                       (cWorld*cCondition|story), 
                     period.habitual)
summary(habitual_per)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(period.habitual, world=="ordinary")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(period.habitual, world=="wonky")))

## Non-habitual model
nonhabitual_per <- lmer(rating ~ cWorld + cCondition + cWorld*cCondition + 
                       (cWorld+cCondition|workerid) + 
                       (cWorld*cCondition|story), 
                     period.nonhabitual)
summary(nonhabitual_per)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(period.nonhabitual, world=="ordinary")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(period.nonhabitual, world=="wonky")))

## Plot Habitual ##
period.habitual.data_plot <- period.habitual
period.habitual.data_plot$world <- 
  factor(period.habitual.data_plot$world, levels=c("wonky", "ordinary"))
period.habitual.data_plot$condition <- 
  factor(period.habitual.data_plot$condition, levels=c("after","before"))

dodge <- position_dodge(width = 0.85)

period.habitual.summary <- period.habitual.data_plot %>%
  group_by(world, condition) %>%
  summarise(val = mean(rating))

ggplot(period.habitual.data_plot, aes(x = world, y = rating, fill=condition)) + 
  geom_violin(position = dodge) +
  geom_boxplot(width=.175, aes(fill=NULL, group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = period.habitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(period.habitual.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches"))) +
  geom_line(data = filter(period.habitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) + 
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="right", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary")) + 
  coord_flip()

## Plot Non-habitual ##
period.nonhabitual.data_plot <- period.nonhabitual
period.nonhabitual.data_plot$world <- 
  factor(period.nonhabitual.data_plot$world, levels=c("wonky", "ordinary"))
period.nonhabitual.data_plot$condition <- 
  factor(period.nonhabitual.data_plot$condition, levels=c("after","before"))

dodge <- position_dodge(width = 0.85)

period.nonhabitual.summary <- period.nonhabitual.data_plot %>%
  group_by(world, condition) %>%
  summarise(val = mean(rating))

ggplot(period.nonhabitual.data_plot, aes(x = world, y = rating, fill=condition)) + 
  geom_violin(position = dodge, width=0.33) +
  geom_boxplot(width=.175, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = period.nonhabitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(period.nonhabitual.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches"))) +
  geom_line(data = filter(period.nonhabitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "first", type = "closed", 
                          length = unit(0.1, "inches"))) + 
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="right", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary")) + 
  coord_flip()

#### Cross-Experiment Comparison ####
habitual_all <- lmer(rating ~ experiment*cWorld*cCondition + 
                       (cWorld+cCondition|workerid) + 
                       (experiment+cWorld*cCondition|story), 
                     habitual.data)
summary(habitual_all)

nonhabitual_all <- lmer(rating ~ experiment*cWorld*cCondition + 
                          (cWorld+cCondition|workerid) + 
                          (cWorld*cCondition|story), 
                        nonhabitual.data)
summary(nonhabitual_all)

## Plot Habitual ##

habitual.all.data_plot <- habitual.data
habitual.all.data_plot$world <- factor(habitual.all.data_plot$world, 
                                       levels=c("ordinary","wonky"))
habitual.all.data_plot$condition <- factor(habitual.all.data_plot$condition,
                                           levels=c("before","after"))

dodge <- position_dodge(width = 0.85)

habitual.all.data_plot$experiment <- 
  fct_recode(habitual.all.data_plot$experiment, 
             "Exclamation"="exclamation",
             "Oh yeah, and..."="ohyeah",
             "Period"="period")

habitual.all.summary <- habitual.all.data_plot %>%
  group_by(experiment, world, condition) %>%
  summarise(val = mean(rating))

ggplot(habitual.all.data_plot, aes(x = world, y = rating, fill = condition)) + 
  geom_violin(position = dodge) +
  facet_grid(. ~ experiment) + 
  geom_boxplot(width=.2, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = habitual.all.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(habitual.all.summary, world=="ordinary"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "last", type = "closed", 
                          length = unit(0.1, "inches")))+
  geom_line(data = filter(habitual.all.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) + 
  ylab("Activity Habituality Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="top", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary"))

## Plot Non-habitual ##

nonhabitual.all.data_plot <- nonhabitual.data
nonhabitual.all.data_plot$world <- 
  factor(nonhabitual.all.data_plot$world, levels=c("ordinary","wonky"))
nonhabitual.all.data_plot$condition <- 
  factor(nonhabitual.all.data_plot$condition, levels=c("before","after"))

dodge <- position_dodge(width = 0.85)

nonhabitual.all.data_plot$experiment <- 
  fct_recode(nonhabitual.all.data_plot$experiment, 
             "Exclamation"="exclamation",
             "Oh yeah, and..."="ohyeah",
             "Period"="period")

nonhabitual.all.summary <- nonhabitual.all.data_plot %>%
  group_by(experiment, world, condition) %>%
  summarise(val = mean(rating))

ggplot(nonhabitual.all.data_plot, aes(x = world, y = rating, fill=condition)) +
  geom_violin(position = dodge, width=0.33) +
  facet_grid(. ~ experiment) + 
  geom_boxplot(width=.2, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = nonhabitual.all.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(nonhabitual.all.summary, 
                          experiment %in% c("Oh yeah, and...","Period")), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "last", type = "closed", 
                          length = unit(0.1, "inches")))+
  geom_line(data = filter(nonhabitual.all.summary, experiment=="Exclamation"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85))+
  ylab("Activity Frequency Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="top", 
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.2, end = 0.8, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("wonky\n('non-payer')","ordinary"))

## Scatterplot ##

total1 <- data %>%
  filter(condition=="before") %>%
  group_by(story,world,activity,experiment,Qtype) %>%
  summarise(meanRating = mean(rating))
total2 <- data %>%
  filter(condition=="after") %>%
  group_by(story,world,activity,experiment,Qtype) %>%
  summarise(meanRating = mean(rating))
total <- left_join(total2,total1,by=c("story","world","experiment","Qtype"))
total <- filter(total, as.character(Qtype)==as.character(activity.x)) %>%
  select(c(1:4,6,8)) %>%
  rename(activity=activity.x, after = meanRating.x, before = meanRating.y) %>%
  mutate(change = after-before)
modelTotal <- total
total <- unite(total,condition,c(world,activity),sep=" ")
total$experiment <- factor(total$experiment,
                           levels=c("exclamation","ohyeah","period"))
total$condition <- factor(total$condition,
                          levels=c("ordinary habitual",
                                   "ordinary non-habitual",
                                   "wonky habitual",
                                   "wonky non-habitual"))

# Exclamation slope
lm1 <- lm(after ~ before, data=filter(modelTotal, experiment=="exclamation"))
summary(lm1)

# Oh yeah slope
lm2 <- lm(after ~ before, data=filter(modelTotal, experiment=="ohyeah"))
summary(lm2)

# Period slope
lm3 <- lm(after ~ before, data=filter(modelTotal, experiment=="period"))
summary(lm3)

### Plot

cutoff <- data.frame( x = c(0, 100), y = c(0, 100), 
                      cutoff = factor("No effect on\nactivity perception"))

total$condition <- 
  fct_recode(total$condition, 
             "ordinary habitual\n(cashier-paying)"="ordinary habitual",
             "ordinary non-habitual\n(apple-buying)"="ordinary non-habitual",
             "wonky habitual\n(cashier-paying)"="wonky habitual",
             "wonky non-habitual\n(apple-buying)"="wonky non-habitual")

total$experiment <- fct_recode(total$experiment, 
                               "Exclamation:"="exclamation",
                               "Oh yeah, and...:"="ohyeah",
                               "Period:"="period")

ggplot(total, aes(x=before, y=after)) +
  geom_point(aes(color=change, shape = condition), size=3) + 
  scale_shape_manual(values=c(15,16,3,8)) +    
  geom_smooth(method=lm, color="black") + 
  labs(x = "Pre-utterance habituality rating", 
       y = "Post-utterance habituality rating", 
       shape='Condition:', 
       color="Habituality after\nutterance seen:") + 
  geom_line(aes( x, y, linetype = cutoff ), cutoff) +
  ggtitle("Exp. 1-3: All Activities") + 
  theme(legend.position="right", 
        plot.title = element_text(hjust = 0.5)) + 
  coord_cartesian(xlim=c(0,100), ylim = c(0,100), expand=FALSE) +
  scale_linetype_manual(name="Along Line:", 
                        values=c("dotdash", "dotted")) +
  guides(col=guide_legend(nrow=5, keyheight=1), 
         shape=guide_legend(nrow=4, keyheight=1.75)) +
  facet_wrap(~ experiment, ncol = 1) + 
  scale_color_gradient(low="#000000", high="#CCCCCC", 
                       breaks = c(-25,0,25), 
                       labels = c("-25: decrease", 
                                  "   0: no change", 
                                  " 25: increase")) +
  theme_classic()
