rm(list=ls(all=TRUE))

library(lmerTest)
library(tidyverse)
library(lme4)
library(Hmisc)

set.seed(42)

#### Setup ####

supp_data <- read_csv("supp_data.csv") %>% 
  mutate_if(is.character, funs(factor(.))) %>%
  mutate(workerid = factor(workerid))

supp_data <- filter(supp_data, eligible=="True")

supp_data$cWorld <- ifelse(supp_data$world=="ordinary",0.5,-0.5)
supp_data$cCondition <- ifelse(supp_data$condition=="after",0.5,-0.5)

supp_data$experiment <- factor(supp_data$experiment,
                          levels=c("exclamation","ohyeah","period"))
contrast <- matrix(c(-0.5,0.5,0,-1/3,-1/3,2/3),nrow=3)
contrasts(supp_data$experiment) <- contrast

habitual.supp_data <- filter(supp_data,(activity=="habitual"|is.na(activity)),
                        Qtype=="habitual")
nonhabitual.supp_data <- 
  filter(supp_data,(activity=="non-habitual"|is.na(activity)),
                           Qtype=="non-habitual")

exc.habitual_supp <- filter(habitual.supp_data, experiment=="exclamation")
exc.nonhabitual_supp <- filter(nonhabitual.supp_data, experiment=="exclamation")
oh.habitual_supp <- filter(habitual.supp_data, experiment=="ohyeah")
oh.nonhabitual_supp <- filter(nonhabitual.supp_data, experiment=="ohyeah")
period.habitual_supp <- filter(habitual.supp_data, experiment=="period")
period.nonhabitual_supp <- filter(nonhabitual.supp_data, experiment=="period")

#### Models ####

## Habitual
supp_habitual <- lmer(rating ~ experiment * cWorld * cCondition + 
                        (cWorld + cCondition|workerid) + 
                        (cWorld + cCondition|story), 
                      habitual.supp_data)
summary(supp_habitual)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(habitual.supp_data, 
                    experiment=="exclamation" & world=="ordinary")))
summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(habitual.supp_data, 
                    experiment=="exclamation" & world=="wonky")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(habitual.supp_data, 
                    experiment=="ohyeah" & world=="ordinary")))
summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(habitual.supp_data, 
                    experiment=="ohyeah" & world=="wonky")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(habitual.supp_data, 
                    experiment=="period" & world=="ordinary")))
summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(habitual.supp_data, 
                    experiment=="period" & world=="wonky")))

## Non-habitual
supp_nonhabitual <- lmer(rating ~ experiment * cWorld * cCondition + 
                          (cWorld + cCondition|workerid) + 
                          (experiment + cWorld + cCondition|story), 
                        nonhabitual.supp_data)
summary(supp_nonhabitual)

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(nonhabitual.supp_data, 
                    experiment=="exclamation" & world=="ordinary")))
summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(nonhabitual.supp_data, 
                    experiment=="exclamation" & world=="wonky")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(nonhabitual.supp_data, 
                    experiment=="ohyeah" & world=="ordinary")))
summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(nonhabitual.supp_data, 
                    experiment=="ohyeah" & world=="wonky")))

summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(nonhabitual.supp_data, 
                    experiment=="period" & world=="ordinary")))
summary(lmer(rating ~ cCondition + 
               (1|workerid) + 
               (cCondition|story), 
             filter(nonhabitual.supp_data, 
                    experiment=="period" & world=="wonky")))

#### Plot Habitual ####
supp_habitual.data_plot <- habitual.supp_data
supp_habitual.data_plot$world <- factor(supp_habitual.data_plot$world, 
                                        levels=c("ordinary","wonky"))
supp_habitual.data_plot$condition <- factor(supp_habitual.data_plot$condition,
                                            levels=c("before","after"))

dodge <- position_dodge(width = 0.85)

supp_habitual.data_plot$experiment <- 
  fct_recode(supp_habitual.data_plot$experiment, 
             "Exclamation"="exclamation",
             "Oh yeah, and..."="ohyeah",
             "Period"="period")

supp_habitual.summary <- supp_habitual.data_plot %>%
  group_by(experiment, world, condition) %>%
  summarise(val = mean(rating))

ggplot(supp_habitual.data_plot, aes(x = world, y = rating, fill = condition)) + 
  geom_violin(position = dodge) +
  facet_grid(. ~ experiment) + 
  geom_boxplot(width=.2, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = supp_habitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(supp_habitual.summary, 
                          world=="ordinary" & experiment!="Period"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "last", type = "closed", 
                          length = unit(0.1, "inches"))) +
  geom_line(data = filter(supp_habitual.summary, 
                          world=="ordinary" & experiment=="Period"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) +
  geom_line(data = filter(supp_habitual.summary, world=="wonky"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "last", type = "closed", 
                          length = unit(0.1, "inches"))) + 
  ylab("Activity Frequency Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.8, end = 0.2, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("ordinary","wonky\n('non-payer')"))

#### Plot Non-habitual ####
supp_nonhabitual.data_plot <- nonhabitual.supp_data
supp_nonhabitual.data_plot$world <- 
  factor(supp_nonhabitual.data_plot$world, levels=c("ordinary","wonky"))
supp_nonhabitual.data_plot$condition <- 
  factor(supp_nonhabitual.data_plot$condition, levels=c("before","after"))

supp_nonhabitual.data_plot$experiment <- 
  fct_recode(supp_nonhabitual.data_plot$experiment, 
                                       "Exclamation"="exclamation",
                                       "Oh yeah, and..."="ohyeah",
                                       "Period"="period")

dodge <- position_dodge(width = 0.85)

supp_nonhabitual.summary <- supp_nonhabitual.data_plot %>%
  group_by(experiment, world, condition) %>%
  summarise(val = mean(rating))

ggplot(supp_nonhabitual.data_plot, aes(x = world, y = rating, fill=condition)) + 
  geom_violin(position = dodge, width=0.33) +
  facet_grid(. ~ experiment) + 
  geom_boxplot(width=.2, aes(fill=NULL,group=interaction(world,condition)), 
               fill="white", position = dodge) +   
  geom_point(data = supp_nonhabitual.summary, aes(y = val), 
             position = dodge, shape=21) +
  geom_line(data = filter(supp_nonhabitual.summary, 
                          world=="ordinary" & experiment=="Period"), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "last", type = "closed", 
                          length = unit(0.1, "inches"))) +
  geom_line(data = filter(supp_nonhabitual.summary, 
                          world=="wonky" & 
                            experiment %in% c("Oh yeah, and...","Period")), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85), 
            arrow = arrow(angle = 25, ends = "last", type = "closed", 
                          length = unit(0.1, "inches"))) +
  geom_line(data = filter(supp_nonhabitual.summary, 
                          experiment=="Exclamation"| 
                          (experiment=="Oh yeah, and..." & world=="ordinary")), 
            aes(y = val, group = world), 
            position = position_dodge(width = 0.85)) +
  ylab("Activity Frequency Estimate") +
  xlab("Common Ground Context") +
  theme(legend.position="top",
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"),
        axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=12,face = "italic")) +
  scale_fill_grey(start = 0.8, end = 0.2, name="Beliefs:", 
                  breaks=c("before","after"), 
                  labels=c("pre-utterance","post-utterance")) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), 
                     labels = c("0 (Never)","25","50","75","100 (Always)")) + 
  scale_x_discrete(labels = c("ordinary","wonky\n('non-payer')"))
