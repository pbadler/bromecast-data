## Preliminaries ####

# Load libraries
library(tidyverse)
library(here)
library(patchwork)
library(lme4)
library(mgcv); library(gratia)
library(geomtextpath)

## Read in all Boise data ####
# Read in derived phenology data
phen <- read_csv(here("gardens/deriveddata/Boise2022_growthphenology_by_plantID.csv"))
# Read in plant ID info
ids <- read_csv(here("gardens/deriveddata/Boise2022_plantID.csv"))
# Read in garden treatment info
gardens <-read_csv(here("gardens/rawdata/garden_treatments.csv"))
# Read in flagging data
flags <- read_csv(here("gardens/deriveddata/Boise2022_flags.csv"))

# Merge together datasets
phen_id <- merge(phen, ids)

# Rename 'garden' column and remove cum_plot column
gardens %>% 
  mutate(site = garden) %>% 
  select(-cum_plot, -garden) -> gardens_sub

# Merge together datasets
phen_id_garden <- merge(phen_id, gardens_sub)

# Bring in flag data
flags <- read_csv(here("gardens/deriveddata/Boise2022_flags.csv"))

# Merge together datasets
phen <- merge(phen_id_garden, flags)

# Set appropriate factors for variables
phen %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         growout = as.factor(growout),
         density = as.factor(density),
         gravel = as.factor(gravel),
         genotype = as.factor(genotype)) %>% 
  mutate(plot_unique = as.factor(paste(site, block, plot, sep = "_")),
         block_unique = as.factor(paste(site, block, sep = "_")))-> phen_Boise

## Read in Sheep Station data ####

# Read in derived phenology data
phen <- read_csv(here("gardens/deriveddata/SS2022_growthphenology_by_plantID.csv"))
# Read in plant ID info
ids <- read_csv(here("gardens/deriveddata/SS2022_plantID.csv"))
# Read in garden treatment info
gardens <-read_csv(here("gardens/rawdata/garden_treatments.csv"))
# Read in flagging data
flags <- read_csv(here("gardens/deriveddata/SS2022_flags.csv"))

# Merge together datasets
phen_id <- merge(phen, ids)

# Rename 'garden' column and remove cum_plot column
gardens %>% 
  mutate(site = garden) %>% 
  select(-cum_plot, -garden) -> gardens_sub

# Merge together datasets
phen_id_garden <- merge(phen_id, gardens_sub)

# Bring in flag data
flags <- read_csv(here("gardens/deriveddata/SS2022_flags.csv"))

# Merge together datasets
phen <- merge(phen_id_garden, flags)

# Set appropriate factors for variables
phen %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         growout = as.factor(growout),
         density = as.factor(density),
         gravel = as.factor(gravel),
         genotype = as.factor(genotype)) %>% 
  mutate(plot_unique = as.factor(paste(site, block, plot, sep = "_")),
         block_unique = as.factor(paste(site, block, sep = "_")))-> phen_SS

## Bring together Sheep Station and Boise data sets ####
# Remove tillers for SS
phen_SS %>% 
  select(-tillers) -> phen_SS
phen <- rbind(phen_Boise, phen_SS)


phen %>% 
  group_by(frost_heave) %>% 
  summarize(n = n())

## Survival model ####

# Assessing survival rate
phen %>% 
  group_by(plantID) %>% 
  slice(which.max(jday)) %>% 
  mutate(survived = ifelse(live == "Y", 1, 0)) %>%
  select(plantID, site, block, block_unique, plot, plot_unique, frostheave_date, density, gravel, genotype, survived) %>% 
  distinct() -> phen_sub1_plant

sum(phen_sub1_plant$survived)

# Fit survival model
m1_gam <- gam(survived ~ density*gravel*site +
                # Random intercept for block
                s(block_unique, bs = 're') + 
                # Random intercept for plot nested within block
                s(plot_unique, bs = 're') +
                # Random intercept for genotype
                s(genotype, bs = 're'),
              data = phen_sub1_plant, method = 'REML', family = "binomial")

# Get confidence intervals around variance components
variance_comp(m1_gam)

# Plot effects of genotype
confint(m1_gam, parm = "s(genotype)") %>% 
  ggplot(aes(x = reorder(genotype, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = genotype, xend = genotype)) +
  xlab("genotype") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> a

# Plot effects of block
confint(m1_gam, parm = "s(block_unique)") %>% 
  ggplot(aes(x = reorder(block_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = block_unique, xend = block_unique)) +
  xlab("block")+
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> b

# Plot effects of unique plot
confint(m1_gam, parm = "s(plot_unique)") %>% 
  ggplot(aes(x = reorder(plot_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = plot_unique, xend = plot_unique)) +
  xlab("plot") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> c

png("~/Desktop/re_survival.png", height = 7, width = 11.5, res = 300, units = "in")
a / b / c
dev.off()

# Get predicted means of density:gravel treatments
dg_means <- summary(emmeans::emmeans(m1_gam, ~density*gravel*site, non.nuis = c("density", "gravel", "site"), type = "response"))

# Plot density:gravel effects
dg_means %>% 
  mutate(survived = prob) %>% 
  ggplot(aes(x = density, y = survived, color = gravel)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0,
               position = position_dodge(width = 0.2)) +
  geom_jitter(data = phen_sub1_plant, aes(x = density, y = survived),
              height = 0.05, width = 0.3, alpha = 0.2, shape = 1) +
  facet_wrap(~site) +
  theme_bw(base_size = 14) +
  ylab("probability of survival") +
  scale_color_manual(values = c("black", "gray70")) -> fixed_survival

png("~/Desktop/fix_survival.png", height = 5, width = 7, res = 300, units = "in")
fixed_survival
dev.off()

anova(m1_gam)

## Probability of flowering model ####

# Create column for if flowered during the course of the experiment. First get
# plants that flowered at least once during the experiment but also survived
phen_sub1_plant %>%
  filter(survived == 1) %>% 
  pull(plantID) -> survived_plants

phen %>% 
  filter(v %in% c("FG", "FB", "FP") & plantID %in% survived_plants) %>% 
  group_by(plantID) %>%
  slice(which.min(jday)) %>% 
  ungroup() %>% 
  mutate(flowered = 1) -> flowered_plants

# Subset to get plants that did not flower
`%notin%` <- Negate(`%in%`) # Not in operator

phen %>% 
  filter(plantID %in% survived_plants) %>% 
  filter(plantID %notin% flowered_plants$plantID) %>%
  group_by(plantID) %>% 
  slice(which.min(jday)) %>% 
  ungroup() %>% 
  mutate(flowered = 0) -> unflowered_plants

# Join together flowered and unflowered data sets
phen_sub1_plant3 <- rbind(flowered_plants, unflowered_plants)

# Fit flowering model
m3_gam <- gam(flowered ~ density*gravel*site +
                # Random intercept for block
                s(block_unique, bs = 're') + 
                # Random intercept for plot nested within block
                s(plot_unique, bs = 're') +
                # Random intercept for genotype
                s(genotype, bs = 're'),
              data = phen_sub1_plant3, method = 'REML', family = "binomial")

# Get confidence intervals around variance components
variance_comp(m3_gam)

# Plot effects of genotype
confint(m3_gam, parm = "s(genotype)") %>% 
  ggplot(aes(x = reorder(genotype, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = genotype, xend = genotype)) +
  xlab("genotype") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> h

#Plot effects of block
confint(m3_gam, parm = "s(block_unique)") %>%
  ggplot(aes(x = reorder(block_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = block_unique, xend = block_unique)) +
  xlab("block")+
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14)-> i

# Plot effects of unique plot
confint(m3_gam, parm = "s(plot_unique)") %>% 
  ggplot(aes(x = reorder(plot_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = plot_unique, xend = plot_unique)) +
  xlab("plot") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> j

png("~/Desktop/re_flower.png", height = 7, width = 11.5, res = 300, units = "in")
h / i / j
dev.off()

# Get predicted means of density:gravel treatments
dg_means_flower <- summary(emmeans::emmeans(m3_gam, ~density*gravel*site, non.nuis = c("density", "gravel", "site"), type = "response"))

# Plot density:gravel effects
dg_means_flower %>% 
  mutate(flowered = prob) %>% 
  ggplot(aes(x = density, y = flowered, color = gravel)) +
  geom_point() +
  geom_segment(aes(x = density, xend = density, y = lower.CL, yend = upper.CL)) +
  #geom_jitter(data = phen_sub1_plant3, aes(x = density, y = flowered),
  #height = 0.05, width = 0.3, alpha = 0.1, shape = 1) +
  facet_wrap(~site) +
  theme_bw(base_size = 14) +
  ylab("probability of flowering (if survived)") +
  scale_color_manual(values = c("black", "gray70")) -> fixed_flowering

png("~/Desktop/fix_flower.png", height = 3, width = 9, res = 300, units = "in")
fixed_flowering
dev.off()

## Assessing time to flowering ####

# Calculate when the earliest flowering day is for each plant. This procedure
# removes plants that do not flower of the course of the experiment.
phen %>%
  filter(v %in% c("FG", "FB", "FP")) %>%
  group_by(plantID) %>%
  slice(which.min(jday)) -> phen_sub1_plant2

# Get summary statistics
# phen_sub1_plant2 %>% 
#   group_by(jday) %>% 
#   summarize(n = n())

# Try fitting an ordered categorical model for the response variable (because sampled every 2 weeks)
phen_sub1_plant2 %>% 
  mutate(ord = case_when(jday %in% 108:109 ~ 1,
                         jday %in% 115:116 ~ 2,
                         jday %in% 123:125 ~ 3,
                         jday %in% 130:132 ~ 4,
                         jday %in% 138:140 ~ 5,
                         jday %in% 151:154 ~ 6,
                         jday %in% 165:166 ~ 7,
                         jday %in% 179:181 ~ 8)) -> phen_sub1_plant2

m2_gam <- gam(ord ~ density*gravel*site +
                # Random intercept for block
                s(block_unique, bs = 're') + 
                # Random intercept for plot nested within block
                s(plot_unique, bs = 're') +
                s(genotype, bs = 're'),
              # Random slope for genotype x density:gravel?
              #s(genotype, by = trt, bs = 're'),
              family = ocat(R = 8),
              data = phen_sub1_plant2)

summary(m2_gam)

# Plot effects of genotype
confint(m2_gam, parm = "s(genotype)") %>% 
  ggplot(aes(x = reorder(genotype, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = genotype, xend = genotype)) +
  xlab("genotype") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> d

# Plot effects of block
confint(m2_gam, parm = "s(block_unique)") %>% 
  ggplot(aes(x = reorder(block_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = block_unique, xend = block_unique)) +
  xlab("block")+
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14)-> e

# Plot effects of unique plot
confint(m1_gam, parm = "s(plot_unique)") %>% 
  ggplot(aes(x = reorder(plot_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = plot_unique, xend = plot_unique)) +
  xlab("plot") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> f

png("~/Desktop/re_timing.png", height = 7, width = 11.5, res = 300, units = "in")
d/e/f
dev.off()

new_data <- expand.grid(density = unique(phen_sub1_plant2$density),
                        plot_unique = unique(phen_sub1_plant2$plot_unique),
                        genotype = unique(phen_sub1_plant2$genotype),
                        gravel = unique(phen_sub1_plant2$gravel)) %>% 
  separate(plot_unique, c("site", "block", "plot"), "_", remove = FALSE) %>% 
  mutate(block_unique = factor(paste(site, block, sep = "_")),
         site = as.factor(site)) %>% 
  select(density, plot_unique, genotype, gravel, block_unique, site)

# Predict means across all possible combinations
predict(m2_gam, newdata = new_data, type = "response") -> m2_preds
# Group together with data
new_data %>% 
  mutate(prob1 = m2_preds[,1],
         prob2 = m2_preds[,2],
         prob3 = m2_preds[,3],
         prob4 = m2_preds[,4],
         prob5 = m2_preds[,5],
         prob6 = m2_preds[,6],
         prob7 = m2_preds[,7],
         prob8 = m2_preds[,8]) -> pred_data
# Average across combinations of interest
pred_data %>% 
  group_by(density, gravel, site) %>% 
  summarize(across(where(is.numeric), mean)) %>% 
  mutate(cum_prob_1 = prob1,
         cum_prob_12 = prob1 + prob2,
         cum_prob_123 = prob1 + prob2 + prob3,
         cum_prob_1234 = prob1 + prob2 + prob3 + prob4,
         cum_prob_12345 = prob1 + prob2 + prob3 + prob4 + prob5,
         cum_prob_123456 = prob1 + prob2 + prob3 + prob4 + prob5 + prob6,
         cum_prob_1234567 = prob1 + prob2 + prob3 + prob4 + prob5 + prob6 + prob7,
         cum_prob_12345678 = 1) %>% 
  gather(key = cat, value = cum_prob, cum_prob_1:cum_prob_12345678) %>% 
  mutate(jday = case_when(cat == "cum_prob_1" ~ 108,
                          cat == "cum_prob_12" ~ 115,
                          cat == "cum_prob_123" ~ 124,
                          cat == "cum_prob_1234" ~ 131,
                          cat == "cum_prob_12345" ~ 139,
                          cat == "cum_prob_123456" ~ 152,
                          cat == "cum_prob_1234567" ~ 165,
                          cat == "cum_prob_12345678" ~ 180),
         treatment = paste(density, gravel, sep = "_"),
         density = ifelse(density == "hi", "high", "low")) %>% 
  ggplot(aes(x = jday, y = cum_prob, group = treatment, color = gravel, shape = density, linetype = density)) +
  geom_textline(aes(label = paste(density, gravel, sep = "-")), hjust = 0.42) +
  geom_point(size = 3) +
  ylab("cumulative probability of flowering by date") +
  theme_classic(base_size = 14) +
  xlab("julian day") +
  scale_color_manual(values = c("black", "gray70")) +
  scale_x_continuous(breaks = c(108,115,124,131,139,152,165,180)) +
  theme(legend.position = "none") +
  facet_wrap(~site) -> fixed_timing

png("~/Desktop/fix_timing.png", height = 4, width = 11.5, res = 300, units = "in")
fixed_timing
dev.off()

anova(m2_gam)
