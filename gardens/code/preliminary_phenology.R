## Preliminaries ####

# Load libraries
library(tidyverse)
library(here)
library(patchwork)
library(lme4)
library(mgcv); library(gratia)
library(geomtextpath);
library(dismo)

# Source code that creates data frame with gps points of each genotype
source(here("gardens/code/create_local_adapt_covariate.R"))

## Read in all Boise data ####
# Read in derived phenology data
phen <- read_csv(here("gardens/deriveddata/Boise2022_growthphenology_with_harvest.csv"))
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
  dplyr::select(-cum_plot, -garden) -> gardens_sub

# Merge together datasets
phen_id_garden <- merge(phen_id, gardens_sub)

# Bring in flag data
flags <- read_csv(here("gardens/deriveddata/Boise2022_flags.csv"))

# Merge together datasets
phen <- merge(phen_id_garden, flags)
phen <- merge(phen, genotype_PCclimate)

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
phen <- read_csv(here("gardens/deriveddata/SS2022_growthphenology_with_harvest.csv"))
# Read in plant ID info
ids <- read_csv(here("gardens/deriveddata/SS2022_plantID.csv"))
# Read in flagging data
flags <- read_csv(here("gardens/deriveddata/SS2022_flags.csv"))

# Merge together datasets
phen_id <- merge(phen, ids)

# Rename 'garden' column and remove cum_plot column
gardens %>% 
  mutate(site = garden) %>% 
  dplyr::select(-cum_plot, -garden) -> gardens_sub

# Merge together datasets
phen_id_garden <- merge(phen_id, gardens_sub)

# Bring in flag data
flags <- read_csv(here("gardens/deriveddata/SS2022_flags.csv"))

# Merge together datasets
phen <- merge(phen_id_garden, flags)
phen <- merge(phen, genotype_PCclimate)

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

## Read in Cheyenne data ####
phen <- read_csv(here("gardens/deriveddata/CH2022_growthphenology_by_plantID.csv"))
# Read in plant ID info
ids <- read_csv(here("gardens/deriveddata/CH2022_plantID.csv"))

# Merge together datasets
phen_id <- merge(phen, ids)

phen <- merge(phen_id, genotype_PCclimate)

# Additional organizing to accommodate CH data (before data cleaning)
phen %>% 
  mutate(site = "CH",
         missing_plant = NA,
         emergence_date = NA,
         death_date = NA,
         resurrection_date = NA,
         pheno_regress = NA,
         growth_regress_mm = NA,
         herbivory_date = NA,
         frostheave_date = NA,
         bad_position = NA,
         other = NA)  %>% 
  dplyr::select(names(phen_Boise)) -> phen

# Set appropriate factors for variables
phen %>% 
  mutate(block = as.factor(block),
         plot = as.factor(plot),
         growout = as.factor(growout),
         density = as.factor(density),
         gravel = as.factor(gravel),
         genotype = as.factor(genotype)) %>% 
  mutate(plot_unique = as.factor(paste(site, block, plot, sep = "_")),
         block_unique = as.factor(paste(site, block, sep = "_")))-> phen_CH


## Bring together all data sets ####
# Remove tillers for SS
phen_SS %>% 
  dplyr::select(-tillers) -> phen_SS

phen <- rbind(phen_Boise, phen_SS, phen_CH)

## Survival model ####

# Assessing survival rate
phen %>% 
  group_by(plantID) %>% 
  slice(which.max(jday)) %>% 
  mutate(survived = ifelse(live == "Y", 1, 0)) %>%
  dplyr::select(plantID, site, block, block_unique, plot, plot_unique, frostheave_date, density, gravel, genotype, survived) %>% 
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
m3_gam <- gam(flowered ~ density*gravel +
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
  mutate(ord = case_when(jday %in% 108 ~ 1,
                         jday %in% 123:125 ~ 2,
                         jday %in% 130:131 ~ 3,
                         jday %in% 138:140 ~ 4,
                         jday %in% 144:147 ~ 5,
                         jday %in% 151:154 ~ 6,
                         jday %in% 165:166 ~ 7,
                         jday %in% 182 ~ 8,
                         jday %in% 186:195 ~ 9)) -> phen_sub1_plant2


m2_gam <- gam(ord ~ density*gravel*site + pc1 + pc2 +
                # Random intercept for block
                s(block_unique, bs = 're') + 
                # Random intercept for plot nested within block
                s(plot_unique, bs = 're') +
                s(genotype, bs = 're'),
              # Random slope for genotype x density:gravel?
              #s(genotype, by = trt, bs = 're'),
              family = ocat(R = 9),
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
confint(m2_gam, parm = "s(plot_unique)") %>% 
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
         site = as.factor(site),
         pc1 = mean(phen_sub1_plant2$pc1),
         pc2 = mean(phen_sub1_plant2$pc2)) %>% 
  dplyr::select(density, plot_unique, genotype, gravel, block_unique, site, pc1, pc2)

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
         prob8 = m2_preds[,8],
         prob9 = m2_preds[,9]) -> pred_data
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
         cum_prob_12345678 = prob1 + prob2 + prob3 + prob4 + prob5 + prob6 + prob7 + prob8,
         cum_prob_123456789 = 1) %>% 
  gather(key = cat, value = cum_prob, cum_prob_1:cum_prob_123456789) %>% 
  mutate(jday = case_when(cat == "cum_prob_1" ~ 108,
                          cat == "cum_prob_12" ~ 123,
                          cat == "cum_prob_123" ~ 130,
                          cat == "cum_prob_1234" ~ 138,
                          cat == "cum_prob_12345" ~ 144,
                          cat == "cum_prob_123456" ~ 151,
                          cat == "cum_prob_1234567" ~ 165,
                          cat == "cum_prob_12345678" ~ 182,
                          cat == "cum_prob_123456789" ~ 186),
         treatment = paste(density, gravel, sep = "_"),
         density = ifelse(density == "hi", "high", "low")) %>% 
  ggplot(aes(x = jday, y = cum_prob, group = treatment, color = gravel, shape = density, linetype = density)) +
  geom_textline(aes(label = paste(density, gravel, sep = "-")), hjust = 0.42) +
  geom_point(size = 3) +
  ylab("cumulative probability of flowering by date") +
  theme_classic(base_size = 14) +
  xlab("julian day") +
  scale_color_manual(values = c("black", "gray70")) +
  scale_x_continuous(breaks = c(108,123,130,138,144,151,165,182,186)) +
  theme(legend.position = "none") +
  facet_wrap(~site) -> fixed_timing

png("~/Desktop/fix_timing.png", height = 4, width = 11.5, res = 300, units = "in")
fixed_timing
dev.off()

anova(m2_gam)

# Also just try with linear model

phen_sub1_plant2$site <- as.factor(phen_sub1_plant2$site)
phen_sub1_plant2$pc2 <- scale(phen_sub1_plant2$pc2)[,1]
phen_sub1_plant2$pc1 <- scale(phen_sub1_plant2$pc1)[,1]

gam_linear <- gam(jday ~ density*gravel*pc1 + density*gravel*pc2 + site*pc1 + site*pc2 + 
                    # Random intercept for block
                    s(block_unique, bs = 're') + 
                    # Random intercept for plot nested within block
                    s(plot_unique, bs = 're') +
                    s(genotype, bs = 're') +
                    s(genotype, density, bs = 're') +
                    s(genotype, site, bs = 're') +
                    s(genotype, gravel, bs = "re"), method = "REML", 
                  data = phen_sub1_plant2)


lmer_linear <- lmer(log(jday) ~ density*gravel*pc1 + density*gravel*pc2 + site*pc1 + site*pc2 + 
                      # Random intercept for block
                      #s(block_unique, bs = 're') + 
                      # Random intercept for plot nested within block
                      (1|plot_unique) + (site+density*gravel|genotype),
                    data = phen_sub1_plant2)

phen_sub1_plant2 %>% filter(genotype == 70)

library(lmerTest)
anova(lmer_linear)
anova(gam_linear)
appraise(gam_linear)

# Plot effects of genotype
confint(gam_linear, parm = "s(genotype)") %>% 
  ggplot(aes(x = reorder(genotype, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = genotype, xend = genotype)) +
  xlab("genotype") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) -> a

# Plot effects of unique plot
confint(gam_linear, parm = "s(plot_unique)") %>% 
  ggplot(aes(x = reorder(plot_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = plot_unique, xend = plot_unique)) +
  xlab("plot") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) -> b

confint(gam_linear, parm = "s(block_unique)") %>% 
  ggplot(aes(x = reorder(block_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = block_unique, xend = block_unique)) +
  xlab("block") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) -> c

png("~/Desktop/re_pres.png", height = 8, res = 300, width = 11, units = "in")
c/b/a
dev.off()
# Make fixed effects plot of density x gravel x pc1
summary(emmeans(gam_linear, ~density:gravel:pc1, non.nuis = c("density", "gravel", "pc1"),
                at = list(pc1 = seq(-2.5,2.5, 0.5)))) %>% 
  mutate(jday = emmean) %>% 
  ggplot(aes(x = pc1, y = jday, color = density)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill = "gray", alpha = 0.1) +
  geom_textline(aes(group = density, label = density), linewidth = 2) +
  geom_point(data = phen_sub1_plant2, alpha = 0.1) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("darkblue", "dodgerblue")) +
  theme(legend.position = "none") +
  ylab("julian day") +
  facet_wrap(~gravel) +
  xlab("PC 1") -> int3

png("~/Desktop/int3.png", height = 6.5, width = 8.5, res = 300, units = "in")
int3
dev.off()

# Make fixed effects plot of pc2 x site
summary(emmeans(gam_linear, ~site:pc2, non.nuis = c("site", "pc2"),
                at = list(pc2 = seq(-2.3,1.9, 0.5)))) %>% 
  mutate(jday = emmean) %>% 
  ggplot(aes(x = pc2, y = jday, color = site)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill = "gray", alpha = 0.1) +
  geom_textline(aes(group = site, label = site), linewidth = 2) +
  geom_point(data = phen_sub1_plant2, alpha = 0.1) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("#d95f02", "#7570b3")) +
  theme(legend.position = "none") +
  ylab("julian day") +
  xlab("PC 2") -> sitepc2

png("~/Desktop/sitepc2.png", height = 5.25, width = 5, res = 300, units = 'in')
sitepc2
dev.off()
# Make fixed effects plot of pc1 x gravel
summary(emmeans(gam_linear, ~site:pc1, non.nuis = c("gravel", "pc1"),
                at = list(pc1 = seq(-2.5,2.5, 0.5)))) %>% 
  mutate(jday = exp(emmean)) %>% 
  ggplot(aes(x = pc1, y = jday, color = gravel)) +
  geom_ribbon(aes(ymin=exp(lower.CL), ymax=exp(upper.CL)), fill = "gray", alpha = 0.1) +
  geom_textline(aes(group = gravel, label = gravel), linewidth = 2) +
  geom_point(data = phen_sub1_plant2, alpha = 0.1) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  theme(legend.position = "none") +
  ylab("julian day") 


confint(gam_linear, parm = "s(genotype)") %>% 
  dplyr::select(genotype, mean = est) -> intercepts

confint(gam_linear, parm = "s(genotype,site)") %>% 
  dplyr::select(genotype, site, effect = est, lower, upper) -> slopes

merge(intercepts, slopes)

slopes %>% 
  dplyr::select(genotype,site, effect) %>% 
  spread(key = site, value = effect) %>% 
  mutate(color_code = ifelse(SS > WI, "SS", "WI")) -> color_codes

merge(slopes, color_codes) -> slopes_color_codes
merge(slopes_color_codes, intercepts) %>% 
  mutate(calc_intercept = mean + effect) -> plot_re

plot_re %>% 
  mutate(site = ifelse(site == "SS", "Sheep Station", "Wildcat (Boise Low)")) %>% 
  mutate(color_code = ifelse(color_code == "SS", "SS > WI", "WI > SS")) %>% 
  #filter(genotype %in% as.factor(c(6,13,14,81,79,2))) %>% 
  ggplot(aes(x = site, y = calc_intercept, group = genotype, color = color_code, position_dodge())) +
  geom_point(position = position_dodge(0.1)) + geom_line(position = position_dodge(0.1)) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0,position = position_dodge(0.2)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  ylab("genotype effect on flowering time") +
  scale_color_manual(values = c("#d95f02", "#7570b3")) +
  labs(color = "") -> g_by_e

png("~/Desktop/ge.png", height = 7, width = 5, res = 300, units = "in")
g_by_e
dev.off()
