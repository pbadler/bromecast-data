# Linear vs ordinal model for flowering timing

## Preliminaries ####

# Load libraries
library(tidyverse)
library(here)
library(patchwork)
library(lme4)
library(mgcv); library(gratia)
library(geomtextpath);
library(emmeans)

# Read in Sheep Station data

# Read in derived phenology data
phen <- read_csv(here("gardens/deriveddata/SS2022_growthphenology_with_harvest.csv"))
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

# Filter out plants that flowered and get first day that they flowered
phen_SS %>%
  filter(v %in% c("FG", "FB", "FP")) %>%
  group_by(plantID) %>%
  slice(which.min(jday)) -> phen_SS_plant

# Create ordinal categories
#sort(unique(phen_SS_plant$jday))

phen_SS_plant %>% 
  mutate(jday_ord = case_when(jday %in% 123:125 ~ 1,
                              jday %in% 138:140 ~ 2,
                              jday %in% 151:154 ~ 3,
                              jday %in% 165:166 ~ 4,
                              jday %in% 182:195 ~ 5)) -> phen_SS_plant

## Fit ordinal model ####

gam_ord <- gam(jday_ord ~ density*gravel +
                 # Random intercept for block
                 s(block_unique, bs = 're') + 
                 # Random intercept for plot nested within block
                 s(plot_unique, bs = 're') +
                 s(genotype, bs = 're'),
               family = ocat(R = 5), method = "REML",
               data = phen_SS_plant)

# Get summary of model
summary(gam_ord)

# Get summary of random effects
variance_comp(gam_ord) # Genotype >> block and plot

# Get ANOVA for fixed effects
anova(gam_ord) # Interaction between density and gravel

# Make plot of predictions
new_data <- expand.grid(density = unique(phen_SS_plant$density),
                        plot_unique = unique(phen_SS_plant$plot_unique),
                        genotype = unique(phen_SS_plant$genotype),
                        gravel = unique(phen_SS_plant $gravel)) %>% 
  separate(plot_unique, c("site", "block", "plot"), "_", remove = FALSE) %>% 
  mutate(block_unique = factor(paste(site, block, sep = "_"))) %>% 
  select(density, plot_unique, genotype, gravel, block_unique)

# Predict means across all possible combinations
predict(gam_ord, newdata = new_data, type = "response") -> ord_preds

# Group together with data
new_data %>% 
  mutate(prob1 = ord_preds[,1],
         prob2 = ord_preds[,2],
         prob3 = ord_preds[,3],
         prob4 = ord_preds[,4],
         prob5 = ord_preds[,5]) -> pred_data
# Average across combinations of interest
pred_data %>% 
  group_by(density, gravel) %>% 
  summarize(across(where(is.numeric), mean)) %>% 
  mutate(cum_prob_1 = prob1,
         cum_prob_12 = prob1 + prob2,
         cum_prob_123 = prob1 + prob2 + prob3,
         cum_prob_1234 = prob1 + prob2 + prob3 + prob4,
         cum_prob_12345 = 1) %>% 
  gather(key = cat, value = cum_prob, cum_prob_1:cum_prob_12345) %>% 
  mutate(jday = case_when(cat == "cum_prob_1" ~ 123,
                          cat == "cum_prob_12" ~ 138,
                          cat == "cum_prob_123" ~ 151,
                          cat == "cum_prob_1234" ~ 165,
                          cat == "cum_prob_12345" ~ 182),
         treatment = paste(density, gravel, sep = "_"),
         density = ifelse(density == "hi", "high", "low")) %>% 
  ggplot(aes(x = jday, y = cum_prob, group = treatment,
             color = gravel, shape = density, linetype = density)) +
  geom_textline(aes(label = paste(density, gravel, sep = "-")), vjust = 0.42, hjust = 0.62) +
  geom_point(size = 3) +
  ylab("cumulative prob. of flowering") +
  theme_classic(base_size = 14) +
  xlab("julian day") +
  scale_color_manual(values = c("black", "gray70")) +
  scale_x_continuous(breaks = c(123, 138, 151, 165, 182)) +
  theme(legend.position = "none") -> SS_timing

# Plot effects of genotype
confint(gam_ord, parm = "s(genotype)") %>% 
  ggplot(aes(x = reorder(genotype, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = genotype, xend = genotype)) +
  xlab("genotype") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> ord_genotype

# Plot effects of block
confint(gam_ord, parm = "s(block_unique)") %>% 
  ggplot(aes(x = reorder(block_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = block_unique, xend = block_unique)) +
  xlab("block")+
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> ord_block

# Plot effects of unique plot
confint(gam_ord, parm = "s(plot_unique)") %>% 
  ggplot(aes(x = reorder(plot_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = plot_unique, xend = plot_unique)) +
  xlab("plot") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> ord_plot

ord_genotype / ord_block / ord_plot -> ord_REs


## Fit linear model ####

gam_linear <- gam(jday ~ density*gravel*pc1 + density*gravel*pc2 + 
                 # Random intercept for block
                 s(block_unique, bs = 're') + 
                 # Random intercept for plot nested within block
                 s(plot_unique, bs = 're') +
                 s(genotype, bs = 're'),
               data = phen_SS_plant)

# Get summary of model
summary(gam_linear)

# Get summary of random effects
variance_comp(gam_linear) # Genotype >> block and plot

# Get ANOVA for fixed effects
anova(gam_linear) # Interaction between density and gravel, but not significant

# Check model assumptions
appraise(gam_linear)# Not horrible, not amazing

# Make fixed effects plot
summary(emmeans(gam_linear, ~density:gravel:pc1, non.nuis = c("density", "gravel", "pc2"),
                at = list(pc1 = seq(-6.8,6.8, 0.5)))) %>% 
  mutate(jday = emmean) %>% 
  ggplot(aes(x = pc1, y = jday, color = gravel)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill = "gray", alpha = 0.1) +
  geom_textline(aes(group = gravel, label = gravel)) +
  geom_point(data = phen_SS_plant, alpha = 0.2) +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("black", "orange")) +
  theme(legend.position = "none") +
  ylab("julian day") +
  facet_wrap(~density)


# See what is driving the interaction
pairs(emmeans(gam_linear, ~gravel|density, non.nuis = c("density", "gravel")))

# Plot effects of genotype
confint(gam_linear, parm = "s(genotype)") %>% 
  ggplot(aes(x = reorder(genotype, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = genotype, xend = genotype)) +
  xlab("genotype") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> linear_genotype

# Plot effects of block
confint(gam_linear, parm = "s(block_unique)") %>% 
  ggplot(aes(x = reorder(block_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = block_unique, xend = block_unique)) +
  xlab("block")+
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> linear_block

# Plot effects of unique plot
confint(gam_linear, parm = "s(plot_unique)") %>% 
  ggplot(aes(x = reorder(plot_unique, est), y = est)) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = plot_unique, xend = plot_unique)) +
  xlab("plot") +
  ylab("estimate") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))-> linear_plot

linear_genotype / linear_block / linear_plot -> linear_REs

## Bring together all plots for comparison ####
SS_timing + SS_timing_linear + ord_REs + linear_REs + 
  plot_layout(heights = c(1,2), nrow = 2) -> comp

png("~/Desktop/linear_ord.png", height = 9, width = 13, res = 300, units = "in")
comp
dev.off()