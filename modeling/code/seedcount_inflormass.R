library(tidyverse); library(ggpubr)

theme_set(theme_bw(base_size = 16))

cg <- read_csv("~/Git/Bromecast/gardens/deriveddata/cg_fullData_withFlags.csv")

# Create %notin% operator
`%notin%` <- Negate(`%in%`) 

cg %>% 
  filter(note_standard_phen %notin% c("seed_drop", "smut")) %>%
  filter(note_standard_harvest %notin% c("seed_drop", "smut")) %>%
  filter(year == 2022 & seed_count_total > 0 & inflor_mass > 0) %>% 
  ggplot(aes(x = inflor_mass, y = seed_count_total)) + 
  geom_point() 

cg %>% 
  filter(year == 2022 & seed_count_total > 0 & inflor_mass > 0) %>%
  filter(note_standard_phen %notin% c("seed_drop", "smut")) %>%
  filter(note_standard_harvest %notin% c("seed_drop", "smut")) -> cg_clean

# Add logs
cg_clean$ln_seed_count <- log(cg_clean$seed_count_total)
cg_clean$ln_inflor_mass <- log(cg_clean$inflor_mass)
  
# Model with no treatments
null_mod <- lm(ln_seed_count ~ ln_inflor_mass, data = cg_clean)
summary(null_mod) # R2 = 0.9403

cg_clean %>% 
  ggplot(aes(x = ln_inflor_mass, y = ln_seed_count)) +
  geom_point() +
  geom_smooth(method = "lm")

# Create unique ID for treatment
cg_clean$trt <- paste(cg_clean$albedo, cg_clean$density, sep = "_")

trt_mod <- lm(ln_seed_count ~ ln_inflor_mass * trt, data = cg_clean)
summary(trt_mod) # R2 = 0.9446

png("~/Git/Bromecast-cg_demography/figs/FigS1_seedcount_informass.png", height = 5.3, width = 7, res = 300, units = "in")
cg_clean %>% 
  ggplot(aes(x = ln_inflor_mass, y = ln_seed_count, color = albedo,
             shape = density, linetype = density)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", aes(linetype = density), linewidth = 2) +
  xlab("ln(inflorescence mass)") +
  ylab("ln(seed count)") +
  labs(color = "gravel") +
  scale_shape_manual(values = c(16, 21)) +
  scale_color_manual(values = c("black", "goldenrod")) +
  guides(shape = guide_legend(override.aes = list(size = 6,
                                                  fill = NA,
                                                  color = "black",
                                                  linewidth = 0.8)),
         color = guide_legend(override.aes = list(size = 6,
                                                  fill = NA)))
dev.off()

# Get regression coefficients
coef(null_mod)
# (Intercept) ln_inflor_mass 
# 5.9266903      0.9227253
