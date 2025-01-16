genotype_codes <- read_csv("gardens/rawdata/sitecode2genotypenumber.csv")
genotype_codes %>% 
  mutate(site_code = Site.code,
         genotype = parse_number(genotypeID)) %>% 
  dplyr::select(site_code, genotype) -> genotype_codes

# VWC

png("~/Desktop/vwc_gxe_f.png", height = 4, width = 10, units = "in", res = 300)
a + b + plot_layout(guides = "collect", widths = c(2,1))
dev.off()
png("~/Desktop/vwc_gxe_s.png", height = 4, width = 10, units = "in", res = 300)
a_s + b_s + plot_layout(guides = "collect", widths = c(2,1))
dev.off()

png("~/Desktop/vwc_gxe_fit.png", height = 5, width = 6, units = "in", res = 300)
merge(vwc_g_pred %>% distinct(), vwc_g_preds %>% distinct()) %>% 
  mutate(fitness = survival * exp(ln_seed_count)) %>% 
  ggplot(aes(x = vwc_avg, y = log(fitness), group = genotype, color = PC1)) +
  geom_line(size = 1.2) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(fitness)",
       x = "VWC") +
  scale_color_distiller(palette = "RdYlBu", direction = 1)
dev.off()

png("~/Desktop/clim_fit.png")
merge(clim_g_pred %>% distinct(), clim_g_preds %>% distinct()) %>% 
  mutate(fitness = survival * exp(ln_seed_count)) %>% 
  ggplot(aes(x = clim_dist, y = fitness, group = genotype)) +
  geom_line(size = 1, alpha = 0.5) +
  theme_classic(base_size = 16) +
  labs(y = "fitness",
       x = "climate distance")

# Temp
png("~/Desktop/temp_gxe_f.png", height = 4, width = 10, units = "in", res = 300)
a_temp + b_temp + plot_layout(guides = "collect", widths = c(2,1)) 
dev.off()
png("~/Desktop/temp_gxe_s.png", height = 4, width = 10, units = "in", res = 300)
a_temp_s + b_temp_s + plot_layout(guides = "collect", widths = c(2,1)) 
dev.off()

temp_g_pred %>%
  distinct() %>% 
  arrange(genotype, temp_fecun) -> temp_g_pred

png("~/Desktop/temp_gxe_fit.png", height = 5, width = 6, units = "in", res = 300)
temp_g_preds %>%
  distinct() %>% 
  arrange(genotype, temp_surv) %>% 
  cbind(temp_fecun = temp_g_pred$temp_fecun,
        seed_count = exp(temp_g_pred$ln_seed_count)) %>% 
  mutate(fitness = survival * seed_count) %>% 
  ggplot(aes(x = temp_fecun, y = log(fitness), group = genotype, color = PC1)) +
  geom_line(size = 1.2) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(fitness)",
       x = "temperature (°C)") +
  scale_color_distiller(palette = "RdYlBu", direction = 1)
dev.off()

# Neighbors
png("~/Desktop/neighbors_gxe.png", height = 4, width = 10.5, units = "in", res = 300)
a_dens_s + b_dens_s + fitness_density + plot_layout(guides = "collect", widths = c(2,1,1))
dev.off()

nb_g_pred %>% 
  filter(round(new_neighbors) %in% c(9,56)) %>% 
  mutate(density = ifelse(round(new_neighbors) == 9, "low", "high")) %>% 
  merge(nb_g_preds) %>% 
  mutate(fitness = survival * exp(ln_seed_count)) %>% 
  ggplot(aes(x = density, y = log(fitness), group = genotype, color = PC1)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_classic(base_size = 16) +
  #theme(legend.position = "none") +
  labs(y = "ln(fitness)",
       x = "density") +
  scale_color_distiller(palette = "RdYlBu", direction = 1) -> fitness_density


tibble(fitness = (exp(sum_stat_climdist) * sum_stat_climdists)[2,],
       lower = (exp(sum_stat_climdist) * sum_stat_climdists)[1,],
       upper = (exp(sum_stat_climdist) * sum_stat_climdists)[3,],
       climate_distance = seq_climdist) %>% 
  ggplot(aes(x = climate_distance, y = log(fitness))) +
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper)), fill = "purple3", alpha = 0.2) +
  geom_line(size = 2, color = "purple3") +
  theme_classic(base_size = 16) +
  labs(y = "ln(fitness)",
       x = "climate distance") -> fit_clim_dist

tibble(value = draws$`beta[4]`) %>% 
  ggplot(aes(x = value)) +
  geom_density(fill = "purple3", alpha = 0.5, color = "purple3") +
  geom_vline(aes(xintercept = quantile(value, 0.95))) +
  theme_classic(base_size = 16) +
  annotate(geom = "text", x = 0.021, y = 25, label = "95th quantile") +
  labs(y = "density",
       x = expression(beta[4])) -> beta4

tibble(value = draws_s$`beta[4]`) %>% 
  ggplot(aes(x = value)) +
  geom_density(fill = "purple3", alpha = 0.5, color = "purple3") +
  geom_vline(aes(xintercept = quantile(value, 0.95))) +
  theme_classic(base_size = 16) +
  annotate(geom = "text", x = 0.031, y = 14, label = "95th quantile") +
  labs(y = "density",
       x = expression(gamma[4])) -> gamma4

png("~/Desktop/local_adapt.png", height = 7.4, width = 9.3, res = 300, units = "in")
(climdist_plot + beta4 + plot_layout(widths = c(2,1))) / (climdist_plot_s +gamma4+plot_layout(widths = c(2,1)))
dev.off()

png("~/Desktop/dens_int.png", height = 4, width = 6, units = "in", res = 300)
dens_vwc_plot_s
dev.off()

png("~/Desktop/climdist_gxe.png", height = 4, width = 9, units = "in", res = 300)
clim_dist_plot_s + clim_dist_plot_f
dev.off()
