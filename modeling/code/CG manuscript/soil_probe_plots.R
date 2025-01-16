png("~/Desktop/temp_line.png", height = 6, width = 12, res = 300, units = "in")

for_model_sample %>% 
  dplyr::select(site_year_gravel,temp_fecun, vwc_avg) %>%
  distinct() %>% 
  mutate(site = substr(site_year_gravel, 1, 2),
         year = substr(site_year_gravel, 6, 7),
         site_year = paste(site, year, sep = "\n"),
         color = substr(site_year_gravel, 9, 13)) %>% 
  ggplot(aes(x = temp_fecun, y = 0, color = color)) +
  annotate("segment",x=3,xend=15, y=0, yend=0, size=2) +
  annotate("segment",x=3,xend=3, y=-0.1,yend=0.1, size=2) +
  annotate("segment",x=15,xend=15, y=-0.1,yend=0.1, size=2) +
  geom_point(size = 15) +
  geom_point(size = 15, shape = 1, color = "brown1", stroke = 1.5, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = site_year), col="black", nudge_y = 0.2, fontface = "bold") +
  scale_x_continuous(limits = c(3,15)) +
  scale_y_continuous(limits = c(-1,1)) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("black", "white"))
dev.off()

png("~/Desktop/vwc_line.png", height = 6, width = 12, res = 300, units = "in")
for_model_sample %>% 
  dplyr::select(site_year_gravel,temp_fecun, vwc_avg) %>%
  distinct() %>% 
  mutate(site = substr(site_year_gravel, 1, 2),
         year = substr(site_year_gravel, 6, 7),
         site_year = paste(site, year, sep = "\n"),
         color = substr(site_year_gravel, 9, 13)) %>% 
  ggplot(aes(x = vwc_avg, y = 0, color = color)) +
  annotate("segment",x=0.10,xend=0.25, y=0, yend=0, size=2) +
  annotate("segment",x=0.10,xend=0.10, y=-0.1,yend=0.1, size=2) +
  annotate("segment",x=0.25,xend=0.25, y=-0.1,yend=0.1, size=2) +
  geom_point(size = 15) +
  geom_point(size = 15, shape = 1, color = "purple3", stroke = 1.5, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = site_year), col="black", nudge_y = 0.2, fontface = "bold") +
  scale_x_continuous(limits = c(0.10,0.25)) +
  scale_y_continuous(limits = c(-1,1)) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("black", "white"))
dev.off()
