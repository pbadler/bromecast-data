reps <- 1000

text_high <- grid::textGrob("current env\nhotter & drier\nthan source env",
                            gp=gpar(fontsize=9, col = "brown1", fontface = "bold.italic", lineheight = 0.8))
text_low <- grid::textGrob("current env\ncooler & wetter\nthan source env",
                           gp=gpar(fontsize=9, col = "dodgerblue", fontface = "bold.italic", lineheight = 0.8))

x_seq <- seq(min(for_model_sample$clim_dist_sc), max(for_model_sample$clim_dist_sc), length.out = reps)

out <- matrix(NA, nrow = reps, ncol = 3000)

for(i in 1:reps){
  out[i,] <- draws$alpha + draws$`beta[4]`*x_seq[i] + draws$`beta[5]`*x_seq[i]^2 +
    + draws$`beta[6]`*x_seq[i]^3
}

tibble(fecundity = (rowMeans(out)),
       x = x_seq,
       lower = (apply(out, 1, quantile, 0.025)),
       upper = (apply(out, 1, quantile, 0.975))) %>% 
  ggplot(aes(x = x, y = fecundity)) +
  geom_line(linewidth = 1.5, aes(color = x > 0)) + 
  annotate(geom = "point", x = x_seq[which(rowMeans(out) == max(rowMeans(out)))],
           y = rowMeans(out)[which(rowMeans(out) == max(rowMeans(out)))], size = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.3) +
  theme_classic(base_size = 14) +
  labs(x = "",
       y = "ln(fecundity)") +
  geom_vline(aes(xintercept = x_seq[which(rowMeans(out) == max(rowMeans(out)))]), linetype = "dashed") +
  #annotation_custom(text_high,xmin=1.7,xmax=1.7,ymin=2.83,ymax=2.83) + 
  #annotation_custom(text_low,xmin=-2.2,xmax=-2.2,ymin=2.83,ymax=2.83)+
  coord_cartesian(clip="off") +
  scale_color_manual(values = c("dodgerblue", "brown1")) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt")) -> fecundity_plot

out_s <- matrix(NA, nrow = reps, ncol = 1500)

for(i in 1:reps){
  out_s[i,] <- draws_s$alpha + draws_s$`beta[4]`*x_seq[i] + draws_s$`beta[5]`*x_seq[i]^2 +
    + draws_s$`beta[6]`*x_seq[i]^3
}

tibble(survival = plogis(rowMeans(out_s)),
       x = x_seq,
       lower = plogis(apply(out_s, 1, quantile, 0.025)),
       upper = plogis(apply(out_s, 1, quantile, 0.975))) %>% 
  ggplot(aes(x = x, y = survival)) +
  geom_line(linewidth = 1.5, aes(color = x > 0)) + 
  annotate(geom = "point", x = x_seq[which(rowMeans(out_s) == max(rowMeans(out_s)))],
           y = plogis(rowMeans(out_s)[which(rowMeans(out_s) == max(rowMeans(out_s)))]), size = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.3) +
  theme_classic(base_size = 14) +
  labs(x = "",
       y = "P(survival)") +
  geom_vline(aes(xintercept = x_seq[which(rowMeans(out_s) == max(rowMeans(out_s)))]), linetype = "dashed") +
  #annotation_custom(text_high,xmin=1.7,xmax=1.7,ymin=0.06,ymax=0.06) + 
  #annotation_custom(text_low,xmin=-2.2,xmax=-2.2,ymin=0.06,ymax=0.06)+
  coord_cartesian(clip="off") +
  scale_color_manual(values = c("dodgerblue", "brown1")) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "pt")) -> survival_plot

out_sub <- out[,1:1500]

fitness <- rowMeans(log(exp(out_sub) * plogis(out_s)))
fitness_low <- apply(log(exp(out_sub) * plogis(out_s)), 1, quantile, 0.025)
fitness_high <- apply(log(exp(out_sub) * plogis(out_s)), 1, quantile, 0.975)

tibble(fitness = fitness,
       x = x_seq,
       lower = fitness_low,
       upper = fitness_high) %>% 
  ggplot(aes(x = x, y = fitness)) +
  geom_line(linewidth = 1.5, aes(color = x > 0)) + 
  annotate(geom = "point", x = x_seq[which(fitness == max(fitness))],
           y = fitness[which(fitness == max(fitness))], size = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.3) +
  theme_classic(base_size = 14) +
  labs(x = expression(atop("climate difference", paste("(",PC1[source]-PC1[current],")"))),
       y = "ln(fitness)") +
  geom_vline(aes(xintercept = x_seq[which(fitness == max(fitness))]), linetype = "dashed") +
  annotation_custom(text_high,xmin=1.7,xmax=1.7,ymin=0.6,ymax=0.6) + 
  annotation_custom(text_low,xmin=-2.2,xmax=-2.2,ymin=0.6,ymax=0.6)+
  coord_cartesian(clip="off") +
  scale_color_manual(values = c("dodgerblue", "brown1")) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 10, 0, "pt")) -> fitness_plot

png("modeling/figs/climate_dist.png", height = 7, width = 4, units = "in", res = 300)
survival_plot / fecundity_plot / fitness_plot 
dev.off()

plot(for_model_sample$clim_dist_sc, log(for_model_sample$seed_count))
