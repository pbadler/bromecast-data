phen %>% 
  group_by(site) %>% 
  summarize(min = min(jday, na.rm = T))

# Fall emergence SS
phen %>% 
  filter(site == "SS" & jday %in% -49:-43) %>% 
  mutate(live_winter = live) %>% 
  select(plantID, gravel, density, genotype, live_winter)-> winter_SS
# Spring emergence SS
phen %>% 
  filter(site == "SS" & jday %in% 87:91) %>% 
  mutate(live_spring = live) %>% 
  select(plantID, gravel, density, genotype, live_spring)-> spring_SS

# All SS
merge(winter_SS, spring_SS) -> SS_emerge

SS_emerge %>% 
  filter(complete.cases(live_winter) & complete.cases(live_spring)) %>% 
  group_by(live_winter, live_spring) %>% 
  summarize(n = n()) %>% 
  ungroup() -> SS_out

# Fall emergence WI
phen %>% 
  filter(site == "WI" & jday %in% -24:-23) %>% 
  mutate(live_winter = live) %>% 
  select(plantID, gravel, density, genotype, live_winter)-> winter_WI
# Spring emergence WI
phen %>% 
  filter(site == "WI" & jday %in% 87:90) %>% 
  mutate(live_spring = live) %>% 
  select(plantID, gravel, density, genotype, live_spring)-> spring_WI

# All WI
merge(winter_WI, spring_WI) -> WI_emerge

WI_emerge %>% 
  filter(complete.cases(live_winter) & complete.cases(live_spring)) %>% 
  group_by(live_winter, live_spring) %>% 
  summarize(n = n()) %>% 
  ungroup() -> WI_out

# Fall emergence BA
phen %>% 
  filter(site == "BA" & jday %in% -21) %>% 
  mutate(live_winter = live) %>% 
  select(plantID, gravel, density, genotype, live_winter)-> winter_BA
# Spring emergence WI
phen %>% 
  filter(site == "BA" & jday %in% 96:97) %>% 
  mutate(live_spring = live) %>% 
  select(plantID, gravel, density, genotype, live_spring)-> spring_BA

# All WI
merge(winter_BA, spring_BA) -> BA_emerge

BA_emerge %>% 
  filter(complete.cases(live_winter) & complete.cases(live_spring)) %>% 
  group_by(live_winter, live_spring) %>% 
  summarize(n = n())  %>% 
  ungroup() -> BA_out

table(live_winter = c(0, 1), live_spring = c(0, 1))
matrix(data = rev(BA_out$n), nrow = 2, byrow = T,
       dimnames = list(c("spring_Y", "spring_N"),
                       c("winter_Y", "winter_N")))

ggplot(BA_out, aes(x = live_spring, y = live_winter, fill = n)) +
  geom_tile() + theme_bw(base_size = 16) + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = n), color = "black", size = 5) +
  labs(x = "spring emergence",
       y = "winter emergence") +
  ggtitle("Baltzor 2022") -> c

ggplot(WI_out, aes(x = live_spring, y = live_winter, fill = n)) +
  geom_tile() + theme_bw(base_size = 16) + coord_equal() +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = n), color = "black", size = 5) +
  labs(x = "spring emergence",
       y = "winter emergence") +
  ggtitle("Wildcat 2022") -> b

ggplot(SS_out, aes(x = live_spring, y = live_winter, fill = n)) +
  geom_tile() + theme_bw(base_size = 16) + coord_equal() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = n), color = "black", size = 5) +
  labs(x = "spring emergence",
       y = "winter emergence") +
  ggtitle("Sheep Station 2022") -> a

a+b+c

tibble(x = c(-46, 89, 198, -23.5, 87, 154, -21, 96.5, 189),
       y = rep(0,9),
       site = c("SS", "SS", "SS", "WI", "WI", "WI", "BA", "BA", "BA")) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_segment(aes(x = -50, xend = 200, y = 0, yend = 0), color = "gray") +
  geom_point(aes(color = site), size = 4, alpha = 0.5) +
  labs(x = "day of year", y = "") +
  theme_classic(base_size = 16) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = c("darkblue", "darkred", "darkgreen")) -> d



surv <- read_csv("~/Git/Bromecast/gardens/deriveddata/2022_allsites_flower_harvest_cleanest.csv")
surv %>% 
  filter(site == "BA") %>% 
  group_by(live) %>% 
  summarize(n = n())

tibble(x = c(3694, 3542, 3577, 3015, 2877, 2826, 2138, 1732, 1662),
       site = c("SS" ,"SS", "SS", "WI", "WI", "WI", "BA", "BA", "BA"),
       time = rep(c("1 - winter", "2 - spring", "3 - harvest"), 3)) %>% 
  mutate(`proportion alive` = x/4000) %>% 
  ggplot(aes(x = time, y = `proportion alive`, color = site, group = site)) +
  geom_point(size = 4) +
  geom_line() +
  scale_color_manual(values = c("darkblue", "darkred", "darkgreen")) +
  theme_bw(base_size = 16) -> e

library(patchwork)

(d/(a+b+c)) | e

# Create custom design
design <- c(
  area(1, 1, 1, 3),
  area(2, 1, 3, 1),
  area(2, 2, 3, 2),
  area(2, 3, 3, 3),
  area(1, 4, 3, 4)
)

plot(design)
png("~/Desktop/emergence.png", height = 5.8, width = 15.2, res = 300, units = "in")
d + a + b + c + e + 
plot_layout(design = design, guides = "collect")
dev.off()
