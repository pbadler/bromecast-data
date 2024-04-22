## Fix block 1 ####

# Block 1 looks mostly fine. Some plots observed in 5/18, others observed in 7/12
## Fix block 2 ####
rawD %>% 
  filter(block == 2) %>% 
  group_by(plot, gravel) %>% 
  summarize(n = n())

rawD_2 <- rawD %>% filter(block == 2)
# Plots 1 & 2 are fine
rawD_2_12 <- rawD_2 %>% filter(plot %in% 1:2)
# Plots 3 & 4 are messed up. Plot 3 "black" is correct
rawD_2_3 <- rawD_2 %>% filter(plot == 3 & gravel == "Black")
# Plot 4 "white" is correct but missing some values that were recorded as 2.3w
rawD_2_4 <- rawD_2 %>% filter(plot == 4)
rawD_2 %>% 
  filter(plot == 3 & gravel == "White") %>% 
  # Remove 5/11 date which seems messed up (only include 5/9 and 5/18)
  filter(date %in% c("2023-05-09", "2023-05-18")) %>% 
  rbind(rawD_2_4) %>% 
  mutate(plot = 4) -> rawD_2_4

# Bring all back together
rawD_2_fixed <- rbind(rawD_2_12, rawD_2_3, rawD_2_4)

## Fix block 3 ####
# Plots 1 and 2 are fine
rawD_3_12 <- rawD %>% filter(block == 3 & plot %in% 1:2)
# Plot 3 should be black
rawD_3_3 <- rawD %>% filter(block == 3 & plot == 3 & gravel == "Black")
# Plot 4 should be white
rawD_3_4 <- rawD %>% filter(block == 3 & plot == 4 & gravel == "White")
# These are some extra plot 3s that should be plot 4s
rawD %>% 
  filter(block == 3 & plot == 3 & gravel == "White") %>% 
  mutate(plot = 4) %>% 
  # Bind back to rest of plot 4
  rbind(rawD_3_4) -> rawD_3_4

# Bring all data back together
rawD_3_fixed <- rbind(rawD_3_12, rawD_3_3, rawD_3_4)

## Fix block 4 ####
# Plots 1 and 2 look good
rawD %>% filter(block == 4 & plot %in% 1:2) -> rawD_4_12

# A lot of plants have two recorded observations on 5/18 in plot 3
rawD %>% 
  filter(block == 4 & plot == 3 & date == "2023-05-18") 

rawD %>% 
  filter(block ==4 & plot == 3) %>% 
  filter(date != "2023-07-12") %>% 
  # x should only go up to 9
  filter(x %in% 1:9) %>% 
  distinct() %>% 
  arrange(x, y, date) %>% 
  filter(date == "2023-05-18") %>% 
  group_by(x,y) %>% 
  summarize(n = n()) %>% 
  filter(n > 1) -> coords_to_fix_4_3
  
rawD %>% 
  filter(block == 4 & plot == 3) %>% 
  filter(x %in% 1:9) %>% 
  distinct() %>% 
  filter(date == "2023-05-18") -> rawD_4_3_tofix

# Have to fix block 4, plot 3 manually for 05/18/2023
# write_csv(rawD_4_3_tofix, "gardens/deriveddata/wi_block4_plot3_tofix.csv")
# Read in manually fixed data
rawD_4_3_fixed <- read_csv("gardens/deriveddata/wi_block4_plot3_tofix.csv")

rawD %>% 
  filter(block == 4 & plot == 3 & x %in% 1:9) %>% 
  filter(date %in% c("2023-03-23", "2023-04-26", "2023-05-02","2023-05-09")) %>% 
  rbind(rawD_4_3_fixed %>% mutate(date = mdy(date))) -> rawD_4_3_fixed

# Plot 4 has duplicates on 5/2
rawD %>% filter(block == 4 & plot == 4 & date == "2023-05-02") %>% 
  arrange(x,y) %>% 
  distinct() %>% 
  rbind(rawD %>% filter(block == 4 & plot == 4 & date != "2023-05-02"))-> rawD_4_4

# Bring all block 4 subplots together
rbind(rawD_4_12,
      rawD_4_3_fixed,
      rawD_4_4) -> rawD_4_fixed

# There's also an observation that has x = "c"; this should be x = 7
rawD_4_fixed[rawD_4_fixed$x == "c", "x"] <- 7

## Fix block 5 ####
# Plots 1 and 2 are good
rawD %>% 
  filter(block == 5 & plot %in% 1:2) -> rawD_5_12
# Plot 4 should be white gravel
rawD %>% 
  filter(block == 5 & plot == 4 & gravel == "White") -> rawD_5_4
# Plot 3 is messed up
rawD %>% 
  filter(block == 5 & plot == 3 & gravel == "Black") %>% 
  group_by(x,y) %>% 
  summarize(n = n()) %>% 
  print(n = Inf)

rawD %>% 
  filter(block == 5 & plot == 3 & gravel == "Black") %>% 
  arrange(x,y) -> rawD_5_3_most

rawD %>% 
  filter(block == 5 & plot == 3 & gravel == "White") %>% 
  arrange(x,y) %>% 
  filter(date == "2023-05-11") %>% 
  mutate(gravel = "Black") -> rawD_5_3_rest

rbind(rawD_5_12,
      rawD_5_3_most, rawD_5_3_rest,
      rawD_5_4) -> rawD_5_fixed
## Fix block 6 ####
# Plot 1 and 2 are good
rawD_6_12 <- rawD %>% filter(block == 6 & plot %in% 1:2)
# Plot 4 should be block gravel
rawD_6_4 <- rawD %>% filter(block == 6 & plot == 4 & gravel == "Black")
# Plot 3 white gravel has some assigned to plot 4
rawD_6_3_most <- rawD %>% filter(block == 6 & plot == 3 & gravel == "White")
rawD_6_3_rest <- rawD %>% filter(block == 6 & plot == 4 & gravel == "White") %>% 
  mutate(plot = 3)
# Bring all back together
rbind(rawD_6_12, rawD_6_3_most, rawD_6_3_rest, rawD_6_4) -> rawD_6_fixed
## Fix block 7 ####
# Plot 1 is good
rawD_7_1 <- rawD %>% filter(block == 7 & plot == 1)

# Plot 2 has repeats for 3/23 and is missing observations for 4/26
rawD_7_2 <- rawD %>% filter(block == 7 & plot == 2) %>% 
  filter(date == "2023-03-23") %>% 
  arrange(x,y) %>% 
  distinct() %>% 
  rbind(rawD %>% filter(block == 7 & plot == 2 & date != "2023-03-23")) %>% 
  arrange(x, y, date)

rawD %>% 
  filter(block == 7 & plot == 3 & gravel == "White" & x %in% 1:9) %>% 
  arrange(x, y, date) %>% 
  group_by(x, y) %>% 
  summarize(n = n()) %>% 
  print(n = Inf)

rawD_7_3 <- rawD %>% 
  filter(block == 7 & plot == 3 & gravel == "White") %>% 
  # Only 3 dates are complete and make sense
  filter(date %in% c("2023-03-23", "2023-04-27", "2023-05-18")) %>% 
  arrange(x,y)

rawD %>% 
  filter(block == 7 & plot == 4 & gravel == "White") %>%
  filter(date == "2023-05-04") %>% 
  arrange(x,y) -> some_7_4_may

rawD %>% 
  filter(block == 7 & plot == 3 & gravel == "White") %>%
  filter(date == "2023-05-04")  %>% 
  arrange(x,y) %>% 
  distinct() %>% 
  mutate(plot = 4) %>% 
  rbind(some_7_4_may) %>% 
  arrange(x,y) %>% 
  # (1,2) (1,3) (1,4) (1,5) all have duplicates so need to choose one. They are
  # all pretty similar across the 2 observations (v doesn't change much if at
  # all)
  mutate(v = case_when(x == 1 & y == 2 ~ "FG",
                       x == 1 & y == 5 ~ "Bootstage",
                       T ~ v),
         live = case_when(x == 1 & y == 3 ~ "N",
                          x == 1 & y == 5 ~ "Y",
                          T ~ live),
         harvested = ifelse(x == 1 & y == 5, "N", harvested),
         herbivory = ifelse(x == 1 & y == 5, "N", herbivory),
         frost_heave = ifelse(x == 1 & y == 5, "N", frost_heave)) %>% 
  distinct() %>% 
  filter(y %in% 1:5) %>% 
  rbind(rawD %>% filter(block == 7 & plot == 4 & date != "2023-05-04")) -> rawD_7_4

# Bring all back together
rbind(rawD_7_1, rawD_7_2, rawD_7_3, rawD_7_4) -> rawD_7_fixed

## Fix block 8 ####

# Block 8 looks mostly fine. Plot 3 is missing observations on 5/18 and for plot
# 4 there are only 5 observations on 5/4

# rawD %>% filter(block == 8 & plot == 4 & date == "2023-05-04") 

## Fix block 9 ####

rawD %>% filter(block == 9) %>% 
  group_by(plot, date) %>% 
  summarize(n = n())
# Duplicate observations in Plot 3 for 4/27 and in Plot 4 for 5/4

# Plots 1 and 2 are fine
rawD_9_12 <- rawD %>% filter(block == 9 & plot %in% 1:2)

# Remove extras in plot 3
rawD %>% 
  filter(block == 9 & plot == 3 & gravel == "Black") %>% 
  filter(date == "2023-04-27") %>% 
  arrange(x,y) %>% 
  rbind(rawD %>% filter(block == 9 & plot == 3 & date != "2023-04-27")) -> rawD_9_3

# Remove extras in plot 4
rawD %>% 
  filter(block == 9 & plot == 4) %>% 
  arrange(x,y) %>% 
  filter(x %in% 1:9) -> plot4_most

plot4_most %>% 
  filter(date == "2023-05-04") %>% 
  arrange(x,y) -> block9_plot4_tofix

# Need to fix block 9, plot 4 manually
# write_csv(block9_plot4_tofix,"~/Desktop/block9_plot4_tofix.csv")

# Read back in manually altered csv. Made best guesses for which observation was
# correct based on other dates.
block9_plot4_fixed <- read_csv("~/Desktop/block9_plot4_tofix.csv")
block9_plot4_fixed %>% 
  mutate(date = mdy(date)) -> block9_plot4_fixed

# Bring everything back together
plot4_most %>% 
  filter(date != "2023-05-04") %>% 
  rbind(block9_plot4_fixed) %>% 
  rbind(rawD %>% filter(block == 9 & plot == 3 & gravel == "White") %>% 
          mutate(plot = 4)) %>% 
  arrange(x,y, date) %>% 
  rbind(rawD_9_12, rawD_9_3) -> rawD_9_fixed


## Fix block 10 ####

# Plot 1 is missing observations on 4/27 - seems like they could be mislabeled
# as plot 3 - check densities (plot 1 should be low, 3 should be high)

rawD %>% filter(block == 10 & plot == 3 & gravel == "White") %>% 
  mutate(plot = 1) %>% 
  rbind(rawD %>% filter(block == 10 & plot == 1 & gravel == "White")) -> rawD_10_1

# Plot 2 seems fine
rawD %>% filter(block == 10 & plot == 2) -> rawD_10_2 

# Plot 3 has duplicate observations on 5/11
rawD %>% 
  filter(block == 10 & plot == 3) %>% 
  filter(date == "2023-05-11") %>% 
  arrange(x,y) %>% 
  # Remove duplicates
  distinct() %>% 
  rbind(rawD %>% filter(block == 10 & plot == 3 & gravel == "Black" & date != "2023-05-11")) -> rawD_10_3

# Plot 4 seems fine so bind rest together
rbind(rawD_10_1, rawD_10_2, rawD_10_3, rawD %>% filter(block == 10 & plot == 4)) -> rawD_10_fixed

## Bring all blocks back together ####
rbind(rawD_2_fixed,
      rawD_3_fixed,
      rawD_4_fixed,
      rawD_5_fixed,
      rawD_6_fixed,
      rawD_7_fixed,
      rawD_9_fixed,
      rawD_10_fixed,
      rawD %>% filter(block %in% c(1,8))) -> rawD_fixed

# Set x to be numeric to match y's
rawD_fixed$x <- as.numeric(rawD_fixed$x)

# There are a number of observations that were recorded as being made on 7/12...
# this isn't possible given the field season length. It seems like these should
# be 5/18 since these plots are missing observations from 5/18.

rawD_fixed %>% 
  mutate(date = case_when(date == ymd("2023-07-12") ~ ymd("2023-05-18"),
                          T ~ date)) -> rawD_fixed

# Change phenology codes
rawD_fixed %>% 
  mutate(v = ifelse(v == "Bootstage", "BS", v)) %>% 
  mutate(v = ifelse(v == ">V3", "V3+", v)) -> rawD_fixed

# Remove all intermediate datasets
rm(list=setdiff(ls(), "rawD_fixed"))

## Bring in harvest data set ####
v_harvest <- read_csv("gardens/rawdata/CG_harvest2023.csv")

v_harvest %>% 
  filter(Site == "BoiseLow") %>% 
  select(notes) %>% 
  distinct() -> v_harvest_notes

# Write notes csv to code manually
#write_csv(v_harvest_notes, "gardens/deriveddata/WI_harvest23_notes_actions.csv")

v_harvest %>% 
  filter(Site == "BoiseLow") %>% 
  mutate(site = "WI",
         date = mdy(Date),
         block = Block,
         plot = Plot,
         gravel = ifelse(Albedo == "white", "White", "Black"),
         density = ifelse(Density == "high", "High", "Low"),
         live = Live,
         v = V,
         biomass_whole = biomass_whole) %>% 
  select(site, date, block, plot, gravel, density, x, y,
         live, v, biomass_whole, notes) %>% 
  # Fix phenology codes - use the latest stage if multiple stages
  mutate(v = case_when(v == "FP_FG" ~ "FP",
                       v == "FB_FP" ~ "FB",
                       v == "FG_FP" ~ "FP",
                       v == "FG_BS" ~ "FG",
                       v == "FP_FB" ~ "FB",
                       v == "FG_FB" ~ "FB",
                       v == "FG_FP_FB" ~ "FB",
                       v == "FB_FG" ~ "FB",
                       v == "FP_FG_FB" ~ "FB",
                       v == "FB_FG_FP" ~ "FB",
                       v == "UNK" ~ NA,
                       T ~ v)) -> v_harvest

# Read in standardized notes and drop bad coordinates, duplicates, etc.
v_harvest_notes_std <- read_csv("gardens/deriveddata/WI_harvest23_notes_actions.csv")
v_harvest_notes_std %>% 
  filter(action == "action") %>% 
  select(notes, note_standard) %>% 
  merge(v_harvest, all = T) -> v_harvest 
  # This removes bad positions, duplicates, and harvests without recorded date
  #filter(is.na(note_standard) | note_standard %in% c("smut", "seed_drop",
  #                                                  "physical_damage", "no_date",
  #                                                 "mortality", "herbivory")) -> v_harvest

# Only keep ones that are low density and go up to x = 18 or high density and go
# up to x = 9 (everything but block 1, plot 1 that has issues)
v_harvest %>% 
  filter(block*plot != 1) %>% 
  filter(density == "High" & x %in% 1:9 | density == "Low" & x %in% 1:18) -> v_harvest_most

# Add in plants from block 1 and plot 1, including those from x = 10
v_harvest %>% 
  filter(block*plot == 1) -> v_harvest_rest

# Bring these two datasets back together
v_harvest <- rbind(v_harvest_most, v_harvest_rest) %>% 
  arrange(block, plot, gravel, x, y)

# Pull out columns needed to merge with phenology data
v_harvest %>% 
  select(date, block, plot, gravel, x, y, live, v, notes) %>% 
  mutate(site = "WI",
         harvested = NA,
         herbivory = NA,
         frost_heave = NA) %>% 
  select(names(rawD_fixed)) %>% 
  rbind(rawD_fixed) -> rawD_with_harvest

# If v stage is recorded, should be live
rawD_with_harvest %>% 
  mutate(live = ifelse(complete.cases(v), "Y", "N")) -> rawD_with_harvest

# Get phenology stage at last check before harvest
rawD_fixed %>% 
  group_by(block, plot, gravel, x, y) %>% 
  slice(which.max(date)) %>% 
  ungroup() %>% 
  select(last_phen_date = date, block, plot, gravel, x, y, live, v, notes) %>% 
  # If v stage is recorded, should be live
  mutate(live = ifelse(complete.cases(v), "Y", "N")) -> rawD_fixed_tomerge

v_harvest %>% 
  select(harvest_date = date, block, plot, gravel, density, x, y, biomass_whole, note_standard, notes) %>% 
  arrange(block, plot, gravel, density, x, y) %>% 
  merge(rawD_fixed_tomerge %>% select(-notes)) -> phen_harvest_merged

# Make indicator column for being harvested
phen_harvest_merged %>% 
  mutate(harvested = ifelse(is.na(biomass_whole), "N", "Y")) %>% 
  # Rename live at phen column and v stage column (v @ last phen check)
  rename(live_last_phen = live,
         v_last_phen = v) %>% 
  # Rearrange columns
  select(block, plot, gravel, density, x, y, last_phen_date, live_last_phen,
         v_last_phen, harvest_date, harvested, biomass_whole, note_standard, notes) %>% 
  # Remove any duplicates
  distinct() %>% 
  # Remove one mislabelled observation. We'll add that to the incorrect
  # coordinates later
  filter(block != 2 | plot != 1 | density != "Low") -> for_checking

# Check to see how observations are distributed across blocks and plots
# for_checking %>% 
#   group_by(block, plot, gravel, density, x, y) %>% 
#   summarize(n = n()) %>% 
#   print(n = Inf)
## ALL LOOKS GOOD! ##

# Make %notin% operator
`%notin%` <- Negate(`%in%`)

# Add indicator that identifies false positives and false negatives
for_checking %>% 
  # If it is a duplicate, it was harvested
  mutate(harvested = case_when(note_standard %in% c("duplicate", "no_date", "bad_position") ~ "Y",
                               note_standard %notin% c("duplicate", "no_date", "bad_position") ~ harvested,
                               is.na(note_standard) ~ harvested)) %>% 
  # Add indicator here
  mutate(live_harvest_check = case_when(live_last_phen == "Y" & harvested == "N" ~ "phenY_harvestN",
                                  live_last_phen == "N" & harvested == "Y" ~ "phenN_harvestY",
                                   T ~ NA)) -> for_checking_all

for_checking_all %>% filter(note_standard == "no_date") %>% 
  filter(block == 2)

# Graphic for high density plots
png("~/Desktop/high_density_plots.png", height = 10, width = 12, res = 300, units = "in")
for_checking_all %>% 
  filter(density == "High") %>% 
  mutate(id = paste("Block", block, "Plot", plot, "-", gravel, sep = " ")) %>% 
  mutate(live_harvest_check =
           case_when(live_harvest_check == "phenN_harvestY" ~ "not alive @ phenology check,\n but harvested",
                     live_harvest_check == "phenY_harvestN" ~ "alive @ phenology check,\n but not harvested",
                     is.na(live_harvest_check) ~ "seems good")) %>% 
  ggplot(aes(x = x, y = y, color = live_harvest_check)) +
  geom_point(size = 4) +
  theme_classic(base_size = 16) +
  scale_x_continuous(breaks = 1:9, position = "top") +
  scale_x_continuous(sec.axis = dup_axis(), breaks = 1:9) +
  scale_y_reverse(breaks = 1:10) +
  scale_y_reverse(sec.axis = dup_axis(), breaks = 1:10) +
  theme(legend.position = "top") +
  facet_wrap(~id) +
  scale_color_manual(values = c("orange", "brown1", "gray47")) +
  labs(y = "y-coordinate",
       x = "x-coordinate",
       color = "issue")
dev.off()

# Graphic for low density plots
png("~/Desktop/low_density_plots.png", height = 11, width = 18, res = 300, units = "in")
for_checking_all %>% 
  filter(density == "Low") %>% 
  mutate(id = paste("Block", block, "Plot", plot, "-", gravel, sep = " ")) %>% 
  mutate(live_harvest_check =
           case_when(live_harvest_check == "phenN_harvestY" ~ "not alive @ phenology check,\n but harvested",
                     live_harvest_check == "phenY_harvestN" ~ "alive @ phenology check,\n but not harvested",
                     is.na(live_harvest_check) ~ "seems good")) %>% 
  ggplot(aes(x = x, y = y, color = live_harvest_check)) +
  geom_point(size = 4) +
  theme_classic(base_size = 16) +
  scale_x_continuous(breaks = 1:18, position = "top") +
  scale_x_continuous(sec.axis = dup_axis(), breaks = 1:18) +
  scale_y_reverse(breaks = 1:5) +
  scale_y_reverse(sec.axis = dup_axis(), breaks = 1:5) +
  theme(legend.position = "top") +
  facet_wrap(~id, nrow = 5) +
  scale_color_manual(values = c("orange", "brown1", "gray47")) +
  labs(y = "y-coordinate",
       x = "x-coordinate",
       color = "issue")
dev.off()

for_checking %>% 
  filter(note_standard %in% c("duplicate", "bad_position", "no_date")) %>% 
  # If it is a duplicate, it was harvested
  mutate(harvest = ifelse(note_standard == "duplicate", "Y", "N")) %>% 
  arrange(note_standard) %>% 
  select(block, plot, gravel, x, y, harvest_date, density, biomass_whole,
         note_standard, notes, last_phen_date, live = live_last_phen, v = v_last_phen, harvest)-> for_checking_errors

v_harvest %>% 
  filter(grepl("incorrect coordinates to check", notes)) %>% 
  mutate(last_phen_date = NA,
         harvest = "Y",
         note_standard = "incorrect_coords") %>% 
  select(block, plot, gravel, x, y, harvest_date = date, density, biomass_whole,
         note_standard, notes, last_phen_date, live, v, harvest) -> for_checking_errors2

v_harvest %>% 
  filter(block == 1 & plot == 1 & x == 10 & biomass_whole > 0) %>%
  mutate(last_phen_date = NA,
         harvest = "Y",
         note_standard = "incorrect_coords") %>% 
  select(block, plot, gravel, x, y, harvest_date = date, density, biomass_whole,
         note_standard, notes, last_phen_date, live, v, harvest) -> for_checking_errors3

rbind(for_checking_errors, for_checking_errors2, for_checking_errors3) %>% 
  select(block, plot, gravel, density, x, y, last_phen_date, harvest_date,
         v, live, harvest, note_standard, notes) %>% 
  arrange(block, plot, gravel, density, x, y) -> all_to_check

all_to_check %>% 
  rename(v_last_phen = v, live_last_phen = live) %>% 
  mutate(live_last_phen = ifelse(live_last_phen == "Y", "Y", "N"),
         harvest = "Y")-> all_to_check

write_csv(all_to_check, "~/Desktop/ids_to_check.csv")
write_csv(for_checking_all, "~/Desktop/all_wildcat.csv")


