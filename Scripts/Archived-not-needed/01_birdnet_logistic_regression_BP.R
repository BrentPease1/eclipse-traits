library(data.table)
library(lubridate)
library(here)
library(ggplot2)
library(ggeffects)
library(glmmTMB)
library(ggalt)
# Read data
birdnet <- fread(here::here("Data/birdnet_cleaned_v03.csv"))
birdnet[common_name == "Woodhouse's Scrub-Jay", common_name := "California Scrub-Jay"] #fuckit
birdnet[common_name == "Pacific Wren", common_name := "Winter Wren"] #fuckit
birdnet[common_name == "Green-winged Teal", common_name := "Common Teal"] #fuckit
birdnet[common_name == "Wilson's Snipe", common_name := "Common Snipe"] #fuckit

birdnet[, quick_index := 1:nrow(birdnet)]


# elton traits for family name
elton <- fread(here('Data/elton.txt'))

# fix some names manually
elton[English == 'Whip-poor-will', English := 'Eastern Whip-poor-will']
elton[English == 'Blue-grey Gnatcatcher', English := 'Blue-gray Gnatcatcher']
elton[English == 'Bare-throated Tiger-heron', English := 'Bare-throated Tiger-Heron']
elton[English == 'Black-bellied Whistling-duck', English := 'Black-bellied Whistling-Duck']
elton[English == 'Black-crowned Night-heron', English := 'Black-crowned Night-Heron']
elton[English == 'Yellow-crowned Night-heron', English := 'Yellow-crowned Night-Heron']
elton[English == 'American Treecreeper', English := 'Brown Creeper']
elton[English == 'Eastern Screech-owl', English := 'Eastern Screech-Owl']
elton[English == 'Western Screech-owl', English := 'Western Screech-Owl']
elton[English == 'Whiskered Screech-owl', English := 'Whiskered Screech-Owl']
elton[English == "Eastern Wood-pewee", English := "Eastern Wood-Pewee"]
elton[English == "Brent Goose", English := "Brant"]
elton[English == "Western Scrub-jay", English := "California Scrub-Jay"]
elton[English == "Clay-coloured Sparrow", English := "Clay-colored Sparrow"]
elton[English == "Tricoloured Heron", English := "Tricolored Heron"]
elton[English == "Grey Plover", English := "Black-bellied Plover"]
elton[English == "American Swallow-tailed Kite", English := "Swallow-tailed Kite"]
elton[English == "Greyish Saltator", English := "Cinnamon-bellied Saltator"] #fuckit
elton[English == "Collared Forest-falcon", English := "Collared Forest-Falcon"]
elton[English == "Common Moorhen", English := "Common Gallinule"] #fuckit
elton[English == "Common Ground-dove", English := "Common Ground Dove"]
elton[English == "Eurasian Collared-dove", English := "Eurasian Collared-Dove"]
elton[English == "Grey Catbird", English := "Gray Catbird"]
elton[English == "Common Starling", English := "European Starling"]
elton[English == "Northern Pygmy-owl", English := "Northern Pygmy-Owl"]
elton[English == "Rusty-crowned Ground-sparrow", English := "Rusty-crowned Ground-Sparrow"]
elton[English == "Leach's Storm-petrel", English := "Leach's Storm-Petrel"]
elton[English == "Common Pheasant", English := "Ring-necked Pheasant"]
elton[English == "Northern Beardless-tyrannulet", English := "Northern Beardless-Tyrannulet"]
elton[English == "Purple Swamphen", English := "Purple Gallinule"]
elton[English == "Ruddy Ground-dove", English := "Ruddy Ground Dove"]
elton[English == "Yellow-headed Amazon", English := "Yellow-headed Parrot"]

 
 


# english loses fewer rows than scientific
birdnet <- merge(birdnet, elton[, .(BLFamilyLatin, English)], by.x = "common_name", by.y = 'English')
setnames(birdnet, old = 'BLFamilyLatin', new = 'family')
birdnet <- birdnet[!duplicated(quick_index),]

# missing rows (FIXED!)
# these_indices <- which(!(birdnet$quick_index %in% birdnet2$quick_index))
# test <- sort(birdnet[these_indices,unique(common_name)])
# test
# 
# elton[English %like% "Pheasant", .(Scientific, English)]
# elton[Scientific %like% "Amazona", .(Scientific, English)]

# Filter within_path
dt_in_path <- birdnet[within_path == "within_path"]

# Filter focal time detections
focal_time_detections <- dt_in_path[
  start_time >= SecondContactTimeUTC & start_time <= ThirdContactTimeUTC &
    date %in% ymd(c("2024-04-07", "2024-04-08", "2024-04-09")),
  .(station = ESIDNumber, common = common_name, sci = scientific_name, date, activity,
    family)]

# Process test dataset (detected species)
test <- unique(focal_time_detections[, .(station, common, sci, family, activity, date)])
test[, y := 1]

# Reshape test data: retain detections
test <- dcast(test, station + common + sci + family + activity ~ date, value.var = "y", fill = 0)
setnames(test, c("2024-04-07", "2024-04-08", "2024-04-09"), c("tm1", "t", "tp1"))

# Create template (all potential species-station combinations)
template <- unique(dt_in_path[date %in% ymd(c("2024-04-07", "2024-04-08", "2024-04-09")), 
                              .(station = ESIDNumber, common = common_name, 
                                sci = scientific_name, activity, family)])

# Merge test (detections) with template (all spp) 
det <- merge(template, test, by = c("station", "common", "sci", "family", "activity"), all.x = TRUE)

# Ensure detected species retain y=1, while undetected ones are set to 0
det[, c("tm1", "t", "tp1") := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = c("tm1", "t", "tp1")]

# Reshape long format
det <- melt(det, id.vars = c("station", "common", "sci", "family", "activity"), 
            measure.vars = c("tm1", "t", "tp1"), variable.name = "eclipse", value.name = "y")

# Convert eclipse indicator
det[, eclipse := fifelse(eclipse == "t", 1, 0)]

# Assign species-station group ID
det[, sp.station := .GRP, by = .(station, common)]
det[, fam.station := .GRP, by = .(station, family)]


# set minimum dets for a species
filtered_det <- det[, .N, by = family][N >= 10][det, on = "family"]
lots_of_dets <- det[, .N, by = family][N >= 100]

filtered_det_fam <- det[family %in% lots_of_dets$family]

lots_of_dets <- det[, .N, by = common][N >= 150]
filtered_det <- det[common %in% lots_of_dets$common]

# estimates negative effect of eclipse; think we should maybe have a random intercept for station/sp tho...?
m1 <- glmmTMB::glmmTMB(
  y ~ 1 + factor(eclipse) + activity + (1 + factor(eclipse) | common),
  data = filtered_det,
  family = binomial(link = "logit")) 

#
m2 <- glmmTMB::glmmTMB(
  y ~ 1 + factor(eclipse) + (1 + factor(eclipse)|common) + (1|station),
  data = filtered_det,
  family = binomial(link = "logit"),
) 

AIC(m1, m2) #m2 is better

# generate prediction
# Get predicted probabilities across eclipse 
preds <- ggpredict(m2, terms = c("eclipse"))

# Plot
ggplot(preds, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, col = x)) +
  scale_color_manual(values = c("0" = 'gold', "1" = "black")) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(width = 0.5, position = position_dodge(0.3), size = 2) +
  labs(x = NULL, y = "Probability of Vocalization") +
  theme_classic() +
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 14))

ggsave(plot = last_plot(), filename = here('Results/Figures/prob_of_vocal_eclipse.png'),
       width = 8, height = 8, dpi = 300)



####### MODEL 1 #####
# Extract random effects
# ranef_df <- as.data.frame(ranef(m1, condVar = TRUE)$cond$common)
# 
# # Add species names
# ranef_df$common <- rownames(ranef_df)
# 
# # Rename columns for clarity
# colnames(ranef_df) <- c("Intercept", "EclipseEffect", "common")
# 
# # Compute the random intercepts for eclipse = 0 and eclipse = 1
# ranef_df[, 'Intercept_eclipse_0'] <- ranef_df$Intercept  # When eclipse = 0
# ranef_df[, 'Intercept_eclipse_1'] <- ranef_df$Intercept + ranef_df$EclipseEffect  # When eclipse = 1
# ranef_df$prob_vocal_0 <- plogis(ranef_df$Intercept_eclipse_0)
# ranef_df$prob_vocal_1 <- plogis(ranef_df$Intercept_eclipse_1)
# 
# # Gather the data into a long format for ggplot
# ranef_long <- reshape(ranef_df, 
#                       varying = c("Intercept_eclipse_0", "Intercept_eclipse_1"), 
#                       v.names = "Random_Intercept", 
#                       timevar = "eclipse", 
#                       times = c("eclipse_0", "eclipse_1"),
#                       direction = "long")
# 
# # Plot random effects for eclipse = 0 and eclipse = 1
# ggplot(ranef_long, aes(x = reorder(common, Random_Intercept), y = Random_Intercept, color = eclipse)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = Random_Intercept - 1.96 * EclipseEffect, ymax = Random_Intercept + 1.96 * EclipseEffect), width = 0.2) +
#   coord_flip() +
#   labs(x = "Species", y = "Random Intercept", title = "Species-Specific Random Effects for Eclipse Conditions") +
#   scale_color_manual(values = c("eclipse_0" = "blue", "eclipse_1" = "red")) +
#   theme_minimal() +
#   theme(legend.title = element_blank())



# MODEL 2 ####
# Extract random effects
# ranef_df <- as.data.frame(ranef(m2, condVar = TRUE)$cond$common)
# 
# # Add species names
# ranef_df$common <- rownames(ranef_df)
# 
# # Rename columns for clarity
# colnames(ranef_df) <- c("Intercept", "EclipseEffect", "common")
# 
# # Compute the random intercepts for eclipse = 0 and eclipse = 1
# ranef_df[, 'Intercept_eclipse_0'] <- ranef_df$Intercept  # When eclipse = 0
# ranef_df[, 'Intercept_eclipse_1'] <- ranef_df$Intercept + ranef_df$EclipseEffect  # When eclipse = 1

# Gather the data into a long format for ggplot
# ranef_long <- reshape(ranef_df, 
#                       varying = c("Intercept_eclipse_0", "Intercept_eclipse_1"), 
#                       v.names = "Random_Intercept", 
#                       timevar = "eclipse", 
#                       times = c("eclipse_0", "eclipse_1"),
#                       direction = "long")
# 
# # Plot random effects for eclipse = 0 and eclipse = 1
# ggplot(ranef_long, aes(x = reorder(common, Random_Intercept), y = Random_Intercept, color = eclipse)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = Random_Intercept - 1.96 * EclipseEffect, ymax = Random_Intercept + 1.96 * EclipseEffect), width = 0.2) +
#   coord_flip() +
#   labs(x = "Species", y = "Random Intercept", title = "Species-Specific Prob of Vocalization") +
#   scale_color_manual(values = c("eclipse_0" = "blue", "eclipse_1" = "red")) +
#   theme_minimal() +
#   theme(legend.title = element_blank())


## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# THIS IS IMPORTANT #### 
# getting probability of vocalization for fixed + random effects
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# Extract fixed effects for the conditional model
fixef_m2_cond <- fixef(m2)$cond

# Extract random effects for each species (all common species)
ranef_common <- ranef(m2, condVar = TRUE)$cond$common

# Create a data frame to store the results
pred_probs <- data.frame(species = rownames(ranef_common), prob_eclipse_0 = NA, prob_eclipse_1 = NA)

# Loop through each species and calculate probabilities
for (i in 1:nrow(ranef_common)) {
  species <- rownames(ranef_common)[i]
  ranef_values <- ranef_common[i, ]
  
  # Calculate log-odds for both levels of eclipse (0 and 1)
  log_odds_eclipse_0 <- fixef_m2_cond[1] + ranef_values[1]  # Intercept for eclipse = 0
  log_odds_eclipse_1 <- fixef_m2_cond[1] + fixef_m2_cond[2] + ranef_values[1] + ranef_values[2]  # Intercept + eclipse effect for eclipse = 1
  
  # Convert log-odds to probabilities
  prob_eclipse_0 <- 1 / (1 + exp(-log_odds_eclipse_0))
  prob_eclipse_1 <- 1 / (1 + exp(-log_odds_eclipse_1))
  
  # Store the probabilities
  pred_probs$prob_eclipse_0[i] <- prob_eclipse_0
  pred_probs$prob_eclipse_1[i] <- prob_eclipse_1
}

# Ensure the columns are numeric, not lists
pred_probs$prob_eclipse_0 <- unlist(pred_probs$prob_eclipse_0)
pred_probs$prob_eclipse_1 <- unlist(pred_probs$prob_eclipse_1)
pred_probs$difference <- pred_probs$prob_eclipse_1 - pred_probs$prob_eclipse_0
pred_probs$effect <- ifelse(pred_probs$prob_eclipse_1 > pred_probs$prob_eclipse_0, "Increased Vocalization", "Decreased Vocalization")
pred_probs$direction <- ifelse(pred_probs$difference > 0, "Increased", "Decreased")

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# EXAMPLE SPECIES - TUFTED TITMOUSE ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

Example <- pred_probs[pred_probs$species == 'Tufted Titmouse',]

gg <- ggplot(Example, aes(x=species, y=difference, label=difference)) + 
  geom_bar(stat='identity', aes(fill=direction), width=.5) + 
  scale_fill_manual(name="Effect of Eclipse", 
                    labels = c("Decreased Vocalization"), 
                    values = c("Decreased"="gold")) + 
  labs(y="Change in Vocalization Activity", x = "Speices") + 
  coord_flip()

gg <- gg + theme_bw()
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(legend.position="top")
gg <- gg + theme(panel.border=element_blank(),
                 axis.title = element_text(size = 18),
                 axis.text.x = element_text(size = 14))
gg
ggsave(plot = gg, filename = here('Results/Figures/prob_of_vocal_eclipse_EXAMPLE.png'),
       width = 8, height = 8, dpi = 300)

# 
# 
# gg <- ggplot(Example, aes(x=prob_eclipse_0 , xend=prob_eclipse_1, y=reorder(species, difference), group=species))
# gg <- gg + geom_dumbbell(colour="gold", size=3, colour_xend="black", 
#                          dot_guide=T, dot_guide_size=0.15)
# #gg <- gg + geom_vline(xintercept = 0)
# #gg <- gg + scale_x_continuous(limits = c(0,1))
# gg <- gg + labs(x="Probability of Vocalization", y=NULL)
# gg <- gg + theme_bw()
# gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
# gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
# gg <- gg + theme(panel.grid.minor=element_blank())
# gg <- gg + theme(panel.grid.major.y=element_blank())
# gg <- gg + theme(panel.grid.major.x=element_line())
# #gg <- gg + theme(axis.ticks=element_blank())
# #gg <- gg + theme(axis.text.x=element_blank())
# gg <- gg + theme(legend.position="top")
# gg <- gg + theme(panel.border=element_blank(),
#                  axis.title = element_text(size = 18),
#                  axis.text = element_text(size = 14))
# 
# 
# ggsave(plot = gg, filename = here('Results/Figures/prob_of_vocal_eclipse_EXAMPLE.png'),
#        width = 8, height = 8, dpi = 300)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

## ALL SPECIES ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
preds_sorted <- pred_probs[order(pred_probs$difference), ]
preds_sorted$species <- factor(preds_sorted$species, levels = preds_sorted$species)  # convert to factor to retain sorted order in plot
# trying again
gg <- ggplot(preds_sorted, aes(x=species, y=difference, label=difference)) + 
  geom_bar(stat='identity', aes(fill=direction), width=.5) + 
  scale_fill_manual(name="Effect of Eclipse", 
                    labels = c("Decreased Vocalization", "Increased Vocalization"), 
                    values = c("Increased"="black", "Decreased"="gold")) + 
  labs(y="Change in Vocalization Activity", x = "Speices") + 
  coord_flip()

gg <- gg + theme_bw()
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(legend.position="top")
gg <- gg + theme(panel.border=element_blank(),
                 axis.title = element_text(size = 18),
                 axis.text.x = element_text(size = 14))
gg
ggsave(plot = gg, filename = here('Results/Figures/prob_of_vocal_eclipse_species.png'),
       width = 8, height = 8, dpi = 300)

# gg <- ggplot(pred_probs, aes(x=prob_eclipse_0 , xend=prob_eclipse_1, y=reorder(species, difference), group=species))
# gg <- gg + geom_dumbbell(colour="gold", size=3, colour_xend="black", 
#                          dot_guide=T, dot_guide_size=0.15)
# 
# gg <- gg + labs(x="Probability of Vocalization", y=NULL)
# gg <- gg + theme_bw()
# gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
# gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
# gg <- gg + theme(panel.grid.minor=element_blank())
# gg <- gg + theme(panel.grid.major.y=element_blank())
# gg <- gg + theme(panel.grid.major.x=element_line())
# gg <- gg + theme(legend.position="top")
# gg <- gg + theme(panel.border=element_blank(),
#                  axis.title = element_text(size = 18),
#                  axis.text.x = element_text(size = 14))
# gg <- gg + facet_grid(~effect)
# 
# ggsave(plot = gg, filename = here('Results/Figures/prob_of_vocal_eclipse_species.png'),
#        width = 8, height = 8, dpi = 300)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## FAMILY LEVEL ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# estimates negative effect of eclipse; think we should maybe have a random intercept for station/sp tho...?
m1 <- glmmTMB::glmmTMB(
  y ~ 1 + factor(eclipse) + activity + (1 + factor(eclipse) | family),
  data = filtered_det_fam,
  family = binomial(link = "logit")) 

#
m2 <- glmmTMB::glmmTMB(
  y ~ 1 + factor(eclipse) + (1 + factor(eclipse)|family) + (1|station),
  data = filtered_det_fam,
  family = binomial(link = "logit"),
) 

AIC(m1, m2) #m2 is better



# Extract fixed effects for the conditional model
fixef_m2_cond <- fixef(m2)$cond

# Extract random effects for each species (all common species)
ranef_family <- ranef(m2, condVar = TRUE)$cond$family

# Create a data frame to store the results
pred_probs <- data.frame(families = rownames(ranef_family), prob_eclipse_0 = NA, prob_eclipse_1 = NA)

# Loop through each species and calculate probabilities
for (i in 1:nrow(ranef_family)) {
  families <- rownames(ranef_family)[i]
  ranef_values <- ranef_family[i, ]
  
  # Calculate log-odds for both levels of eclipse (0 and 1)
  log_odds_eclipse_0 <- fixef_m2_cond[1] + ranef_values[1]  # Intercept for eclipse = 0
  log_odds_eclipse_1 <- fixef_m2_cond[1] + fixef_m2_cond[2] + ranef_values[1] + ranef_values[2]  # Intercept + eclipse effect for eclipse = 1
  
  # Convert log-odds to probabilities
  prob_eclipse_0 <- 1 / (1 + exp(-log_odds_eclipse_0))
  prob_eclipse_1 <- 1 / (1 + exp(-log_odds_eclipse_1))
  
  # Store the probabilities
  pred_probs$prob_eclipse_0[i] <- prob_eclipse_0
  pred_probs$prob_eclipse_1[i] <- prob_eclipse_1
}

# Ensure the columns are numeric, not lists
pred_probs$prob_eclipse_0 <- unlist(pred_probs$prob_eclipse_0)
pred_probs$prob_eclipse_1 <- unlist(pred_probs$prob_eclipse_1)
pred_probs$difference <- pred_probs$prob_eclipse_1 - pred_probs$prob_eclipse_0
pred_probs$effect <- ifelse(pred_probs$prob_eclipse_1 > pred_probs$prob_eclipse_0, "Increased Vocalization", "Decreased Vocalization")
pred_probs$direction <- ifelse(pred_probs$difference > 0, "Increased", "Decreased")

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## ALL FAMILIES ####
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
preds_sorted <- pred_probs[order(pred_probs$difference), ]
preds_sorted$families <- factor(preds_sorted$families, levels = preds_sorted$families)  # convert to factor to retain sorted order in plot
# trying again
gg <- ggplot(preds_sorted, aes(x=families, y=difference, label=difference)) + 
  geom_bar(stat='identity', aes(fill=direction), width=.5) + 
  scale_fill_manual(name="Effect of Eclipse", 
                    labels = c("Decreased Vocalization", "Increased Vocalization"), 
                    values = c("Increased"="black", "Decreased"="gold")) + 
  labs(y="Change in Vocalization Activity", x = "Family") + 
  coord_flip()

gg <- gg + theme_bw()
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(legend.position="top")
gg <- gg + theme(panel.border=element_blank(),
                 axis.title = element_text(size = 18),
                 axis.text.x = element_text(size = 14))
gg
ggsave(plot = gg, filename = here('Results/Figures/prob_of_vocal_eclipse_family.png'),
       width = 8, height = 8, dpi = 300)

# 
# gg <- ggplot(pred_probs, aes(x=prob_eclipse_0 , xend=prob_eclipse_1, y=reorder(families, difference), group=families))
# gg <- gg + geom_dumbbell(colour="gold", size=3, colour_xend="black", 
#                          dot_guide=T, dot_guide_size=0.15)
# 
# gg <- gg + labs(x="Probability of Vocalization", y=NULL)
# gg <- gg + theme_bw()
# gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
# gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
# gg <- gg + theme(panel.grid.minor=element_blank())
# gg <- gg + theme(panel.grid.major.y=element_blank())
# gg <- gg + theme(panel.grid.major.x=element_line())
# gg <- gg + theme(legend.position="top")
# gg <- gg + theme(panel.border=element_blank(),
#                  axis.title = element_text(size = 18),
#                  axis.text.x = element_text(size = 14))
# gg <- gg + facet_grid(~effect)
# 
# ggsave(plot = gg, filename = here('Results/Figures/prob_of_vocal_eclipse_family.png'),
#        width = 8, height = 8, dpi = 300)
