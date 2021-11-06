#This script was used to perform hypothesis testings not contained in plots.
#It will pull 3 outputs from 'preprocessing.R' srcipt, so please run it before
#using this script

#Enjoy :)

#### Time ####

#### Test differences on richness through time ####
#What is the minimum time that achieve statistical significance with 10 min?

#Load data
Accs <- readRDS('R_Objects/Accs.rds')

#Sum entries to get richness
cum_rich <- lapply(seq_along(Accs), function(x, Accs) data.frame(richness = rowSums(Accs[[x]]),
                                                                 time = rep(x, nrow(Accs[[x]])),
                                                                 video = rownames(Accs[[x]])), Accs)
#Merge items into a data.frame
cum_rich <- plyr::rbind.fill(cum_rich)
colnames(cum_rich)
library(ggplot2)
#Observe raw data
ggplot(cum_rich, aes(x = time, y = richness)) +
  geom_line(aes(group = video, color = time)) +
  geom_smooth() + theme_minimal()

#Transform x axis with log to linearize tendencies
ggplot(cum_rich, aes(x = time, y = richness)) +
  geom_line(aes(group = video, color = time)) +
  geom_smooth()+ theme_minimal() +
  scale_x_log10()


#Test if recorded number of species increases with time
#Richness as response, time as numeric and video as random
LMM <- lme4::lmer(richness ~ log(time) + (log(time) | video), cum_rich)

#Add predicted values to the initial data.frame
cum_rich$mod <- predict(LMM, cum_rich)

#Add sites as a column to cum_rich
cum_rich$sites <- vapply(strsplit(cum_rich$video, "_"), function(x) x[1], character(1))

head(cum_rich)
#Graph1: rich vs time
pl1 <- cum_rich %>%
  plyr::mutate(sites = forcats::fct_relevel(sites, 
                                            c("Parrachos",
                                              "Cabeco",
                                              "Barreirinha",
                                              "BatenteAgulhas",
                                              "Pedra"))) %>%
  plyr::mutate(Sites =plyr::revalue(sites,
                                    c('Barreirinha'="Barreirinha",
                                      'BatenteAgulhas' = "Batente das Agulhas", 
                                      'Cabeco' = "Cabeço do Leandro", 
                                      'Parrachos'="Parrachos (Rio do Fogo)",
                                      "Pedra" = "Pedra do Silva"))) %>%
  ggplot(aes(x = time, color = Sites, fill = Sites)) +
  geom_line(aes(y = richness, group = video), alpha = 0.3, 
            position = position_jitter(width = 0.2, height = 0.2)) +
  geom_smooth(aes(y = richness), se = FALSE, linetype = 2) +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Richness", x = "Observed Time", 
       title = "Recorded accumulation") +
  theme_classic() +
  theme(panel.grid = element_blank())
pl1

#Graph2: pred rich vs time
pl2 <- cum_rich %>%
  plyr::mutate(sites = forcats::fct_relevel(sites, 
                                            c("Parrachos",
                                              "Cabeco",
                                              "Barreirinha",
                                              "BatenteAgulhas",
                                              "Pedra"))) %>%
  plyr::mutate(Sites =plyr::revalue(sites,
                                    c('Barreirinha'="Barreirinha",
                                      'BatenteAgulhas' = "Batente das Agulhas", 
                                      'Cabeco' = "Cabeço do Leandro", 
                                      'Parrachos'="Parrachos (Rio do Fogo)",
                                      "Pedra" = "Pedra do Silva"))) %>%
  ggplot(aes(x = time, color = Sites, fill = Sites)) +
  geom_line(aes(y = mod, group = video), alpha = 0.3) +
  geom_smooth(aes(y = mod), se = FALSE) +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Richness", x = "Observed Time", 
       title = "Predicted accumulation") +
  theme_classic() + theme(panel.grid = element_blank())
pl2

pl3 <- cum_rich %>%
  plyr::mutate(sites = forcats::fct_relevel(sites, 
                                            c("Parrachos",
                                              "Cabeco",
                                              "Barreirinha",
                                              "BatenteAgulhas",
                                              "Pedra"))) %>%
  plyr::mutate(Sites =plyr::revalue(sites,
                                    c('Barreirinha'="Barreirinha",
                                      'BatenteAgulhas' = "Batente das Agulhas", 
                                      'Cabeco' = "Cabeço do Leandro", 
                                      'Parrachos'="Parrachos (Rio do Fogo)",
                                      "Pedra" = "Pedra do Silva"))) %>%
  ggplot(aes(x = time)) +
  geom_smooth(aes(y = mod, color = Sites, fill = Sites), 
              linetype = 2, alpha = .1) +
  geom_smooth(aes(y = richness, color = Sites, fill = Sites), se = FALSE) +

  # geom_smooth(aes(y = richness), se = TRUE, col = "black",
              # size = 2, alpha = .6) +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Richness", x = "Observed Time",
       title = "Predicted vs Recorded (sites averages) ") +
  theme_classic() + theme(panel.grid.minor = element_blank())
pl3

pl4 <- cum_rich %>%
  plyr::mutate(sites = forcats::fct_relevel(sites, 
                                            c("Parrachos",
                                              "Cabeco",
                                              "Barreirinha",
                                              "BatenteAgulhas",
                                              "Pedra"))) %>%
  plyr::mutate(Sites =plyr::revalue(sites,
                                    c('Barreirinha'="Barreirinha",
                                      'BatenteAgulhas' = "Batente das Agulhas", 
                                      'Cabeco' = "Cabeço do Leandro", 
                                      'Parrachos'="Parrachos (Rio do Fogo)",
                                      "Pedra" = "Pedra do Silva"))) %>%
  ggplot(aes(x = time)) +
  geom_smooth(aes(y = mod), linetype = 2, alpha = .1, col = "black") +
  geom_smooth(aes(y = richness), se = FALSE, col = "black") +
  scale_color_viridis_d() + scale_fill_viridis_d() +
  scale_x_continuous(breaks = 1:10) +
  labs(y = "Richness", x = "Observed Time", 
       title = "Predicted vs Recorded (average) ") +
  theme_classic() + theme(panel.grid.minor = element_blank())
pl4
pl1 + pl2 + pl3 + patchwork::plot_layout(ncol = 3, guides = "collect")
ggpubr::ggarrange(plotlist = list(pl1, pl2, pl3, pl4), common.legend = TRUE)

summary(LMM)
performance::r2_nakagawa(LMM)


#Observe residuals
res <- data.frame('Time' = cum_rich$time, "Video" = cum_rich$video, 
                  'Richness' = cum_rich$richness,
                  'Fitted' = fitted(LMM), 'Residuals' = resid(LMM))

  #LMM Residuals vs Fitted  

resgraph1 <- ggplot(aes(x = Fitted, y = Residuals), data = res) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  coord_cartesian(ylim = c(-2.6, 2.6)) +
  theme_minimal() + labs(title = 'Residuals vs Fitted Values') +
    #LMM Residuals vs Response
    ggplot(aes(x = Richness, y = Residuals), data = res) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_point(aes(color = Time)) +
    scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                               "#F78800", "#F90004"),
                                   breaks = waiver(), n.breaks = 10,
                                   name = 'Time') +
    labs(title = 'Residuals vs Response') +
    coord_cartesian(ylim = c(-2.6, 2.6))+
    theme_minimal() + 
    
  #Residual vs Predictor
  ggplot(aes(x = Time, y = Residuals), data = res) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time), alpha = 0.5) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  coord_cartesian(ylim = c(-6, 6)) +
  theme_minimal() + labs(title = 'Residuals vs Predictor') +
  
  #Fitted vs Observed
  ggplot(aes(x = Richness, y = Fitted), data = res) +
  geom_smooth(method = 'lm', se = FALSE, color = 'grey', 
                       size = 0.5, formula = y ~ x) +
  geom_point(aes(color = Time), alpha = 0.5) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  theme_minimal() + labs(title = 'Fitted vs Observed') +
  
  #Residuals density  
  ggplot(aes(x = Residuals), data = res) +
  geom_histogram(aes(y =..density..), alpha=0.8, bins = 23,
                          fill = rgb(0.6, 0.6, 0.8), color =  rgb(0.5, 0.5, 0.8)) +
  stat_function(fun = dnorm, args = list(mean = mean(res$Residuals), 
                                                  sd = sd(res$Residuals))) +
  labs(title = 'Residuals Density') + 
  xlab( 'Residuals') + ylab ('Density') +
  coord_cartesian(xlim = c(-3,3), ylim = c(0,0.6)) +
  theme_minimal() +
  
  # QQ plot
  ggplot(aes(sample = Residuals), data = res) +
  qqplotr::stat_qq_band(bandType = "ks", fill = "#8DA0CB", alpha = 0.4) +
  qqplotr::stat_qq_line(colour = "#8DA0CB") +
  qqplotr::stat_qq_point(alpha = 0.2) +
  xlab("Theoretical quantiles") + ylab("Sample Quantiles") +
  labs(title = 'QQ Plot (Kolmogorov conf.)') +
  theme_minimal()

resgraph1 + patchwork::plot_layout(guides = 'collect')

#Looks acceptable


#This function extracts multiple comparison outputs and convert it to letters
let <- function(mc, factor_name = 'as.factor(time)') {
  require(multcomp)
  if (any(class(mc) == 'TukeyHSD')) {
    r <- `names<-`(mc[['as.factor(time)']][, 'p adj'],
                   unlist(lapply(stringr::str_split(rownames(mc[['as.factor(time)']]), '-'), 
                                 function(x) paste(rev(x), collapse = '-'))))
  } else if (any(class(mc) == 'glht')) {
    r <- summary(mc, test = adjusted(type = "bonferroni"))$test$pvalues
    names(r) <- unlist(lapply(stringr::str_split(names(r), ' - '), 
                              function(x) paste(rev(x), collapse = '-')))
  } else if (any(class(mc) == 'pwadonis')) {
    r <- `names<-`(mc$p.value,
                   unlist(lapply(stringr::str_split(mc$pairs, ' vs '),  
                                 function(x) paste(x, collapse = '-'))))
  } else stop("Unknown class. Methods described only for 'pwadonis', 'TukeyHSD' and 'glht'")
  return(multcompView::multcompLetters(r))
}


#Without considering video nested design
mc <- TukeyHSD(aov(richness ~ as.factor(time), cum_rich))
(mcletters <- let(mc))
#5 min is the mininum
mcletters <- data.frame(levels = mcletters$Letters, time = names(mcletters$Letters))

saveRDS(mcletters, 'R_Objects/mcletters.rds')

#### Plot richness through time ####
#Use violin plots to represent differences between time steps
Accs <- readRDS('R_Objects/Accs.rds')

cum_rich <- lapply(seq_along(Accs), function(x, Accs) data.frame(richness = rowSums(Accs[[x]]),
                                                                 time = rep(x, nrow(Accs[[x]])),
                                                                 video = rownames(Accs[[x]])), Accs)
cum_rich <- plyr::rbind.fill(cum_rich)

#Retrieve post-hoc comparison results
mcletters <- readRDS('R_Objects/mcletters.rds')

#Plot
violin <- ggplot(cum_rich, aes(x = as.factor(time), y = richness)) +
  geom_violin(aes(group = time, color = time, fill = time),
                       scale = 'width', trim = FALSE, alpha = 0.2,
                       draw_quantiles = c(0.25, 0.75)) +
  ylim(0, NA) +
  scale_fill_gradientn(colours = c("#004C8E", "#52BF00", 
                                            "#F78800", "#F90004"),
                                breaks = waiver(), n.breaks = 10,
                                name = 'Time') +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  stat_summary(aes(ymin = richness, ymax = richness,
                                     group = time, color = time, fill = time),
                        geom = "crossbar", fun = "mean", alpha = 0, fatten = 3) +
  geom_text(aes(x = time,
                                  y = c(14, 19, 24.5, 25.5, 26, 27, 28, 28.5, 29, 30.5)+1,
                                  #     1   2   3      4    5    6  7    8    9    10
                                  label = levels),
                     data = mcletters, colour = 'black') +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())+
  labs(title = "a. Richness through time",
                x = "Observed Time", 
                y = "Richness")
violin

#### Test Differences in composition through time ####
#Load original acc object to draw ellipses based on minutes
Accs <- readRDS('R_Objects/Accs.rds')

#Add time and video as columns
accs <- Map(function(x, accs) {
  accs$video <- rownames(accs)
  accs$time <- rep(x, nrow(accs))
  accs
}, x = seq_along(Accs), accs = Accs)

#Merge data.frames into a single object
accs <- plyr::rbind.fill(accs)

# Test if composition changes with time, adding videos as predictors
cmp_tst <- vegan::adonis(accs[, !colnames(accs) %in% c("time", "video")] ~ video + log(time), 
                         data = accs, method = 'bray', by = 'terms', 
                         parallel = getOption("mc.cores", 8L))
cmp_tst

# This function performs the adonis algorith pairwisely
#It Requires as input a community matrix (as a data.frame) and a 
#vector of factors containing the desired groups to compare. It is possible
#to chose the dissimilarity method and to adjust or not
#the p values of pairwise comparisons
#Further methods are passed down to vegan's adonis
pairwise_adonis <- function(x, factors, sim.method = 'bray', 
                            p.adjust.m ='none', ...) {
  
  co <- combn(unique(as.character(factors)),2)
  pairs <- c()
  F.Model <- c()
  R2 <- c()
  p.value <- c()
  
  
  for(elem in 1:ncol(co)){
    ad <- vegan::adonis(x[factors %in% c(co[1, elem], co[2, elem]), ] ~ 
                          factors[factors %in% c(co[1, elem], co[2, elem])], 
                        method = sim.method)
    pairs <- c(pairs, paste(co[1, elem], 'vs', co[2, elem]))
    F.Model <- c(F.Model, ad$aov.tab[1, 4])
    R2 <- c(R2, ad$aov.tab[1, 5])
    p.value <- c(p.value, ad$aov.tab[1, 6])
  }
  
  sig = c(rep('', length(p.value)))
  sig[p.value <= 0.1] <-'.'
  sig[p.value <= 0.05] <-'*'
  sig[p.value <= 0.01] <-'**'
  sig[p.value <= 0.001] <-'***'
  pairw.res <- data.frame(pairs, F.model = round(F.Model, 2),
                          R2 = round(R2, 2), 
                          p.value = p.adjust(p.value, p.adjust.m))
  
  
  class(pairw.res) <- c("pwadonis", "data.frame") 
  return(pairw.res)
} 

pw_cmp <- pairwise_adonis(accs[,!colnames(accs) %in% c("time", "video")], factors = as.factor(accs$time), 
                          sim.method = 'bray', parallel = getOption("mc.cores", 8L))
# write.csv(pw_cmp, 'R_Objects/pwcmp.csv')
let(pw_cmp)
(mcletters_comp <-  data.frame('levels' = let(pw_cmp)$Letters, 'time' = names(let(pw_cmp)$Letters)))

saveRDS(mcletters_comp, 'R_Objects/mcletters_comp.rds')

#### Plot Composition though time ####
# Goal here is to observe how species composition changed with time

#Load original acc object to draw ellipses based on minutes
Accs <- readRDS('R_Objects/Accs.rds')

#Add time and video as columns
accs <- Map(function(x, accs) {
  accs$video <- rownames(accs)
  accs$time <- rep(x, nrow(accs))
  accs
}, x = seq_along(Accs), accs = Accs)

#Merge data.frames into a single object
accs <- plyr::rbind.fill(accs)

# Apply NMDS and get scores 
# WARNING: May take a while
NMDS <- vegan::scores(vegan::metaMDS(accs[, !colnames(accs) %in%
                                            c("time", "video")], parallel = getOption("mc.cores", 8L),
                                     distance = 'bray', try = 30, trymax = 1000))

#Add minutes and videos as a columns
NMDS <- data.frame(NMDS, time = accs$time, video = accs$video)

#Plot
#Retrieve post-hoc comparison results
mcletters_comp <- readRDS('R_Objects/mcletters_comp.rds')
mcletters_comp

#Define coordinates based on NMDS first axis
ycoord <- seq(min(NMDS$NMDS2), max(NMDS$NMDS2), length.out = 10) * 1.1
xcoord <- rep(2.1, 10)

mcletters_comp$xcoord <- xcoord
mcletters_comp$ycoord <- ycoord

SppNMDS <- ggplot(NMDS, aes(x = NMDS1, y = NMDS2)) +
  # geom_line(aes(group = video, color = time), 
  #                    alpha = 0.3)  +
  geom_point(aes(color = time), 
                      alpha = 0.3)  +
  stat_ellipse(aes(group = time, color = time)) +
  scale_fill_gradientn(colours = c("#004C8E", "#52BF00", 
                                            "#F78800", "#F90004"),
                                breaks = waiver(), n.breaks = 10,
                                name = 'Time') +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  geom_text(aes(x = xcoord, y = ycoord,
                                  label = rev(levels)),
                     data = mcletters_comp, colour = 'black') +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())+
  labs(title = "b. Composition through time",
                x = "NMDS 1", 
                y = "NMDS 2")
SppNMDS

#### Number of Videos ####

#### Richness accumulation curves ####

Accs <- readRDS('R_Objects/Accs.rds')

# Apply random accumulation curves
sppaccs <- lapply(Accs, vegan::specaccum, method = 'random')

# Extract results
sppaccs <- plyr::rbind.fill(lapply(seq_along(sppaccs), function(x) {
  d <- data.frame(n = sppaccs[[x]]$sites, 
                  richness = sppaccs[[x]]$richness, 
                  sd = sppaccs[[x]]$sd,
                  time = rep(x, length(sppaccs[[x]]$sites)),
                  rep1 = sppaccs[[1]]$richness)
}))


# Plot using ggplot2
SppAccum <- ggplot(sppaccs, aes(x = n, y = richness, group = time)) +
  geom_ribbon(aes(ymin = richness - sd, 
                                    ymax = richness + sd, fill = time), 
                       alpha = 0.2) +
  geom_line(aes(colour = time)) +
  scale_fill_gradientn(colours = c("#004C8E", "#52BF00", 
                                            "#F78800", "#F90004"),
                                breaks = waiver(), n.breaks = 10,
                                name = 'Time') +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())+
  labs(title = "c. Species accumulation",
                x = "Videos", 
                y = "Richness")
SppAccum

#### Broken stick regression on accumullation curves ####
# Goal here is to estimate break points to provide the minimum number of 
#videos required to sample each time


#Load Data
Accs <- readRDS('R_Objects/Accs.rds')

# Apply random accumulation curves
sppaccs <- lapply(Accs, vegan::specaccum, method = 'random')

# Extract results
sppaccs <- plyr::rbind.fill(lapply(seq_along(sppaccs), function(x) {
  d <- data.frame(n = sppaccs[[x]]$sites, 
                  richness = sppaccs[[x]]$richness, 
                  sd = sppaccs[[x]]$sd,
                  time = rep(x, length(sppaccs[[x]]$sites)),
                  rich10 = sppaccs[[10]]$richness)
}))

# Make sure time is a factor
sppaccs$time <- as.factor(sppaccs$time)

# Get interactions with model.matrix
X <- model.matrix(~ 0 + time, data = sppaccs) * sppaccs$n
colnames(X)

# Add them to the data
sppaccs[,c(6:15)] <- X
colnames(sppaccs) <- c(colnames(sppaccs[,1:5]), colnames(X[,c(1:10)]))
colnames(sppaccs)

#Make a linear model with a 0 intercept and varying slopes
mod <- lm(richness ~ 0 + time1 + time2 + time3 + time4 + time5 + 
            time6 + time7 + time8 + time9 + time10,
          data = sppaccs)

# Create a segmented model using 1 as starting value for
  smod1 <-segmented::segmented(mod, 
                             seg.Z = ~ time1 + time2 + time3 + time4 + time5 + 
                               time6 + time7 + time8 + time9 + time10, 
                             psi = `names<-`(split(rep(1, 10), 1:10), colnames(X)),
                             npsi = 1)

#Extract estimates
fst_trial <- smod1$psi[,"Est."]
summary(smod1)

#### LM Richness minimum of videos ####
#Test if the relation is quadratic
mod_bp <- lm(mod_bp$psi[,2] ~ poly(c(1:10), 2))
#It is!
summary(mod_bp)

# Extract 95% confidence interval of break point estimates for a plot
bp <- plyr::ldply(paste0('time', 1:10), function(x) segmented::confint.segmented(smod1, x))
bp$time <- as.numeric(rownames(bp))



#Observe model residuals
# Expect to see a poor model adjustment at the beginning with a subtle increase
res_bp <- data.frame('Time' = as.numeric(bp$time), "Videos" = bp$Est., 
                       'Fitted' = fitted(mod_bp), 'Residuals' = resid(mod_bp))

#Segmented LM Residuals vs Fitted  
resgraph2 <- ggplot(aes(x = Fitted, y = Residuals), data = res_bp) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +  
  coord_cartesian(ylim = c(-2.6, 2.6)) +
  theme_minimal() + labs(title = 'Residuals vs Fitted Values') +
  
  #Segmented LM Residuals vs Response
  ggplot(aes(x = Videos, y = Residuals), data = res_bp) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  labs(title = 'Residuals vs Response') +
  coord_cartesian(ylim = c(-2.6, 2.6))+
  theme_minimal() +  
  #Residual vs Predictor
  ggplot(aes(x = Time, y = Residuals), data = res_bp) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time), alpha = 0.5) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  coord_cartesian(ylim = c(-6, 6)) +
  theme_minimal() + labs(title = 'Residuals vs Predictor') +
  
  #Fitted vs Observed
  ggplot(aes(x = Videos, y = Fitted), data = res_bp) +
  geom_smooth(method = 'lm', se = FALSE, color = 'grey', 
                       size = 0.5, formula = y ~ x) +
  # geom_smooth(method = 'loess', se = FALSE, color = 'blue', 
  #                      size = 0.5, formula = y ~ x) +
  geom_point(aes(color = Time), alpha = 0.5) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  theme_minimal() + labs(title = 'Fitted vs Observed') +
  
  
  #Residuals density  
  ggplot(aes(x = Residuals), data = res_bp) +
  geom_histogram(aes(y =..density..), alpha=0.8, bins = 3,
                          fill = rgb(0.6, 0.6, 0.8), color =  rgb(0.5, 0.5, 0.8)) +
  stat_function(fun = dnorm, args = list(mean = mean(res_bp$Residuals), 
                                                  sd = sd(res_bp$Residuals))) +
  labs(title = 'Residuals Density') + 
  xlab( 'Residuals') + ylab ('Density') +
  # coord_cartesian(xlim = c(-3,3), ylim = c(0,0.6)) +
  theme_minimal() +
  
  # QQ plot
  ggplot(aes(sample = Residuals), data = res_bp) +
  qqplotr::stat_qq_band(bandType = "ks", fill = "#8DA0CB", alpha = 0.4) +
  qqplotr::stat_qq_line(colour = "#8DA0CB") +
  qqplotr::stat_qq_point(alpha = 0.2) +
  xlab("Theoretical quantiles") + ylab("Sample Quantiles") +
  labs(title = 'QQ Plot (Kolmogorov conf.)') +
  theme_minimal() 

resgraph2 + patchwork::plot_layout(guides = 'collect')


#Make the plot
colnames(bp)
BreakAccs <- ggplot(data = bp, aes(x = time, y = Est., )) +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1, alpha = 0.3) +
  geom_pointrange(aes(ymin = `CI(95%).low`,
                                        ymax = `CI(95%).up`,
                                        color = time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  # coord_cartesian(ylim = c(0,10)) +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())+
  labs(title = "a. Minimum effort for richness",
                x = "Video length", 
                y = "Breakpoints")

BreakAccs
#### Correlation in composition subsets ####
# The goal is to compare how composition changes with an increasing number of 
#videos

# This function calculates how the similarity in detected composition builds up 
#as more videos are analyzed. It is based on how much of the total richness is 
#detected as more replicates are added, using as measure of association the 
#Jaccard index

comp_dist <- function(mat, ref, nperm, method = 'jaccard', mc = getOption("mc.cores", 4L)) {
  #Test if mat and ref matches in number of species
  stopifnot(all(dim(mat) == dim(ref)), colnames(mat) == colnames(ref))
  
  #Randomization process
  trials <- parallel::mclapply(seq_len(nrow(mat)), function(y) {
    #Create a data.frame to fill
    dst <- data.frame(matrix(colSums(ref) > 0, nrow = 1, dimnames = list('Ref', colnames(ref))))
    #Add randomization as rows
    for (z in seq_len(nperm)) {
      dst[(1 + z), ] <- as.numeric(colSums(mat[sample(nrow(mat),y),]) > 0)
    }
    #Make a dist object and take just the first column
    dst <- as.matrix(vegan::vegdist(dst, method = method))[,1]
    #Return mean and SD
    return(data.frame(mn = mean((1 - dst)[-1]),
                      sd = sd((1 - dst)[-1])))
  }, mc.cores = mc)
  
    cat('Done \n')
  return(plyr::rbind.fill(trials))
}

#Apply function to all times
system.time(
compsim <- lapply(Accs, comp_dist, ref = Accs[[10]], nperm = 1000, method = 'bray',
                  mc = getOption("mc.cores", 8L))
)

names(compsim) <- 1:10

# Merge into a single object
compsim <- plyr::rbind.fill(
Map(function(x,y) {
  x$videos = seq_len(nrow(x))
  x$time = rep(y, nrow(x))
  x
}, compsim, names(compsim)))

# Plot using ggplot2
CompCor <- ggplot(compsim, aes(x = videos, y = mn, group = as.integer(time))) +
  geom_ribbon(aes(ymin = mn - sd, 
                                    ymax = mn + sd, 
                                    fill = as.integer(time)),  alpha = 0.2) +
  geom_line(aes(colour = as.integer(time))) +
  # geom_hline(yintercept = .5, linetype = 2, alpha = 0.3) +
  # geom_vline(xintercept = c(3, 5, 20), linetype = 2, alpha = 0.3) +
  coord_cartesian(ylim = c(0,1)) +
  scale_fill_gradientn(colours = c("#004C8E", "#52BF00", 
                                            "#F78800", "#F90004"),
                                breaks = waiver(), n.breaks = 10,
                                name = 'Time') +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())+
  labs(title = "d. Similarity in species composition",
                x = "Videos", 
                y = "Sorensen similarity index")
CompCor

#### Broken stick regression on composition similarity curves ####
# Goal here is to estimate break points to provide the minimum number of 
#videos required to sample composition

# Make sure time is a factor
compsim$time <- as.factor(compsim$time)

# Get interactions with model.matrix
X2 <- model.matrix(~ 0 + time, data = compsim) * compsim$videos
colnames(X2)

# Add them to the data
compsim <- cbind(compsim, X)

#Make a linear model with a fixed intercept and varying slopes
compmod <- lm(mn ~ 0 + time1 + time2 + time3 + time4 + time5 + 
            time6 + time7 + time8 + time9 + time10,
          data = compsim)

# Create a segmented model using 1 as starting value for
compsmod1 <-segmented::segmented(compmod, 
                             seg.Z = ~ time1 + time2 + time3 + time4 + time5 + 
                               time6 + time7 + time8 + time9 + time10, 
                             psi = `names<-`(split(rep(1, 10), 1:10), colnames(X2)),
                             npsi = 1)

#Extract estimates
comp_fst_trial <- compsmod1$psi[,"Est."]
names(comp_fst_trial) <- stringr::str_remove(names(comp_fst_trial), 'psi1.')
# Use estimates provided by the laste model as starts
compsmod2 <-segmented::segmented(compmod, 
                             seg.Z = ~ time1 + time2 + time3 + time4 + time5 + 
                               time6 + time7 + time8 + time9 + time10, 
                             psi = split(as.numeric(comp_fst_trial), names(comp_fst_trial)),
                             npsi = 1)

#Estimates appears to follow a quadratic decrease
(compmod_bp <- summary(compsmod2))



#### LM Similarity minimum of videos ####
#Test if the relation is quadratic
compmod_bp <- lm(compmod_bp$psi[,2] ~ poly(c(1:10), 2))
#It is!
summary(compmod_bp)

# Extract 95% confidence interval of break point estimates for a plot
compbp <- plyr::ldply(paste0('time', 1:10), function(x) segmented::confint.segmented(compsmod2, x))
compbp$time <- as.numeric(rownames(compbp))


#Observe model residuals
# Expect to see a poor model adjustment at the beginning with a subtle increase
res_compbp <- data.frame('Time' = as.numeric(compbp$time), "Videos" = compbp$Est., 
                     'Fitted' = fitted(compmod_bp), 'Residuals' = resid(compmod_bp))

#Segmented LM Residuals vs Fitted  
resgraph3 <- ggplot(aes(x = Fitted, y = Residuals), data = res_compbp) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +  
  coord_cartesian(ylim = c(-2.6, 2.6)) +
  theme_minimal() + labs(title = 'Residuals vs Fitted Values') +
  
  #Segmented LM Residuals vs Response
  ggplot(aes(x = Videos, y = Residuals), data = res_compbp) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  labs(title = 'Residuals vs Response') +
  coord_cartesian(ylim = c(-2.6, 2.6))+
  theme_minimal() +  
  #Residual vs Predictor
  ggplot(aes(x = Time, y = Residuals), data = res_compbp) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(aes(color = Time), alpha = 0.5) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  coord_cartesian(ylim = c(-6, 6)) +
  theme_minimal() + labs(title = 'Residuals vs Predictor') +
  
  #Fitted vs Observed
  ggplot(aes(x = Videos, y = Fitted), data = res_compbp) +
  geom_smooth(method = 'lm', se = FALSE, color = 'grey', 
                       size = 0.5, formula = y ~ x) +
  # geom_smooth(method = 'loess', se = FALSE, color = 'blue', 
  #                      size = 0.5, formula = y ~ x) +
  geom_point(aes(color = Time), alpha = 0.5) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  theme_minimal() + labs(title = 'Fitted vs Observed') +
  
  #Residuals density  
  ggplot(aes(x = Residuals), data = res_compbp) +
  geom_histogram(aes(y =..density..), alpha=0.8, bins = 3,
                          fill = rgb(0.6, 0.6, 0.8), color =  rgb(0.5, 0.5, 0.8)) +
  stat_function(fun = dnorm, args = list(mean = mean(res_compbp$Residuals), 
                                                  sd = sd(res_compbp$Residuals))) +
  labs(title = 'Residuals Density') + 
  xlab( 'Residuals') + ylab ('Density') +
  # coord_cartesian(xlim = c(-3,3), ylim = c(0,0.6)) +
  theme_minimal() +
  
  # QQ plot
  ggplot(aes(sample = Residuals), data = res_compbp) +
  qqplotr::stat_qq_band(bandType = "ks", fill = "#8DA0CB", alpha = 0.4) +
  qqplotr::stat_qq_line(colour = "#8DA0CB") +
  qqplotr::stat_qq_point(alpha = 0.2) +
  xlab("Theoretical quantiles") + ylab("Sample Quantiles") +
  labs(title = 'QQ Plot (Kolmogorov conf.)') +
  theme_minimal() 

resgraph3 + patchwork::plot_layout(guides = 'collect')


#Make the plot
colnames(compbp)
BreakSim <- ggplot(data = compbp, aes(x = time, y = Est., )) +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 1, alpha = 0.3) +
  geom_pointrange(aes(ymin = `CI(95%).low`,
                                        ymax = `CI(95%).up`,
                                        color = time)) +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  # coord_cartesian(ylim = c(0,10)) +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())+
  labs(title = "b. Minimum effort for similarity",
                x = "Video length", 
                y = "Breakpoints")

BreakAccs + BreakSim

#### Plate 1 ####

Graph1 <- violin + SppNMDS + SppAccum + CompCor + #BreakAccs + BreakSim + 
  patchwork::plot_layout(guides = 'collect', byrow = FALSE)
Graph1

# Residuals from LMM, lm 1 and lm 2
# ggpubr::ggarrange(resgraph1[[1]], resgraph1[[2]], resgraph1[[3]], 
#                   resgraph1[[4]], resgraph1[[5]], resgraph1[[6]],
#                   ncol = 1, common.legend = TRUE) +
  ggpubr::ggarrange(resgraph2[[1]], resgraph2[[2]], resgraph2[[3]], 
                    resgraph2[[4]], resgraph2[[5]], resgraph2[[6]],
                    ncol = 3, nrow = 2, common.legend = TRUE) +
  ggpubr::ggarrange(resgraph3[[1]], resgraph3[[2]], resgraph3[[3]], 
                    resgraph3[[4]], resgraph3[[5]], resgraph3[[6]],
                    ncol = 3, nrow = 2, common.legend = TRUE) +
  patchwork::plot_layout(nrow = 2, guides = 'collect')


#### Permute videos and minutes order ####
records <- readRDS('R_Objects/records.rds')

perm_by_strata <- function(records, videos, minutes, nperm, permute = c('minutes', 'videos')) {
  require(permute)
  #Check inputs
  stopifnot(is.data.frame(records) | is.matrix(records) , 
            is.numeric(minutes), is.character(videos) | is.factor(videos))
  
  #Function to fill in rows with 1
  place_1 <- function(x) {
    #Disconsider 0 filled collumns
    zero <- colSums(x) > 0
    # Find detection on each row
    vec <- apply(x[,zero], c(2), function(y) Position(function(w) w == 1, y))
    #roll across x and vec pointing which rows of x are larger than vec
    #This indicate where to place 1
    rplc <- simplify2array(
      Map(function(y, v) {
        yrows <- seq_len(length(y))
        return(yrows >= v)
      }, x, vec))
    #Place 1
    x[,zero][rplc] <- 1
    return(x)
  }
  
  #If the permutation blocks display minutes, then reorder records and blocks based on minutes
  if(permute == 'minutes') {
    
    #Order by minutes
    records <- records[order(as.numeric(minutes)), ]
    videos <- videos[order(as.numeric(minutes))]
    minutes <- minutes[order(as.numeric(minutes))]
    
    #Define permutation structure based on blocks
    CTRL <- how(blocks = minutes, within = Within('free'))
    
    Rep <- replicate(nperm, {
      #Apply randomization
      rndm <- records[shuffle(seq_len(nrow(records)), CTRL), ]
      rndm_1 <- place_1(rndm)
      
      rowSums(rndm_1)}, simplify = 'array')
    
    
  } else if (permute == 'videos') {
    
    #Order by videos
    records <- records[order(videos), ]
    minutes <- minutes[order(videos)]
    videos <- videos[order(videos)]
    
    #Define permutation structure based on blocks
    CTRL <- how(plots = Plots(videos, type = 'free'), within = Within('none'))
    Rep <- replicate(nperm, {
    
      #Apply randomization
      rndm <- records[shuffle(seq_len(nrow(records)), CTRL), ]
      rndm_1 <- place_1(rndm)
      
      rowSums(rndm_1)}, simplify = 'array')
    
  } else stop("'permute' must be defined either as minutes or video")
  
  return(data.frame('Mean' = rowMeans(Rep), 'SD' = apply(Rep, 1, sd)))
  
}

#Permute minutes
perm_min <- perm_by_strata(records = records[,-c(1,2)], minutes = as.numeric(records$min), videos = records$video,
                           nperm = 1000, permute = 'minutes')

#Permute videos
perm_vid <- perm_by_strata(records[,-c(1,2)], minutes = as.numeric(records$min), videos = records$video,
                           nperm = 1000, permute = 'videos')


# Gather results for the plot
results <- data.frame(index = 1:nrow(perm_min), 
                      time = sort(as.numeric(records$min)),
                      `colnames<-`(perm_min, paste0(colnames(perm_min), "_min")), 
                      `colnames<-`(perm_vid, paste0(colnames(perm_vid), "_vid")))

colnames(results)
# Make plot
gplot1<- ggplot(results, aes(x = index)) +
  #Add errors
  geom_ribbon(aes(ymin = Mean_vid - SD_vid,
                                    ymax = Mean_vid + SD_vid,), 
                       fill = rgb(0.9, 0.7, 0.7), alpha = 0.2) +
  geom_ribbon(aes(ymin = Mean_min - SD_min,
                                    ymax = Mean_min + SD_min,), 
                       fill = rgb(0.7, 0.7, 0.9), alpha = 0.4) +
  #Add lines
  geom_line(aes(y = Mean_min, colour = "Minutes")) +
  geom_line(aes(y = Mean_vid, colour = "Videos")) +

  scale_colour_manual("", 
                      breaks = c("Minutes", "Videos"),
                      values = c("Minutes" = rgb(0.3, 0.3, 1),
                                 "Videos" = rgb(1, 0.3, 0.3))) +
  
  #Add axis
  xlab('Analizing time') +
  ylab('Richness') +
  labs(title = 'c. Richness through analyzed time') +
  
  #Define plot limits
  scale_y_continuous(breaks = 0:5*10) +
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA)) +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())
  
gplot2 <- ggplot(results, aes(y = Mean_min, x = Mean_vid)) +
  #Add treatment line
  geom_line(aes(color = time), size = 1) +
  # #Add a confidence level on minutes order
  # geom_line(aes(y = Mean_min + SD_min, color = time), alpha = 0.3) +
  # geom_line(aes(y = Mean_min - SD_min, color = time), alpha = 0.3) +
  # geom_ribbon(aes( ymax = Mean_min + SD_min,
  #                                    ymin = Mean_min - SD_min), 
  #                      fill = 'grey', alpha = 0.3) +
  # #Add a confidence level on video randomizations
  # geom_line(aes(y = Mean_vid + SD_vid, color = min), alpha = 0.3) +
  # geom_line(aes(y = Mean_vid - SD_vid, color = min), alpha = 0.3) +
  # geom_ribbon(aes( ymax = Mean_vid + SD_vid,
  #                                    ymin = Mean_vid - SD_vid), 
  #                      fill = 'grey', alpha = 0.3) +
  scale_fill_gradientn(colours = c("#004C8E", "#52BF00", 
                                            "#F78800", "#F90004"),
                                breaks = waiver(), n.breaks = 10,
                                name = 'Time') +
  scale_color_gradientn(colours = c("#004C8E", "#52BF00", 
                                             "#F78800", "#F90004"),
                                 breaks = waiver(), n.breaks = 10,
                                 name = 'Time') +
  #Add reference line
  geom_abline(intercept = 0, slope = 1, col = 'black', alpha = 0.2) +
  #Define plot limits
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA)) +
  scale_y_continuous(breaks = 0:5*10) +
  scale_x_continuous(breaks = 0:5*10) +
  #Add axis
  ylab('Permuting videos within minutes') +
  xlab('Permuting videos freely') +
  labs(title = 'd. Richness contrast') +
  theme(panel.grid = element_blank(), 
                 panel.background = element_blank())
  # theme(panel.background = element_rect(fill = "white", ),
  #                panel.grid.major = element_line(size = 0.3, linetype = 'solid',
  #                                                         colour = "grey"), 
  #                panel.grid.minor = element_line(size = 0.15, linetype = 'solid',
  #                                                         colour = "lightgrey"), 
  #                legend.justification = 'top') 


#### Plate 2 ####
Graph2 <- BreakAccs + BreakSim + gplot1 + gplot2 + patchwork::plot_layout(guides = 'collect')
Graph2 

