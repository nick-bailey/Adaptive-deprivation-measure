# FRS analysis v9b.R
#   bring in MD data from FRS 2010/11-latest year
#   - prepared by "FRS - matdep adaptive - change v3.sps"
#   IRT analysis on FRS deprivation data + adaptive measures
#   makes shortened scales, compares with full scale 
#   measures saving in survey time and loss of information
#   tweaks to give b/w figs for publication


# PAPERFIG - marks figs used in paper
# PAPERTABLE - marks outputs for tables
# PAPERNUMBER - number quoted in paper


# #### FURTHER WORK
# FIG 3 GONE AWRY - FIX HERE AND "v7 - 20 item"
# CHECK DWP REPORTS - % OF CHILDREN 'DEPRIVED'?
# MAKE MISSING MEASURE WITH DEPVD AND LOW INCOME?
# ####

## data frames
# df_md - original data (N=87,842 - all years)
# - MDSCORECH - HBAI prev-weighted score (0-100)
# - mdscore_nb - my recreation of prev-weighted score
# - mdsum - count of items lacked
# - mdsum_trunc/2 - trancated
# - MDCH - depvd (>=25)
# - MDCH2 - depv (>=40)
# - mdch_nb - my version from mdscore_nb

# df_md2 - s4: one-stage adaptive
# - using fn_adapt on df_md for 2-8 initial group of items (N=518,812)
# - excluding earliest year as no prev weighting
# - mdsum_1 - count of items lacked from initial group

# df_md5 - s5: run fn_adapt for various group sizes, using extracts of df_md2 (N=370,580)

# df_md8 - s6: adaptive on last year of data only
# - df_result & df_result2 summarise

# df_md8a - s6a: adaptive on all years
# - mdsum_1/2/3 - score with 1/2/3 groups of items by rank
# - grpsize 
# df_result4 summarises

# df_md12 - s6c: md_full and md_adapt

# df_md9/10/11 - s6c: various checking stages



# Packages ----------------------------------------------------------------

# # install packages - first time only
# packs <- c("tidyverse", "dplyr", "readxl")
# install.packages(packs)
# 
# # # package for Latent Trait Models and hoslem.test()
# install.packages('ltm')
# install.packages("ResourceSelection")

# update.packages()

# load libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(ltm)
library(ResourceSelection)

# # get citations
# citation("dplyr")
# citation("tidyverse")
# citation("readxl")
# citation("ltm")
# citation("ResourceSelection")


# Functions ---------------------------------------------------------------

# function to make 'mdsum' using items with rank <= 'grpsize' only
# Args:
#   df_d: data frame w/ 'year' (numeric) & 'lack' vars (list - lack)
#   grpsize: constant - max rank of items to include
#   df_r: data frame w/ 'year' (numeric) & 'rank' of each item
#     - default is df_rank (items with rank by Diff by year - numeric),
#   l: a list of the 'lack' var names
#     - default is 'lack'
#   r: a list of the 'rank' var names
#     - default is 'rank'
#   returns df1 (indiv level)
fn_adapt <- function(df_d, grpsize, df_r = df_rank, l = lack, r = rank) {
  
  # make list 'lacka' and 'ranka'
  lacka <- paste0("lacka_", l)
  ranka <- r %>%
    str_replace("rank_", "ranka_")
  
  # new copy of df_d
  df1 <- df_d 
  
  # make ranka - var is logical: in grp y/n
  df_r1 <- df_r
  colnames(df_r1) <- c("year", ranka)
  df_r1[ranka] <- df_r1[ranka] <= grpsize 
  
  # add ranka to main file
  df1 <- df1 %>%
    left_join(df_r1, by = "year")
  # count item if lack and in first group
  df1[lacka] <- df1[l] * df1[ranka]
  
  # make sum of items lacked from first group
  df1$mdsum_a <- rowSums(df1[lacka], na.rm = TRUE)
  
  # return both indiv and grouped data
  return(df1)
}



# Set plot params etc. ----------------------------------------------------

title.text.size <- 18
axis.text.size <- 16
geom_text.size <- 6   # for some reason, scales diff - 5 here = 14 elsewhere!


# 0. Get data -------------------------------------------------------------

# import data from csv file
# latest verions: 
#    col 1 - year; 
#    col 2:22 - lack vars (0/1); 
#    col 23-43 - have vars (0/1); 
#    col 44/45 - MDCH (0/1) and MDSCORECH (depvn score 0-100) 
#    col 46 - weight 

df_md <- read_csv("data/frs_md.csv")

# # 87,842 children for 2010-17
# nrow(df_md)

# set year_min/max
year_min <- min(df_md$year)   # earliest year
year_max <- max(df_md$year)   # latest year

# make year a factor
df_md$year <- factor(df_md$year)

# make some col names clearer for later figs
colnames(df_md) <- colnames(df_md) %>%
  str_replace("ad_holiday", "adult_hols") %>%
  str_replace("ch_holiday", "child_hols") %>%
  str_replace("bills", "[NEW]_bills") %>%
  str_replace("fruit_veg", "[NEW]_fruit_veg") %>%
  str_replace("warm_coat", "[NEW]_warm_coat") %>%
  str_replace("activities", "[NEW]_activities")

# set the '-1' missing values to 'NA' 
df_md[df_md == -1] <- NA

# lists with var names for 'lack' and 'have' vars
lack <- colnames(df_md[2:22])
have <- colnames(df_md[23:43])

# number of items
n_items <- length(lack)


# 0a. Percent lack items --------------------------------------------------

# figure to show % lack each item each year
df_lack <- df_md[c("year", lack)]
df_lack[is.na(df_lack)] <- 0
df_lack <- df_lack %>%
  group_by(year) %>%
  summarise_all(list(~mean(.))) %>%
  gather(key = "Item", value = "Percent", - year)
df_lack$Percent <- df_lack$Percent * 100

# PAPERFIG 1 boxplot - percent lacking each item by year
p0a <- ggplot(data=df_lack,
             aes(x = reorder(Item, Percent, FUN = median),
                 y = Percent/100))
p0a + geom_boxplot() +
  coord_flip() +
  geom_jitter(position=position_jitter(0.1)) +
  labs(x = "Item\n",
       y = "\nPercent lack item") +
  scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
  theme(axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 1 ", n_items, " - percent lack each item.png"))
# 
# # line - percent lacking each item by year
# p0b <- ggplot(data=df_lack,
#              aes(x = year,
#                  y = Percent/100,
#                  group = Item))
# p0b + geom_line(aes(colour = Item)) +
#   geom_jitter(position=position_jitter(0.1)) +
#   labs(x = "Item\n",
#        y = "\nPercent lack item") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) + 
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line - percent lacking each item by year - zoomed
# p0c <- ggplot(data=df_lack,
#              aes(x = year,
#                  y = Percent/100,
#                  group = Item))
# p0c + geom_line(aes(colour = Item)) +
#   geom_jitter(position=position_jitter(0.1)) +
#   labs(x = "Item\n",
#        y = "\nPercent lack item") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2), 
#                      limits = c(0, .15)) + 
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))


# 0b. % depvd and DWP vs sum score ----------------------------------------

# sum of items lacked plus truncated version as factor
df_md$mdsum <- rowSums(df_md[lack], na.rm = TRUE)
df_md$mdsum_trunc <- df_md$mdsum
df_md$mdsum_trunc[df_md$mdsum_trunc > 8] <- 8 
df_md$mdsum_trunc2 <- df_md$mdsum
df_md$mdsum_trunc2[df_md$mdsum_trunc2 > 12] <- 12
levels(df_md$mdsum_trunc)[levels(df_md$mdsum_trunc)=="8"] <- "8+"
# table(df_md$mdsum, as.integer(df_md$mdsum_trunc))

# MDCH2 - depvd by higher threshold 
df_md$MDCH2 <- 0
df_md$MDCH2[df_md$MDSCORECH >= 40] <- 1

# % deprvd - all years
# - MDCH - 24.6% - lack 7+ items
# - MDCH2 - 9.5% - lack 10+ items
weighted.mean(df_md$MDCH, df_md$GS_NEWCH, na.rm = TRUE)
weighted.mean(df_md$MDCH2, df_md$GS_NEWCH, na.rm = TRUE)

## df_depvd 
# % deprived each year with diff thresholds
df_depvd <- df_md[c("year", "MDCH", "MDCH2", "MDSCORECH", "GS_NEWCH")] %>%
  group_by(year) %>%
  summarise_all(list(~weighted.mean(., GS_NEWCH))) 

# PAPERTABLE 1 - save file for table
cols <- c("mdsum_trunc2", "MDCH", "MDCH2")
# rows by level of depvn
df_csv <- df_md[cols] %>%
  group_by(mdsum_trunc2) %>%
  summarise(pct = n(),
            MDCH = mean(MDCH, na.rm = TRUE), 
            MDCH2 = mean(MDCH2, na.rm = TRUE))
sum(df_csv$pct)    # no. of children
df_csv$pct <- df_csv$pct/sum(df_csv$pct)
# 'total' row
df_csv2 <- df_md[cols] 
df_csv2$mdsum_trunc2 <- 0   #set same for all cases
df_csv2 <- df_csv2 %>%
  group_by(mdsum_trunc2) %>%
  summarise(pct = n(),
            MDCH = mean(MDCH, na.rm = TRUE), 
            MDCH2 = mean(MDCH2, na.rm = TRUE))
sum(df_csv2$pct)    # no. of children
df_csv2$pct <- df_csv2$pct/sum(df_csv2$pct)
#join 'total' to other rows
df_csv <- rbind(df_csv, df_csv2)
# write file
write.csv(df_csv, file = paste0("Data/table 1 ", n_items," - MDCH by mdsum ", 
                                n_items, ".csv"))
# 
# # number of cases, count of '1's & % answers '1'
# nrow(df_md)
# sum(df_md[lack], na.rm = TRUE)
# 100*sum(df_md[lack], na.rm = TRUE)/(nrow(df_md)*n_items)
# 
# PAPERNUMBER - correlations
# # correlation
# cor(df_md$mdsum, df_md$MDSCORECH)
# 
# # correlation by year
# df_temp <- data.frame()
# for (i in year_min:year_max) {
#   df_temp <- df_md[df_md$year == i,]
#   corr <- cor(df_temp$mdsum, df_temp$MDSCORECH)
#   print(paste(i, signif(corr, 3)))
# }
# rm(df_temp)
# 
# 
# # make text for correlation
# corr <- signif(
#   cor(df_md$mdsum, df_md$MDSCORECH), 
#   digits = 3)
# corr_text <- paste("Correlation (R) =", corr)
# 
# 
# # scatter plot - mdsum (lack count) vs MDSCORECH (weighted)
# p0d <- ggplot(data=df_md,
#              aes(x = mdsum, y = MDSCORECH))
# p0d + geom_point() +
#   labs(x = "Count of items lacked",
#        y = "DWP measure (weighted score)")
# 
# # box plot - mdsum (lack count) vs MDSCORECH (weighted)
# p0e <- ggplot(data=df_md,
#              aes(x = factor(mdsum), y = MDSCORECH))
# p0e + geom_boxplot() +
#   labs(x = "\nCount of items lacked",
#        y = "Weighted score\n") +
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size)) +
#   geom_text(x = 4, y = 80,
#             label = corr_text, size = geom_text.size)
# ggsave(paste0("Figs/fig", n_items, "- corr of two measures.png"))


# 0c. Probs with outdoor play ---------------------------------------------

# Exploring percent lack by income - years
#   digging into problems with outdoor_play
# make df_lack_inc for % lack item (latest year) by income group
df_lack_inc <- df_md
df_lack_inc$year <- as.numeric(as.character(df_lack_inc$year))
df_lack_inc <- df_lack_inc[!is.na(df_lack_inc$S_OE_AHC) & 
                   df_lack_inc$year == year_max, ]
df_lack_inc$inc_dec <- cut(df_lack_inc$S_OE_AHC, 
                     c(-1000, 100, 175, 250, 325, 400, 500, 600, 800, 20000), 
                     right = FALSE)
# table(df_lack_inc$inc_dec)
df_lack_inc <- df_lack_inc %>%
  group_by(inc_dec) %>%
  summarise_at(vars(colnames(df_lack_inc[lack])),
               list(~weighted.mean(., GS_NEWCH, na.rm = TRUE))) %>%
  gather(lack, key = "item", value = "Percent")

# # line chart - % lack by income by item - zoom in
# p0f <- ggplot(data = df_lack_inc, 
#             aes(x = inc_dec, 
#                 y = Percent,
#                 group = item))
# p0f + geom_line(stat = "identity", aes(colour = item)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,.25)) 
# 
# # line chart - % lack outdoor_play 
# p0g <- ggplot(data = df_lack_inc[df_lack_inc$item == "outdoor_play", ], 
#             aes(x = inc_dec, 
#                 y = Percent,
#                 group = item))
# p0g + geom_line(stat = "identity", aes(colour = item)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,.25)) 


# 1a. model for all years in one go ---------------------------------------

# lt model with one latent trait; 2-param logistic model
fit_allyears <- ltm(df_md[lack] ~ z1, IRT.param = TRUE)

# # ICC plot
# plot(fit_allyears, type = "ICC", zrange = c(-3, 3), legend = TRUE)
# # IIC plot
# plot(fit_allyears, type = "IIC", zrange = c(-3, 3), legend = TRUE)
# # test information plot
# plot(fit_allyears, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
#      zrange = c(-1,4),
#      cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

# capture information by centiles
info <- list()
range <- list()
for (i in 1:20) {
  lower <- qnorm((i-1)/20)
  upper <- qnorm(i/20)
  info_temp <- information(fit_allyears, c(lower, upper))
  info <- c(info, info_temp[3])
  range <- c(range, info_temp[4][1])
}
# convert to df
df_info_all <- data.frame(
  centiles = seq(1,20),
  info = unlist(info),  
  matrix(unlist(range), nrow = length(range), byrow = TRUE)
)
df_info_all$centiles <- factor(df_info_all$centiles, 
                        levels = seq(1,20),
                        labels = paste0(seq(0,95,5), "-", seq(5,100,5), "th"))
rm(info_temp)
rm(lower)
rm(upper)
rm(info)
rm(range)


# put coefficients in data frame with var names
df_dffclt_all <- coef(fit_allyears, prob = TRUE) %>%
  as_tibble() %>%
  cbind(rn = row.names(coef(fit_allyears, prob = TRUE)), .) %>%
  tibble::rowid_to_column("ID")

# # plot - ICCs for all
# plot(fit_allyears, type = "ICC",
#      zrange = c(-2, 4), legend = TRUE)
# 
# # plot - IICs for all
# plot(fit_allyears, type = "IIC",
#      zrange = c(-2, 4), legend = TRUE)
# 
# # plot - high difficulty items only
# high_diff_cols <- df_dffclt_all$ID[df_dffclt_all$Dffclt > 2]
# plot(fit_allyears, type = "ICC",
#      items = high_diff_cols, zrange = c(1, 4), legend = TRUE)

# make probability from Z score for Difficulty
df_dffclt_all$Dffclt_prob <- round(pnorm(df_dffclt_all$Dffclt)*100,1)

# make string var 'item' from rn for later use
df_dffclt_all$item <- as.character(df_dffclt_all$rn)

# make rank order of difficulty
df_dffclt_all$Dffclt_rank <- rank(df_dffclt_all$Dffclt)


# 1b. Figures for single ltm ----------------------------------------------

# # dot plots - difficulty as z score
# p1a <- ggplot(data=df_dffclt_all, aes(x = Dffclt))
# p1a + geom_dotplot(binwidth = 0.04, method = "histodot") +
#   labs(x = "Difficulty (std deviations)")
# 
# # dot plots - difficulty as pbility
# p1b <- ggplot(data=df_dffclt_all, aes(x = Dffclt_prob))
# p1b + geom_dotplot(binwidth = .5, method = "histodot")  +
#   labs(x = "Difficulty (probability)")
# 
# # Dffclt vs Dscrmn
# p1c <- ggplot(data=df_dffclt_all, aes(x = Dffclt, y = Dscrmn))
# p1c + geom_point() + 
#   geom_text(aes(label = item), hjust = 0, vjust = 0) +
#   labs(x = "Difficulty", y = "Discrimination")


# # bar chart of info
# p1 <- ggplot(data = df_info_all,
#              aes(x = centiles, 
#                  y = info))
# p1 + geom_bar(stat = "identity") +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent of information",
#         x = "Percentile range") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text.y=element_text(size=axis.text.size),
#         axis.text.x = element_text(angle=90, vjust=0.5, size=axis.text.size))
# 
# # line chart of info
# p1 <- ggplot(data = df_info_all,
#              aes(x = as.numeric(centiles),
#                  y = info))
# p1 + geom_line(stat = "identity") +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent of information",
#         x = "Vigintiles") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text.y=element_text(size=axis.text.size),
#         axis.text.x = element_text(angle=90, vjust=0.5, size=axis.text.size))


# 2a. separate models for diff years --------------------------------------


# empty dfs for results
df_dffclt <- data.frame()
df_info_yr <- data.frame()

# loop through years, storing coeffs and info
for (yr in year_min:year_max) {
  # make ltm model for year == yr
  fit <- ltm(df_md[df_md$year == yr, lack] ~ z1, 
             IRT.param = TRUE)
  
  # capture diff and discrim for year
  coeffs <- coef(fit, prob = TRUE) %>%
    as_tibble() %>%
    cbind(rn = row.names(coef(fit, prob = TRUE)), .) %>%
    tibble::rowid_to_column("ID")
  coeffs$year = yr
  df_dffclt <- rbind(df_dffclt, coeffs)   # capture item difficulties

  # capture information by vigintiles for year
  info <- list()
  range <- list()
  for (i in 1:20) {
    lower <- qnorm((i-1)/20)
    upper <- qnorm(i/20)
    info_temp <- information(fit, c(lower, upper))
    info <- c(info, info_temp[3])
    range <- c(range, info_temp[4][1])
  }
  # convert to df
  df_info <- data.frame(
    centiles = seq(1,20),
    info = unlist(info),  
    matrix(unlist(range), nrow = length(range), byrow = TRUE)
  )
  # give centiles their range as labels
  df_info$centiles <- factor(df_info$centiles, 
                                 levels = seq(1,20),
                                 labels = paste0(seq(0,95,5), "-", seq(5,100,5), "th"))
  # make year variable and store in df_info_yr
  df_info$year <- yr
  df_info_yr <- rbind(df_info_yr, df_info)   # capture item difficulties
}
# tidy up
rm(info_temp)
rm(lower)
rm(upper)
rm(info)
rm(range)


# make probability from Z score for Difficulty
df_dffclt$Dffclt_prob <- round(pnorm(df_dffclt$Dffclt)*100,1)

# make string var 'item' from rn for later use
df_dffclt$item <- as.character(df_dffclt$rn)

# make rank order of difficulty
df_dffclt <- df_dffclt %>%
  group_by(year) %>%
  mutate(Dffclt_rank = rank(Dffclt))

# df to compare lack and dffclt_prob
df_lack_diff <- df_lack
df_lack_diff$year <- as.numeric(as.character(df_lack_diff$year))
df_lack_diff <- df_lack_diff %>%
  left_join(df_dffclt, by = c("year" = "year", "Item"= "item"))


# 2b. Figures for ltm models ----------------------------------------------

# boxplots - diff (z scores) by year
#   shows that ordering of items varies little by year
# #   also highlights how information is compressed at one end of scale
# # hline at qnorm(75%) - ave child depvn last 6yrs=25%
# p2a <- ggplot(data=df_dffclt,
#              aes(x=reorder(rn, Dffclt, FUN = median),
#                  y=Dffclt))
# p2a + geom_boxplot(aes(fill = rn)) +
#   geom_hline(aes(yintercept = qnorm(.75), size = 2, alpha = .4)) +
#   coord_flip() +
#   geom_jitter(position=position_jitter(0.2)) +
#   labs (y = "Difficulty (std dev)", x = "Item") +
#   theme(legend.position = "none",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# PAPERFIG 2 boxplots - diff (pbility) by year
#   hline at 75% - ave child depvn last six years is 12% (DWP HBAI reports)
p2b <- ggplot(data=df_dffclt,
             aes(x=reorder(rn, Dffclt, FUN = median),
                 y=Dffclt_prob))
p2b + geom_boxplot() +
  geom_hline(aes(yintercept = 75, size = 2, alpha = .4)) +
  coord_flip() +
  geom_jitter(position=position_jitter(0.2)) +
  labs (y = "\nItem difficulty in each year\n(percentile scale)", x = "Item\n") +
  theme(legend.position = "none",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 2 ", n_items, " - item diff-pbility by year.png"))

# PAPERFIG 2 colour boxplots - diff (pbility) by year
#   hline at 75% - ave child depvn last six years is 12% (DWP HBAI reports)
p2b <- ggplot(data=df_dffclt,
              aes(x=reorder(rn, Dffclt, FUN = median),
                  y=Dffclt_prob))
p2b + geom_boxplot(aes(fill = rn)) +
  geom_hline(aes(yintercept = 75, size = 2, alpha = .4)) +
  coord_flip() +
  geom_jitter(position=position_jitter(0.2)) +
  labs (y = "\nItem difficulty in each year\n(percentile scale)", x = "Item\n") +
  theme(legend.position = "none",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 2 col ", n_items, " - item diff-pbility by year.png"))

#  
# # line graphs - diff(z) by year
# #   shows that ordering of items varies little by year
# p2c <- ggplot(data=df_dffclt,
#              aes(x=year,
#                  y=Dffclt,
#                  group=rn))
# p2c + geom_line(aes(colour = rn), size = 1.5) +
#   labs(x = "Year", y = "Difficulty (std dev)") +
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line graphs - diff(pbility) by year
# p2d <- ggplot(data=df_dffclt,
#              aes(x=year,
#                  y=Dffclt_prob,
#                  group=rn))
# p2d + geom_line(aes(colour = rn), size = 1.5) +
#   labs(x = "Year", y = "Difficulty (std dev)") +
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # difficulty vs Percent lack - years
# #   outdoor_play is above trend, bottom right
# #   bedrooms is below trend, bottom centre
# p2e <- ggplot(data=df_lack_diff, aes(x = Dffclt, y = Percent))
# p2e + geom_point() +
#   geom_line(aes(color = Item)) +
#   labs(x = "Difficulty", y = "Percent lack")
# 
# # discrimination vs Percent lack - years
# p2f <- ggplot(data=df_lack_diff, aes(x = Dscrmn, y = Percent))
# p2f + geom_point() +
#   geom_line(aes(color = Item)) +
#   labs(x = "Discrimination", y = "Percent lack")


# 2c. Figure for information ----------------------------------------------

# # information by z score and by vigintiles - most recent year
# # - 'fit' holds results for most recent year
# par(mfrow = c(1,2))
# plot(fit, type = "IIC", items = 0, lwd = 2, xlab = "Latent trait (z score)",
#      zrange = c(-1,4),
#      main = "(i) Information by Z score",
#      cex.main = 1.5, cex.lab = 1.4, cex.axis = 1.2)
# abline(v=qnorm(.95), lty=2)   # vert line at 95th centile
# barplot(df_info_all$info*100, names.arg = as.character(df_info_all$centiles), las=2, 
#         # xlab = "\n\n\nLatent trait (percentiles)", 
#         ylab = "Percent of Information", 
#         main = "(ii) Information by percentile",
#         cex.main = 1.5, cex.lab = 1.4, cex.axis = 1.2)
# 
# PAPERFIG 4 same as previous but saving in png file
png(paste0("Figs/fig 4 ",
           n_items, " - info plots.png"),
    width = 800)
par(mfrow = c(1,2))
plot(fit, type = "IIC", items = 0, lwd = 2, xlab = "Latent trait (z score)",
     zrange = c(-1,4),
     main = "(i) Information by Z score",
     cex.main = 1.5, cex.lab = 1.4, cex.axis = 1.2)
abline(v=qnorm(.95), lty=2)   # vert line at 95th centile
barplot(df_info_all$info*100, names.arg = as.character(df_info_all$centiles), las=2,
        # xlab = "\n\n\nLatent trait (percentiles)",
        ylab = "Percent of Information",
        main = "(ii) Information by percentile",
        cex.main = 1.5, cex.lab = 1.4, cex.axis = 1.2)
dev.off()


# 3. Dropping items -------------------------------------------------------


# 3a. Prevalence scores ---------------------------------------------------

# make prevalence scores ('have_xxx') for each var+year - weighted mean
df_prev <- df_md %>%
  group_by(year) %>%
  summarise_at(vars(colnames(df_md[have])),
               list(~weighted.mean(., GS_NEWCH, na.rm = TRUE)))

# drop the 'have' cols from main data now
df_md <- df_md[ , - grep("^have", colnames(df_md))]

# re-scale prev scores so total 100 in each year
df_prev[have] <- df_prev[have] * 100/rowSums(df_prev[have])

# change 'have_' to 'wght_' in colnames
colnames(df_prev) <- colnames(df_prev) %>%
  str_replace("have_", "wght_")

# set of var names for wght - drop 'year' (col 1)
wght <- colnames(df_prev[-1])

# # range of weights - 3.1 to 6.3
# max(df_prev[wght])
# min(df_prev[wght])

# attach prev scores to df_md to make weighted index for indivs
df_md <- df_md %>% 
  left_join(df_prev, by = "year")

# multiply wght by lack to get wghted lack for mdscore
df_md[wght] <- df_md[wght] * df_md[lack]

# make sum of wghtd lack (equiv to DWP MDSCORE) and convert to 0/1
df_md$mdscore_nb <- rowSums(df_md[wght], na.rm = TRUE)
df_md$mdch_nb <- cut(df_md$mdscore_nb, 
                     c(0, 25, 101), right = FALSE, labels=c(0,1))

# # correlation of DWP and my measure = .9997
# cor(df_md$mdscore_nb, df_md$MDSCORECH)

# # scatter to compare my scores with original - v close
# p7 <- ggplot(data=df_md,
#              aes(x=MDSCORECH, y=mdscore_nb, colour=year))
# p7 + geom_point() +
#   labs (x = "\nDWP score (MDSCORECH)", 
#         y = "Recreated score (mdscore_nb)\n") +
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# ggsave(paste0("Figs/fig", n_items, "- DWP vs NB score.png"))
# 
# # density plots for distribution of depvn scores by year
# p7a <- ggplot(data=df_md,
#              aes(x=mdscore_nb, colour=year))
# p7a + geom_density(size = 1) + 
#   geom_vline(aes(xintercept = 25), size=1.5, alpha=0.4, linetype=2) +
#   labs (x = "\nDeprivation scores by year\n(prevalence weighted)", 
#         y = "Density\n") +
#   theme(legend.position = "none",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))


# # histogram for distribution of depvn scores and sums
# df_temp <- df_md[c('mdscore_nb', 'mdsum')] %>%
#   gather(key = "var", value = "score")
# df_temp$var <- factor(df_temp$var)
# levels(df_temp$var) <- c("Prevalence weighted", "Simple sum")
# p7a <- ggplot(data=df_temp,
#               aes(x=score, colour = var))
# p7a + geom_histogram(aes(fill = var), bins=n_items) +
#   facet_wrap(~ var, scales = "free") + 
#   labs (x = "\nDeprivation scores") +
#   theme(legend.position = "none",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size),
#         strip.text.x=element_text(size=title.text.size, face="bold"))
# # ggsave(paste0("Figs/fig", n_items, "- histogram score vs sum.png"))
# rm(df_temp)

# # scatter plot - mdsum (lack count) vs MDSCORECH (weighted)
# p0 <- ggplot(data=df_md,
#              aes(x = mdsum, y = MDSCORECH))
# p0 + geom_point() +
#   labs(x = "Count of items lacked",
#        y = "DWP measure (weighted score)")
# 
# # box plot - mdsum (lack count) vs MDSCORECH (weighted)
# p0 <- ggplot(data=df_md,
#              aes(x = factor(mdsum), y = MDSCORECH))
# p0 + geom_boxplot() +
#   labs (x = "\nCount of items lacked", y = "Weighted score\n") +
#   theme(axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size)) +


# 3b. Dropping items ------------------------------------------------------

# make n_items versions of mdch dropping each item in turn 
# first make copy of 'wghtd lack' vars + year and set 'na' to 0
df_md_dropitem <- cbind(df_md["year"], df_md[wght])
df_md_dropitem[is.na(df_md_dropitem)] <- 0

# if indiv lacks item, subtract wghtd lack from total  
# so score for item is now total wghtd EXCL. that item
df_md_dropitem[wght] <- df_md$mdscore_nb - df_md_dropitem[wght]
# then recode to 0/1 - 25+ is 1; < 25 is 0
df_md_dropitem[wght][df_md_dropitem[wght] < 25] <- 0
df_md_dropitem[wght][df_md_dropitem[wght] >= 25] <- 1

# rename so cols in dropitem have same name as orig indices
colnames(df_md_dropitem) <- colnames(df_md_dropitem) %>%
  str_replace("wght_", "excl_")

# make list of names for 'excl_' cols
excl <- colnames(df_md_dropitem[-1])

# bring across my 'full' estimate of depvn and convert to numeric
df_md_dropitem$mdch_nb <- 
  as.numeric(levels(df_md$mdch_nb))[df_md$mdch_nb]

# then take diff for each measure of mdch without item from original
df_md_dropitem[excl] <- df_md_dropitem$mdch_nb - df_md_dropitem[excl]

# make list 'excl2' to include mdch_nb
excl2 <- colnames(df_md_dropitem[-1])

# summarise by year to give % of cases which change 1 to 0 when item dropped
df_md_dropitemsum <- df_md_dropitem %>%
  group_by(year) %>%
   summarise_at(vars(excl2),
             list(~mean(., na.rm = TRUE)))
df_md_dropitemsum[excl] <- df_md_dropitemsum[excl] 

# re-shape to tidy format - bring % deprived (mdch_nb) for year
df_md_dropitemsum <- df_md_dropitemsum %>% 
  gather(excl, key = "item", value = "Percent", -mdch_nb)

# merge in item difficulty from global model
# first rename
df_md_dropitemsum$item <- df_md_dropitemsum$item %>%
  str_replace("excl_", "")
df_md_dropitemsum <- df_md_dropitemsum %>%
  left_join(df_dffclt_all[c("Dffclt","item")], by = "item")

# PAPERNUMBER
# % change if warm coat dropped - 2017 - 0.02% - 1-in-5000
df_md_dropitemsum$Percent[df_md_dropitemsum$item == "[NEW]_warm_coat" &
                            levels(df_md_dropitemsum$year) == "2017"]
# % poor change if warm coat dropped - 2017 - 0.1% - 1-in-1000
df_md_dropitemsum$Percent[df_md_dropitemsum$item == "[NEW]_warm_coat" &
                            levels(df_md_dropitemsum$year) == "2017"]/
  df_md_dropitemsum$mdch_nb[df_md_dropitemsum$item == "[NEW]_warm_coat" &
                              levels(df_md_dropitemsum$year) == "2017"]


# # plot - percent change - all years
# p8 <- ggplot(data = df_md_dropitemsum,
#              aes(x=reorder(item, Dffclt, FUN = median),
#                  y = Percent, group = year))
# p8 + geom_point(aes(colour = year), size = 3) +
#   coord_flip() +
#   labs (y = "\nPercent of poor change status",
#         x = "Item omitted (ordered by difficulty)\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # 2017 only
# p9 <- ggplot(
#   data = df_md_dropitemsum[df_md_dropitemsum$year %in% c("2017"), ],
#   aes(x=reorder(item, Dffclt, FUN = median),
#       y = Percent, group = year))
# p9 + geom_point(aes(colour = year), size = 3) +
#   coord_flip() +
#   labs (y = "\nPercent of poor change status",
#         x = "Item omitted (ordered by difficulty)\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# ggsave(paste0("Figs/fig", n_items, "- cases lost by drop item.png"))
# 
# # plot - percent of poor change - all years
# p8 <- ggplot(data = df_md_dropitemsum,
#              aes(x=reorder(item, Dffclt, FUN = median),
#                  y = Percent/mdch_nb, group = year))
# p8 + geom_point(aes(colour = year), size = 3) +
#   coord_flip() +
#   labs (y = "\nPercent of deprived not captured",
#         x = "Item omitted\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# PAPERFIG 3 plot - percent of poor change - 2017 only
p8 <- ggplot(data = df_md_dropitemsum[df_md_dropitemsum$year %in% c("2017"), ],
             aes(x=reorder(item, Dffclt, FUN = median),
                 y = Percent/mdch_nb, group = year))
p8 + geom_point(aes(colour = year), size = 3) +
  scale_colour_grey() +
  coord_flip() +
  labs (y = "\nPercent of deprived not captured",
        x = "Item omitted\n") +
  scale_y_continuous(labels = scales::percent_format(accuracy=2),
                     breaks = c(0, .02, .04, .06, .08, .10, .12, .14, .16, .18)) +
  theme(legend.position = "",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 3 ", n_items, " - percent poor lost by drop item.png"))


# PAPERFIG 3 unused plot - percent of poor change - 2017 only
p8 <- ggplot(data = df_md_dropitemsum[df_md_dropitemsum$year %in% c("2017"), ],
             aes(x=reorder(item, Dffclt, FUN = median),
                 y = Percent/mdch_nb, group = year))
p8 + geom_point(aes(colour = year), size = 3) +
  coord_flip() +
  labs (y = "\nPercent not captured",
        x = "Item omitted\n") +
  scale_y_continuous(labels = scales::percent_format(accuracy=2),
                     breaks = c(0, .02, .04, .06, .08, .10, .12, .14, .16, .18)) +
  theme(legend.position = "",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 3 unused ", n_items, " - percent poor lost by drop item.png"))


# 4. Adaptive - first round -----------------------------------------------

# make rank of items in each year, spread and re-order to match df_md
df_rank <- df_dffclt[c("year", "item", "Dffclt_rank")] %>%
  spread(item, Dffclt_rank)
df_rank <- df_rank[c("year", lack)]

# increment year so ranks from yr x join to data for year (x+1)
df_rank$year <- df_rank$year + 1

# make list of vars names 'rank_' and 'ranka_' [outside of fn_adapt]
rank <- paste("rank_", lack, sep = "")
ranka <- rank %>%
  str_replace("rank_", "ranka_")

# change names in df_rank
colnames(df_rank) <- c("year", rank)

# set year in df_md to numeric
df_md$year <- as.numeric(as.character(df_md$year))   # for join

# merge in ranks (for year before)
df_md <- df_md%>%
  left_join(df_rank, by = "year")


## df_md2 
# - using fn_adapt on df_md for 2-8 initial group of items
# - excluding earliest year as no prev weighting
# - mdsum_1 - count of items lacked from initial group
df_md2 <- data.frame()
df_temp <- data.frame()
for (grp1size in 2:8){
  df_temp <- fn_adapt(df_md, grp1size)
  df_temp$grp1size <- grp1size
  df_md2 <- rbind(df_md2, df_temp)
}
rm(df_temp)
# remove earliest year of data - invalid
df_md2 <- df_md2[df_md2$year != min(df_md2$year), ]
# change ranka/mdsum_a to rank1/mdsum_1
# - mdsum_1 - sum based on first N items
colnames(df_md2) <- colnames(df_md2) %>%
  str_replace("ranka_", "rank1_") %>%
  str_replace("mdsum_a", "mdsum_1")
table(df_md2$mdsum, df_md2$mdsum_1)

## df_md3 
# summarising df_md2 by year, grp1size and mdsum_1
# - MDCH/2 - proportion deprived (MDCH/2=1) in each case i.e. % 'missed'
# - N_MDCH/2 - number MDCH/2 in each case
# - N - number cases [so sum equals nrow in df_md2]
# - N_yr - number cases in year
# - N_MDCH/2_yr - number MDCH in year
# - pct - % of cases in year
# - pct_cum - cumulative % cases in year
# - MDCH/2_pct - % of depvd cases in year 'missed' by that mdsum_1 value
# - MDCH/2_pct_cum - cumulative % of depvd cases in year 'missed' [the 'loss' measure]
# - save - no. qns saved (n_items-grp1size) * cum pct in grp (N_pct)
#          divided by n_items [the 'savings' measure]
df_md3 <- df_md2 %>%
  group_by(grp1size, year, mdsum_1) %>%
  summarise(N_MDCH = sum(MDCH, na.rm = TRUE),
            MDCH = mean(MDCH, na.rm = TRUE), 
            N_MDCH2 = sum(MDCH2, na.rm = TRUE),
            MDCH2 = mean(MDCH2, na.rm = TRUE), 
            N = n()) %>%
  group_by(grp1size, year) %>%
  mutate(N_yr = sum(N), 
         N_MDCH_yr = sum(N_MDCH), 
         N_MDCH2_yr = sum(N_MDCH2), 
         pct = N/sum(N),
         pct_cum = cumsum(pct),
         MDCH_pct = N_MDCH/sum(N_MDCH), 
         MDCH_pct_cum = cumsum(MDCH_pct),
         MDCH2_pct = N_MDCH2/sum(N_MDCH2), 
         MDCH2_pct_cum = cumsum(MDCH2_pct),
         save = (n_items-grp1size)*pct_cum/n_items)
# # cross-check with df_md - cases per year and number deprived
# table(df_md$year, df_md$MDCH)


## PAPERTABLE 2 df_csv - output for table
# for latest year, from df_md3, just one line for each grp1size: 
# - grp1size - number items in first group
# - pct - % cases in that year with 0 lacking in first group
# - MDCH - % cases deprived tho' zero lack in first group i.e. 'missed'
# - save - % saving in question time
df_csv <- df_md3[df_md3$year == year_max & 
                   df_md3$mdsum_1 ==0,] 
cols <- c("grp1size", "pct", "MDCH", "MDCH2", "save")
write.csv(df_csv[cols], file = paste0("Data/table 2 ",
                                      n_items, " - data saving & loss.csv"))


## df_md4
# from df_md2, cases where zero lack in initial group (mdsum_1 == 0)
# summarised by year, grp1size, number of items lacking (mdsum)
# - N_mdsum - number with each level of mdsum
# - pct - % cases in year with each level of mdsum
df_md4 <- df_md2[df_md2$mdsum_1 == 0, ] 
df_md4$mdsum[df_md4$mdsum > 8] <- 8
df_md4 <- df_md4 %>%
  group_by(grp1size, year, mdsum) %>%
  summarise(N_mdsum = n()) %>% 
  group_by(grp1size, year) %>%
  mutate(pct = N_mdsum/sum(N_mdsum)) 
sum(df_md4$N_mdsum)


# 4a. Illustrations of ordering -------------------------------------------

# illustration 1 for most recent year:
#   45% lack 2+ of 'first six'
#   - of these, only 7% lack 2+ of last six
#   3% lack 2+ of the 'last six'
#   - of these, 98% lack 2+ of first six
#   44% lack none of 'first six':
#   -  none are deprived (mdch_nb)
#   -  just 1.1% lacks 2 or 3 items; none lacks more
#   -  3% lack any of last six
df_temp <- df_md2[df_md2$year == year_max, ]
ranklast <- rank %>%
  str_replace("rank_", "ranklast_")
rankfirst <- rank %>%
  str_replace("rank_", "rankfirst_")
df_temp[ranklast] <- df_temp[rank] >= 16
df_temp[ranklast] <- df_temp[ranklast] * df_temp[lack]
df_temp[rankfirst] <- df_temp[rank] <= 6
df_temp[rankfirst] <- df_temp[rankfirst] * df_temp[lack]
df_temp$mdsum_last <- rowSums(df_temp[ranklast], na.rm = TRUE)
df_temp$mdsum_first <- rowSums(df_temp[rankfirst], na.rm = TRUE)
# table(df_temp$mdsum, df_temp$mdsum_last)
# table(df_temp$mdsum, df_temp$mdsum_first)

# lack 2+ of first or last six items
df_temp$mdsum_first2 <- 0
df_temp$mdsum_first2[df_temp$mdsum_first > 1] <- 1
df_temp$mdsum_last2 <- 0
df_temp$mdsum_last2[df_temp$mdsum_last > 1] <- 1
#
table(df_temp$mdsum_first2, df_temp$mdsum_last2)  # first var makes rows
sum(df_temp$mdsum_first2)/nrow(df_temp) # % lack 2 of first six
sum(df_temp$mdsum_last2)/nrow(df_temp) # % lack 2 of last six
# if lack 2 of first six, % lack 2 of last six - 7%
df_temp1 <- df_temp[df_temp$mdsum_first2 == 1, ]
# if lack 2 of last six, % lack 2 of first six - 98%
df_temp2 <- df_temp[df_temp$mdsum_last2 == 1, ]
sum(df_temp2$mdsum_first2)/nrow(df_temp2) # % lack 2 of first six

# - none in first six
df_temp$mdsum_first1 <- 0
df_temp$mdsum_first1[df_temp$mdsum_first > 0] <- 1
df_temp$mdsum_last1 <- 0
df_temp$mdsum_last1[df_temp$mdsum_last > 0] <- 1
#
table(df_temp$mdsum_first1, df_temp$mdsum_last1)  # first var makes rows
(1 - sum(df_temp$mdsum_first1)/nrow(df_temp)) # % lack none of first six
(sum(df_temp$mdsum_last1)/nrow(df_temp)) # % lack 1+ of last six
# mdsum for those lacking none of first six
summary(factor(df_temp$mdsum[df_temp$mdsum_first1 == 0]))
# % lack 1+ items overall if none of first six
nrow(df_temp[df_temp$mdsum_first1 == 0 & df_temp$mdsum > 0, ])/
  nrow(df_temp[df_temp$mdsum_first1 == 0, ])
# % lack 1+ items of last six if none of first six
nrow(df_temp[df_temp$mdsum_first1 == 0 & df_temp$mdsum_last1 == 1, ])/
  nrow(df_temp[df_temp$mdsum_first1 == 0, ])
# mdch_nb for those lacking none of first six
summary(factor(df_temp$mdch_nb[df_temp$mdsum_first1 == 0]))

rm(df_temp)
rm(df_temp1)
rm(df_temp2)

# PAPERNUMBER
# illustration 2 for most recent year - first/last eight:
#   38% lack 3+ of 'first eight'
#   - of these, only 8% lack 3+ of last eight
#   3% lack 3+ of the 'last eight'
#   - of these, 98% lack 2+ of first eight
df_temp <- df_md2[df_md2$year == year_max, ]
ranklast <- rank %>%
  str_replace("rank_", "ranklast_")
rankfirst <- rank %>%
  str_replace("rank_", "rankfirst_")
df_temp[ranklast] <- df_temp[rank] >= 14
df_temp[ranklast] <- df_temp[ranklast] * df_temp[lack]
df_temp[rankfirst] <- df_temp[rank] <= 8
df_temp[rankfirst] <- df_temp[rankfirst] * df_temp[lack]
df_temp$mdsum_last <- rowSums(df_temp[ranklast], na.rm = TRUE)
df_temp$mdsum_first <- rowSums(df_temp[rankfirst], na.rm = TRUE)
# table(df_temp$mdsum, df_temp$mdsum_last)
# table(df_temp$mdsum, df_temp$mdsum_first)

# lack 3+ of first or last 8 items
df_temp$mdsum_first2 <- 0
df_temp$mdsum_first2[df_temp$mdsum_first >= 3] <- 1
df_temp$mdsum_last2 <- 0
df_temp$mdsum_last2[df_temp$mdsum_last >= 3] <- 1
#
table(df_temp$mdsum_first2, df_temp$mdsum_last2)  # first var makes rows
sum(df_temp$mdsum_first2)/nrow(df_temp) # % lack 3+ of first eight
sum(df_temp$mdsum_last2)/nrow(df_temp) # % lack 3+ of last eight
# if lack 3+ of first eight, % lack 3+ of last eight - 8%
df_temp1 <- df_temp[df_temp$mdsum_first2 == 1, ]
sum(df_temp1$mdsum_last2)/nrow(df_temp1) # % lack 2 of first six
# if lack 3+ of last eight, % lack 3+ of first eight - 98%
df_temp2 <- df_temp[df_temp$mdsum_last2 == 1, ]
sum(df_temp2$mdsum_first2)/nrow(df_temp2) # % lack 2 of first six

rm(df_temp)
rm(df_temp1)
rm(df_temp2)


# 4b. Figures - adaptive first round --------------------------------------

# ## df_md2
# # stacked bar charts - grp1size=5
# p10 <- ggplot(data = df_md2[df_md2$grp1size == 5,],
#               aes(x = mdsum_1,
#                   fill = factor(mdsum_trunc)))
# p10 + geom_bar(aes(weight = GS_NEWCH),
#                position = position_fill(reverse = TRUE)) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent of cases",
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# ## df_md3
# # line chart - %md by first group by year - grp1size=5
# p11 <- ggplot(data = df_md3[df_md3$grp1size ==5, ],
#               aes(x = mdsum_1,
#                   y = MDCH,
#                   group = factor(year)))
# p11 + geom_line(aes(colour = factor(year))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived",
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # line chart - %md by first group by year - facet: grp1size
# p11 <- ggplot(data = df_md3,
#               aes(x = mdsum_1,
#                   y = MDCH,
#                   group = factor(year)))
# p11 + geom_line(aes(colour = factor(year))) +
#   facet_wrap(~ grp1size) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived",
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # line chart - %md by first group by grp1size - facet: year
# p11 <- ggplot(data = df_md3,
#               aes(x = mdsum_1,
#                   y = MDCH,
#                   group = factor(grp1size)))
# p11 + geom_line(aes(colour = factor(grp1size))) +
#   facet_wrap(~ year) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived",
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # line chart - %md by first group - latest year
# p11 <- ggplot(data = df_md3[df_md3$year == max(df_md3$year), ],
#               aes(x = mdsum_1,
#                   y = MDCH,
#                   group = factor(grp1size)))
# p11 + geom_line(aes(colour = factor(grp1size))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived",
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # line chart - %md by first group - latest year - zoom in
# p12 <- ggplot(data = df_md3[df_md3$year == max(df_md3$year), ],
#               aes(x = mdsum_1,
#                   y = MDCH,
#                   group = factor(grp1size)))
# p12 + geom_line(aes(colour = factor(grp1size))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived",
#         x = "Items lacked in first group") +
#   scale_x_continuous(limits = c(0, 3),
#                      breaks = c(0, 1, 2, 3)) +
#   coord_cartesian(ylim=c(0, .16)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      breaks = c(0, .02, .04, .06, .08, .10, .12, .14, .16)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # lack zero in first grp - % MD by grp1size and year
# p12 <- ggplot(data = df_md3[df_md3$mdsum_1 == 0, ],
#               aes(x = year,
#                   y = MDCH,
#                   group = factor(grp1size)))
# p12 + geom_line(aes(colour = factor(grp1size))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived",
#         x = "Year") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0, .013),
#                      breaks = c(0, .002, .004, .006, .008, .010, .012)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# PAPERFIG 6 bw - saving vs loss trade off
p12 <- ggplot(data = df_md3[df_md3$year == year_max &
                              df_md3$grp1size > 2 &
                              df_md3$mdsum_1 < 3, ],
              aes(x = save,
                  y = MDCH_pct_cum,
                  group = factor(mdsum_1)))
p12 + geom_line(aes(linetype = factor(mdsum_1)), size = .5) +
  geom_point(aes(shape = factor(grp1size)), size = 2.5) +
  labs (y = "Percent of 'deprived' missed\n",
        x = "\nSaving") +
  scale_x_continuous(labels = scales::percent_format(accuracy=2)) +
  coord_cartesian(ylim=c(0, .15), xlim=c(0.25, .6)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  guides(shape=guide_legend(title="Threshold")) +
  guides(linetype=guide_legend(title="Items in\nfirst group")) +
  guides(size=guide_legend(title="")) +
  theme(legend.position = "right",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 6 ", n_items, " - saving vs loss.png"))


# PAPERFIG 6 col - saving vs loss trade off
p12 <- ggplot(data = df_md3[df_md3$year == year_max &
                              df_md3$grp1size > 2 &
                              df_md3$mdsum_1 < 3, ],
              aes(x = save,
                  y = MDCH_pct_cum,
                  group = factor(mdsum_1)))
p12 + geom_line(aes(colour = factor(mdsum_1)), size = 1) +
  geom_point(aes(shape = factor(grp1size)), size = 2) +
  scale_fill_brewer(palette = "Spectral") +
  labs (y = "Percent of 'deprived' missed\n",
        x = "\nSaving") +
  scale_x_continuous(labels = scales::percent_format(accuracy=2)) +
  coord_cartesian(ylim=c(0, .15), xlim=c(0.2, .6)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1)) +
  guides(colour=guide_legend(title="Threshold")) +
  guides(shape=guide_legend(title="Items in\nfirst group")) +
  guides(size=guide_legend(title="")) +
  theme(legend.position = "right",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 6 col", n_items, " - saving vs loss.png"))
# 
# 
#  - saving vs loss trade off - higher threshold
# p12 <- ggplot(data = df_md3[df_md3$year == year_max &
#                               df_md3$grp1size > 2 &
#                               df_md3$mdsum_1 < 3, ],
#               aes(x = save,
#                   y = MDCH2_pct_cum,
#                   group = factor(mdsum_1)))
# p12 + geom_line(aes(colour = factor(mdsum_1)), size = 1) +
#   geom_point(aes(shape = factor(grp1size)), size = 2) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent of 'very deprived' missed\n",
#         x = "\nSaving") +
#   scale_x_continuous(labels = scales::percent_format(accuracy=2)) +
#   coord_cartesian(ylim=c(0, .15), xlim=c(0.2, .6)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy=.1)) +
#   guides(colour=guide_legend(title="Threshold")) +
#   guides(shape=guide_legend(title="Items in\nfirst group")) +
#   guides(size=guide_legend(title="")) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# ggsave(paste0("Figs/fig", n_items, "- saving vs loss - v depvd.png"))


# ## df_md4
# # zero in first group (df_md4) - % lack none (mdsum=0), by grp1size/year
# p12 <- ggplot(data = df_md4[df_md4$mdsum == 0, ],
#               aes(grp1size,
#                   y = pct,
#                   fill = factor(year)))
# p12 + geom_bar(stat = "identity",
#                colour = "grey",
#                position = position_dodge()) +
#   scale_fill_brewer() +
#   labs (title = "Percent of cases lack no items\n",
#         y = "Percent of cases\n",
#         x = "\nItems in first group",
#         fill = "") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      limits = c(0, 1),
#                      breaks = c(0, .2, .4, .6, .8, 1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # zero in first group (df_md4) - % lack 0/1/2/etc. by grp1size (facet:year)
# p12 <- ggplot(data = df_md4,
#               aes(grp1size,
#                   y = pct,
#                   fill = factor(mdsum)))
# p12 + geom_bar(stat = "identity", colour = "grey") +
#   scale_fill_brewer() +
#   facet_wrap(~ year) +
#   labs (title = "Figure title\n",
#         y = "Percent of cases\n",
#         x = "\nItems in first group",
#         fill = "No. items\nlacked\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # zero in first group (df_md4) - % lack 0/1/2/etc. by grp1size 5-8 (facet:year)
# p12 <- ggplot(data = df_md4[df_md4$grp1size >=5 & df_md4$grp1size <= 8, ],
#               aes(grp1size,
#                   y = pct,
#                   fill = factor(mdsum)))
# p12 + geom_bar(stat = "identity", colour = "grey") +
#   scale_fill_brewer() +
#   facet_wrap(~ year) +
#   labs (title = "Figure title\n",
#         y = "Percent of cases\n",
#         x = "\nItems in first group",
#         fill = "No. items\nlacked\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # zero in first group (df_md4) - % lack 1/2/etc. by grp1size 5-8 (facet:year)
# p12 <- ggplot(data = df_md4[df_md4$mdsum > 0 &
#                             df_md4$grp1size >=5 &
#                             df_md4$grp1size <=8, ],
#               aes(grp1size,
#                   y = pct,
#                   fill = factor(mdsum)))
# p12 + geom_bar(stat = "identity", colour = "grey") +
#   scale_fill_brewer() +
#   facet_wrap(~ year) +
#   labs (title = "Figure title\n",
#         y = "Percent of cases\n",
#         x = "\nItems in first group",
#         fill = "No. items\nlacked\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      limits = c(0, .12),
#                      breaks = c(0, .02, .04, .06, .08, .1, .12)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # zero in first group (df_md4) - % lack none by grp1size 3-8 (latest yr)
# p12a <- ggplot(data = df_md4[df_md4$mdsum == 0 &
#                               df_md4$grp1size >=3 &
#                               df_md4$grp1size <=8 &
#                               df_md4$year == year_max, ],
#               aes(grp1size,
#                   y = pct)) + 
#   geom_bar(stat = "identity",
#                fill = "lightblue") +
#   scale_fill_brewer() +
#   labs (y = "Percent of cases",
#         x = "\nItems in first group",
#         fill = "") +
#   coord_cartesian(ylim=c(0.8, 1)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      breaks = c(0.8, .84, .88, .92, .96, 1)) +
#   scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8)) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# p12a
# ggsave(paste0("Figs/fig", n_items, "- first round percent missed.png"))
# 
# 
# PAPERFIG 5 bar chart: lck 0 in first N (df_md4) - % lack 1/2/etc. by grp1size 3-8 (lastest yr)
p12b <- ggplot(data = df_md4[df_md4$mdsum > 0 &
                              df_md4$grp1size >=3 &
                              df_md4$grp1size <=8 &
                              df_md4$year == year_max, ],
              aes(grp1size,
                  y = pct,
                  fill = factor(mdsum)))
p12b + geom_bar(stat = "identity", colour = "grey") +
  scale_fill_grey(start = 1, end = 0) +
  labs (y = "\nPercent of cases",
        x = "\nItems (N) in first group",
        fill = "Total items\nlacked") +
  coord_cartesian(ylim=c(0, .2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, .04, .08, .12, .16, .2)) +
  scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8)) +
  theme(legend.position = "right",
        legend.text=element_text(size=axis.text.size),
        legend.title=element_text(size=title.text.size),
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 5 ", n_items, " - first round number missed.png"))


# 5. Adaptive - second round ----------------------------------------------

# df_md5 - run fn_adapt for various group sizes, using extracts of df_md2
df_md5 <- data.frame()
df_temp <- data.frame()
for (grp2size in 4:8){
  df_temp <- fn_adapt(df_md2[df_md2$grp1size == grp2size, ], 
                      (2 * grp2size))   
  df_temp$grp2size <- 2 * grp2size
  df_md5 <- rbind(df_md5, df_temp)
}
rm(df_temp)

# change ranka/mdsum_a to rank2/mdsum_2
colnames(df_md5)  <- colnames(df_md5) %>%
  str_replace("ranka_", "rank2_") %>%
  str_replace("mdsum_a", "mdsum_2")

# where 0 missing first round, make mdsum_2 'na'
df_md5$mdsum_2[df_md5$mdsum_1 == 0] <- NA
summary(factor(df_md5$grp1size))
summary(factor(df_md5$grp2size))
summary(factor(df_md5$mdsum_1))
summary(factor(df_md5$mdsum_2))

# make means for figs
df_md6 <- df_md5 %>%
  group_by(grp2size, year, mdsum_2) %>%
  summarise(MDCH = mean(MDCH, na.rm = TRUE))

## make means for figs where 1 in first two grp2
df_md7 <- df_md5[df_md5$mdsum_2 == 1, ] 
df_md7$mdsum[df_md7$mdsum > 8] <- 8
df_md7 <- df_md7 %>%
  group_by(grp2size, year, mdsum) %>%
  summarise(N_mdsum = n()) %>% 
  group_by(grp2size, year) %>%
  mutate(pct = N_mdsum/sum(N_mdsum)) 



# 5a. figures for second round --------------------------------------------

# # stacked bar charts - grp2size=5/10
# p10 <- ggplot(data = df_md5[df_md5$grp2size == 10,], 
#               aes(x = mdsum_2, 
#                   fill = factor(mdsum_trunc)))
# p10 + geom_bar(aes(weight = GS_NEWCH), 
#                position = position_fill(reverse = TRUE)) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent of cases", 
#         x = "Items lacked in first & second\ngroups (10 items)") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
#   scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
#   theme(legend.position = "right", 
#         axis.title=element_text(size=title.text.size, face="bold"), 
#         axis.text=element_text(size=axis.text.size))
# 
# # line chart - %md by second group by year - grp2size=10
# p11 <- ggplot(data = df_md6[df_md6$grp2size == 10, ], 
#               aes(x = mdsum_2, 
#                   y = MDCH, 
#                   group = factor(year)))
# p11 + geom_line(aes(colour = factor(year))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent deprived", 
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=2),
#                      limits = c(0,1)) +
#   scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
#   theme(legend.position = "right", 
#         axis.title=element_text(size=title.text.size, face="bold"), 
#         axis.text=element_text(size=axis.text.size))
# 
# 
# # '1' in first group (df_md7) - % lack 1 (mdsum=1), by grp2size/year
# p12 <- ggplot(data = df_md7[df_md7$mdsum == 1, ], 
#               aes(grp2size, 
#                   y = pct, 
#                   fill = factor(year)))
# p12 + geom_bar(stat = "identity", 
#                colour = "grey", 
#                position = position_dodge()) +
#   scale_fill_brewer() +
#   labs (title = "Figure title\n", 
#         y = "Percent of cases\n", 
#         x = "\nItems in first two groups",
#         fill = "") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      limits = c(0, 1), 
#                      breaks = c(0, .2, .4, .6, .8, 1)) +
#   scale_x_continuous(breaks = c(8, 10, 12, 14, 16)) +
#   theme(legend.position = "right", 
#         axis.title=element_text(size=title.text.size, face="bold"), 
#         axis.text=element_text(size=axis.text.size))
# 
# # '1' in first two groups (df_md7) - % lack 1/2/etc. by grp2size (facet:year)
# p12 <- ggplot(data = df_md7, 
#               aes(grp2size, 
#                   y = pct, 
#                   fill = factor(mdsum)))
# p12 + geom_bar(stat = "identity", colour = "grey") +
#   scale_fill_brewer() +
#   facet_wrap(~ year) +
#   labs (title = "Figure title\n", 
#         y = "Percent of cases\n", 
#         x = "\nItems in first two groups",
#         fill = "No. items\nlacked\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_x_continuous(breaks = c(8, 10, 12, 14, 16)) +
#   theme(legend.position = "right", 
#         axis.title=element_text(size=title.text.size, face="bold"), 
#         axis.text=element_text(size=axis.text.size))
# 
# # '1' in first two groups (df_md7) - % lack 1/2/etc. by grp2size (facet:year)
# p12 <- ggplot(data = df_md7[df_md7$mdsum > 1, ], 
#               aes(grp2size, 
#                   y = pct, 
#                   fill = factor(mdsum)))
# p12 + geom_bar(stat = "identity", colour = "grey") +
#   scale_fill_brewer() +
#   facet_wrap(~ year) +
#   labs (title = "Figure title\n", 
#         y = "Percent of cases\n", 
#         x = "\nItems in first two groups",
#         fill = "No. items\nlacked\n") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_x_continuous(breaks = c(8, 10, 12, 14, 16)) +
#   theme(legend.position = "right", 
#         axis.title=element_text(size=title.text.size, face="bold"), 
#         axis.text=element_text(size=axis.text.size))



# 6. Adaptive - last year only ---------------------------------------------

# df_md8: from df_md - indiv data for all years (dropping first year)
#    mdsum_1/2/3 - score with 1/2/3 groups of items by rank
#    grpsize 
# df_result & df_result2 - from df_md8 (last year only)
#    summaries of loss and saving for various cuts points
#    mdsum_adapt - adaptive measure applying cut points (cuts)
df_md8 <- data.frame()

# set group size and number of groups
grp_size <- 5
grp_number <- 3

df_temp <- data.frame()
df_temp2 <- data.frame()

# loop to make mdsum_i measures for each group as df_temp2
# - fn_adapt makes 'mdsum_i' using items with rank <= 'grpsize' only
# - df_temp - vars for current group
for (i in 1:grp_number){
  grpsize <- grp_size * i
  df_temp <- fn_adapt(df_md[df_md$year == year_max, ],
                      grpsize)   
  df_temp$grpsize <- grp_size    # i.e. original grp_size
  if (i == 1){
    df_temp2 <- df_temp[ranka]
  } else {
    df_temp2 <- cbind(df_temp2, df_temp[ranka])
  }
  df_temp2$mdsum_a <- df_temp$mdsum_a
  df_temp2$grpsize <- grp_size
  colnames(df_temp2) <- colnames(df_temp2) %>%
    str_replace("ranka_", paste0("rank", i, "_")) %>%
    str_replace("mdsum_a", paste0("mdsum_", i))
}
rm(df_temp)

# make df_md8 by combining orig vars with df_temp2
df_md8 <- cbind(df_md[df_md$year == year_max, ], df_temp2)
rm(df_temp2)


# df_result/2 
# from df_md8, aggregate to each i/j combination
# - measure of savings and info loss
# - loss_pct - sum of lacks missed as pct of all lacks
# - loss2_pct - number of dpvd missed as % of all depvd
# - miss_pct - miss as % of cases
# - miss2_pct - miss as % of depvd [same loss2_pct]
df_result <- data.frame()

# loop thru diff cut points - i.e. thresholds for stopping
# -  - min number for first group
# - i+j - min number for second group etc.
# - using i=0/j=0 should re-produce the single group approach 
#   - so checking consistency with one stage approach above
iter_max <- 5     # limit to increment in additional items
for (j in 0:(iter_max - 1)) {
  for (i in 0:(iter_max - 2)) {

    # set cut points or stopping rules
    cuts <- c(i, (i+j), (i+2*j))
    
    # make temp df for calcs
    list <- c('mdsum', 'mdsum_1', 'mdsum_2', 'mdsum_3', 'grpsize')
    df <- df_md8[list]
    
    # ## THINK THIS IS REDUNDANT SO # OUT
    # # make measure of 'saving' in qn time
    # grpsize <- df$grpsize[1]    # takes first value
    
    # make the adaptive measure using cuts
    # - default is full measure
    df$mdsum_adapt <- df$mdsum
    # - cases where stop after first group
    df$mdsum_adapt[df$mdsum_1 <= cuts[1]] <- df$mdsum_1[df$mdsum_1 <= cuts[1]]
    # - cases where stop after second group
    df$mdsum_adapt[df$mdsum_1 > cuts[1] & 
                     df$mdsum_2 <= cuts[2]] <- df$mdsum_2[df$mdsum_1 > cuts[1] & 
                                                            df$mdsum_2 <= cuts[2]]
    # - cases where stop after third group
    df$mdsum_adapt[df$mdsum_1 > cuts[1] & 
                     df$mdsum_2 > cuts[2] & 
                     df$mdsum_3 <= cuts[3]] <- df$mdsum_3[df$mdsum_1 > cuts[1] & 
                                                            df$mdsum_2 > cuts[2] & 
                                                            df$mdsum_3 <= cuts[3]]
     
    # make measure of 'information loss' - no. lacks missed
    # - for each indiv - number items 'missed'
    df$loss <- df$mdsum - df$mdsum_adapt
    # - sum of items missed
    loss <- sum(df$loss)
    loss_pct <- loss/sum(df$mdsum)
    
    # make the savings measure using cuts
    df$save <- 0
    df$save[df$mdsum_1 <= cuts[1]] <- (n_items - grp_size)
    df$save[df$mdsum_1 > cuts[1] & 
              df$mdsum_2 <= cuts[2]] <- (n_items - 2*grp_size)
    df$save[df$mdsum_1 > cuts[1] & 
              df$mdsum_2 > cuts[2] & 
              df$mdsum_3 <= cuts[3]] <- (n_items - 3*grp_size)
    save <- sum(df$save)
    save_pct <- save/(nrow(df) * n_items)
    
    # make the misclassification measure
    # - num of items lacking to be deprvd - from analysis of MDCH and mdsum
    cut_miss <- 7   # 7+ = deprived
    # df$mdch <- 0
    # df$mdch[df$mdsum >= cut_miss] <- 1
    # df$mdch_adapt <- 0
    # df$mdch_adapt[df$mdsum_adapt >= cut_miss] <- 1
    # table(df$mdch, df$mdch_adapt)
    df$miss <- (df$mdsum >= cut_miss) - (df$mdsum_adapt >= cut_miss)
    miss <- sum(df$miss)
    miss_pct <- miss/nrow(df)
    miss2_pct <- miss/sum(df$mdsum >= cut_miss)
    
    # savings and losses
    df_result <- rbind(df_result, c(i, j, loss_pct,  
                                    save_pct, miss_pct, miss2_pct))
  }
}
rm(df)

colnames(df_result) <- c("i", "j", "loss_pct", 
                         "save_pct", "miss_pct", "miss2_pct")
write.csv(df_result, file = paste0("Data/df_result - group5 item",
                                   n_items, ".csv"))

df_result2 <- df_result %>%
  gather(c("loss_pct", "save_pct", "miss_pct", "miss2_pct"), 
         key = "variable", value = "percent")

# # line chart - % loss by i/j - grpsize == 5
# #    as you raise the thresholds (i, j), you stop measuring sooner
# #    so 'percent loss' rises
# p11 <- ggplot(data = df_result2[df_result2$variable == "loss_pct", ],
#               aes(x = i, y = percent, group = factor(j)))
# p11 + geom_line(aes(colour = factor(j))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent loss",
#         x = "Items lacked in first group") +
#   guides(colour=guide_legend(title="Additional\nitems lacked")) + 
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line chart - % save by i/j - grpsize == 5
# #   as you raise i,j and stop quicker, so saving is greater
# p11 <- ggplot(data = df_result2[df_result2$variable == "save_pct", ],
#               aes(x = i, y = percent, group = factor(j)))
# p11 + geom_line(aes(colour = factor(j))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent saved",
#         x = "Items lacked in first group") +
#   guides(colour=guide_legend(title="Additional\nitems lacked")) + 
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line chart - % misclassified (miss) by i/j - grpsize == 5
# p11 <- ggplot(data = df_result2[df_result2$variable == "miss_pct", ],
#               aes(x = i, y = percent, group = factor(j)))
# p11 + geom_line(aes(colour = factor(j))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent misclassified (depvd/not)",
#         x = "Items lacked in first group") +
#   guides(colour=guide_legend(title="Additional\nitems lacked")) + 
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line chart - % of poor misclassified (miss2) by i/j - grpsize == 5
# p11 <- ggplot(data = df_result2[df_result2$variable == "miss2_pct", ],
#               aes(x = i, y = percent, group = factor(j)))
# p11 + geom_line(aes(colour = factor(j))) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent of poor misclassified",
#         x = "Items lacked in first group") +
#   guides(colour=guide_legend(title="Additional\nitems lacked")) + 
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# facet line chart - % loss/save/miss/miss2 by i/j - grpsize == 5
# p11 <- ggplot(data = df_result2,
#               aes(x = i, y = percent, group = factor(j)))
# p11 + geom_line(aes(colour = factor(j))) +
#   facet_wrap(~ variable) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent loss",
#         x = "Items lacked in first group") +
#   scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
#   guides(colour=guide_legend(title="Additional\nitems lacked")) +
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line chart - % save vs loss trade off by i/j - grpsize == 5
# p11 <- ggplot(data = df_result[df_result$i < 3 &
#                                  df_result$j < 4,],
#               aes(x = save_pct, y = loss_pct, group = factor(i)))
# p11 + geom_line(aes(colour = factor(i))) +
#   geom_point(aes(shape = factor(j)), size = 2) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent loss",
#         x = "Percent saving") +
#   scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
#   guides(colour=guide_legend(title="Initial\nitems lacked")) + 
#   guides(shape=guide_legend(title="Additional\nitems lacked")) + 
#   guides(size=guide_legend(title="")) + 
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# # line chart - % save vs miss by i/j - grpsize == 5
# p11 <- ggplot(data = df_result,
#               aes(x = save_pct, y = miss_pct, group = factor(i)))
# p11 + geom_line(aes(colour = factor(i))) +
#   geom_point(aes(shape = factor(j), size = 2)) +
#   scale_fill_brewer(palette = "Spectral") +
#   labs (y = "Percent 'missed'",
#         x = "Percent saving") +
#   guides(colour=guide_legend(title="Initial\nitems lacked")) + 
#   guides(shape=guide_legend(title="Additional\nitems lacked")) + 
#   guides(size=guide_legend(title="")) + 
#   theme(legend.position = "right",
#         axis.title=element_text(size=title.text.size, face="bold"),
#         axis.text=element_text(size=axis.text.size))
# 
# PAPERFIG 7 - line chart - % save vs % of poor missed by i/j - grpsize == 5
p11 <- ggplot(data = df_result[df_result$i < 3 &
                                 df_result$j < 4 & df_result$j >0,],
              aes(x = save_pct, y = miss2_pct, group = factor(i)))
p11 + geom_line(aes(linetype = factor(i))) +
  geom_point(aes(shape = factor(j)), size = 2) +
  scale_fill_brewer(palette = "Spectral") +
  labs (y = "Percent of 'deprived' missed\n",
        x = "\nSaving") +
  scale_x_continuous(labels = scales::percent_format(accuracy=2)) +
  coord_cartesian(ylim=c(0, .15), xlim=c(0.35, .65)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  guides(linetype=guide_legend(title="Initial\nitems lacked")) +
  guides(shape=guide_legend(title="Additional\nitems lacked")) +
  guides(size=guide_legend(title="")) +
  theme(legend.position = "right",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 7 ", n_items, ".", grp_size, 
              " - saving vs loss adaptive.png"))


# 6a. Adaptive - all years ------------------------------------------------

## repeat previous for all years (ex. first)

# df_md8a: from df_md, indiv data for all years (ex. first)
# - mdsum_1/2/3 - score with 1/2/3 groups of items by rank
# - grpsize 
# df_result4 - from df_md8a, summary for all years
# - mdsum_adapt - adaptive measure applying cut points (cuts)

# set group size and number of groups
grp_size <- 5
grp_number <- 3

# set up dfs
df_md8a <- data.frame()
df_result4 <- data.frame()

# loop thru' years
for (yr in (year_min+1):year_max) {
  df_temp <- data.frame()
  df_temp2 <- data.frame()
  
  # loop to make mdsum_i measures
  # - fn_adapt makes 'mdsum_i' using items with rank <= 'grpsize' only
  for (i in 1:grp_number){
    grpsize <- grp_size * i
    df_temp <- fn_adapt(df_md[df_md$year == yr, ],
                        grpsize)   
    df_temp$grpsize <- grp_size    # i.e. original grp_size
    if (i == 1){
      df_temp2 <- df_temp[ranka]
    } else {
      df_temp2 <- cbind(df_temp2, df_temp[ranka])
    }
    df_temp2$mdsum_a <- df_temp$mdsum_a
    df_temp2$grpsize <- grp_size
    colnames(df_temp2) <- colnames(df_temp2) %>%
      str_replace("ranka_", paste0("rank", i, "_")) %>%
      str_replace("mdsum_a", paste0("mdsum_", i))
  }
  rm(df_temp)
  
  # add in the original vars - dropping first year
  df_temp2 <- cbind(df_md[df_md$year == yr, ], df_temp2)
  
  # df_result3 
  # from df_temp2, aggregate to each i/j combination
  # - measure of savings and info loss
  # - loss_pct - sum of lacks missed as pct of all lacks
  # - loss2_pct - number of dpvd missed as % of all depvd
  # - miss_pct - miss as % of cases
  # - miss2_pct - miss as % of depvd [same loss2_pct]
  df_result3 <- data.frame()
  
  # loop thru diff cut points - i.e. thresholds for stopping
  # - i - min number for first group
  # - i+j - min number for second group etc.
  # - using i=0/j=0 should re-produce the single group approach 
  #   - so checking consistency with one stage approach above
  iter_max <- 5     # limit to increment in additional items
  for (j in 0:(iter_max - 1)) {
    for (i in 0:(iter_max - 2)) {
      
      # set cut points or stopping rules
      cuts <- c(i, (i+j), (i+2*j))
      
      # make temp df for calcs
      list <- c('mdsum', 'mdsum_1', 'mdsum_2', 'mdsum_3', 'grpsize')
      df <- df_temp2[list]
      
      # ## THINK THIS IS REDUNDANT SO # OUT
      # # make measure of 'saving' in qn time
      # grpsize <- df$grpsize[1]    # takes first value
      
      # make the adaptive measure using cuts
      # - default is full measure
      df$mdsum_adapt <- df$mdsum
      # - cases where stop after first group
      df$mdsum_adapt[df$mdsum_1 <= cuts[1]] <- df$mdsum_1[df$mdsum_1 <= cuts[1]]
      # - cases where stop after second group
      df$mdsum_adapt[df$mdsum_1 > cuts[1] & 
                       df$mdsum_2 <= cuts[2]] <- df$mdsum_2[df$mdsum_1 > cuts[1] & 
                                                              df$mdsum_2 <= cuts[2]]
      # - cases where stop after third group
      df$mdsum_adapt[df$mdsum_1 > cuts[1] & 
                       df$mdsum_2 > cuts[2] & 
                       df$mdsum_3 <= cuts[3]] <- df$mdsum_3[df$mdsum_1 > cuts[1] & 
                                                              df$mdsum_2 > cuts[2] & 
                                                              df$mdsum_3 <= cuts[3]]
      # table(df$mdsum, df$mdsum_adapt)
      
      # make measure of 'information loss' - no. lacks missed
      # - for each indiv - number items 'missed'
      df$loss <- df$mdsum - df$mdsum_adapt
      # - sum of items missed
      loss <- sum(df$loss)
      loss_pct <- loss/sum(df$mdsum)
      
      # make the savings measure using cuts
      df$save <- 0
      df$save[df$mdsum_1 <= cuts[1]] <- (n_items - grp_size)
      df$save[df$mdsum_1 > cuts[1] & 
                df$mdsum_2 <= cuts[2]] <- (n_items - 2*grp_size)
      df$save[df$mdsum_1 > cuts[1] & 
                df$mdsum_2 > cuts[2] & 
                df$mdsum_3 <= cuts[3]] <- (n_items - 3*grp_size)
      save <- sum(df$save)
      save_pct <- save/(nrow(df) * n_items)
      
      
      # make the misclassification measure
      # - num of items lacking to be deprvd - from analysis of MDCH and mdsum
      cut_miss <- 7   # 7+ = deprived
      # df$mdch <- 0
      # df$mdch[df$mdsum >= cut_miss] <- 1
      # df$mdch_adapt <- 0
      # df$mdch_adapt[df$mdsum_adapt >= cut_miss] <- 1
      # table(df$mdch, df$mdch_adapt)
      df$miss <- (df$mdsum >= cut_miss) - (df$mdsum_adapt >= cut_miss)
      miss <- sum(df$miss)
      miss_pct <- miss/nrow(df)   # % of cases
      miss2_pct <- miss/sum(df$mdsum >= cut_miss)    # % of depvd
      
      # savings and losses
      df_result3 <- rbind(df_result3, c(i, j, loss_pct,  
                                      save_pct, miss_pct, miss2_pct))
    }
  }
  rm(df)
  
  df_result3$year <- yr   # add year
  cols <- c("initial", "increment", "loss_pct", "save_pct", "miss_pct", 
            "miss2_pct", "year")
  colnames(df_result3) <- cols
  
  # add results for current year (df_result3) to df_result4
  df_result4 <- rbind(df_result4, df_result3)  # store all 
  
  # add current year to combined file
  df_md8a <- rbind(df_md8a, df_temp2)
}
rm(df_result3)


#  - line chart - % save vs % of poor missed by i/j - grpsize == 5
p11 <- ggplot(data = df_result4[df_result4$initial == 1 & 
                                  df_result4$increment == 1, ],
              aes(x = save_pct, y = miss2_pct))
p11 + geom_point(size = 2) +
  labs (y = "Percent of 'deprived' missed\n",
        x = "\nSaving") +
  scale_x_continuous(labels = scales::percent_format(accuracy=2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=.01)) +
  guides(colour=guide_legend(title="Initial\nitems lacked")) +
  guides(shape=guide_legend(title="Additional\nitems lacked")) +
  guides(size=guide_legend(title="")) +
  theme(legend.position = "right",
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))



# 6b. Evaluation figs (cuts = 1/2/3) --------------------------------------
#  version with bigger saving but more loss at item level

# 6b.0 overall rate figs --------------------------------------------------
#  add on to df_md8a a 'final' mdsum_adapt

# set cut points or stopping rules
cuts <- c(1, 2, 3)

# make the adaptive measure using cuts
# - default is full measure
df_md8a$mdsum_adapt <- df_md8a$mdsum
# - cases where stop after first group
df_md8a$mdsum_adapt[df_md8a$mdsum_1 <= cuts[1]] <- df_md8a$mdsum_1[df_md8a$mdsum_1 <= cuts[1]]
# - cases where stop after second group
df_md8a$mdsum_adapt[df_md8a$mdsum_1 > cuts[1] & 
                 df_md8a$mdsum_2 <= cuts[2]] <- df_md8a$mdsum_2[df_md8a$mdsum_1 > cuts[1] & 
                                                        df_md8a$mdsum_2 <= cuts[2]]
# - cases where stop after third group
df_md8a$mdsum_adapt[df_md8a$mdsum_1 > cuts[1] & 
                 df_md8a$mdsum_2 > cuts[2] & 
                 df_md8a$mdsum_3 <= cuts[3]] <- df_md8a$mdsum_3[df_md8a$mdsum_1 > cuts[1] & 
                                                        df_md8a$mdsum_2 > cuts[2] & 
                                                        df_md8a$mdsum_3 <= cuts[3]]
table(df_md8a$mdsum, df_md8a$mdsum_adapt)

# make depvd dummies using full mdsum and mdsum_adapt (0/1)
df_md8a$md_full <- as.numeric(cut(df_md8a$mdsum, 
                     c(0, 7, 25), right = FALSE, labels=c(0,1))) - 1
df_md8a$md_adapt <- as.numeric(cut(df_md8a$mdsum_adapt, 
                     c(0, 7, 25), right = FALSE, labels=c(0,1))) -1
# compare full and adaptive depvn - just 29 cases in 14,759 depvd missed!!
# - 0.2% of depvd missed on ave
table(df_md8a$md_full, df_md8a$md_adapt)

# make very depvd dummies using full mdsum and mdsum_adapt (0/1)
df_md8a$md_full2 <- as.numeric(cut(df_md8a$mdsum, 
                                  c(0, 10, 25), right = FALSE, labels=c(0,1))) - 1
df_md8a$md_adapt2 <- as.numeric(cut(df_md8a$mdsum_adapt, 
                                   c(0, 10, 25), right = FALSE, labels=c(0,1))) -1
# compare full and adaptive depvn - just 29 cases in 14,759 depvd missed!!
# - 0.2% of depvd missed on ave
table(df_md8a$md_full2, df_md8a$md_adapt2)


# figs
vars <- c("year", "md_full", "md_adapt", "md_full2", "md_adapt2")
df_temp <- df_md8a[vars] %>%
  group_by(year) %>%
  summarise_all(list(~mean(.))) %>%
  gather(key = "Measure", value = "Percent", - year)

# make level
df_temp$level <- 0
df_temp$level[df_temp$Measure == "md_full2" | 
                df_temp$Measure == "md_adapt2"] <- 1

# rename Measure
df_temp$Measure[df_temp$Measure == "md_full"] <- "Full scale"
df_temp$Measure[df_temp$Measure == "md_adapt"] <- "Adaptive scale"
df_temp$Measure[df_temp$Measure == "md_full2"] <- "Full scale"
df_temp$Measure[df_temp$Measure == "md_adapt2"] <- "Adaptive scale"

df_temp$level <- factor(df_temp$level, levels = c(0,1),
                        labels = c("Lack 7+  (>= 25/100)", "Lack 10+  (>= 40/100)"))

# PAPERFIG 8
# stacked bar chart - proportion of lack in each group by level of depvn
p11 <- ggplot(data = df_temp,
              aes(x = year, y = Percent))
p11 + geom_line(aes(colour = Measure, linetype = Measure), size = 1.2) +
  scale_color_grey(start = 0.2, end = 0.7) +
  facet_wrap(~ level) + 
  labs (y = "Percent deprived\n",
        x = "\nYear") +
  guides(fill=guide_legend(title="")) + 
  scale_x_continuous(breaks = seq((year_min+1),year_max,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), 
                     limits = c(0, .25)) +
  theme(legend.position = "right",
        legend.title = element_text(size=title.text.size, face="bold"), 
        legend.text=element_text(size=title.text.size), 
        strip.text = element_text(size=axis.text.size, face="bold"),
        axis.ticks = element_blank(),
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text.x = element_text(size=axis.text.size, angle=90),
        axis.text.y = element_text(size=axis.text.size))
ggsave("Figs/fig 8 - evaluation 1.png")
rm(df_temp)


# 6b.1 correlations -------------------------------------------------------
# ought to be better way than loop to get this but works
# two correlations: 
#  - all cases
#  - dropping those where mdsum == 0 since may swamp

vars <- c("year", "mdsum", "mdsum_adapt")
df_temp <- df_md8a[vars] 
for (yr in 2011:2017){
  df_t <- df_temp[df_temp$year == yr,]
  print(cor(df_t$mdsum, df_t$mdsum_adapt))
}

# correlations - two versions  
df_temp <- df_md8a[vars]
df_t2 <- data.frame()
for (yr in 2011:2017){
  df_t <- df_temp[df_temp$year == yr,]
  c <- cor(df_t$mdsum, df_t$mdsum_adapt)
  df_t <- df_temp[df_temp$year == yr & df_temp$mdsum > 0,]
  c_ex_0 <- cor(df_t$mdsum, df_t$mdsum_adapt)
  df_t1 <- data.frame(yr, c, c_ex_0)
  df_t2 <- rbind(df_t2, df_t1)
}
names(df_t2) <- c("Year", "Correlation", "Correlation ex. zeroes")
df_t2
# write file
write.csv(df_t2, file = paste0("Data/table 6b correlations.csv"))

# proportion of zero cases in each year
df_temp <- df_md8a[vars]
df_t2 <- data.frame()
for (yr in 2011:2017){
  pct <- nrow(df_temp[df_temp$year == yr & df_temp$mdsum == 0,])/
    nrow(df_temp[df_temp$year == yr,])
  df_t1 <- data.frame(yr, pct)
  df_t2 <- rbind(df_t2, df_t1)
}
names(df_t2) <- c("yr", "pct")
df_t2
# write file
write.csv(df_t2, file = paste0("Data/table 6b correlations.csv"))


rm(df_temp)
rm(df_t)
rm(df_t1)
rm(df_t2)


# 6b2. Lacking %s ----------------------------------------------------------

# df_md12 - df_md8a, for latest year
# - lack cols have all lacks
# - rank1_xx - lack item and in first group
df_md12 <- df_md8a[df_md8a$year == year_max,]

# adapt_grp - adaptive measure groups
# - default is 0 (full measure)
df_md12$adapt_grp <- 4
# - cases where stop after first group
df_md12$adapt_grp[df_md12$mdsum_1 <= cuts[1]] <- 1
# - cases where stop after second group
df_md12$adapt_grp[df_md12$mdsum_1 > cuts[1] &
                      df_md12$mdsum_2 <= cuts[2]] <- 2
# - cases where stop after third group
df_md12$adapt_grp[df_md12$mdsum_1 > cuts[1] &
                      df_md12$mdsum_2 > cuts[2] &
                      df_md12$mdsum_3 <= cuts[3]] <- 3
table(df_md12$adapt_grp, df_md12$mdsum)

# make lack_adapt list
lack_adapt <- paste0("adapt_", lack)
rank1 <- paste0("rank1_", lack)
rank2 <- paste0("rank2_", lack)
rank3 <- paste0("rank3_", lack)
rank4 <- lack    # where asked all qns

# lack_adapt vars: 1 if lack item and item is in the relevant group of vars 
df_md12[lack_adapt] <- df_md12[lack] * df_md12[rank1] * (df_md12$adapt_grp == 1) +
  df_md12[lack] * df_md12[rank2] * (df_md12$adapt_grp == 2) +
  df_md12[lack] * df_md12[rank3] * (df_md12$adapt_grp == 3) +
  df_md12[lack] * df_md12[rank4] * (df_md12$adapt_grp == 4)

# # check this produces same total as mdsum_adapt
# table(df_md12$mdsum_adapt, rowSums(df_md12[lack_adapt], na.rm = TRUE))

# # captures 95.2% of all item lacking
# temp <- df_md12[c(lack, lack_adapt)] 
# temp[is.na(temp)] <- 0
# sum(temp[lack_adapt])/sum(temp[lack])
# rm(temp)

# manipulations to end up with table for figs
# - includes patching in Dffclt_rank for the year to order bars
df_md13 <- df_md12[c("year", lack, lack_adapt)] %>%
  summarise_all(list(~mean(., na.rm = TRUE))) %>%
  gather(key = "item", value = "Percent", - year) %>%
  mutate(adapt = (grepl("adapt_", item)), 
         item = gsub("adapt_", "", item)) %>%
  spread(adapt, Percent) %>%
  rename(Full = "FALSE", Adaptive = "TRUE") %>%
  mutate(Difference = (Full - Adaptive)/Full,
         Diff_label = sprintf("%.1f%%", 100*Difference)) %>%
  gather(key = "Measure", value = "Percent", - year, - item, - Difference, - Diff_label) %>%
  left_join(df_dffclt[c("year", "item", "Dffclt_rank")], 
            by = c("item", "year"))
df_md13$Diff_label[df_md13$Measure == "Adaptive"] <- ""
df_md13$Diff_label[df_md13$Difference == 0] <- ""

# PAPERFIG 9A
# overlapping bar chart - proportion of lack on full and adpative measures
p11 <- ggplot(data = df_md13,
              aes(x = reorder(item, Dffclt_rank, FUN = median),
                  y = Percent))
p11 + geom_bar(stat = "identity", position = "identity", aes(fill = Measure)) +
  scale_fill_grey(start = 0.3, end = 0.6) +
  geom_text(aes(label=Diff_label), nudge_y = .07, size = 5) + 
  coord_flip() + 
  labs (y = "Percent lacking\n",
        x = "\nItem") +
  guides(fill=guide_legend(title="")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
  theme(legend.position = "right",
        legend.text=element_text(size=title.text.size), 
        axis.ticks = element_blank(),
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 9a - lacking full & adaptive 2017.png"))


# 6c. Evaluation figs (cuts = 0/1/2) --------------------------------------
# version with lower level of saving but less info loss at item level
# NB that this writes over 6b/6b1

# 6c.0 overall rate figs --------------------------------------------------
#  add on to df_md8a a 'final' mdsum_adapt

# set cut points or stopping rules
cuts <- c(0, 1, 2)

# make the adaptive measure using cuts
# - default is full measure
df_md8a$mdsum_adapt <- df_md8a$mdsum
# - cases where stop after first group
df_md8a$mdsum_adapt[df_md8a$mdsum_1 <= cuts[1]] <- df_md8a$mdsum_1[df_md8a$mdsum_1 <= cuts[1]]
# - cases where stop after second group
df_md8a$mdsum_adapt[df_md8a$mdsum_1 > cuts[1] & 
                      df_md8a$mdsum_2 <= cuts[2]] <- df_md8a$mdsum_2[df_md8a$mdsum_1 > cuts[1] & 
                                                                       df_md8a$mdsum_2 <= cuts[2]]
# - cases where stop after third group
df_md8a$mdsum_adapt[df_md8a$mdsum_1 > cuts[1] & 
                      df_md8a$mdsum_2 > cuts[2] & 
                      df_md8a$mdsum_3 <= cuts[3]] <- df_md8a$mdsum_3[df_md8a$mdsum_1 > cuts[1] & 
                                                                       df_md8a$mdsum_2 > cuts[2] & 
                                                                       df_md8a$mdsum_3 <= cuts[3]]
table(df_md8a$mdsum, df_md8a$mdsum_adapt)

# make depvd dummies using full mdsum and mdsum_adapt (0/1)
df_md8a$md_full <- as.numeric(cut(df_md8a$mdsum, 
                                  c(0, 7, 25), right = FALSE, labels=c(0,1))) - 1
df_md8a$md_adapt <- as.numeric(cut(df_md8a$mdsum_adapt, 
                                   c(0, 7, 25), right = FALSE, labels=c(0,1))) -1
# compare full and adaptive depvn - just 29 cases in 14,759 depvd missed!!
# - 0.2% of depvd missed on ave
table(df_md8a$md_full, df_md8a$md_adapt)

# make very depvd dummies using full mdsum and mdsum_adapt (0/1)
df_md8a$md_full2 <- as.numeric(cut(df_md8a$mdsum, 
                                   c(0, 10, 25), right = FALSE, labels=c(0,1))) - 1
df_md8a$md_adapt2 <- as.numeric(cut(df_md8a$mdsum_adapt, 
                                    c(0, 10, 25), right = FALSE, labels=c(0,1))) -1
# compare full and adaptive depvn - just 29 cases in 14,759 depvd missed!!
# - 0.2% of depvd missed on ave
table(df_md8a$md_full2, df_md8a$md_adapt2)


# figs
vars <- c("year", "md_full", "md_adapt", "md_full2", "md_adapt2")
df_temp <- df_md8a[vars] %>%
  group_by(year) %>%
  summarise_all(list(~mean(.))) %>%
  gather(key = "Measure", value = "Percent", - year)

# make level
df_temp$level <- 0
df_temp$level[df_temp$Measure == "md_full2" | 
                df_temp$Measure == "md_adapt2"] <- 1

# rename Measure
df_temp$Measure[df_temp$Measure == "md_full"] <- "Full scale"
df_temp$Measure[df_temp$Measure == "md_adapt"] <- "Adaptive scale"
df_temp$Measure[df_temp$Measure == "md_full2"] <- "Full scale"
df_temp$Measure[df_temp$Measure == "md_adapt2"] <- "Adaptive scale"

df_temp$level <- factor(df_temp$level, levels = c(0,1),
                        labels = c("Lack 7+  (>= 25/100)", "Lack 10+  (>= 40/100)"))

# equiv to fig 8 but diff trade-off
# stacked bar chart - proportion of lack in each group by level of depvn
p11 <- ggplot(data = df_temp,
              aes(x = year, y = Percent))
p11 + geom_line(aes(colour = Measure, linetype = Measure), size = 1.2) +
  facet_wrap(~ level) + 
  scale_fill_brewer() +
  labs (y = "Percent deprived\n",
        x = "\nYear") +
  guides(fill=guide_legend(title="")) + 
  scale_x_continuous(breaks = seq((year_min+1),year_max,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), 
                     limits = c(0, .25)) +
  theme(legend.position = "right",
        legend.title = element_text(size=title.text.size, face="bold"), 
        legend.text=element_text(size=title.text.size), 
        strip.text = element_text(size=axis.text.size, face="bold"),
        axis.ticks = element_blank(),
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text.x = element_text(size=axis.text.size, angle=90),
        axis.text.y = element_text(size=axis.text.size))
rm(df_temp)



# 6c.1 correlations -------------------------------------------------------
# ought to be better way than loop to get this but works
# two correlations: 
#  - all cases
#  - dropping those where mdsum == 0 since may swamp

vars <- c("year", "mdsum", "mdsum_adapt")
df_temp <- df_md8a[vars] 
for (yr in 2011:2017){
  df_t <- df_temp[df_temp$year == yr,]
  print(cor(df_t$mdsum, df_t$mdsum_adapt))
}

df_temp <- df_md8a[vars]
df_t2 <- data.frame()
for (yr in 2011:2017){
  df_t <- df_temp[df_temp$year == yr,]
  c <- cor(df_t$mdsum, df_t$mdsum_adapt)
  df_t <- df_temp[df_temp$year == yr & df_temp$mdsum > 0,]
  c_ex_0 <- cor(df_t$mdsum, df_t$mdsum_adapt)
  df_t1 <- data.frame(yr, c, c_ex_0)
  df_t2 <- rbind(df_t2, df_t1)
}
names(df_t2) <- c("Year", "Correlation", "Correlation ex. zeroes")
df_t2

# don't write to file here - paper uses first version

rm(df_temp)
rm(df_t)
rm(df_t1)
rm(df_t2)


# 6c.2 Lacking %s ----------------------------------------------------------

# df_md12 - df_md8a, for latest year
# - lack cols have all lacks
# - rank1_xx - lack item and in first group
df_md12 <- df_md8a[df_md8a$year == year_max,]

# adapt_grp - adaptive measure groups
# - default is 0 (full measure)
df_md12$adapt_grp <- 4
# - cases where stop after first group
df_md12$adapt_grp[df_md12$mdsum_1 <= cuts[1]] <- 1
# - cases where stop after second group
df_md12$adapt_grp[df_md12$mdsum_1 > cuts[1] &
                    df_md12$mdsum_2 <= cuts[2]] <- 2
# - cases where stop after third group
df_md12$adapt_grp[df_md12$mdsum_1 > cuts[1] &
                    df_md12$mdsum_2 > cuts[2] &
                    df_md12$mdsum_3 <= cuts[3]] <- 3
table(df_md12$adapt_grp, df_md12$mdsum)

# make lack_adapt list
lack_adapt <- paste0("adapt_", lack)
rank1 <- paste0("rank1_", lack)
rank2 <- paste0("rank2_", lack)
rank3 <- paste0("rank3_", lack)
rank4 <- lack    # where asked all qns

# lack_adapt vars: 1 if lack item and item is in the relevant group of vars 
df_md12[lack_adapt] <- df_md12[lack] * df_md12[rank1] * (df_md12$adapt_grp == 1) +
  df_md12[lack] * df_md12[rank2] * (df_md12$adapt_grp == 2) +
  df_md12[lack] * df_md12[rank3] * (df_md12$adapt_grp == 3) +
  df_md12[lack] * df_md12[rank4] * (df_md12$adapt_grp == 4)

# # check this produces same total as mdsum_adapt
# table(df_md12$mdsum_adapt, rowSums(df_md12[lack_adapt], na.rm = TRUE))

# captures 97.6% of all item lacking
temp <- df_md12[c(lack, lack_adapt)]
temp[is.na(temp)] <- 0
sum(temp[lack_adapt])/sum(temp[lack])
# drop outdoor_play
(sum(temp[lack_adapt])-sum(temp$adapt_outdoor_play))/(sum(temp[lack])-sum(temp$outdoor_play))
rm(temp)


# manipulations to end up with table for figs
# - includes patching in Dffclt_rank for the year to order bars
df_md13 <- df_md12[c("year", lack, lack_adapt)] %>%
  summarise_all(list(~mean(., na.rm = TRUE))) %>%
  gather(key = "item", value = "Percent", - year) %>%
  mutate(adapt = (grepl("adapt_", item)), 
         item = gsub("adapt_", "", item)) %>%
  spread(adapt, Percent) %>%
  rename(Full = "FALSE", Adaptive = "TRUE") %>%
  mutate(Difference = (Full - Adaptive)/Full,
         Diff_label = sprintf("%.1f%%", 100*Difference)) %>%
  gather(key = "Measure", value = "Percent", - year, - item, - Difference, - Diff_label) %>%
  left_join(df_dffclt[c("year", "item", "Dffclt_rank")], 
            by = c("item", "year"))
df_md13$Diff_label[df_md13$Measure == "Adaptive"] <- ""
df_md13$Diff_label[df_md13$Difference == 0] <- ""

# PAPERFIG 9B
# overlapping bar chart - proportion of lack on full and adpative measures
p11 <- ggplot(data = df_md13,
              aes(x = reorder(item, Dffclt_rank, FUN = median),
                  y = Percent))
p11 + geom_bar(stat = "identity", position = "identity", aes(fill = Measure)) +
  scale_fill_grey(start = 0.3, end = 0.6) +
  geom_text(aes(label=Diff_label), nudge_y = .07, size = 5) + 
  coord_flip() + 
  labs (y = "Percent lacking\n",
        x = "\nItem") +
  guides(fill=guide_legend(title="")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
  theme(legend.position = "right",
        legend.text=element_text(size=title.text.size), 
        axis.ticks = element_blank(),
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig 9b - lacking full & adaptive 2017.png"))


# 6d. boxplot - items lacked by level (latest year) ------------------------

# df_md9 - subset df_md8 (latest year only)
vars <- c("year", "mdsum", "mdsum_1", "mdsum_2", "mdsum_3")
df_md9 <- df_md8[vars][df_md8$mdsum > 0, ]
# make count of no. items lacked in each of the four groups
df_md9$grp1 <- df_md9$mdsum_1
df_md9$grp2 <- df_md9$mdsum_2 - df_md9$mdsum_1
df_md9$grp3 <- df_md9$mdsum_3 - df_md9$mdsum_2
df_md9$grp4 <- df_md9$mdsum - df_md9$mdsum_3
# lists to refer to cols and reverse order
vars2 <- colnames(df_md9[6:9])
vars3 <- rev(vars2)
# # check
# df_md9$check <- df_md9$mdsum - rowSums(df_md9[vars2])
# summary(df_md9$check)

# aggregate to mdsum x four groups and tidy
df_md9 <- df_md9 %>%
  group_by(mdsum) %>%
  summarise_at(vars2,
               list(~mean(., na.rm = TRUE))) %>%
  gather(key = "group", value = "percent", -mdsum) 

# make group a factor, reverse order and change labels
df_md9$group <- factor(df_md9$group, levels=rev(vars2))
levels(df_md9$group) <- c("Last group", "Third group", "Second group", "First group")

# stacked bar chart - proportion of lack in each group by level of depvn
p11 <- ggplot(data = df_md9,
              aes(x = mdsum, y = percent))
p11 + geom_bar(stat = "identity", aes(fill = group),
               position = "fill") +
  scale_fill_brewer(palette = "") +
  labs (y = "Percent\n",
        x = "\nNo. of items lacked") +
  guides(fill=guide_legend(title="")) + 
  scale_x_continuous(breaks = seq(2,20,2), 
                     minor_breaks = seq(1,20,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy=2)) +
  theme(legend.position = "right",
        legend.text=element_text(size=title.text.size), 
        axis.ticks = element_blank(),
        axis.title=element_text(size=title.text.size, face="bold"),
        axis.text=element_text(size=axis.text.size))
ggsave(paste0("Figs/fig", n_items, "- item group by mdsum stack bar.png"))


## curious anomaly for mdsum=1 - higher % in grp2/grp3

# df_md10 - just cases with depvn 1
df_temp1 <- df_md8[c(lack, rank)][df_md8$mdsum == 1, ] %>%
  summarise_at(lack, list(~mean(., na.rm = TRUE))) %>%
  gather(value = "percent")
df_temp2 <- df_md8[c(lack, rank)][df_md8$mdsum == 1, ] %>%
  summarise_at(rank, list(~max(.))) %>%
  gather(value = "rank")
df_md10 <- cbind(df_temp1, df_temp2)
df_md10 <- df_md10[with(df_md10, order(rank)), ]
# summaries show that it is 'outdoor_play' which is anomaly!
barplot(df_md10$percent)
summary(df_md8[rank])


## repeat for all mdsum

# df_md11 - all cases - latest year
df_temp1 <- df_md8[c("mdsum", lack, rank)] %>%
  group_by(mdsum) %>%
  summarise_at(lack, list(~mean(., na.rm = TRUE))) %>%
  gather(lack, value = "percent", -mdsum)
df_temp2 <- df_md8[c("mdsum", lack, rank)] %>%
  summarise_at(rank, list(~max(.))) %>%
  gather(value = "rank")
df_temp2$key <- str_replace(df_temp2$key, "rank_", "")

df_temp1 <- df_temp1 %>%
  left_join(df_temp2, by = c("lack" = "key"))
df_md11 <- df_temp1[with(df_temp1, order(df_temp1$rank)), ]
df_md11$lack <- factor(df_md11$lack)

# heatmap 
p <- ggplot(df_md11, aes(x = mdsum, y = reorder(lack, rank))) + 
  geom_tile(aes(fill = percent), colour = "white") 
p + scale_fill_gradient(low = "white", high = "steelblue")

# tidy up
rm(df_temp1)
rm(df_temp2)


  

# 8. Relationship with poverty --------------------------------------------

# make the mdsum_adapt measure on df_md8
#  - select cuts on basis of info in figs
#  - make mdsum_adapt on df_md8

# set cut points
cuts <- c(0, 1, 2)   # gives 36% saving
# cuts <- c(0, 2, 4)   # gives 42% saving
# cuts <- c(2, 3, 4)   # gives 51% saving

## LOOKS REDUNDANT - JUST USE grp_size SET AT START OF 6?
# make measure of 'saving' in qn time
grpsize <- df_md8$grpsize[1]    # takes first value

# make the adaptive measure using cuts
df_md8$mdsum_adapt <- df_md8$mdsum
df_md8$mdsum_adapt[df_md8$mdsum_1 <= cuts[1]] <- df_md8$mdsum_1[df_md8$mdsum_1 <= cuts[1]]
df_md8$mdsum_adapt[df_md8$mdsum_1 > cuts[1] & 
                 df_md8$mdsum_2 <= cuts[2]] <- df_md8$mdsum_2[df_md8$mdsum_1 > cuts[1] & 
                                                        df_md8$mdsum_2 <= cuts[2]]
df_md8$mdsum_adapt[df_md8$mdsum_1 > cuts[1] & 
                 df_md8$mdsum_2 > cuts[2] & 
                 df_md8$mdsum_3 <= cuts[3]] <- df_md8$mdsum_3[df_md8$mdsum_1 > cuts[1] & 
                                                        df_md8$mdsum_2 > cuts[2] & 
                                                        df_md8$mdsum_3 <= cuts[3]]
cor(df_md8$mdsum, df_md8$mdsum_adapt)

# # box plot 
# p14 <- ggplot(data=df_md8,
#              aes(x = factor(mdsum), y = mdsum_adapt))
# p14 + 
#   geom_jitter() +
#   labs(x = "Items lacked - full measure",
#        y = "Items lacked - adaptive measure")

# make log income
df_md8$income_log <- log(df_md8$S_OE_AHC)

# box plot - income
p14 <- ggplot(data=df_md8,
              aes(x = factor(mdsum), y = income_log))
p14 + geom_boxplot() +
  labs(x = "Items lacked - full measure",
       y = "Equivalised income")

# linear regression model - exclude income=- as log inc missing
fit1 <- lm(income_log ~ mdsum + factor(year) + mdsum * factor(year), 
           data = df_md8[df_md8$S_OE_AHC >0, ])
fit2 <- lm(income_log ~ factor(mdsum) + factor(year)+ factor(mdsum) * factor(year), 
           data = df_md8[df_md8$S_OE_AHC >0, ])
fit1a <- lm(income_log ~ mdsum_adapt + factor(year)+ mdsum_adapt * factor(year), 
            data = df_md8[df_md8$S_OE_AHC >0, ])
fit2a <- lm(income_log ~ factor(mdsum_adapt) + factor(year) + factor(mdsum_adapt) * factor(year), 
            data = df_md8[df_md8$S_OE_AHC >0, ])

summary(fit1)

print(c(summary(fit1)$adj.r.squared *100, 
        summary(fit1a)$adj.r.squared*100))
print(c(summary(fit2)$adj.r.squared*100, 
        summary(fit2a)$adj.r.squared*100))


# as previously but with logistic regression models
log1 <- glm(LOW60AHC ~ mdsum + factor(year), 
                data=df_md8, 
                family=binomial(link="logit"))
log1a <- glm(LOW60AHC ~ mdsum_adapt + factor(year), 
            data=df_md8, 
            family=binomial(link="logit"))
log2 <- glm(LOW60AHC ~ factor(mdsum) + factor(year), 
            data=df_md8, 
            family=binomial(link="logit"))
log2a <- glm(LOW60AHC ~ factor(mdsum_adapt) + factor(year), 
             data=df_md8, 
             family=binomial(link="logit"))
summary(log1)
summary(log1a)
summary(log2)
summary(log2a)

# Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(df_md8$LOW60AHC, fitted(log1))
hoslem.test(df_md8$LOW60AHC, fitted(log1a))

