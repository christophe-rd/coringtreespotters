# 12 September 2025
# Cleaning and plotting core measurements 


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 150) 
options(digits = 3)
# quartz()

# Load library 
library(ggplot2)
library(wesanderson)

# Set main directory
directory <- "/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/input/cores"

setwd(directory)

files <- list.files(pattern = "\\.csv$")

process_core <- function(f) {
  df <- read.csv(f)
  
  df$Year <- seq(2024, by = -1, length.out = nrow(df))
  
  fname <- sub("\\.csv$", "", f)
  
  parts <- strsplit(fname, "_")[[1]]
  species <- parts[1]
  code <- parts[2]
  letter <- parts[3]
  rep <- parts[4]
  
  df$Species <- species
  df$Code <- code
  df$Letter <- letter
  df$Rep <- rep
  
  return(df)
}

# bind all files together
all_cores <- do.call(rbind, lapply(files, process_core))

# clean some names
all_cores$Code[which(all_cores$Code == "525-2069")] <- "525-2009"
all_cores$Code[which(all_cores$Code == "385-82")] <- "358-82"
all_cores$Code[which(all_cores$Code == "22-834")] <- "22834"


all_cores$id <- paste(all_cores$Species, all_cores$Code, all_cores$Letter, sep = "_")
all_cores$idrep <- paste(all_cores$Species, all_cores$Code, all_cores$Letter, all_cores$Rep, sep = "_")

# keep only the columns i want
all_cores2 <- all_cores[, c("X", "id", "idrep", "Species", "Code", "Letter", "Rep", "Year", "Length")]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Make some checks
# read og file
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
og <- read.csv("output/treesToCoredes.csv")

corescannotscan <- read.csv("input/cores/coresCannotScan/coresCannotScan.csv")

og$name <- gsub("\\*", "_",og$name)
og$name <- gsub("\\|", "_", og$name)
og$name <- gsub(" ", "", og$name)

og$name[grepl("6990", og$name)]


# temporary id col to fit these guys
all_cores$id2 <- paste(all_cores$Code, all_cores$Letter, all_cores$Species, sep = "_")
veccores <- unique(all_cores$id2)
og_vec <- og$name

setdiff(og_vec, veccores) 
# beni 1199 D is ok.
# beni 1199 J_I: will be scanned
# 12651_I_AEFL: ok
# 1323-82_A_TIAM: ok
# 17527_D_TIAM: ok
# 17538_A_TIAM: ok
# 19804_A_TIAM: ok
# 20098_A_CAGL: ok
# TIAM_7141_A_I: ok
# 925-79_B_AEFL: ok

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# convert inches to cm
all_cores2$lengthCM <- all_cores2$Length*2.54

all_cores2$scaled_length <- scale(all_cores2$lengthCM)

all_cores2$yearCor <- all_cores2$Year

#One of the cores is rotten outside, so Ill check if changing the ring dates may change something
all_cores2$yearCor[all_cores2$idrep == "CAOV_12907_G_II"] <- 
  all_cores2$year[all_cores2$idrep == "CAOV_12907_G_II"] - 5

# start by removing sepecies with litle replication **for now
coressub <- subset(all_cores2, !(Species %in% c("AEFL", "CAGL")))

# === === === === === === === === === === === === === === === === === 
# Plot each species #### 
# === === === === === === === === === === === === === === === === === 
# MARKER YEARS ##### 
markers <- c(1965, 
             1966, 
             1981, # gypsy moth defoliation
             2012, 
             2016)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# ACSA ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
ACSA <- subset(all_cores2, Species == "ACSA")
ggplot(ACSA, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = markers, linetype = "dashed", color = "black") +
  labs(
    title = "ACSA ring width series",
    x = "Year",
    y = "Ring width (Length)",
    color = "Core ID"
  ) +
  facet_wrap(~id, nrow = 5, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(ACSA$yearCor), max(ACSA$yearCor), by = 5)) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  scale_color_manual(values = wes_palette("FantasticFox1"))

ggsave("figures/acsaspaghetti_plot.jpeg", width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# BEAL ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
BEAL <- subset(all_cores2, Species == "BEAL")
ggplot(BEAL, aes(x = X, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "BEAL ring width series",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~id, nrow = length(unique(BEAL$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)+
  theme_minimal(base_size = 14) +
  scale_x_reverse(breaks=unique(BEAL$X))+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # tilt labels
  )


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# BENI ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
BENI <- subset(all_cores2, Species == "BENI")
ggplot(BENI, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "BENI ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~id, nrow = length(unique(BENI$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# BEAL and BENI ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
betula <- subset(all_cores2, Species %in% c("BENI", "BEAL"))
ggplot(betula, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "BEAL and BENI ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~id, nrow = length(unique(betula$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)
ggsave("figures/betulaspaghetti_plot.jpeg", width = 6, height = 8, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# CAOV ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
CAOV <- subset(all_cores2, Species == "CAOV")
ggplot(CAOV, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "CAOV ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~id, nrow = length(unique(CAOV$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# QUAL ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
QUAL <- subset(all_cores2, Species == "QUAL")
ggplot(QUAL, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = markers, linetype = "dashed") +
  labs(title = "QUAL ring width series",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~id, nrow = 5, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(ACSA$yearCor), max(ACSA$yearCor), by = 5)) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_manual(values = wes_palette("FantasticFox1")) 
ggsave("figures/qualspaghetti_plot.jpeg", width = 10, height = 6, units = "in", dpi = 300)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# QURU ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
QURU <- subset(all_cores2, Species == "QURU")
ggplot(QURU, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "QURU ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  facet_wrap(~id, nrow = length(unique(QURU$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)

# make a plot to show how many 

  

# check some pairs along
acrutest1 <- subset(all_cores2, Code == "525-2009")

ggplot(acrutest1, aes(x = yearCor, y = scaled_length, color = id, group = id)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Ring-width series per core",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  theme_minimal(base_size = 14)

acsatest1 <- subset(all_cores2, Code == "689-2010")

ggplot(acsatest1, aes(x = yearCor, y = scaled_length, color = id, group = id)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Ring-width series per core",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  theme_minimal(base_size = 14)


# LET'S DO THE dplR tutorial ####
library(dplR)
library(tidyverse)

# convert to long as rwl objects need to have series as columns and years as rows
### start with only 
qu <- subset(all_cores2, Species %in% c("QUAL", "QURU"))
nodup <- all_cores2[!duplicated(all_cores2$idrep),]
table(nodup$Species)

caov <- subset(all_cores2, Species %in% c("CAOV"))

rw_df <- all_cores2%>%
  select(idrep, Year, lengthCM) %>%
  pivot_wider(names_from = idrep, values_from = lengthCM) %>%
  arrange(Year) %>%
  column_to_rownames("Year") %>%
  as.data.frame()            # keep as data.frame
rw_df

# assign rwl class necessary for downstream analyses
class(rw_df) <- c("rwl", "data.frame")

# plot ring width series
# open jpeg device from the ggsave settings above
jpeg(filename = "figures/fullspag_plot.jpeg",
     width = 16,       
     height = 8,      
     units = "in",    
     res = 300)    
plot.rwl(rw_df, plot.type = "spag")  # spaghetti plot
dev.off()

plot.rwl(rw_df, plot.type = "seg", nyrs = 30)  # running segment means

# build stanadardized chronology
rw_chron <- chron(rw_df, prewhiten = TRUE)

# Plot chronology
plot(rw_chron, type = "l", lwd = 2,
     main = "Standard Chronology",
     xlab = "Year", ylab = "Ring Width Index")

# segment-wise crossdating
n_years <- apply(rw_df, 2, function(x) sum(!is.na(x)))
min_years <- min(n_years, na.rm = TRUE)

# Set segment length ≤ 1/2 shortest series
seg_len <- floor(min_years / 2)
seg_len <- max(seg_len, 3)  # at least 3 years

bin_floor <- max(floor(seg_len / 3), 2)  # rule-of-thumb

# Now run segment correlations safely
res <- corr.rwl.seg(rw_df, seg.length = seg_len, bin.floor = bin_floor, make.plot = TRUE)
summary(res)

# report
class(rw_df)
rwl.report(rw_df)
rw_df.stats <- summary(rw_df) 

boxplot(rw_df.stats$ar1,ylab=expression(phi[1]),col = "lightblue")
stripchart(rw_df.stats$ar1, vertical = TRUE,  
           method = "jitter", jitter = 0.02,add = TRUE, pch = 20, col = 'darkblue',cex=1.25)
ar1Quant <- quantile(rw_df.stats$ar1,probs = c(0.25,0.5,0.75))
abline(h=ar1Quant,lty="dashed",col="grey")
mtext(text = names(ar1Quant),side = 4,at = ar1Quant,las=2)

ca533.ids <- read.ids(rw_df, stc = c(3, 2, 1))
ca533.rwi <- detrend(rwl = rw_df, method = "AgeDepSpline")
rwi.stats(ca533.rwi, ca533.ids, prewhiten=TRUE)

ca533.rho <- interseries.cor(rw_df, prewhiten=TRUE,
                             method="spearman")
ca533.rho[1:5, ]

ca533.crn <- chron(ca533.rwi)
plot(ca533.crn, add.spline=TRUE, nyrs=20)


# CROSS-DATING ####
# FROM: https://rpubs.com/andybunn/xdate
dat.sum <- summary(rw_df)
dat.sum
rw_df_long2 <- rw_df[, dat.sum$year >= 40]
rwl.60 <- corr.rwl.seg(rw_df_long2, seg.length =1, pcrit=0.01)
