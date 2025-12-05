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

makeplots <- FALSE

# Set main directory
directory <- "/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/input/cores"

setwd(directory)

files <- list.files(pattern = "\\.csv$")

process_core <- function(f) {
  df <- read.csv(f)
  
  df$year <- seq(2024, by = -1, length.out = nrow(df))
  
  fname <- sub("\\.csv$", "", f)
  
  parts <- strsplit(fname, "_")[[1]]
  species <- parts[1]
  code <- parts[2]
  letter <- parts[3]
  rep <- parts[4]
  
  df$species <- species
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


all_cores$id <- paste(all_cores$species, all_cores$Code, all_cores$Letter, sep = "_")
all_cores$idrep <- paste(all_cores$species, all_cores$Code, all_cores$Letter, all_cores$Rep, sep = "_")

# keep only the columns i want
all_cores2 <- all_cores[, c("X", "id", "idrep", "species", "Code", "Letter", "Rep", "year", "Length")]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Make some checks
# read og file
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/")
og <- read.csv("output/treesToCoredes.csv")

# corescannotscan <- read.csv("input/cores/coresCannotScan/coresCannotScan.csv")

og$name <- gsub("\\*", "_",og$name)
og$name <- gsub("\\|", "_", og$name)
og$name <- gsub(" ", "", og$name)

og$name[grepl("6990", og$name)]


# temporary id col to fit these guys
all_cores$id2 <- paste(all_cores$Code, all_cores$Letter, all_cores$species, sep = "_")
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
# 3 cols
parts <- strsplit(og$name, "_")

og$spp    <- sapply(parts, `[`, 3)
og$code   <- sapply(parts, `[`, 1)
og$letter <- sapply(parts, `[`, 2)

og$id2 <- paste(og$spp, og$code, og$letter, sep = "_")



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# convert inches to cm
all_cores2$lengthCM <- all_cores2$Length*2.54

all_cores2$scaled_length <- scale(all_cores2$lengthCM)

all_cores2$yearCor <- all_cores2$year

#One of the cores is rotten outside, so Ill check if changing the ring dates may change something
all_cores2$yearCor[all_cores2$idrep == "CAOV_12907_G_II"] <- 
  all_cores2$year[all_cores2$idrep == "CAOV_12907_G_II"] - 5

# assuming there is a missing ring in 2021
all_cores2$yearCor[all_cores2$id == "BENI_1251-79_E"& all_cores2$year < 2022] <- 
  all_cores2$year[all_cores2$year < 2022 & all_cores2$id == "BENI_1251-79_E"] - 1 

# remove the first ring which i wasn't sure it was a ring but confirmed its something else
all_cores2 <- all_cores2[ !(all_cores2$idrep == "QUAL_22886_D_II" & 
                              all_cores2$year == 2024), ]
all_cores2$yearCor[all_cores2$idrep == "QUAL_22886_D_II"] <- 
  all_cores2$year[all_cores2$idrep == "QUAL_22886_D_II"] + 1

# start by removing sepecies with litle replication **for now
coressub <- subset(all_cores2, !(species %in% c("AEFL", "CAGL")))
coressub <- subset(coressub, select = c("id", "idrep", "species", "Code", "Letter", "Rep", "lengthCM", "yearCor"))


# === === === === === === === === === === === === === === === === === 
# Save csv #### 
# === === === === === === === === === === === === === === === === === 
write.csv(coressub, "output/ringWidthTS.csv")

if (makeplots) {

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
# ACRU ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
ACRU <- subset(all_cores2, species == "ACRU")
ggplot(ACRU, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = markers, linetype = "dashed", color = "black") +
  labs(
    title = "ACRU ring width series",
    x = "Year",
    y = "Ring width (Length)",
    color = "Core ID"
  ) +
  # facet_wrap(~id, nrow = 5, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(ACRU$yearCor), max(ACRU$yearCor), by = 5)) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
scale_color_manual(values = wes_palette("FantasticFox1"))

ggsave("figures/acsaspaghetti_plot.jpeg", width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# ACSA ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
all_cores2$yearCor[all_cores2$id == "ACSA_187-2006_B"] <-
  all_cores2$year[all_cores2$id == "ACSA_187-2006_B"] - 1

ACSA <- subset(all_cores2, species == "ACSA")
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
BEAL <- subset(all_cores2, species == "BEAL")
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
BENI <- subset(all_cores2, species == "BENI")

ggplot(BENI, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "BENI ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  # facet_wrap(~id, nrow = length(unique(BENI$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# BEAL and BENI ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
betula <- subset(all_cores2, species %in% c("BENI", "BEAL"))
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
# CAGL ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
CAGL <- subset(all_cores2, species == "CAGL")
ggplot(CAGL, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "CAGL ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  # facet_wrap(~id, nrow = length(unique(CAGL$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# CAOV ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
CAOV <- subset(all_cores2, species == "CAOV")
ggplot(CAOV, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "CAOV ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  # facet_wrap(~id, nrow = length(unique(CAOV$id)), ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# PODE ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
PODE <- subset(all_cores2, species == "PODE")
ggplot(PODE, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = markers, linetype = "dashed") +
  labs(title = "PODE ring width series",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  # facet_wrap(~id, nrow = 5, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(PODE$yearCor), max(PODE$yearCor), by = 5)) +
  # scale_y_continuous(breaks = seq(min(PODE$lengthCM), max(PODE$lengthCM), by = )) +
    theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  # ylim(0, 0.3) +
  xlim(2000, 2024)
  scale_color_manual(values = wes_palette("FantasticFox1")) 
ggsave("figures/podespaghetti_plot.jpeg", width = 10, height = 6, units = "in", dpi = 300)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# QUAL ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
QUAL <- subset(all_cores2, species == "QUAL")

QUAL <- subset(QUAL, id != "QUAL_358-82_A")



ggplot(QUAL, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = markers, linetype = "dashed") +
  labs(title = "QUAL ring width series",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  # facet_wrap(~id, nrow = 5, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(min(ACSA$yearCor), max(ACSA$yearCor), by = 5)) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
  # scale_color_manual(values = wes_palette("FantasticFox1")) 
ggsave("figures/qualspaghetti_plot.jpeg", width = 10, height = 6, units = "in", dpi = 300)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# QURU ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
QURU <- subset(all_cores2, species == "QURU")
ggplot(QURU, aes(x = yearCor, y = lengthCM, color = id, group = idrep)) +
  geom_line(linewidth = 0.6) +
  labs(title = "QURU ring width series",
       x = "yearCor",
       y = "Ring width (Length)",
       color = "Core ID") +
  # facet_wrap(~id, nrow = length(unique(QURU$id)), ncol = 1, scales = "free_y") +
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
qu <- subset(all_cores2, species %in% c("QUAL", "QURU"))
nodup <- all_cores2[!duplicated(all_cores2$idrep),]
table(nodup$species)

qu_df <- qu%>%
  select(idrep, yearCor, lengthCM) %>%
  pivot_wider(names_from = idrep, values_from = lengthCM) %>%
  arrange(yearCor) %>%
  column_to_rownames("yearCor") %>%
  as.data.frame()            # keep as data.frame
qu_df

all_df <- all_cores2%>%
  select(idrep, yearCor, lengthCM) %>%
  pivot_wider(names_from = idrep, values_from = lengthCM) %>%
  arrange(yearCor) %>%
  column_to_rownames("yearCor") %>%
  as.data.frame()            # keep as data.frame
all_df

# assign rwl class necessary for downstream analyses
class(qu_df) <- c("rwl", "data.frame")
class(all_df) <- c("rwl", "data.frame")

# plot ring width series
# open jpeg device from the ggsave settings above
jpeg(filename = "figures/fullspag_plot.jpeg",
     width = 16,       
     height = 8,      
     units = "in",    
     res = 300)    
plot.rwl(qu_df, plot.type = "spag")  # spaghetti plot
dev.off()

jpeg(filename = "figures/fullspag_plot.jpeg",
     width = 20,       
     height = 8,      
     units = "in",    
     res = 300)    
plot.rwl(all_df, plot.type = "spag")  # spaghetti plot
dev.off()

plot.rwl(qu_df, plot.type = "seg", nyrs = 30)  # running segment means

# build stanadardized chronology
rw_chron <- chron(qu_df, prewhiten = TRUE) 

# Plot chronology
plot(rw_chron, type = "l", lwd = 2,
     main = "Standard Chronology",
     xlab = "Year", ylab = "Ring Width Index")

# segment-wise crossdating
n_years <- apply(qu_df, 2, function(x) sum(!is.na(x)))
min_years <- min(n_years, na.rm = TRUE)

# Set segment length ≤ 1/2 shortest series
seg_len <- floor(min_years / 2)
seg_len <- max(seg_len, 3)  # at least 3 years

bin_floor <- max(floor(seg_len / 3), 2)  # rule-of-thumb

# Now run segment correlations safely
jpeg(filename = "figures/segmentCorrelations.jpeg",
     width = 8,       
     height = 8,      
     units = "in",    
     res = 300) 
res <- corr.rwl.seg(qu_df, seg.length = seg_len, bin.floor = bin_floor, make.plot = TRUE)
dev.off()
summary(res)

# report
class(qu_df)
rwl.report(qu_df)
qu_df.stats <- summary(qu_df) 

boxplot(qu_df.stats$ar1,ylab=expression(phi[1]),col = "lightblue")
stripchart(qu_df.stats$ar1, vertical = TRUE,  
           method = "jitter", jitter = 0.02,add = TRUE, pch = 20, col = 'darkblue',cex=1.25)
ar1Quant <- quantile(qu_df.stats$ar1,probs = c(0.25,0.5,0.75))
abline(h=ar1Quant,lty="dashed",col="grey")
mtext(text = names(ar1Quant),side = 4,at = ar1Quant,las=2)

ca533.ids <- read.ids(qu_df, stc = c(3, 2, 1))
ca533.rwi <- detrend(rwl = qu_df, method = "AgeDepSpline")
rwi.stats(ca533.rwi, ca533.ids, prewhiten=TRUE)

ca533.rho <- interseries.cor(qu_df, prewhiten=TRUE,
                             method="spearman")
ca533.rho[1:5, ]

ca533.crn <- chron(ca533.rwi)
plot(ca533.crn, add.spline=TRUE, nyrs=20)


# CROSS-DATING ####
# FROM: https://rpubs.com/andybunn/xdate
dat.sum <- summary(qu_df)
dat.sum
qu_df_long2 <- qu_df[, dat.sum$year >= 40]
rwl.60 <- corr.rwl.seg(qu_df_long2, seg.length =1, pcrit=0.01)

# === === === === === === === === === === === === === === === === === === === ===
# === === === === === === === === === === === === === === === === === === === ===
# === === === === === === === === === === === === === === === === === === === ===
# === === === === === === === === === === === === === === === === === === === ===


# EXPORT TO RWL ####
# shorten the names
all_cores2$sppshort <- paste0(substr(all_cores2$species, 1, 1),
                             substr(all_cores2$species, 3, 3))

all_cores2$Code2 <- gsub("-", "", all_cores2$Code)
all_cores2$codeshort <- substr(all_cores2$Code2, 1, 4)

all_cores2$repnum <- ifelse(all_cores2$Rep == "I", 1, 2)


# add length in mm
all_cores2$lengthMM <- all_cores2$lengthCM*10


# create separate chronologies per genera
all_cores2$Genus <- NA
all_cores2$Genus[grepl("AC", all_cores2$species)] <- "Acer"
all_cores2$Genus[grepl("BE", all_cores2$species)] <- "Betula"
all_cores2$Genus[grepl("CA", all_cores2$species)] <- "Carya"
all_cores2$Genus[grepl("QU", all_cores2$species)] <- "Quercus"

acer <- subset(all_cores2, Genus == "Acer")

acer$idshort <- paste0(acer$codeshort,
                             acer$Letter,
                             acer$repnum)

betula <- subset(all_cores2, Genus == "Betula")
carya <- subset(all_cores2, Genus == "Carya")
quercus <- subset(all_cores2, Genus == "Quercus")

acer_df <- acer %>%
  select(idshort, yearCor, lengthMM) %>%
  pivot_wider(names_from = idshort, values_from = lengthMM) %>%
  arrange(yearCor) %>%
  column_to_rownames("yearCor") %>%
  as.data.frame()            
acer_df

betula_df <- betula %>%
  select(idshort, yearCor, lengthMM) %>%
  pivot_wider(names_from = idshort, values_from = lengthMM) %>%
  arrange(yearCor) %>%
  column_to_rownames("yearCor") %>%
  as.data.frame()            
betula_df

carya_df <- carya %>%
  select(idshort, yearCor, lengthMM) %>%
  pivot_wider(names_from = idshort, values_from = lengthMM) %>%
  arrange(yearCor) %>%
  column_to_rownames("yearCor") %>%
  as.data.frame()            
carya_df

quercus_df <- quercus %>%
  select(idshort, yearCor, lengthMM) %>%
  pivot_wider(names_from = idshort, values_from = lengthMM) %>%
  arrange(yearCor) %>%
  column_to_rownames("yearCor") %>%
  as.data.frame()            
quercus_df


acer_data <- as.rwl(acer_df)
rownames(acer_data) <- as.numeric(rownames(acer_data))
betula_data <- as.rwl(betula_df)
carya_data <- as.rwl(carya_df)
quercus_data <- as.rwl(quercus_df)
head(acer_data)
summary(acer_data)

write.rwl(acer_data, fname = "cofecha/chronologyAcer.rwl")
}