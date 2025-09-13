# 12 September 2025
# Cleaning and plotting core measurements 

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

all_cores$id <- paste(all_cores$Species, all_cores$Code, all_cores$Letter, all_cores$Rep, sep = "_")

# keep only the columns i want
all_cores2 <- all_cores[, c("id", "Species", "Code", "Letter", "Rep", "Year", "Length")]

# convert inches to cm
all_cores2$lengthCM <- all_cores2$Length*2.54

all_cores2$scaled_length <- scale(all_cores2$lengthCM)


ggplot(all_cores2, aes(x = Year, y = scaled_length, color = id, group = id)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Ring-width series per core",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  theme_minimal(base_size = 14)

# check some pairs along
acrutest1 <- subset(all_cores2, Code == "525-2009")

ggplot(acrutest1, aes(x = Year, y = scaled_length, color = id, group = id)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Ring-width series per core",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  theme_minimal(base_size = 14)

acsatest1 <- subset(all_cores2, Code == "689-2010")

ggplot(acsatest1, aes(x = Year, y = scaled_length, color = id, group = id)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Ring-width series per core",
       x = "Year",
       y = "Ring width (Length)",
       color = "Core ID") +
  theme_minimal(base_size = 14)


# lets try some shitty chat gpt analyses: 
library(dplR)
library(tidyverse)

# ---- Convert long-format df to RWL data.frame ----
rw_df <- all_cores2 %>%
  select(id, Year, Length) %>%
  pivot_wider(names_from = id, values_from = Length) %>%
  arrange(Year) %>%
  column_to_rownames("Year") %>%
  as.data.frame()            # keep as data.frame

# Assign class "rwl"
class(rw_df) <- c("rwl", "data.frame")

# ---- Plot raw ring-width series ----
plot.rwl(rw_df, plot.type = "spag")  # spaghetti plot
plot.rwl(rw_df, plot.type = "seg", nyrs = 30)  # running segment means

# ---- Build standardized chronology ----
rw_chron <- chron(rw_df, prewhiten = TRUE)

# Plot chronology
plot(rw_chron, type = "l", lwd = 2,
     main = "Standard Chronology",
     xlab = "Year", ylab = "Ring Width Index")

# ---- Segment-wise crossdating (COFECHA-style) ----
# seg.plot works now because rw_df is a data.frame
n_years <- apply(rw_df, 2, function(x) sum(!is.na(x)))
min_years <- min(n_years, na.rm = TRUE)

# Set segment length ≤ 1/2 shortest series
seg_len <- floor(min_years / 2)
seg_len <- max(seg_len, 3)  # at least 3 years

bin_floor <- max(floor(seg_len / 3), 2)  # rule-of-thumb

# Now run segment correlations safely
res <- corr.rwl.seg(rw_df, seg.length = seg_len, bin.floor = bin_floor, make.plot = TRUE)
summary(res)
