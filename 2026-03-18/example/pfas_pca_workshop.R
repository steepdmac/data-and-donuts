
# PCA Workshop: PFAS Concentrations
# Requires packages: tidyverse, FactoMineR, factoextra (optional for nicer plots)

# install.packages(c('tidyverse','FactoMineR','factoextra'))

library(tidyverse)
library(FactoMineR)
library(factoextra)

path <- 'water_grab_samples_matt_dunn_cape_cod.csv'
df <- read.csv(path)

# Count '<MDL' strings per column
mdl_count_per_col <- colSums(df == "<MDL")

# Remove columns with more than 30% '<MDL' values
threshold = 0.3 * nrow(df)
df = df[, mdl_count_per_col <= threshold]

# Replace remaining '<MDL' with 0
df[df == "<MDL"] <- 0
cols_to_convert <- c("PFPrS..ng.L.", "FOSA..ng.L.", "PFECHS..ng.L.")
df[cols_to_convert] <- lapply(df[cols_to_convert], as.numeric)

# drop site and scale features
cols_to_scale <- setdiff(names(df), "Site")

# PCA
res.pca <- FactoMineR::PCA(df, quali.sup = which(names(df) == "Site"), graph = TRUE, ncp=2, scale=TRUE)

# Scores by water type if available
factoextra::fviz_pca_ind(res.pca, geom = 'point', habillage = 'Site')

print(res.pca$var$coord)
