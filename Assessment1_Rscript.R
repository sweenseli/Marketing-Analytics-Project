# ===============================================================
# SMARTWATCH CUSTOMER SEGMENTATION - FULL ANALYSIS
# ===============================================================
# Pipeline:
#   1. Setup & data load
#   2. Data exploration
#   3. Determine optimal number of segments
#   4. Hierarchical clustering (Ward's method)
#   5. K-means on PCA-reduced data (final solution)
#   6. Segment profiling
#   7. Visualisations (cluster plot, radar chart)
#   8. Statistical testing (ANOVA + assumption checks + effect sizes)
#   9. Pairwise comparisons (Tukey HSD)
#  10. Demographic association tests (Chi-square)
#  11. Two-way ANOVA (interaction with gender)
#  12. Export results
# ===============================================================


# ---------------------------------------------------------------
# 1. SETUP
# ---------------------------------------------------------------

# Install once, then comment out:
# install.packages(c("readxl","factoextra","cluster","dplyr","ggplot2",
#                    "NbClust","fmsb","writexl","broom","car","openxlsx"))

library(readxl)       # Read Excel files
library(factoextra)   # Cluster visualisation + nbclust
library(cluster)      # Clustering algorithms
library(dplyr)        # Data manipulation
library(ggplot2)      # Plotting
library(NbClust)      # Optimal cluster count
library(fmsb)         # Radar chart
library(writexl)      # Write Excel
library(broom)        # Tidy model output
library(car)          # Levene's test
library(openxlsx)     # Multi-sheet Excel export

options(scipen = 999)


# ---------------------------------------------------------------
# 2. LOAD & EXPLORE DATA
# ---------------------------------------------------------------

data <- read_excel(file.choose())
data <- as.data.frame(data)
View(data)

# Quick sanity checks
str(data)
summary(data)
dim(data)             # expect 1000 x 12
sum(is.na(data))      # expect 0


# ---------------------------------------------------------------
# 3. PREPARE SEGMENTATION VARIABLES
# ---------------------------------------------------------------

seg_vars_names <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt",
                    "Wellness", "Athlete", "Style")

seg_vars   <- data[, seg_vars_names]
seg_scaled <- scale(seg_vars)

round(colMeans(seg_scaled), 2)   # should all be ~0


# ---------------------------------------------------------------
# 4. DETERMINE OPTIMAL NUMBER OF SEGMENTS (ELBOW METHOD)
# ---------------------------------------------------------------

set.seed(123)

fviz_nbclust(seg_scaled, kmeans, method = "wss", k.max = 8) +
  labs(title    = "Elbow Method: Optimal Number of Segments",
       subtitle = "Look for the 'elbow' where the line bends")


# ---------------------------------------------------------------
# 5. HIERARCHICAL CLUSTERING (WARD'S METHOD)
#    - Used to confirm/validate the choice of k = 4
# ---------------------------------------------------------------

distance <- dist(seg_scaled, method = "euclidean")
hc.w     <- hclust(distance, method = "ward.D2")

# Dendrogram
plot(hc.w,
     main   = "Cluster Dendrogram (Ward's Method)",
     xlab   = "Respondents",
     ylab   = "Height",
     labels = FALSE)

# Elbow plot using merge heights
x <- 1:10
y <- sort(hc.w$height, decreasing = TRUE)[1:10]
plot(x, y,
     type = "b",
     main = "Elbow Plot: Choosing Number of Segments",
     xlab = "Number of Clusters",
     ylab = "Height",
     col  = "blue",
     pch  = 19)

# Dendrogram with 4 highlighted clusters
plot(hc.w,
     main   = "Cluster Dendrogram with 4 Segments",
     xlab   = "Observations",
     ylab   = "Height",
     labels = FALSE)
rect.hclust(hc.w, k = 4, border = 2:5)


# ---------------------------------------------------------------
# 6. K-MEANS ON PCA-REDUCED DATA (FINAL SOLUTION)
# ---------------------------------------------------------------

pca      <- prcomp(seg_scaled)
summary(pca)
pca_data <- pca$x[, 1:2]

fviz_nbclust(pca_data, kmeans, method = "wss")

set.seed(123)
km4_pca <- kmeans(pca_data, centers = 4, nstart = 25)

# Attach cluster to the main data
data$cluster <- as.factor(km4_pca$cluster)

# Segment sizes
percentages <- round(prop.table(table(data$cluster)) * 100, 1)
print(percentages)


# ---------------------------------------------------------------
# 7. SEGMENT PROFILING (MEAN VALUES)
# ---------------------------------------------------------------

segments <- data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), ~round(mean(.x), 2)))

segments$Size_n   <- as.numeric(table(data$cluster))
segments$Size_pct <- as.numeric(percentages)
print(segments)


# ---------------------------------------------------------------
# 8. VISUALISATIONS
# ---------------------------------------------------------------

# 8a. Cluster plot in PCA space
fviz_cluster(km4_pca,
             data         = pca_data,
             geom         = "point",
             ellipse.type = "convex",
             palette      = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
             ggtheme      = theme_minimal(),
             main         = "Smartwatch Customer Segments",
             xlab         = "Dimension 1",
             ylab         = "Dimension 2")

# 8b. Radar chart of segment profiles
radar_data <- segments %>%
  select(cluster, all_of(seg_vars_names)) %>%
  as.data.frame()
rownames(radar_data) <- paste("Segment", radar_data$cluster)
radar_data$cluster   <- NULL

# fmsb requires Max and Min rows at the top (1-7 Likert scale)
max_min <- data.frame(matrix(c(rep(7, 7), rep(1, 7)),
                             nrow = 2, byrow = TRUE,
                             dimnames = list(c("Max","Min"), seg_vars_names)))
radar_df <- rbind(max_min, radar_data)

colors_border <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
colors_fill   <- adjustcolor(colors_border, alpha.f = 0.3)

par(mar = c(1, 1, 2, 1))
radarchart(radar_df,
           axistype    = 1,
           pcol        = colors_border,
           pfcol       = colors_fill,
           plwd        = 2,
           plty        = 1,
           cglcol      = "grey",
           cglty       = 1,
           axislabcol  = "grey30",
           caxislabels = seq(1, 7, 1),
           cglwd       = 0.8,
           vlcex       = 1.1,
           title       = "Smartwatch Customer Segments Profile")
legend(x = 1.2, y = 1.2,
       legend  = rownames(radar_df[-c(1, 2), ]),
       bty     = "n", pch = 20,
       col     = colors_border,
       text.col = "black", cex = 1, pt.cex = 2)


# ---------------------------------------------------------------
# 9. ANOVA WITH ASSUMPTION CHECKS & EFFECT SIZES
#    - eta-squared shows HOW BIG the effect is, not just whether
#      it's significant (with n = 1000 almost everything will be)
#    - Levene's test checks the equal-variance assumption
#    - Bonferroni corrects for testing many variables at once
# ---------------------------------------------------------------

# Variable groupings
continuous_demo <- c("Age", "Income")
binary_demo     <- c("Female", "Degree", "AmznP")

# H0: All segment means are equal.
# H1: At least one segment mean differs.

anova_continuous <- function(v, df) {
  fit  <- aov(as.formula(paste(v, "~ cluster")), data = df)
  s    <- summary(fit)[[1]]
  ss_b <- s$`Sum Sq`[1]
  ss_w <- s$`Sum Sq`[2]
  eta2 <- ss_b / (ss_b + ss_w)
  lev  <- leveneTest(as.formula(paste(v, "~ cluster")), data = df)
  
  data.frame(
    Variable     = v,
    F_value      = round(s$`F value`[1], 2),
    p_value      = signif(s$`Pr(>F)`[1], 4),
    eta_squared  = round(eta2, 3),
    Effect_size  = ifelse(eta2 >= .14, "Large",
                          ifelse(eta2 >= .06, "Medium",
                                 ifelse(eta2 >= .01, "Small", "Negligible"))),
    Levene_p     = signif(lev$`Pr(>F)`[1], 4),
    Levene_warn  = ifelse(lev$`Pr(>F)`[1] < 0.05,
                          "Unequal var - use Welch", "OK")
  )
}

anova_tbl <- do.call(rbind,
                     lapply(c(seg_vars_names, continuous_demo),
                            anova_continuous, df = data))

anova_tbl$p_bonferroni  <- signif(p.adjust(anova_tbl$p_value,
                                           method = "bonferroni"), 4)
anova_tbl$Sig_after_bonf <- ifelse(anova_tbl$p_bonferroni < .001, "***",
                                   ifelse(anova_tbl$p_bonferroni < .01,  "**",
                                          ifelse(anova_tbl$p_bonferroni < .05,  "*", "ns")))
anova_tbl <- anova_tbl[order(-anova_tbl$F_value), ]
print(anova_tbl, row.names = FALSE)


# ---------------------------------------------------------------
# 10. WELCH ANOVA (FALLBACK WHEN VARIANCES ARE UNEQUAL)
# ---------------------------------------------------------------

flagged   <- anova_tbl$Variable[anova_tbl$Levene_warn != "OK"]
welch_tbl <- data.frame()

for (v in flagged) {
  w <- oneway.test(as.formula(paste(v, "~ cluster")),
                   data = data, var.equal = FALSE)
  welch_tbl <- rbind(welch_tbl, data.frame(
    Variable = v,
    Welch_F  = round(w$statistic, 2),
    Welch_p  = signif(w$p.value, 4)
  ))
}
print(welch_tbl, row.names = FALSE)


# ---------------------------------------------------------------
# 11. TUKEY HSD - PAIRWISE COMPARISONS
#     Tells us WHICH segment pairs differ, not just that they do.
# ---------------------------------------------------------------

tukey_summary <- function(v, df) {
  fit <- aov(as.formula(paste(v, "~ cluster")), data = df)
  out <- as.data.frame(TukeyHSD(fit)$cluster)
  out$Pair     <- rownames(out)
  out$Variable <- v
  out$Sig <- ifelse(out$`p adj` < .001, "***",
                    ifelse(out$`p adj` < .01,  "**",
                           ifelse(out$`p adj` < .05,  "*", "ns")))
  out[, c("Variable", "Pair", "diff", "lwr", "upr", "p adj", "Sig")]
}

tukey_all <- do.call(rbind, lapply(seg_vars_names, tukey_summary, df = data))
tukey_all$diff <- round(tukey_all$diff, 2)
tukey_all$lwr  <- round(tukey_all$lwr, 2)
tukey_all$upr  <- round(tukey_all$upr, 2)
tukey_all$`p adj` <- signif(tukey_all$`p adj`, 4)
print(tukey_all, row.names = FALSE)

# ---------------------------------------------------------------
# 12. TWO-WAY ANOVA - DOES GENDER MODERATE CLUSTER DIFFERENCES?
#     If the interaction term is non-significant, segments behave
#     consistently across genders -> no need for separate personas.
# ---------------------------------------------------------------

data$Female_f <- as.factor(data$Female)
two_way <- aov(Wellness ~ cluster * Female_f, data = data)
summary(two_way)


# ---------------------------------------------------------------
# 14. EXPORT ALL RESULTS
# ---------------------------------------------------------------

# Segment profiles + cluster-tagged data
write_xlsx(segments, "Smartwatch_Segment_Profiles_pca_final.xlsx")
write_xlsx(data,     "Final_Smartwatch_Data_PCA_Clusters_pca_final.xlsx")

# Statistical test results - one workbook, four sheets
wb <- createWorkbook()
addWorksheet(wb, "ANOVA");     writeData(wb, "ANOVA",     anova_tbl)
addWorksheet(wb, "Welch");     writeData(wb, "Welch",     welch_tbl)
addWorksheet(wb, "Tukey_HSD"); writeData(wb, "Tukey_HSD", tukey_all)
saveWorkbook(wb, "Strengthened_Test_Results.xlsx", overwrite = TRUE)