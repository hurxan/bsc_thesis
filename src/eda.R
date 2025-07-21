#----LIBRARIES----
library(tidyverse)
library(data.table)

#----MERGE FILES INTO A SINGLE DATASET----
file_list <- list.files(path = "./MachineLearningCVE/", pattern = "*.csv", full.names = TRUE)
dataset <- rbindlist(lapply(file_list, fread), fill = TRUE)

#----EXPLORATORY ANALYSIS---

cat("#Rows:", nrow(dataset), "\n")
cat("#Columns:", ncol(dataset), "\n")

# Class distribution
table(dataset$Label)

# Remove duplicated columns
dataset <- dataset[, !duplicated(colnames(dataset)), with = FALSE]

# Remove unreadable characters
dataset <- dataset %>% mutate(Label = gsub(" � ", ": ", Label))

data_by_freq <- dataset %>%
  mutate(Label = fct_rev(fct_infreq(Label)))

ggplot(data_by_freq, aes(x = Label)) +
  geom_bar(fill = 'steelblue') +
  coord_flip() +
  theme_minimal(base_size = 20) +
  labs(
    x = "Class",
    y = "Frequency"
  )

ggplot(data_by_freq %>% filter(Label != "BENIGN"), aes(x = Label)) +
  geom_bar(fill = 'tomato') +
  coord_flip() +
  theme_minimal(base_size = 20) +
  labs(
    x = "Class",
    y = "Frequency"
  )

#----DATASET CLEANING----
#Remove NA
sum(is.na(dataset))
colSums(is.na(dataset))
dataset <- na.omit(dataset)

# Remove infinity values
sum(sapply(dataset, function(x) sum(is.infinite(x))))
sapply(dataset, function(x) sum(is.infinite(x)))
dataset <- dataset[, lapply(.SD, function(x) ifelse(is.infinite(x), NA, x))]
dataset <- na.omit(dataset)

# Remove features with variance 0
zero_variance <- sapply(dataset, function(x) length(unique(x)) == 1)
zero_variance <- names(zero_variance[zero_variance == TRUE])
dataset <- dataset %>% select(-all_of(zero_variance))

# Remove collinear features
collinear_features <- cor(dataset[, -c("Label")])
collinear_features <- as.data.frame(as.table(collinear_features))
colnames(collinear_features) <- c("Feature1", "Feature2", "Freq")
collinear_features <- collinear_features %>% filter(Feature1 != Feature2)
collinear_features <- collinear_features %>% filter(Freq == 1)
removable_features <- c(
  "Subflow Fwd Packets",
  "Subflow Bwd Packets",
  "Avg Fwd Segment Size",
  "Fwd PSH Flags",
  "RST Flag Count"
)
dataset <- dataset %>% select(-all_of(removable_features))

dataset$Label <- as.factor(dataset$Label)
dataset <- dataset %>%
  mutate(across(where(is.character), ~ as.numeric(.)))
names(dataset) <- make.names(names(dataset))

#----CORRELATION MATRIX----
df_numeric <- dataset %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(.x, na.rm = TRUE) > 0))

cor_matrix <- cor(df_numeric, use = "complete.obs")
cor_melt <- reshape2::melt(cor_matrix)

cor_melt$Var1 <- factor(cor_melt$Var1, levels = rev(colnames(cor_matrix)))
cor_melt$Var2 <- factor(cor_melt$Var2, levels = colnames(cor_matrix))

ggplot(cor_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = NA) +
  scale_fill_viridis_c(
    option = "D",
    limits = c(-1, 1),
    direction = -1,
    name = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_blank(),
  ) +
  labs(x = NULL, y = NULL)

#----REMOVE OBS. BELONGING TO UNDER-REPRESENTED CLASSES----
original_classes = levels(dataset$Label)

dataset <- dataset %>%
  group_by(Label) %>%
  filter(n() >= 50) %>%
  ungroup()
dataset$Label <- droplevels(dataset$Label)

cleaned_classes = levels(dataset$Label)
paste("Removed classes:", paste(setdiff(original_classes, cleaned_classes), collapse = ", "))

dataset$Label <- gsub("Web Attack: Brute Force|Web Attack: XSS", "Web Attack", dataset$Label)
dataset$Label <- as.factor(dataset$Label)

#----EXPORT DATASET----
fwrite(dataset, file = "./CICIDS2017_clean.csv")
saveRDS(dataset, file = "./CICIDS2017_clean.rds")