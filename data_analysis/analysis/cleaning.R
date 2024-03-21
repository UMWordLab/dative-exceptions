library("tidyverse")

# Clean experiment data
data <- read.csv("data_raw/data.csv")
fillers <- read.csv("data_raw/fillers_grade.csv")

# Prep experiment data:
# Don't need quotes on column names
data <- select(data, "Label", "Field name", "Field value", "PennElementType", "PennElementName", "Parameter", "Value", "partid", "sentence", "construct", "classification", "IO", "counter", "group", "item")

# Filter data to only include experiment data (trials + fillers)
data2 <- data |>
  filter(PennElementName == "test" & Parameter == "Choice")

# Calculate z-scores for each participant
data2 <- data2 |> 
  mutate(Value=as.numeric(Value), partid=as.factor(partid), item=as.factor(item)) |>
  group_by(partid) |>
  mutate(zscores=scale(Value))

# Summary of means of z-scores and ratings by sentence construction (DO/PO) and classification (alternating/ non-alternating)
data2 |>
  filter(Label=="experimental_trial") |>
  group_by(construct, classification) |> 
  summarize(mean_z=mean(zscores, na.rm = TRUE), mean_rating=mean(Value, na.rm = TRUE))
  
# Process experiment data with filler criteria:  
fillers <- fillers |>
  mutate(item=as.factor(item))
# Data for specified fillers
filler_thres_dat <- data2 |>
  semi_join(fillers, join_by(item))

# group_by(partid, classification)
# count: for high: sum(Value > 0) for low: sum(Value < 0)
# mutate new column for correctness
filler_thres_dat <- filler_thres_dat |> 
  group_by(partid, classification) |>
  mutate(correct=ifelse(classification=="low", ifelse(zscores < 0, 1, 0), ifelse(zscores > 0, 1, 0)))
# Create list of bad partids
temp <- filler_thres_dat |>
  group_by(partid, classification) |>
  filter((sum(correct, na.rm = TRUE) < 5 & classification == "high") & sum(correct, na.rm = TRUE) < 7 & classification == "low")

# use anti_join to throw out partids that match bad ones
# Unnecessary: none met the criteria for discarding

experiment_data <- data2 |>
  filter(Label == "experimental_trial") |>
  select(partid, item, sentence, construct, classification, IO, counter, group, Value, zscores)

write.csv(experiment_data, "analysis/cleaned_experiment_data.csv", row.names=FALSE)


# Prep model input
stimuli <- read.csv('stimuli/cleaned_200_pairs.csv')
stimuli <- stimuli |>
  select(-c(group)) |>
  pivot_wider(names_from=sent_construct, values_from=sentence)
write.csv(stimuli, "stimuli/wider_pairs.csv")

# Process model output
scores <- read.csv('data_raw/wider_pairs_scores.csv')
scores <- scores |>
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff)

scores |> 
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff, na.rm=TRUE), 
    std_diff=sd(diff, na.rm=TRUE), 
    mean_DO=mean(DO_score, na.rm=TRUE), 
    mean_PD=mean(PD_score, na.rm=TRUE)
    )

# Add sentence pair IDs?
stimuli <- stimuli |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id)
scores <- scores |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id)

temp <- stimuli |>
  pivot_longer(cols=c(DOsentence, PDsentence), names_to='sent_const', values_to='sentence')
experiment_data <- experiment_data |> 
  merge(temp[, c("sentence", "sent_pair_id")], by="sentence") |>
  relocate(sent_pair_id)

# How to summarize experimental data?
experiment_data |>
  group_by(sent_pair_id, )