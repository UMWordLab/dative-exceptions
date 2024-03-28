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

exp_data_mod <- experiment_data |>
  left_join(select(scores, DO_score, PD_score, diff, sent_pair_id), join_by(sent_pair_id))
exp_data_mod <- exp_data_mod |>
  mutate(rel_score=case_when(construct=="PDsentence"~PD_score, construct=="DOsentence"~DO_score))
exp_data_mod <- exp_data_mod |>
  select(-c(DO_score, PD_score))

data_sum <- exp_data_mod |>
  group_by(sent_pair_id, construct, classification) |>
  summarize(mean_z=mean(zscores))
data_sum <- data_sum |>
  pivot_wider(names_from=construct, values_from=mean_z)
data_sum <- data_sum |>
  mutate(human_diff=DOsentence - PDsentence)
data_sum <- data_sum |>
  left_join(select(scores, diff, sent_pair_id), join_by(sent_pair_id))
ggplot(data_sum, aes(x=human_diff, y=diff, color=classification)) + 
  geom_point() +
  geom_smooth(method=lm)



# Process average model output (refresh dataframes because I closed the project, w)
# Reenter experiment data and stimuli
experiment_data <- read.csv("analysis/cleaned_experiment_data.csv")
# Label sentence pair IDs
stimuli <- read.csv("stimuli/wider_pairs.csv") |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id)
# Add sentence pair IDs to experiment data
temp <- stimuli |>
  pivot_longer(cols=c(DOsentence, PDsentence), names_to='sent_const', values_to='sentence')
experiment_data <- experiment_data |> 
  merge(temp[, c("sentence", "sent_pair_id")], by="sentence") |>
  relocate(sent_pair_id)

# Read in model scores
scores1 <- read.csv ("data_raw/wider_pairs_scored_100M_1.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score1=DO_score, PD_score1=PD_score, diff1=diff)
scores2 <- read.csv ("data_raw/wider_pairs_scored_100M_2.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score2=DO_score, PD_score2=PD_score, diff2=diff)
scores3 <- read.csv ("data_raw/wider_pairs_scored_100M_3.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score3=DO_score, PD_score3=PD_score, diff3=diff)
scores4 <- read.csv ("data_raw/wider_pairs_scored_100M_4.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score4=DO_score, PD_score4=PD_score, diff4=diff)
rename(DO_score1=DO_score, PD_score1=PD_score, diff1=diff)
scores5 <- read.csv ("data_raw/wider_pairs_scored_100M_5.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score5=DO_score, PD_score5=PD_score, diff5=diff)

scores1 |> 
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff1, na.rm=TRUE), 
    std_diff=sd(diff1, na.rm=TRUE), 
    mean_DO=mean(DO_score1, na.rm=TRUE), 
    mean_PD=mean(PD_score1, na.rm=TRUE)
  )
scores2 |> 
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff2, na.rm=TRUE), 
    std_diff=sd(diff2, na.rm=TRUE), 
    mean_DO=mean(DO_score2, na.rm=TRUE), 
    mean_PD=mean(PD_score2, na.rm=TRUE)
  )
scores3 |> 
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff3, na.rm=TRUE), 
    std_diff=sd(diff3, na.rm=TRUE), 
    mean_DO=mean(DO_score3, na.rm=TRUE), 
    mean_PD=mean(PD_score3, na.rm=TRUE)
  )
scores4 |> 
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff4, na.rm=TRUE), 
    std_diff=sd(diff4, na.rm=TRUE), 
    mean_DO=mean(DO_score4, na.rm=TRUE), 
    mean_PD=mean(PD_score4, na.rm=TRUE)
  )
scores5 |> 
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff5, na.rm=TRUE), 
    std_diff=sd(diff5, na.rm=TRUE), 
    mean_DO=mean(DO_score5, na.rm=TRUE), 
    mean_PD=mean(PD_score5, na.rm=TRUE)
  )

# Aggregate model data and average
model_scores <- scores1 |>
  merge(scores2[, c("DO_score2", "PD_score2", "diff2", "sent_pair_id")], by="sent_pair_id") |>
  merge(scores3[, c("DO_score3", "PD_score3", "diff3", "sent_pair_id")], by="sent_pair_id") |>
  merge(scores4[, c("DO_score4", "PD_score4", "diff4", "sent_pair_id")], by="sent_pair_id") |>
  merge(scores5[, c("DO_score5", "PD_score5", "diff5", "sent_pair_id")], by="sent_pair_id")
model_scores <- model_scores |>
  rowwise() |>
  mutate(
    DO_avg=mean(c_across(c(DO_score1, DO_score2, DO_score3, DO_score4, DO_score5)), na.rm=TRUE),
    PD_avg=mean(c_across(c(PD_score1, PD_score2, PD_score3, PD_score4, PD_score5)), na.rm=TRUE),
    diff_avg=mean(c_across(c(diff1, diff2, diff3, diff4, diff5)), na.rm=TRUE)
  )
model_scores |>
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff_avg, na.rm=TRUE),
    mean_DO=mean(DO_avg, na.rm=TRUE),
    mean_PD=mean(PD_avg, na.rm=TRUE)
  )
write.csv(model_scores, "analysis/cleaned_model_data.csv")

# Add model data to experiment data
exp_mod_data <- experiment_data |>
  left_join(select(model_scores, DO_avg, PD_avg, diff_avg, sent_pair_id), join_by(sent_pair_id)) |>
  mutate(rel_score=case_when(construct=="PDsentence"~PD_avg, construct=="DOsentence"~DO_avg)) |>
  select(-c(DO_avg, PD_avg)) |> 
  left_join(select(stimuli, frequency_rank, sent_pair_id), join_by(sent_pair_id)) |>
  relocate(frequency_rank, .after=IO)

part_mean_z <- exp_mod_data |>
  group_by(sent_pair_id, construct, classification, IO, frequency_rank) |>
  summarize(mean_z=mean(zscores)) |>
  pivot_wider(names_from=construct, values_from=mean_z) |>
  mutate(human_diff=DOsentence - PDsentence) |>
  left_join(select(model_scores, diff_avg, sent_pair_id), join_by(sent_pair_id))
write.csv(part_mean_z, "analysis/diff_means_data.csv")


ggplot(part_mean_z, aes(x=human_diff, y=diff_avg, color=classification)) +
  geom_point() +
  geom_smooth(method=lm)
