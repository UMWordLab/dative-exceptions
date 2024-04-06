library("tidyverse")


# Experiment data preprocessing -------------------------------------------


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


# Initial model output ----------------------------------------------------

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



# 100M model data  --------------------------------------------------------

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
# Add verbs to experiment data?
verblist <- read.csv("data_raw/verblists.csv")
experiment_data <- experiment_data |>
  left_join(select(verblist, verb_id, verb), join_by(item == verb_id)) |>
  rename(verb_id=item) |>
  relocate(verb, .after=verb_id)


# Read in model scores
scores1 <- read.csv ("data_raw/wider_pairs_scored_100M_1_minicons.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score1=DO_score, PD_score1=PD_score, diff1=diff, verb_id=item)
scores2 <- read.csv ("data_raw/wider_pairs_scored_100M_2_minicons.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score2=DO_score, PD_score2=PD_score, diff2=diff, verb_id=item)
scores3 <- read.csv ("data_raw/wider_pairs_scored_100M_3_minicons.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score3=DO_score, PD_score3=PD_score, diff3=diff, verb_id=item)
scores4 <- read.csv ("data_raw/wider_pairs_scored_100M_4_minicons.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score4=DO_score, PD_score4=PD_score, diff4=diff, verb_id=item)
scores5 <- read.csv ("data_raw/wider_pairs_scored_100M_5_minicons.csv") |> 
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score5=DO_score, PD_score5=PD_score, diff5=diff, verb_id=item)

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
model_scores <- model_scores |>
  left_join(select(verblist, verb, verb_id), join_by(verb_id)) |>
  relocate(verb, .after=verb_id)

# Add pretrained GPT scores to model_scores
gpt2_scores <- read.csv("data_raw/wider_pairs_scored_gpt2_minicons.csv") |>
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score_gpt2=DO_score, PD_score_gpt2=PD_score, diff_gpt2=diff, verb_id=item)
gpt2_med_scores <- read.csv("data_raw/wider_pairs_scored_gpt2m_minicons.csv") |>
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score_gpt2_med=DO_score, PD_score_gpt2_med=PD_score, diff_gpt2_med=diff, verb_id=item)
gpt2_large_scores <- read.csv("data_raw/wider_pairs_scored_gpt2l_minicons.csv") |>
  select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
  mutate(sent_pair_id=row_number()) |>
  relocate(sent_pair_id) |>
  rename(DO_score_gpt2_large=DO_score, PD_score_gpt2_large=PD_score, diff_gpt2_large=diff, verb_id=item)
# gpt2_xl_scores <- read.csv("data_raw/wider_pairs_scored_gpt2xl_minicons.csv") |>
#   select(item, classification, frequency_rank, recipient_id, DOsentence, PDsentence, DO_score, PD_score, diff) |>
#   mutate(sent_pair_id=row_number()) |>
#   relocate(sent_pair_id) |>
#   rename(DO_score_gpt2_xl=DO_score, PD_score_gpt2_xl=PD_score, diff_gpt2_xl=diff, verb_id=item)

gpt2_scores |>
  group_by(classification) |>
  summarize(
    mean_diff=mean(diff_gpt2, na.rm=TRUE),
    mean_DO=mean(DO_score_gpt2, na.rm=TRUE),
    mean_PD=mean(PD_score_gpt2, na.rm=TRUE)
  )

model_scores <- model_scores |>
  left_join(
    select(gpt2_scores, DO_score_gpt2, PD_score_gpt2, diff_gpt2, sent_pair_id), 
    join_by(sent_pair_id)) |>
  left_join(
    select(gpt2_med_scores, DO_score_gpt2_med, PD_score_gpt2_med, diff_gpt2_med, sent_pair_id), 
    join_by(sent_pair_id)) |>
  left_join(
    select(gpt2_large_scores, DO_score_gpt2_large, PD_score_gpt2_large, diff_gpt2_large, sent_pair_id),
    join_by(sent_pair_id)) # |>
  # left_join(
  #   select(gpt2_xl_scores, DO_score_gpt2_xl, PD_score_gpt2_xl, diff_gpt2_xl, sent_pair_id),
  #   join_by(sent_pair_id)
  # )
  
write.csv(model_scores, "analysis/cleaned_model_data.csv")

# Add model data to experiment data
exp_mod_data <- experiment_data |>
  left_join(
    select(
      model_scores, 
      DO_avg, 
      PD_avg, 
      diff_avg, 
      DO_score_gpt2, 
      PD_score_gpt2, 
      diff_gpt2,
      DO_score_gpt2_med,
      PD_score_gpt2_med,
      diff_gpt2_med,
      DO_score_gpt2_large,
      PD_score_gpt2_large,
      diff_gpt2_large,
      DO_score_gpt2_xl,
      PD_score_gpt2_xl,
      diff_gpt2_xl,
      sent_pair_id), 
    join_by(sent_pair_id)) |>
  mutate(
    score_100M=case_when(construct=="PDsentence"~PD_avg, construct=="DOsentence"~DO_avg),
    score_gpt2=case_when(construct=="PDsentence"~PD_score_gpt2, construct=="DOsentence"~DO_score_gpt2),
    score_gpt2med=case_when(construct=="PDsentence"~PD_score_gpt2_med, construct=="DOsentence"~DO_score_gpt2_med),
    score_gpt2large=case_when(construct=="PDsentence"~PD_score_gpt2_large, construct=="DOsentence"~DO_score_gpt2_large),
    score_gpt2xl=case_when(construct=="PDsentence"~PD_score_gpt2_xl, construct=="DOsentence"~DO_score_gpt2_xl)) |>
  select(-c(DO_avg, PD_avg, DO_score_gpt2, PD_score_gpt2, DO_score_gpt2_med,
            PD_score_gpt2_med, DO_score_gpt2_large, PD_score_gpt2_large, DO_score_gpt2_xl, PD_score_gpt2_xl)) |>
  left_join(select(stimuli, frequency_rank, sent_pair_id), join_by(sent_pair_id)) |>
  relocate(frequency_rank, .after=IO) |>
  mutate(sent_pair_id=as_factor(sent_pair_id))
write.csv(exp_mod_data, "analysis/part_model_data.csv")

# Get means and diffs
part_mean_data <- exp_mod_data |>
  group_by(sent_pair_id, verb_id, verb, construct, classification, IO, 
           frequency_rank, diff_avg, diff_gpt2, diff_gpt2_med, diff_gpt2_large, diff_gpt2_xl) |>
  summarize(mean_z=mean(zscores), mean_scores=mean(Value)) |>
  pivot_wider(names_from=construct, values_from=c(mean_z, mean_scores)) |>
  mutate(
    human_zdiff=mean_z_PDsentence - mean_z_DOsentence, 
    human_sdiff=mean_scores_PDsentence - mean_scores_DOsentence) # |>
write.csv(part_mean_data, "analysis/diff_means_data.csv")


ggplot(part_mean_data, aes(x=human_zdiff, y=diff_avg, color=classification)) +
  geom_point() +
  geom_smooth(method=lm)
ggplot(part_mean_data, aes(x=human_sdiff, y=diff_avg, color=classification)) +
  geom_point() +
  geom_smooth(method=lm)
ggplot(part_mean_data, aes(x=human_sdiff, y=diff_gpt2, color=classification)) +
  geom_point() + 
  geom_smooth(method=lm)
ggplot(part_mean_data, aes(x=human_sdiff, y=diff_gpt2_med, color=classification)) +
  geom_point() +
  geom_smooth(method=lm)
ggplot(part_mean_data, aes(x=human_sdiff, y=diff_gpt2_large, color=classification)) +
  geom_point() +
  geom_smooth(method=lm)

part_mean_data |>
  group_by(classification, IO) |>
  summarize(mean_DO=mean(mean_scores_DOsentence),
          mean_PD=mean(mean_scores_PDsentence),
          mean_diff=mean(human_sdiff),
          mean_DO_z=mean(mean_z_DOsentence),
          mean_PD_z=mean(mean_z_PDsentence),
          mean_diff_z=mean(human_zdiff))

ggplot(part_mean_data, aes(x=IO, y=human_zdiff, fill=classification)) + 
  geom_bar(position="dodge", stat="identity")
ggplot(part_mean_data, aes(x=human_zdiff, y=diff_avg, color=IO)) +
  geom_point() +
  geom_smooth(method=lm)

ggplot(part_mean_data, aes(x=frequency_rank, y=human_zdiff, color=classification)) +
  geom_point() + 
  geom_smooth(method=lm)
ggplot(part_mean_data, aes(x=frequency_rank, y=diff_avg, color=classification)) +
  geom_point() +
  geom_smooth(method=lm)


# Linear Modeling and Summary Graphs --------------------------------------


# group by verb 
# use facet_wrap(vars(variable-to-split-by))

library(lme4)
library(lmerTest)

exp_mod_facet <- part_mean_data |>
  select(sent_pair_id, verb_id, verb, classification, 
         IO, frequency_rank, diff_avg, diff_gpt2, diff_gpt2_med, 
         diff_gpt2_large, human_zdiff, human_sdiff) |>
  rename(GPT2_100M = diff_avg, GPT2_small = diff_gpt2, GPT2_med = diff_gpt2_med, GPT2_large = diff_gpt2_large) |>
  pivot_longer(cols=c(GPT2_100M, GPT2_small, GPT2_med, GPT2_large), names_to='model', values_to='diff')
  


# summary model vs. human diffs coded by classification
ggplot(transform(
  exp_mod_facet, 
  model=factor(model, levels=c("GPT2_100M","GPT2_small", "GPT2_med", "GPT2_large"))), 
  aes(x=human_zdiff, y=diff)) +
  geom_point(aes(color=classification)) +
  geom_smooth(method=lm) +
  labs(title = "Model vs. Human Judgments by Sentence Classification",
       y = "Model Diff", x = "Human Diff") +
  facet_wrap(~model)
# same coded by IO, unnecessary
ggplot(transform(
  exp_mod_facet, 
  model=factor(model, levels=c("GPT2_100M","GPT2_small", "GPT2_med", "GPT2_large"))), 
  aes(x=human_zdiff, y=diff)) +
  geom_point(aes(color=IO)) +
  geom_smooth(method=lm) +
  labs(title = "Human vs. Model Judgments by Sentence Classification",
       y = "Model Diff", x = "Human Diff") +
  facet_wrap(~model)

# ggplot(part_mean_data, aes(x=frequency_rank, y=human_zdiff, color=IO)) +
#   geom_point() +
#   geom_smooth(method=lm)


combined_facet <- part_mean_data |>
  select(sent_pair_id, verb_id, verb, classification, 
         IO, frequency_rank, diff_avg, diff_gpt2, diff_gpt2_med, 
         diff_gpt2_large, human_zdiff, human_sdiff) |>
  rename(
    Human = human_zdiff, 
    GPT2_100M = diff_avg, 
    GPT2_small = diff_gpt2, 
    GPT2_med = diff_gpt2_med, 
    GPT2_large = diff_gpt2_large) |>
  mutate(IO = recode(IO, longIndefinite = 'Long', shortIndefinite = 'Short')) |>
  pivot_longer(cols=c(Human, GPT2_100M, GPT2_small, GPT2_med, GPT2_large), names_to='source', values_to='diff')
ggplot(transform(
  combined_facet, 
  model=factor(source, levels=c("Human", "GPT2_100M","GPT2_small", "GPT2_med", "GPT2_large"))), 
  aes(x=-1 * frequency_rank, y=diff, color=classification)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(title = "Diffs vs. Verb Frequency by Sentence Classification",
       y = "Diff", x = "Frequency") +
  facet_wrap(~source, nrow=1, scales="free_y")

# Diffs ranges by IO
ggplot(combined_facet, aes(x=IO, y=diff, fill=classification)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~source, nrow=1)
# Boxplot ver.
ggplot(combined_facet, aes(x=IO, y=diff, fill=classification)) + 
  geom_boxplot() +
  facet_wrap(~source, nrow=1)


stats1 <- lm(Value~classification*construct, data=exp_mod_data)
summary(stats1)

stats2 <- lmer(Value~classification*construct + frequency_rank + (1+classification*construct|partid) + (1+construct|sent_pair_id), data=exp_mod_data)
summary(stats2)

# saving: ggsave
# assign plots to object, save
# can set dimensions, etc., in ggsave
