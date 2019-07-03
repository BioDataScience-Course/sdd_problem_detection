# data_preparation

SciViews::R

sdd_dt <- read("data/sdd_comp.rds", type = "rds")

# wrangling
learnrs <- tribble(
  ~module, ~tutorial,      ~name,
  2,       "sdd1.02a", "02a_base",
  2,       "sdd1.02b", "02b_decouverte",
  2,       "sdd1.02c", "02c_nuage_de_points",
  2,       "sdd1.02d", "02d_np_challenge",
  3,       "sdd1.03a", "03a_test",
  4,       "sdd1.04a", "04a_test",
  5,       "sdd1.05a", "05a_test",
  6,       "sdd1.06a", "06a_test",
  7,       "sdd1.07a", "07a_proba",
  7,       "sdd1.07b", "07b_distri",
  8,       "sdd1.08a", "08a_chi2",
  8,       "sdd1.08b", "08b_chi2",
  9,       "sdd1.09a", "09a_ttest",
  9,       "sdd1.09b", "09b_ttest_wmw",
  10,       "sdd1.10a", "10a_anova",
  10,       "sdd1.10b", "10b_anova_kruskal",
  11,       "sdd1.11a", "11a_anova2",
  11,       "sdd1.11b", "11b_syntaxr",
  12,       "sdd1.12a", "12a_correlation",
  13,       "sdd1.13a", "13a_examen",
  13,       "sdd1.13b", "13b_examen"
)

sdd_dt <- left_join(sdd_dt, learnrs, by = "tutorial")


sdd_dt %>.%
  filter(., !event %in% c("exercise_hint", "section_skipped")) %>.%
  mutate(., date = parse_datetime(date),
         tutorial = factor(tutorial),
         version = factor(version, levels = c("1.0.0", "1.1.0", "1.2.0"),
                          ordered = TRUE),
         user_name = factor(user_name),
         event = factor(event),
         tuto_label = factor(tuto_label),
         row = 1:length(date)) -> sdd_dt

sdd_dt$event <- as.character(sdd_dt$event)
sdd_dt$event[sdd_dt$label == "comm"] <- "comm"
sdd_dt$event <- factor(sdd_dt$event,
                       levels = c("exercise_submission", "comm",
                                  "video_progress","question_submission"),
                       labels = c("questions ouvertes", "commentaires",
                                  "vidéo", "quiz"))

write(sdd_dt, file = "data/sdd_wrangling.rds", type = "rds", compress = "xz")
