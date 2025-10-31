# -------------------------------
# Role: Consultant for female doctors(prof-gender, salary-gender)
# -------------------------------

#library
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(tidymodels)
library(yardstick)
library(patchwork)

# 1) Basic cleaning ----------------------------------------------
df <- read.csv("lawsuit.csv", stringsAsFactors = FALSE)

# Make factors where appropriate
df$Dept   <- factor(df$Dept)                 # 1–6 departments
df$Gender <- factor(df$Gender, levels = c(0,1), labels = c("Female","Male"))
df$Clin   <- factor(df$Clin, levels=c(0,1), labels=c("Research","Clinical"))
df$Cert   <- factor(df$Cert, levels=c(0,1), labels=c("NoCert","Cert"))

# Create binary target per slide’s “Y”: Full Professor vs not
df$Fullprof <- ifelse(df$Rank == 3, 1, 0)
df$Fullprof <- factor(df$Fullprof)
df$Rank <- factor(df$Rank)

# Create Salary difference column
df$Sal_diff <- df$Sal95 - df$Sal94

# Frequencies and cross-tab
table(df$Gender)           # counts by gender
table(df$Rank)             # rank distribution (may be character "Assistant/Associate/Full")
table(df$Rank, df$Gender)  # rank × gender
df$Lprate <- log(pmax(df$Prate,0) + 1)
df$Exper2 <- df$Exper^2


# 2) Proof of Gender Discrimination in Professor Titles -------------

t.test(Sal94 ~ Gender, data = df)  # if p<0.05: in 1994 females significantly lower
t.test(Sal95 ~ Gender, data = df)  # if p<0.05: in 1995 females significantly lower
chisq.test(table(df$Rank, df$Gender))

# 2.1 Full professor rate by gender --------------------------------
df <- df %>%
  mutate(Fullprof = as.numeric(as.character(Fullprof)))

prof_data_raw <- df %>%
  group_by(Gender) %>%
  summarise(FullProfRate = mean(Fullprof, na.rm = TRUE) * 100)

ggplot(prof_data_raw, aes(x = Gender, y = FullProfRate, fill = Gender)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(FullProfRate, 1), "%")),
            vjust = -0.5, size = 5) +
  labs(title = "Rate of Full Professorship",
       x = "Gender",
       y = "Percentage (%)",
       fill = "Gender") +
  ylim(0, max(prof_data_raw$FullProfRate) * 1.2) +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF"))
df$Fullprof <- factor(df$Fullprof)

ggsave("prof_rate.png", width = 8, height = 7, dpi = 300,bg="white")

# Female has a lower rate of professor ------------------------------------

# 2.2 Promotion difference before and after adjustment -------------------
# 2.2.1 logit regression before and after adjustment --------------------
logit_model1 <- glm(Fullprof ~ Gender, 
                    data = df, family = binomial)
summary(logit_model1)

logit_model2 <- glm(Fullprof ~ Gender + Dept + Clin + Cert + Lprate + Exper + Exper2, 
                    data = df, family = binomial)
summary(logit_model2)
exp(coef(logit_model2))         # Odds Ratio

# 2.2.2 Promotion difference rate before and after adjustment -------------
promotion_coef <- data.frame(
  Model = c("Without Controls", "With Controls"),
  PromotionDiff = c(1.5070, 0.9693)  # 从 logit_model1 和 logit_model2
)

ggplot(promotion_coef, aes(x = Model, y = PromotionDiff)) +
  geom_col(fill = "#1E90FF") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = paste0(round(PromotionDiff * 100, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(title = "Gender Impact on Full Professorship",
       subtitle = "Comparison before and after controlling for experience, department, etc.",
       x = "Analysis Condition",
       y = "Promotion Probability Difference (Male - Female)") +
  ylim(0, max(promotion_coef$PromotionDiff) * 1.2)

ggsave("promotion_effect_comparison.png", width = 10, height = 7, dpi = 300,bg="white")
# after adjustment, still has less chance than male -----------------------

# 2.2.3 Odds ratios visualization from logistic regression -----------------
odds_ratios <- data.frame(
  Variable = c("Gender (Male)", "Experience","Cert", "Department 2", "Department 3", 
               "Department 4", "Department 5", "Department 6","Clinical","Prate"),
  OR = c(2.40, 1.30, 1.48, 1.56, 0.95, 0.23, 0.36, 0.19, 0.74, 0.80)  # From exp(coef(logit_model2))
)
theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 12),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 10),
                  legend.position = "bottom",
                  legend.title = element_text(face = "bold")))
ggplot(odds_ratios, aes(x = reorder(Variable, OR), y = OR, fill = Variable == "Gender (Male)")) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(OR, 2)),
            vjust = ifelse(odds_ratios$OR > 1, -0.5, 1.5), size = 4) +
  labs(title = "Impact of Factors on Full Professorship",
       subtitle = "Odds Ratios (OR): >1 increases probability, <1 decreases probability",
       x = "Factor",
       y = "Odds Ratio (OR)") +
  scale_fill_manual(values = c("gray50", "#FF69B4"), guide = "none") +
  coord_flip()
ggsave("odds_ratios.png", width = 10, height = 7, dpi = 300,bg="white")
  
# 2.3 Cart Test in Promotion -----------------------------------------
cart_rank <- rpart(Fullprof ~ Gender + Exper + Dept + Prate + Clin + Cert, data = df, method="class")
png("cart_rank.png", width = 1000, height = 800, res = 300, bg="white")
rpart.plot(cart_rank)
dev.off()
rank_var_imp <- as.data.frame(cart_rank$variable.importance)

summary(cart_rank)
# the probability of gender = 0 (female) is less than gender = 1 (male) -----

# Split test set and training set
set.seed(567)
cart_split <- initial_split(df, 0.7)
df_test <- testing(cart_split)
df_train <- training(cart_split)

# Set 10 fold cross-validation
set.seed(567)
folds <- vfold_cv(df_train, v = 10)

# Specify model
cart_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Build base cart model workflow
base_wf <- workflow() %>%
  add_model(cart_spec) %>%
  add_formula(Fullprof ~ Exper + Dept + Prate)

base_accu <- collect_metrics(
  fit_resamples(
    base_wf,
    resamples = folds,
    metrics = metric_set(accuracy)
  )
)

# Base model adding Gender as new predictor
gender_wf <- workflow() %>%
  add_model(cart_spec) %>%
  add_formula(Fullprof ~ Exper + Dept + Prate + Gender)

gender_accu <- collect_metrics(
  fit_resamples(
    gender_wf,
    resamples = folds,
    metrics = metric_set(accuracy)
  )
)

# Base model adding Clinic as new predictor
clin_wf <- workflow() %>%
  add_model(cart_spec) %>%
  add_formula(Fullprof ~ Exper + Dept + Prate + Clin)

clin_accu <- collect_metrics(
  fit_resamples(
    clin_wf,
    resamples = folds,
    metrics = metric_set(accuracy)
  )
)

# Base model adding Certification as new predictor
cert_wf <- workflow() %>%
  add_model(cart_spec) %>%
  add_formula(Fullprof ~ Exper + Dept + Prate + Cert)

cert_accu <- collect_metrics(
  fit_resamples(
    cert_wf,
    resamples = folds,
    metrics = metric_set(accuracy)
  )
)

# Build comparison dataframe
train_results <- data.frame(
  Model = c(
    "base (Department + Publication rate + Experience)",
    "base with gender",
    "base with clinic",
    "base with certification"
  ),
  Cross_Validation_Accuracy = c(
    base_accu$mean,
    gender_accu$mean,
    clin_accu$mean,
    cert_accu$mean
  )
)

print(train_results)

# Apply the four models on the testing set to visualise accuracy
# Fit final models
base_fit   <- fit(base_wf, data = df_train)
gender_fit <- fit(gender_wf, data = df_train)
clin_fit   <- fit(clin_wf, data = df_train)
cert_fit   <- fit(cert_wf, data = df_train)

# Generate predictions
base_pred <- predict(base_fit, df_test) %>%
  bind_cols(df_test %>% select(Fullprof))

gender_pred <- predict(gender_fit, df_test) %>%
  bind_cols(df_test %>% select(Fullprof))

clin_pred <- predict(clin_fit, df_test) %>%
  bind_cols(df_test %>% select(Fullprof))

cert_pred <- predict(cert_fit, df_test) %>%
  bind_cols(df_test %>% select(Fullprof))

# Create confusion matrices for prediction results
base_cm   <- conf_mat(base_pred,   truth = Fullprof, estimate = .pred_class)
gender_cm <- conf_mat(gender_pred, truth = Fullprof, estimate = .pred_class)
clin_cm   <- conf_mat(clin_pred,   truth = Fullprof, estimate = .pred_class)
cert_cm   <- conf_mat(cert_pred,   truth = Fullprof, estimate = .pred_class)

# Put 4 confusion matrices together for comparison
p1 <- autoplot(base_cm,   type = "heatmap") + ggtitle("Base Model")
p2 <- autoplot(gender_cm, type = "heatmap") + ggtitle("Base + Gender Model")
p3 <- autoplot(clin_cm,   type = "heatmap") + ggtitle("Base + Clinic Model")
p4 <- autoplot(cert_cm,   type = "heatmap") + ggtitle("Base + Certification Model")

# Arrange in 2x2 layout
(p1 | p2) /
  (p3 | p4)

### Exploring the predicted gender difference within each department
# Build a dataframe containing the average experience and publication rate by department
all_depts <- unique(df$Dept) # extract the department names
df_ref <- data.frame()  # empty container

for (dname in all_depts) {
  dept_mean <- df %>%
    filter(Dept == dname) %>%
    summarise(
      Exper = mean(Exper, na.rm = TRUE),
      Prate = mean(Prate, na.rm = TRUE)
    )
  
  # Add two values of gender for each rows: Female (0) and Male (1)
  temp <- data.frame(
    Dept   = dname,
    Exper  = dept_mean$Exper,
    Prate  = dept_mean$Prate,
    Gender = c(0, 1)
  )
  
  df_ref <- bind_rows(df_ref, temp)
}

df_ref$Exper <- round(df_ref$Exper)

# Use the base+gender model to predict the full professorship for two genders with average values
ref_probs <- predict(gender_fit, df_ref, type = "prob") %>%
  as.data.frame()  %>%
  bind_cols(df_ref %>% select(Dept, Gender))

# Rename the department and gender names for visualization
ref_probs <- ref_probs %>%
  mutate(Dept = ifelse(Dept == 1, "Biochemistry/Molecular Biology", 
                       ifelse(Dept == 2, "Physiology",
                              ifelse(Dept == 3, "Genetics",
                                     ifelse(Dept == 4, "Pediatrics",
                                            ifelse(Dept == 5, "Medicine", "Surgery"
                                            )))))) %>%
  mutate(Gender = ifelse(Gender == 0, "Female", "Male"))

# Draw a dumbball plot to visualize the probabilities of two genders by department
ggplot(ref_probs, aes(y = Dept, x = .pred_1, color = Gender)) +
  geom_point(size = 3) +                       # points for each gender
  geom_line(aes(group = Dept), color = "black", linetype = "dashed") + # connect male/female
  scale_color_manual(values = c("Female" = "pink", "Male" = "blue")) +
  labs(
    x = "Predicted Probability of Full Professorship",
    y = "Department",
    title = "Predicted Probability by Gender with Average Experience
    \n and Publication Rate within Each Department ",
    color = "Gender"
  ) +
  theme_minimal()


# 3) Proof of Gender Discrimination in Salary -----------------------
# 3.1 Salary box plot by Gender ------------------------------------
# 1994 Salary by Gender
ggplot(df, aes(x = Gender, y = Sal94, fill = Gender)) +
  geom_boxplot() +
  labs(title = "1994 Salary by Gender", x = "Gender", y = "Salary 1994") +
  scale_fill_manual(values = c("Female"="lightblue","Male"="skyblue")) +
  theme_minimal()
ggsave("boxplot_sal94_gender.png", width=8, height=6, dpi=300,bg="white")

# 1995 Salary by Gender
ggplot(df, aes(x = Gender, y = Sal95, fill = Gender)) +
  geom_boxplot() +
  labs(title = "1995 Salary by Gender", x = "Gender", y = "Salary 1995") +
  scale_fill_manual(values = c("Female"="lightblue","Male"="skyblue")) +
  theme_minimal()
ggsave("boxplot_sal95_gender.png", width=8, height=6, dpi=300,bg="white")
# box plot shows female has average lower salary than male-----------------

# 3.2 Salary bar plot by gender -------------------------------------------
# Calculate 1994 salaries by gender
sal94_data <- df %>%
  group_by(Gender) %>%
  summarise(Salary = mean(Sal94, na.rm = TRUE),
            Year = "1994")

# Calculate 1995 salaries by gender
sal95_data <- df %>%
  group_by(Gender) %>%
  summarise(Salary = mean(Sal95, na.rm = TRUE),
            Year = "1995")

# Combine
salary_data <- rbind(sal94_data, sal95_data)

# Plot
ggplot(salary_data, aes(x = Year, y = Salary, fill = Gender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0("$", round(Salary/1000, 1), "k")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4) +
  labs(title = "Average Salary by Gender",
       x = "Year",
       y = "Average Salary",
       fill = "Gender") +
  scale_y_continuous(labels = scales::dollar) +
  ylim(0, max(salary_data$Salary) * 1.2) +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF"))

ggsave("salary_gender.png", width = 10, height = 7, dpi = 300,bg="white")
# female has lower salary -----------------------------------------------------


# ===== Packages =====
# install.packages(c("ggplot2","broom","scales"))
library(ggplot2); library(broom); library(scales)

# ===== Colors =====
BLUE <- "#4174ff"; PINK <- "#f34ca5"; GRAY <- "#6c6c6c"

# ===== Load & prep =====
PATH <- "/Users/huangyibo/Desktop/6003/Graded Team Assignment - Gender Discrimination Lawsuit/Lawsuit.csv"
df <- read.csv(PATH, stringsAsFactors = FALSE); names(df) <- tolower(names(df))
df$dept   <- factor(df$dept)
df$clin   <- factor(df$clin, levels=c(0,1), labels=c("Research","Clinical"))
df$cert   <- factor(df$cert, levels=c(0,1), labels=c("NoCert","Cert"))
df$gender <- factor(df$gender, levels=c(0,1), labels=c("Female","Male"))
df$fullprof <- factor(ifelse(df$rank==3,"Yes","No"))
df$lprate <- log(pmax(df$prate,0)+1); df$exper2 <- df$exper^2
dat <- na.omit(df[,c("fullprof","gender","dept","clin","cert","lprate","exper","exper2")])

# ===== Logit =====
fit <- glm(fullprof ~ gender + dept + clin + cert + lprate + exper + exper2,
           family=binomial, data=dat)

# ===== Forest plot (Male vs Female) =====
or_tbl <- tidy(fit, exponentiate=TRUE, conf.int=TRUE)
or_tbl <- or_tbl[or_tbl$term!="(Intercept)", ]
lbl <- or_tbl$term
lbl[lbl=="genderMale"]   <- "Gender: Male vs Female"
lbl[lbl=="clinClinical"] <- "Clinical vs Research"
lbl[lbl=="certCert"]     <- "Board Certified"
lbl[lbl=="lprate"]       <- "log(Publication Rate + 1)"
lbl[lbl=="exper"]        <- "Experience (years)"
lbl[lbl=="exper2"]       <- "Experience^2"
lbl[grepl("^dept", lbl)] <- paste("Dept:", sub("^dept","", lbl[grepl("^dept", lbl)]))
or_tbl$Label <- lbl; or_tbl$is_gender <- or_tbl$term=="genderMale"
or_tbl <- or_tbl[order(or_tbl$estimate), ]

p1 <- ggplot(or_tbl, aes(x=estimate, y=reorder(Label, estimate))) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, color=is_gender),
                 height=0.18, linewidth=2) +
  geom_point(aes(color=is_gender), size=3.2) +
  geom_vline(xintercept=1, linetype="dashed", color=GRAY) +
  scale_x_log10() +
  scale_color_manual(values=c(`TRUE`=BLUE, `FALSE`=GRAY), guide="none") +
  labs(title="Odds Ratios (Male vs Female) for Becoming Full Professor",
       x="Odds Ratio (log scale)", y=NULL) +
  theme_minimal(base_size=12) +
  theme(panel.grid.major.y=element_blank(), panel.grid.minor=element_blank())
ggsave("OR_forest_colored.png", p1, width=9, height=max(5, 0.5*nrow(or_tbl)), dpi=300)

# ===== Predicted probability by Dept (annotate 5/20 yrs) =====
lpr_med <- median(dat$lprate); exper_min <- max(0, min(dat$exper)); exper_max <- max(dat$exper)
exper_seq <- seq(exper_min, exper_max, length.out=120)

grid <- expand.grid(
  gender=factor(c("Female","Male"), levels=c("Female","Male")),
  dept=levels(dat$dept), clin="Clinical", cert="Cert",
  lprate=lpr_med, exper=exper_seq
)
grid$exper2 <- grid$exper^2
grid$pred <- predict(fit, newdata=grid, type="response")

ann <- expand.grid(
  gender=factor(c("Female","Male"), levels=c("Female","Male")),
  dept=levels(dat$dept), clin="Clinical", cert="Cert",
  lprate=lpr_med, exper=c(5,20)
)
ann$exper2 <- ann$exper^2
ann$pred <- predict(fit, newdata=ann, type="response")
ann$label <- paste0(sprintf("%.1f", ann$pred*100), "%")

p2 <- ggplot(grid, aes(exper, pred, color=gender)) +
  geom_line(linewidth=1.8) +
  geom_point(data=ann, aes(exper, pred), size=2.8) +
  geom_text(data=ann, aes(exper, pred, label=label),
            size=3.4, fontface="bold", hjust=-0.05, vjust=-0.2, show.legend=FALSE) +
  facet_wrap(~ dept, ncol=3) +
  scale_color_manual(values=c("Male"=BLUE, "Female"=PINK)) +
  scale_y_continuous(labels=label_percent(accuracy=1), limits=c(0,1),
                     expand=expansion(mult=c(0.02,0.03))) +
  labs(title="Predicted Probability of Full Professorship by Experience & Department",
       subtitle="Controls fixed: log(PubRate+1)=median, Clinical=Yes, Certified=Yes",
       x="Experience (years)", y="Predicted Probability", color=NULL) +
  theme_minimal(base_size=12) +
  theme(legend.position="top", legend.justification="left",
        strip.text=element_text(face="bold"), panel.grid.minor=element_blank())
ggsave("pred_prob_by_exper_colored.png", p2, width=13.5, height=8, dpi=300)

