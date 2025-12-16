#load packages
library(lme4)
library(multcomp)
library(dplyr)

# Load your dataset
 df <- read.csv("data/my_data.csv")

#compute relative contribution of each category per month
df <- df %>%
  dplyr::group_by(context, month, sex) %>%
  dplyr::mutate(n_category = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(context, month) %>%
  dplyr::mutate(rel_contrib = n_category / max(n_category))

df <- df %>%
  mutate(prop_leader = n_leader / (n_leader + n_follower))

df$ID <- as.factor(df$ID)
df$sex <- as.factor(df$sex)
df$repro_status <- as.factor(df$repro_status)


# Function to run models ----------------------------- 

run_model_leader_prop <- function(data, predictor, context_name){
  
  cat("\nRunning models for context:", context_name, "and predictor:", predictor, "\n")
  
  # Subset context
  dat <- subset(data, context == context_name)
  
  if(predictor == "repro_status"){
    
    dat <- dat |>
      dplyr::filter(sex == "F")
  }
  
  # Ensure ID and predictor are factors
  dat$ID <- as.factor(dat$ID)
  dat[[predictor]] <- as.factor(dat[[predictor]])
  
  # --- Model 1: leader/follower counts (two-vector response) ---
  formula1 <- as.formula(paste0("cbind(n_leader, n_follower) ~ ", predictor, " + (1|ID)"))
  model1 <- glmer(formula1,
                  family = binomial,
                  data = dat)
  
  # Null model
  formula_null1 <- as.formula("cbind(n_leader, n_follower) ~ (1|ID)")
  null1 <- glmer(formula_null1,
                 family = binomial,
                 data = dat)
  
  # Likelihood ratio test
  lrt1 <- anova(null1, model1, test = "Chisq")
  
  #confidence interval
  ci <- confint(model1, method = "Wald")  
  
  # output list
  out_list <- list(model = model1, summary_model = summary(model1), ci = ci, lrt = lrt1)
  
  #post hoc Tukey if predictor is reproductive state
  if(predictor == "repro_status"){
    posthoc <- glht(model1, linfct = mcp(repro_status = "Tukey"))
    out_list <- c(out_list, list(posthoc = posthoc))
  }
  
  return(out_list)
}


run_model_order_index <- function(data, predictor, context_name) {
  
  cat("\nRunning models for context:", context_name, "and predictor:", predictor, "\n")
  
  #subset by context
  dat <- subset(data, context == context_name)
  
  if(predictor == "repro_status"){
    
    dat <- dat |>
      dplyr::filter(sex == "F")
  }
  
  #ensure ID and predictor are factors
  dat$ID <- as.factor(dat$ID)
  dat[[predictor]] <- as.factor(dat[[predictor]])
  
  # --- Model 2: front/back counts (two-vector response) ---
  formula2 <- as.formula(paste0("cbind(n_front, n_back) ~ ", predictor, " + (1|ID)"))
  model2 <- glmer(formula2,
                  family = binomial,
                  data = dat)
  
  # Null model
  formula_null2 <- as.formula("cbind(n_front, n_back) ~ (1|ID)")
  null2 <- glmer(formula_null2,
                 family = binomial,
                 data = dat)
  
  
  #likelihood ratio test
  lrt2 <- anova(null2, model2)
  
  #confidence interval
  ci <- confint(model2, method = "Wald")  
  
  # output list
  out_list <- list(model = model2, summary_model = summary(model2), ci = ci, lrt = lrt2)
  
  #post hoc Tukey if predictor is reproductive state
  if(predictor == "repro_status"){
    posthoc <- glht(model2, linfct = mcp(repro_status = "Tukey"))
    out_list <- c(out_list, list(posthoc = posthoc))
  }
  
  
  return(out_list)
}


# 1st prediction: predictor = sex and context = Feeding  ----------

## 1st model: proportion of leadership ------


run_model_leader_prop(df, predictor="sex", context_name="Feeding")

# Filtramos solo el contexto Feeding
df_feeding <- subset(df, context == "Feeding")

# Boxplot
ggplot(df_feeding, aes(x = sex, y = prop_leader, fill = sex)) +
  geom_boxplot(width = 0.6, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, color = "red", size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
  theme_bw() +
  facet_wrap(~context) +
  labs(x = "Sex", y = "Proportion of monthly movements as leader") +
  scale_x_discrete(name = "Sex", labels = c("F" = "Female", "M" = "Male")) +
  scale_fill_manual(values = c("F" = "lightblue", "M" = "lightblue"))+
  theme(legend.position = "none")


## 2nd model: progression order index ------


run_model_order_index(df, predictor="sex", context_name="Feeding")

# Crear un dataframe resumido por sexo y posición
df_bar <- df %>%
  filter(context == "Feeding") %>%  # filtrar solo Feeding
  group_by(sex, mean_poi_position,context) %>%
  summarise(n_movements = n(), .groups = "drop")


# Barplot vertical
ggplot(df_bar, aes(x = sex, y = n_movements, fill = mean_poi_position)) +
  geom_bar(stat = "identity", 
           position = "dodge",  # ancho entre barras
           width = 0.6,  # ancho de cada barra
           color = "black") +  # borde negro fino
  geom_hline(yintercept = 17, linetype = "solid") +  # Female
  geom_hline(yintercept = 5, linetype = "dashed") + # Male
  scale_fill_manual(values = c("Front" = "#FF9999", "Back" = "#99CCFF"),
                    name = "Follower position") +
  scale_y_continuous(name = "Number of movements") +
  scale_x_discrete(name = "Sex", labels = c("F" = "Female", "M" = "Male")) +
  theme_bw() +
  facet_wrap(~context) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


# 2nd prediction: predictor = reproductive_status and context  = Feeding -----------

## 1st model: proportion of leadership ------

run_model_leader_prop(df, predictor="repro_status", context_name="Feeding")

# Filtramos solo el contexto Feeding
df_females <- subset(df, sex == "F") |>
  subset(context == "Feeding")


ggplot(df_females, aes(x = repro_status, y = prop_leader, fill = repro_status)) +
  geom_boxplot(width = 0.6, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, color = "red", size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "black") +
  theme_bw() +
  facet_wrap(~context) +
  labs(x = "Reproductive status", y = "Proportion of movements as leader") +
  scale_fill_manual(values = c("AFC" = "lightblue", "AFD" = "lightblue", "AFL" = "lightblue"))+
  scale_x_discrete(name = "Reproductive Status", labels = c("AFC" = "Cycling", "AFD" = "Codependent", "AFL" = "Lactanting")) +
  theme(legend.position = "none")


## 2nd model: progression order index ------

run_model_order_index(df, predictor="repro_status", context_name="Feeding")


#crear un dataframe resumido por repro status y posición
df_bar <- df_females %>%
  filter(context == "Feeding") %>%
  group_by(repro_status, mean_poi_position,context) %>%
  summarise(n_movements = n(), .groups = "drop")

df_bar <- rbind(df_bar, 
                data.frame(repro_status = "AFD", mean_poi_position = "Back", context = "Feeding", n_movements = 0)
                )


# Barplot vertical
ggplot(df_bar, aes(x = repro_status, y = n_movements, fill = mean_poi_position)) +
  geom_bar(stat = "identity", 
           position = "dodge",  # ancho entre barras
           width = 0.6,  # ancho de cada barra
           color = "black") +  # borde negro fino
  geom_hline(yintercept = 7, linetype = "dotted") +  # Cycling
  geom_hline(yintercept = 3, linetype = "solid") + # Codependent
  geom_hline(yintercept = 7, linetype = "dotdash") + # Lactating
  scale_fill_manual(values = c("Front" = "#FF9999", "Back" = "#99CCFF"),
                    name = "Follower position") +
  scale_y_continuous(name = "Number of movements") +
  scale_x_discrete(name = "Reproductive Status", labels = c("AFC" = "Cycling", "AFD" = "Codependent", "AFL" = "Lactanting")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


# 2nd prediction: predictor = sex and context  = Defense -----------

## 1st model: proportion of leadership ------


run_model_leader_prop(df, predictor="sex", context_name="Defense")

# Filtramos solo el contexto Defense
df_defense <- subset(df, context == "Defense")

# Boxplot
ggplot(df_defense, aes(x = sex, y = prop_leader, fill = sex)) +
  geom_boxplot(width = 0.6, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, color = "red", size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "black") +
  theme_bw() +
  facet_wrap(~context) +
  labs(x = "Sex", y = "Proportion of monthly movements as leader") +
  scale_x_discrete(name = "Sex", labels = c("F" = "Female", "M" = "Male")) +
  scale_fill_manual(values = c("F" = "lightblue", "M" = "lightblue"))+
  theme(legend.position = "none")

## 2nd model: progression order index ------

run_model_order_index(df, predictor="sex", context_name="Defense")


# Crear un dataframe resumido por sexo y posición
df_bar <- df %>%
  filter(context == "Defense") %>%  # filtrar solo Feeding
  group_by(sex, mean_poi_position,context) %>%
  summarise(n_movements = n(), .groups = "drop")


# Barplot vertical
ggplot(df_bar, aes(x = sex, y = n_movements, fill = mean_poi_position)) +
  geom_bar(stat = "identity", 
           position = "dodge",  # ancho entre barras
           width = 0.6,  # ancho de cada barra
           color = "black") +  # borde negro fino
  geom_hline(yintercept = 17, linetype = "solid") +  # Female
  geom_hline(yintercept = 5, linetype = "dashed") + # Male
  scale_fill_manual(values = c("Front" = "#FF9999", "Back" = "#99CCFF"),
                    name = "Follower position") +
  scale_y_continuous(name = "Number of movements") +
  scale_x_discrete(name = "Sex", labels = c("F" = "Female", "M" = "Male")) +
  theme_bw() +
  facet_wrap(~context) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


# tabla estadistica -----------
statistical_table <- function(model_list){
  
  # Extraer coeficientes resumidos
  coef_sum <- summary(model_list$model)$coefficients
  
  # Extraer intervalos de confianza
  ci <- confint(model_list$model, parm = "beta_", level = 0.95, method = "Wald")  # o confint.default si Wald
  
  # Crear tabla
  results_table <- data.frame(
    Predictor = rownames(coef_sum),
    β = coef_sum[, "Estimate"],
    SE = coef_sum[, "Std. Error"],
    Z = coef_sum[, "z value"],
    p = coef_sum[, "Pr(>|z|)"],
    CI_lower = ci[,1],
    CI_upper = ci[,2]
  )
  
  # Formatear 95% C.I. como una columna única
  results_table$`95% C.I.` <- paste0("[", round(results_table$CI_lower, 3), ", ", round(results_table$CI_upper, 3), "]")
  
  # Seleccionar columnas finales
  results_table <- results_table[, c("Predictor", "β", "SE", "95% C.I.", "Z", "p")]
  rownames(results_table) <- NULL
  
  return(results_table)
  
}