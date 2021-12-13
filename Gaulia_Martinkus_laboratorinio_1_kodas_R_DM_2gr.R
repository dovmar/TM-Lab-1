library(tidyverse)
library(faux)
library(readr)
library(car)
library(agricolae)

# Duomenų simuliavimas ------

# Naudojamos straipsnyje aprašytos charakteristikos
mu_l <- c(13.82, 13.59, 10.55, 25.64)
sigma_l <- c(2.15, 1.68, 1.57, 5.18)

mu_r <- c(14.09, 13.09, 10.41, 22.68)
sigma_r <- c(1.72, 1.87, 2.17, 4.60)


construct_df <- function(hand, mean, sd) {
  pmap(list(rnorm_multi(22, 4, mean, sd, r = 0.5), sd, mean), wanted_mean_sd) %>%
    set_names("t1", "t2", "t3", "t4") %>%
    as_tibble() %>%
    mutate(handedness = hand)
}

wanted_mean_sd <- function(x, sd, mean) {
  (x - mean(x)) / sd(x) * sd + mean
}


df_right <- construct_df("right", mu_r, sigma_r)
biggest <- sort(df_right$t4, decreasing = TRUE)[1:4]
for (i in biggest) {
  df_right$t4[which(df_right$t4 == i)] <- df_right$t4[which(df_right$t4 == i)] + abs(rnorm(1, 6))
}
t4 <- df_right$t4
df_right$t4 <- (t4 - mean(t4)) / sd(t4) * 4.60 + 22.68


df <- rbind(construct_df("left", mu_l, sigma_l), df_right)

df <- df %>%
  mutate(
    age = rnorm(44, 17.32, 1.07),
    sex = sample(c(rep("male", 20), rep("female", 24)), 44)
  )



# Duomenų nuskaitymas -----

df <- read_csv("data.csv")



options(contrasts = c("contr.sum", "contr.poly"))


variance_check <- function(x) {
  eval(substitute(leveneTest(x ~ handedness * sex, data = df)))
}

anova_model <- function(x) {
  eval(substitute(aov(x ~ handedness * sex, data = df)))
}



# Tiriamieji grafikai -----

df_pivoted <- df %>% pivot_longer(1:4)
ggplot(df_pivoted, aes(handedness, value, color = sex)) +
  geom_boxplot() +
  theme_minimal(base_size = 16) +
  facet_wrap(vars(name), scales = "free")



# Vidurkių grafikas

ggplot(df_pivoted, aes(handedness, value, color = sex, group = sex)) +
  stat_summary(fun = "mean", geom = "line", size = 1) +
  theme_minimal(base_size = 16) +
  facet_wrap(vars(name), scales = "free")


# Dispersijų lygybės testas ----

variance_checks <- list(variance_check(t1), variance_check(t2), variance_check(t3), variance_check(t4))

variance_checks



# Dispersinė analizė ---- 

# Sukuriami dispersinės analizės modeliai kiekvienam testui
models <- list(anova_model(t1), anova_model(t2), anova_model(t3), anova_model(t4))


# Tikrinamas liekanų normalumas (dispersinės analizės prielaida)
op <- par(mfrow = c(1, 2))
map(models, ~ plot(.x, which = 2))
par(op)



# Nesubalansuotas eksperimento planas -> naudojamos Type III kv. sumos
map(models, ~ Anova(.x, type = "III"))


# Porinių kontrastų testas
HSD.test(models[[4]], trt = c("handedness", "sex"), console = TRUE, unbalanced = TRUE)




# Išskirtys tarp dešiniarankių
boxplot(t4 ~ handedness, data = df)

# Transformuojami duomenys ----

df2 <- df
t4 <- df$t4
limit <- mean(t4[df$handedness=="right"])+ 1.5*sd(t4[df$handedness=="right"])
df2$t4 <- sqrt(ifelse(t4>limit,limit,t4))



# Transformuotas modelis ----

model_trans <- aov(t4 ~ handedness * sex, df2)

Anova(model_trans, type = "III")

pairwise_test <- HSD.test(model_trans, trt = c("handedness", "sex"), console = TRUE, unbalanced = TRUE)
pairwise_test

plot(pairwise_test)
