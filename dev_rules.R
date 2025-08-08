library(tidyverse)
library(readr)
library(emmeans)
library(readxl)
library(lmerTest)
library(optimx)

filename1 = "Data/child_data.csv"
# Deleting 1st column
headers1 = read_csv(filename1, col_names = FALSE, n_max = 1)
# Raw data (deleting 3 rows)
dev_child = read_csv(filename1, skip = 3, col_names = FALSE)
# Naming raw date (with "headers")
colnames(dev_child) = headers1

filename2 = "Data/adult_data.csv"
# Deleting 1st column
headers2 = read_csv(filename2, col_names = FALSE, n_max = 1)
# Raw data (deleting 3 rows)
dev_adult = read_csv(filename2, skip = 3, col_names = FALSE)
# Naming raw date (with "headers")
colnames(dev_adult) = headers2

# manual colname update
colnames(dev_child)[22:45] = c('c1', 'n1', 'u1', 'o1',
                          'c2', 'n2', 'o2', 'u2',
                          'n3', 'c3', 'o3', 'u3',
                          'c4', 'n4', 'u4', 'o4',
                          'c5', 'n5', 'o5', 'u5',
                          'n6', 'c6', 'o6', 'u6')

colnames(dev_adult)[19:42] = c('c1', 'n1', 'u1', 'o1',
                          'c2', 'n2', 'o2', 'u2',
                          'n3', 'c3', 'o3', 'u3',
                          'c4', 'n4', 'u4', 'o4',
                          'c5', 'n5', 'o5', 'u5',
                          'n6', 'c6', 'o6', 'u6')

dev_full = bind_rows(dev_child %>% mutate(group = 'Children'),
                     dev_adult %>% mutate(group = case_when(
                       age >= 18 & age <= 25 ~ 'Adults (18-25)',
                       age >= 45 & age <= 65 ~ 'Adults (45-65)'
                     ))) %>%
  filter(DistributionChannel != 'preview') %>%
  select(timestamp = RecordedDate, 
         group,
         subj_id = ResponseId, 
         child_id = id,
         complete = Finished,
         progress = Progress, 
         completion_time = `Duration (in seconds)`,
         latitute = LocationLatitude,
         longitude = LocationLongitude,
         nielsen_area = Nielsen,
         sex, 
         adult_gender = gender,
         age, grade, c1:u6) %>%
  filter(progress > 90)

glimpse(dev_full)

# demog
xtabs(~ group, dev_full)

dev_full %>%
  group_by(group) %>%
  summarise(mean(sex == 'F', na.rm = TRUE),
            mean(adult_gender == 2, na.rm = TRUE))


dev_long = dev_full %>%
  gather(c1:u6, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(compliance = 2 - resp,
         violation = resp - 1,
         scen = as.factor(str_sub(key, 2, 2)),
         condition = str_sub(key, 1, 1),
         text = if_else(condition %in% c('c','o'), 1,
                        if_else(condition %in% c('u','n'), 0, NA)),
         purpose = if_else(condition %in% c('c','u'), 1,
                           if_else(condition %in% c('o','n'), 0, NA)), 
         group = relevel(as.factor(group), ref = 'Children'))
glimpse(dev_long)

mod1 = glmer(violation ~ group * (text * purpose) + (1 | subj_id) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "nlminb")))

car::Anova(mod1)
jtools::summ(mod1)
emmeans(mod1, pairwise ~ group | text * purpose, type = 'response')
emtrends(mod1, pairwise ~ group, var = 'text')
emtrends(mod1, pairwise ~ group, var = 'purpose')

dev_supp <- read_excel("data_reglas.xlsx") %>%
  filter(!is.na(CODIGO))
names(dev_supp) = c('codigo', 'codigo2', 'grupo', 'sex',
                    'edad', 'edad_meses', 'date', 'birth_date',
                    'stroop_score', 'tom_1', 'tom_2', 'tom_3',
                    'tom_4', 'tom_5', 'tom_6', 'tom_7', 'tom_8',
                    'tom_9', 'tom_total', 'comments_tom', 'forward',
                    'fwd_score', 'span_1', 'backward', 'bwd_score',
                    'span_2', 'comments')
glimpse(dev_supp)
dev_long = left_join(dev_long, dev_supp, join_by(id == codigo))
dev_long = dev_long %>%
  mutate(stroop_ctr = stroop_score - 22,
         meses_ctr = edad_meses - 85,
         tom_ctr = as.numeric(tom_total) - 7,
         fw_ctr = as.numeric(forward) - 6,
         bw_ctr = as.numeric(backward) - 6)

cor.test(~ stroop_score + edad_meses, dev_supp, method = 'spearman')
cor.test(~ as.numeric(tom_total) + edad_meses, dev_supp, method = 'spearman')
cor.test(~ as.numeric(forward) + edad_meses, dev_supp, method = 'spearman')
cor.test(~ as.numeric(backward) + edad_meses, dev_supp, method = 'spearman')

mod2 = glmer(violation ~ meses_ctr * (text * purpose) + (1 | ResponseId) + (1 | scen),
             data = dev_long, family = 'binomial')

car::Anova(mod2)
jtools::summ(mod2, exp = TRUE)
emtrends(mod2, pairwise ~ text, var = 'meses_ctr')
emtrends(mod2, pairwise ~ purpose, var = 'meses_ctr')

interactions::interact_plot(mod2, pred = 'meses_ctr', modx = 'text', 
                            mod2 = 'purpose', interval = TRUE)
interactions::interact_plot(mod2, pred = 'meses_ctr', modx = 'purpose')

mod3 = glmer(violation ~ (meses_ctr + stroop_ctr) * (text + purpose) + (1 | ResponseId) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))
mod3no = glmer(violation ~ (stroop_ctr) * (text * purpose) + (1 | ResponseId) + (1 | scen),
               data = dev_long, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa")))
car::Anova(mod3)
emtrends(mod3, pairwise ~ text, var = 'stroop_ctr')
emtrends(mod3no, pairwise ~ text, var = 'stroop_ctr')
emtrends(mod3no, pairwise ~ purpose, var = 'stroop_ctr')
interactions::interact_plot(mod3, pred = 'stroop_ctr', modx = 'text', 
                            mod2 = 'purpose', interval = TRUE)

mod4 = glmer(violation ~ (meses_ctr + tom_ctr) * (text + purpose) + (1 | ResponseId) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))
mod4no = glmer(violation ~ (tom_ctr) * (text * purpose) + (1 | ResponseId) + (1 | scen),
               data = dev_long, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa")))

car::Anova(mod4)
car::Anova(mod4no)
emtrends(mod4, pairwise ~ text, var = 'tom_ctr')
emtrends(mod4no, pairwise ~ text, var = 'tom_ctr')
emtrends(mod4no, pairwise ~ purpose, var = 'tom_ctr')
interactions::interact_plot(mod4, pred = 'tom_ctr', modx = 'text', 
                            mod2 = 'purpose', interval = TRUE)

mod5 = glmer(violation ~  (meses_ctr + fw_ctr)  * (text + purpose) + (1 | ResponseId) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

mod5no = glmer(violation ~ (fw_ctr) * (text * purpose) + (1 | ResponseId) + (1 | scen),
               data = dev_long, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa")))

car::Anova(mod5)
car::Anova(mod5no)
emtrends(mod5, pairwise ~ text, var = 'fw_ctr')
emtrends(mod5no, pairwise ~ text, var = 'fw_ctr')
emtrends(mod5no, pairwise ~ purpose, var = 'fw_ctr')
interactions::interact_plot(mod5, pred = 'fw_ctr', modx = 'text', 
                            mod2 = 'purpose', interval = TRUE)

mod6 = glmer(violation ~ (meses_ctr + bw_ctr) * (text + purpose) + (1 | ResponseId) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

mod6no = glmer(violation ~ (bw_ctr) * (text * purpose) + (1 | ResponseId) + (1 | scen),
               data = dev_long, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa")))

car::Anova(mod6)

emtrends(mod6, pairwise ~ text, var = 'bw_ctr')
emtrends(mod6no, pairwise ~ text, var = 'bw_ctr')
emtrends(mod6no, pairwise ~ purpose, var = 'bw_ctr')
interactions::interact_plot(mod6, pred = 'bw_ctr', modx = 'text', 
                            mod2 = 'purpose', interval = TRUE)

stroop_data = stroop_times %>%
  mutate(congruent = if_else(ITEM <= 12, 'Con', 'Inc')) %>%
  group_by(CODIGO, congruent) %>%
  summarise(rt = median(`RT ONSET (SECONDS,MS)`, na.rm = TRUE), 
            resp = mean(ERROR, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'congruent', values_from = c('rt', 'resp'))

wilcox.test(stroop_data$rt_Con, stroop_data$rt_Inc)
wilcox.test(stroop_data$resp_Con, stroop_data$resp_Inc)

View(stroop_data)
dev_long = full_join(dev_long, stroop_data, join_by(id == CODIGO))

dev_long = dev_long %>% 
  mutate(diff_rt = rt_Inc - rt_Con, 
         diff_resp = resp_Con - resp_Inc, 
         interference = scale(diff_rt) + scale(diff_resp), 
         efficiency_congruent = scale(rt_Con/resp_Con),
         efficiency_incongruent = scale(rt_Inc/resp_Inc), 
         meses_z = scale(edad_meses),
         efficiency_loss = scale(efficiency_incongruent - efficiency_congruent))

mod3s = glmer(violation ~ (meses_z + efficiency_congruent + efficiency_incongruent) * (text * purpose) + (1 | ResponseId) + (1 | scen),
              data = dev_long, family = 'binomial', 
              control = glmerControl(optimizer = "optimx", 
                                     optCtrl = list(method =  "bobyqa")))

car::Anova(mod3s)

strategies= dev_long %>%
  subset(group != 'peques') %>%
  select(ResponseId, condition, scen, violation) %>%
  pivot_wider(names_from = 'condition', values_from = 'violation') %>%
  mutate(profile = paste(c, o, u, n, sep = "")) %>%
  group_by(ResponseId, profile) %>%
  tally() %>%
  arrange(-n) %>%
  pivot_wider(names_from = 'profile', values_from = 'n', values_fill = 0) %>%
  rename(textualist = `1100`, purposivist = `1010`,
         disjunctive = `1110`, conjunctive = `1000`) %>%
  mutate(other = 6 - textualist - purposivist - disjunctive - conjunctive, 
         max_t = textualist - purposivist) %>%
  select(ResponseId, max_t, textualist:conjunctive) %>%
  gather(-ResponseId, -max_t, key = 'strategy', value = 'count') %>%
  mutate(value = if_else(strategy %in% c('purposivist', 'disjunctive'), 
                         count, -count))

ggplot(strategies, aes(x = reorder(ResponseId, -max_t), 
                       y = value, fill = strategy)) + 
  geom_col() + 
  coord_flip()




emtrends(mod3s, pairwise ~ text, var = 'efficiency_congruent')
emtrends(mod3s, pairwise ~ purpose, var = 'efficiency_congruent')
emtrends(mod3s, pairwise ~ text, var = 'efficiency_incongruent')
emtrends(mod3s, pairwise ~ purpose, var = 'efficiency_incongruent')


interactions::interact_plot(mod3s, pred = 'efficiency_incongruent', 
                            modx = 'text', mod2 = 'purpose', 
                            interval = TRUE)

interactions::interact_plot(mod3s, pred = 'meses_ctr', 
                            modx = 'text', mod2 = 'purpose', 
                            interval = TRUE)
dev_long %>%
  select(edad_meses, id, efficiency_congruent, efficiency_loss) %>%
  unique(.) %>%
  cor.test(~ efficiency_congruent + edad_meses, ., method = 'spearman')

dev_long %>%
  select(edad_meses, id, efficiency_congruent, efficiency_incongruent) %>%
  unique(.) %>%
  cor.test(~ efficiency_incongruent + edad_meses, ., method = 'spearman')

dev_long %>%
  select(edad_meses, id, efficiency_congruent, efficiency_incongruent) %>%
  unique(.) %>%
  cor.test(~ efficiency_incongruent + edad_meses, .)

dev_long %>%
  select(edad_meses, id, interference, efficiency_congruent, efficiency_incongruent) %>%
  unique(.) %>%
  cor.test(~ interference + edad_meses, .)


emtrends(mod3s, pairwise ~ text, var = 'interference')
emtrends(mod3s, pairwise ~ purpose, var = 'rt_Inc')

dev_long %>%
  group_by(ResponseId, condition) %>%
  summarise(stroop = median(stroop_score),
            response = mean(violation, na.rm = TRUE)) %>%
  filter(!is.na(stroop))
dev_long %>%
  group_by(ResponseId, condition) %>%
  summarise(stroop = median(stroop_score),
            response = mean(violation, na.rm = TRUE)) %>%
  filter(!is.na(stroop)) %>%
  ggplot(aes(x = 24 - stroop, y = response, color = condition)) +
  geom_jitter()

dev_long$condition = as.factor(dev_long$condition)
levels(dev_long$condition2)


dev_long$condition2 <- factor(dev_long$condition, levels = c("c", "o", "u", "n"))


devfig1a = dev_long %>%
  group_by(condition2, group, text, purpose) %>%
  summarise(violate = mean(violation, na.rm = TRUE),
            lower_bound = DescTools::BinomCI(sum(violation, na.rm = TRUE),
                                             sum(!is.na(violation), na.rm = TRUE))[2],
            upper_bound = DescTools::BinomCI(sum(violation, na.rm = TRUE),
                                             sum(!is.na(violation), na.rm = TRUE))[3]) %>%
  ggplot(aes(x = reorder(group, -violate), y = violate, group = condition2)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, 
                  fill = condition2),
              alpha = .8) +
  geom_line(aes(linetype = text != purpose), color = 'white', linewidth = .6) +
  geom_point(aes(color = condition2), shape = 21, stroke = 1.5, fill = 'white') +
  scale_x_discrete(labels = c('Children\n5-8', 'Young Adults\n18-25',
                              'Older Adults\n45-65'),
                   expand = c(0.05, 0), name = 'Age group') +
  scale_y_continuous(breaks = seq(0, 1, .5), name = 'Rule judgment',
                     expand = c(0, 0), limits = c(0, 1),
                     labels = c('Compliance', '', 'Violation')) +
  theme_classic() +
  scale_fill_manual(values = c("#2A3553", "#7570B3", "#1B9E77", "#C8D8D5")) +
  #scale_fill_manual(values = c('#E69F00', "#F0C87A", "#7FD6B6",  '#009E73')) +
  scale_color_manual(values = c("#2A3553",  "#7570B3","#1B9E77", "#C8D8D5"),
                     labels = c('Violation',
                                'Violation of Letter', 'Violation of Spirit',
                                'Compliance')) +
  theme(legend.position = c(0.52, 0.22),
        legend.background = element_rect(color = "#cdcdcd"),
        axis.ticks.x = element_blank(),
        text = element_text(size = 14, family = 'Helvetica Neue', 
                            color = 'black'),
        plot.margin = unit(c(.4, .8, .2, .2), 'cm'),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11, family = 'Helvetica Neue', 
                                   color = 'black')) +
  guides(linetype = FALSE, fill = FALSE, 
         color=guide_legend(nrow=2,byrow=FALSE))


# ggsave("dev_rulesA.jpg", width = 15, height = 18, units = 'cm')

glimpse(dev_long)

library(flexmix)
glm_model <- glm(violation ~ t + m, family = binomial(), data = dev_long)
logLik(glm_model)
AIC(glm_model)
BIC(glm_model)

model <- flexmix(violation ~ t + m, data = dev_long, k = 2)
summary(model) # AIC: 2961.5
dev_long$class <- posterior(model)$class

post_probs <- posterior(model)  # This returns a matrix of posterior probabilities, one column per class

# To get the most likely class per observation:
assigned_classes <- apply(post_probs, 1, which.max)

table(dev_long$class, dev_long$condition)

table(subset(dev_long, text == purpose)$class, subset(dev_long, text == purpose)$group)
table(subset(dev_long, text != purpose)$class, subset(dev_long, text != purpose)$group)

prop.table(table(dev_long$class, dev_long$condition), 2)

prop.table(
  table(subset(dev_long, text != purpose)$class, subset(dev_long, text != purpose)$group), 2)

library(brms)

# Define mixture components: logistic regressions with different coefficients
mix <- mixture(bernoulli(), bernoulli())

# Formula for each component (simplified):
formula <- bf(violation ~ t + m)

# Fit 2-component mixture
fit2 <- brm(formula, data = dev_long, family = mix, chains = 4, iter = 2000)


# Add to your data:
dev_long$class <- assigned_classes


dev_long %>%
  ggplot(aes(x = m, y = t, fill = violation)) + 
  geom_jitter(shape = 21, width = .1, height = .1, alpha = .6) + 
  scale_fill_gradient(low = 'red', high = 'green')

kid_means = dev_long %>%
  group_by(edad, condition2, text, purpose) %>%
  summarise(violate = mean(violation, na.rm = TRUE),
            lower_bound = DescTools::BinomCI(sum(violation, na.rm = TRUE),
                                             sum(!is.na(violation), na.rm = TRUE))[2],
            upper_bound = DescTools::BinomCI(sum(violation, na.rm = TRUE),
                                             sum(!is.na(violation), na.rm = TRUE))[3]) %>%
  mutate(edad_meses = edad*12 + 6) %>% filter(!is.na(edad_meses))

devfig1b = dev_long %>%
  group_by(id, condition2, text, purpose) %>%
  summarise(violate = mean(violation, na.rm = TRUE),
            edad_meses = median(edad_meses, na.rm = TRUE)) %>%
  ggplot(aes(x = edad_meses, y = violate, group = condition2)) +
  geom_jitter(aes(color = condition2, shape = text != purpose), 
              alpha = .6, fill = 'white', stroke = 1,
              size = 1.5, height = 1/12) +
  geom_ribbon(data = kid_means, aes(ymin = lower_bound, ymax = upper_bound, 
                                    fill = condition2), alpha = .7) +
  geom_line(data = kid_means, aes(linetype = text != purpose), color = 'white', linewidth = .6) +
  geom_point(data = kid_means, aes(color = condition2), 
             shape = 21, stroke = 1.5, fill = 'white') +
  scale_x_continuous(breaks = seq(60, 110, 10),
                     limits = c(60, 110),
                     expand = c(0, 0), name = 'Age (months)', 
                     sec.axis = dup_axis(transform = ~./12, 
                                         name = 'Age (years)',
                                         breaks = seq(5, 9, 1))) +
  scale_y_continuous(breaks = seq(0, 1, .5), name = NULL,
                     expand = c(0, 0),
                     labels = NULL) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  scale_shape_manual(values = c(16, 21)) +
  scale_fill_manual(values = c("#2A3553", "#7570B3", "#1B9E77", "#C8D8D5")) +
  #scale_fill_manual(values = c('#E69F00', "#F0C87A", "#7FD6B6",  '#009E73')) +
  scale_color_manual(values = c("#2A3553",  "#7570B3","#1B9E77", "#C8D8D5"),
                     labels = c('Full Violation',
                                'Violation of Letter', 'Violation of Spirit',
                                'Compliance')) +
  theme(axis.ticks.x = element_blank(),
        text = element_text(size = 14, family = 'Helvetica Neue', 
                            color = 'black'),
        plot.margin = unit(c(.4, .8, .2, .2), 'cm'), 
        legend.position = 'none') +
  guides(linetype = FALSE, fill = FALSE)


ggpubr::ggarrange(devfig1a, devfig1b, nrow = 1, align = 'h', 
                  widths = c(1.4, 1), labels = c('A', 'B'), 
                  hjust = c(-0.6, 1.6), vjust = c(2.2, 2.2))

ggsave("dev_rules_panel.jpg", width = 24, height = 14, units = 'cm')

filename = "Reglas+supp_May+15,+2025_08.58.csv"

headers = read_csv(filename, col_names = FALSE, n_max = 1)
headers
# Raw data (deleting 3 rows)
supp_rules = read_csv(filename, skip = 3, col_names = FALSE)
# Naming raw date (with "headers")
colnames(supp_rules) = headers
glimpse(supp_rules)
colnames(supp_rules)[19:66] = c('c1t', 'c1m', 'n1t', 'n1m', 'u1t', 'u1m',  'o1t', 'o1m',
                                'c2t', 'c2m', 'n2t','n2m', 'o2t', 'o2m', 'u2t', 'u2m',
                                'n3t', 'n3m', 'c3t', 'c3m', 'o3t', 'o3m', 'u3t', 'u3m',
                                'c4t', 'c4m', 'n4t', 'n4m', 'u4t', 'u4m', 'o4t', 'o4m',
                                'c5t', 'c5m', 'n5t', 'n5m', 'o5t', 'o5m', 'u5t', 'u5m',
                                'n6t', 'n6m', 'c6t', 'c6m', 'o6t', 'o6m', 'u6t', 'u6m')

ggplot(supp_rules, aes(x = LocationLongitude, y = LocationLatitude)) + 
  geom_jitter()

supp_means = supp_rules %>%
  select(c1t:u6m) %>%
  gather() %>%
  mutate(predictor = str_sub(key, -1, -1),
         key = str_sub(key, 1, 2)) %>%
  group_by(key, predictor) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'predictor', values_from = 'mean_value')

supp_means %>%
  group_by(str_sub(key, 1, 1)) %>%
  summarise((5 - mean(m))/4, 2- mean(t))

dev_long = full_join(dev_long, supp_means, by = 'key')

dev_long = dev_long %>%
  mutate(t = 2 - t, 
         m = (5 - m)/4)

model_s = glm(violation ~ group * (m + t), dev_long, family = 'binomial')
car::Anova(model_s)
emtrends(model_s, pairwise ~ group, var = 'm')
emtrends(model_s, pairwise ~ group, var = 't')

model_s = glmer(violation ~ meses_ctr * (t + m) + (1 | ResponseId) + (1 | scen),
                data = dev_long, family = 'binomial', 
                control = glmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "nlminb")))
car::Anova(model_s)
jtools::summ(model_s, digits = 3)

emtrends(model_s, pairwise ~ group, var = 'm')
emtrends(model_s, pairwise ~ group, var = 't')

interactions::sim_slopes(model_s, pred = 'm', modx = 'group', digits = 3)
interactions::sim_slopes(model_s, pred = 't', modx = 'group', digits = 3)



cor.test(~ m + violation, subset(dev_long, group == 'peques')) .67
cor.test(~ t + violation, subset(dev_long, group == 'peques')) .30
cor.test(~ m + violation, subset(dev_long, group == 'adultos1')) .47
cor.test(~ t + violation, subset(dev_long, group == 'adultos1')) .58
cor.test(~ m + violation, subset(dev_long, group == 'adultos2')) .46
cor.test(~ t + violation, subset(dev_long, group == 'adultos2')) .58



pcor_ci.test <-
  function (x, y, z, method = c("pearson", "kendall", "spearman"), conf.level = 0.95, ...) {
    d1 <- deparse(substitute(x))
    d2 <- deparse(substitute(y))
    d3 <- deparse(substitute(z))
    data.name <- paste0(d1, " and ", d2, "; controlling: ", d3)
    method <- match.arg(method)
    Method <- paste0("Partial correlation (", method, ")")
    alternative <- "true partial correlation is not equal to 0"
    
    x <- as.vector(x)
    y <- as.vector(y)
    z <- as.data.frame(z)
    xyz <- data.frame(x, y, z)
    pcor <- ppcor::pcor(xyz, method = method)
    estimate <- pcor$est[1, 2]
    p.value <- pcor$p.value[1, 2]
    parameter <- c(n = pcor$n, gp = pcor$gp)
    statistic <- c(Stat = pcor$statistic[1, 2])
    
    fit1 <- lm(x ~ z, data = xyz)
    fit2 <- lm(y ~ z, data = xyz)
    cortest <- cor.test(resid(fit1), resid(fit2), method = method, conf.level = conf.level, ...)
    ci <- cortest$conf.int
    
    ht <- list(
      statistic = statistic,
      parameter = parameter,
      p.value = p.value,
      estimate = c(partial.cor = estimate),
      alternative = alternative,
      method = Method,
      data.name = data.name,
      conf.int = ci
    )
    class(ht) <- "htest"
    ht
  }

supp_group <- data.frame(
  group = character(),
  predictor = character(),
  Aggregate = character(),
  lower_ci = character(),
  upper_ci = character(),
  stringsAsFactors = FALSE  # avoids auto-conversion of characters to factors
)

for (i in c("peques", "adultos1", "adultos2")) {
  temp = pcor_ci.test(subset(dev_long, group == i)$violation, 
                      subset(dev_long, group == i)$t, 
                      subset(dev_long, group == i)$m)
  
  new_row = c(group = i, predictor = 'Text', 
              as.numeric(temp$estimate), 
              as.numeric(temp$conf.int[1]), 
              as.numeric(temp$conf.int[2]))
  
  names(new_row) = names(supp_group)
  supp_group = bind_rows(supp_group, new_row)
  
  temp = pcor_ci.test(subset(dev_long, group == i)$violation, 
                      subset(dev_long, group == i)$m, 
                      subset(dev_long, group == i)$t)
  
  new_row = c(group = i, predictor = 'Moral', 
              as.numeric(temp$estimate), 
              as.numeric(temp$conf.int[1]), 
              as.numeric(temp$conf.int[2]))
  
  names(new_row) = names(supp_group)
  supp_group = bind_rows(supp_group, new_row)
  
}

supp_group = supp_group %>% 
  mutate(age_order = case_when(group == 'peques' ~ 1,
                               group == 'adultos1' ~ 2,
                               group == 'adultos2' ~ 3),
         group_label = case_when(group == 'peques' ~ 'Children\n5-8',
                                 group == 'adultos1' ~ 'Young Adults\n18-25',
                                 group == 'adultos2' ~ 'Older Adults\n45-65'), 
         Aggregate = if_else(predictor == 'Text', as.numeric(Aggregate), -as.numeric(Aggregate)), 
         lower_ci = if_else(predictor == 'Text', as.numeric(lower_ci), -as.numeric(lower_ci)),
         upper_ci = if_else(predictor == 'Text', as.numeric(upper_ci), -as.numeric(upper_ci)))

pcor_ci.test(subset(dev_long, group == 'adultos2')$violation, 
             subset(dev_long, group == 'adultos2')$t, 
             subset(dev_long, group == 'adultos2')$m)

pcor_ci.test(subset(dev_long, group == 'adultos2')$violation, 
             subset(dev_long, group == 'adultos2')$m, 
             subset(dev_long, group == 'adultos2')$t)

pcor_ci.test(subset(dev_long, group == 'adultos1')$violation, 
             subset(dev_long, group == 'adultos1')$t, 
             subset(dev_long, group == 'adultos1')$m)

pcor_ci.test(subset(dev_long, group == 'adultos1')$violation, 
             subset(dev_long, group == 'adultos1')$m, 
             subset(dev_long, group == 'adultos1')$t)

pcor_ci.test(subset(dev_long, group == 'peques')$violation, 
             subset(dev_long, group == 'peques')$t, 
             subset(dev_long, group == 'peques')$m)

pcor_ci.test(subset(dev_long, group == 'peques')$violation, 
             subset(dev_long, group == 'peques')$m, 
             subset(dev_long, group == 'peques')$t)


supp_results <- data.frame(
  group = character(),
  ResponseId = character(),
  Intercept = numeric(),
  Moral = numeric(),
  Textual = numeric(),
  stringsAsFactors = FALSE  # avoids auto-conversion of characters to factors
)

for (i in levels(as.factor(dev_long$ResponseId))) {
  temp= subset(dev_long, ResponseId == i)
  model_s = lm(violation ~ m + t, 
               temp)
  
  group = temp %>% select(group) %>% unique()
  
  new_row = data.frame(group, i, model_s$coefficients[1], model_s$coefficients[2], model_s$coefficients[3])
  colnames(new_row) = colnames(supp_results)
  supp_results = bind_rows(supp_results, new_row)
}


supp_results2 <- data.frame(
  group = character(),
  ResponseId = character(),
  Textual = numeric(),
  Moral = numeric(),
  Textual_p = numeric(),
  Moral_p = numeric(),
  stringsAsFactors = FALSE  # avoids auto-conversion of characters to factors
)

for (i in levels(as.factor(dev_long$ResponseId))) {
  temp= subset(dev_long, ResponseId == i)
  
  m_coef = cor.test(~ m + violation, temp)$estimate
  t_coef = cor.test(~ t + violation, temp)$estimate
  m_pval = cor.test(~ m + violation, temp)$p.value
  t_pval = cor.test(~ t + violation, temp)$p.value
  group = temp %>% select(group) %>% unique()
  
  new_row = data.frame(group, i, t_coef, m_coef, t_pval, m_pval)
  colnames(new_row) = colnames(supp_results2)
  supp_results2 = bind_rows(supp_results2, new_row)
}

supp_results2 %>% 
  gather(Textual:Moral, key = 'key', value = 'response') %>%
  ggplot(aes(x = response, fill = group)) +
  geom_density(alpha = .3) + 
  facet_grid(. ~ key)

devcog_param = dev_long %>%
  select(id, ResponseId, backward, tom_total, edad_meses,
         rt_Con:resp_Inc) %>%
  unique(.) %>%
  mutate(efficiency_congruent = rt_Con/resp_Con,
         backward = as.numeric(backward),
         efficiency_incongruent = rt_Inc/resp_Inc) %>%
  left_join(supp_results2, by = 'ResponseId')

cor.test(~ as.numeric(tom_total) + Textual, devcog_param, method = 'spearman')
cor.test(~ as.numeric(tom_total) + Moral, devcog_param, method = 'spearman')

cor.test(~ efficiency_incongruent + Textual, devcog_param)
cor.test(~ efficiency_incongruent + Moral, devcog_param)


modT = lm(Textual ~ edad_meses, 
          devcog_param)
jtools::summ(modT, digits = 3)

modT1 = lm(Textual ~ efficiency_incongruent +  bw_ctr + edad_meses, 
           devcog_param)
jtools::summ(modT1, digits = 3)

anova(modT, modT1)

## previous color schme
#scale_fill_manual(values = c('#009E73', '#E69F00')) +
# scale_color_manual(values = c('#009E73', '#E69F00')) +

a4 = devcog_param %>%
  gather(Textual,Moral, key = 'param', value = 'resp') %>%
  ggplot(aes(x = edad_meses, y = resp, color = param)) + 
  geom_jitter(width = .5, height = 0, shape = 21, stroke = 1) + 
  geom_smooth(aes(fill = param), method = 'lm', linewidth = .6) +
  scale_x_continuous(name = 'Age (months)', 
                     expand = c(0.01, 0.01)) +  
  scale_y_continuous(name = "",
                     labels = c("0", ".25", ".50", ".75", "1"),
                     breaks = seq(0, 1, .25),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(ylim = c(0, 1)) + 
  theme_classic() + 
  scale_fill_manual(values = c("#1B9E77", "#7570B3")) +
  scale_color_manual(values = c("#1B9E77", "#7570B3")) +
  scale_alpha_manual(values = c(.1, .7)) +
  theme(axis.text.y = element_text(size = 10, angle = 0),
        axis.ticks = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'none')

b4 = devcog_param %>%
  gather(Textual,Moral, key = 'param', value = 'resp') %>%
  ggplot(aes(x = backward, y = resp, color = param)) + 
  geom_jitter(width = .1, height = 0, shape = 21, stroke = 1) + 
  geom_smooth(aes(fill = param), method = 'lm', linewidth = .6) +
  scale_x_continuous(name = 'Backward digit span',
                     expand = c(0.01, 0.01))  +
  scale_y_continuous(name = 'Coefficient', 
                     labels = c("0", ".25", ".50", ".75", "1"),
                     breaks = seq(0, 1, .25),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() + 
  scale_fill_manual(values = c("#1B9E77", "#7570B3")) +
  scale_color_manual(values = c("#1B9E77", "#7570B3")) +
  scale_alpha_manual(values = c(.1, .7)) +
  theme(axis.text.y = element_text(size = 10, angle = 0),
        axis.ticks = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'none')

c4 = devcog_param %>%
  gather(Textual,Moral, key = 'param', value = 'resp') %>%
  mutate(efficiency_incongruent = if_else(efficiency_incongruent > 3, 2, 
                                          efficiency_incongruent)) %>%
  ggplot(aes(x = -efficiency_incongruent, y = resp, color = param)) + 
  geom_jitter(width = 0, height = 0, shape = 21, stroke = 1) + 
  scale_x_continuous(name = 'Stroop task efficiency (-RT/accuracy)', 
                     expand = c(0.01, 0.01))  +
  scale_y_continuous(name = "", 
                     labels = c("0", ".25", ".50", ".75", "1"),
                     breaks = seq(0, 1, .25),
                     expand = c(0.01, 0.01)) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_smooth(aes(fill = param), method = 'lm', linewidth = .6) +
  theme_classic() + 
  scale_fill_manual(values = c("#1B9E77", "#7570B3")) +
  scale_color_manual(values = c("#1B9E77", "#7570B3")) +
  scale_alpha_manual(values = c(.1, .7)) +
  theme(axis.text.y = element_text(size = 10, angle = 0),
        axis.ticks = element_blank(), 
        strip.background = element_blank(), 
        legend.position = 'none')

ggpubr::ggarrange(d4, ggpubr::ggarrange(a4, b4, c4, ncol = 1, 
                                        labels = c('B', 'C', 'D')), 
                  ncol = 2, labels = c('A', ''), widths = c(1.2, 1))


ggsave("dev_rules_panel3.jpg", width = 24, height = 14, units = 'cm')


ggplot(devcog_param)

BIS <- function(data) {
  n <- length(data$group)    # sample size to correct var()-function result (which uses n-1)
  srt <- sqrt( ((n-1)/n) * var(data$mean_rt_c) )     # sample standard deviation across all rts
  spc <- sqrt( ((n-1)/n) * var(data$pc) )            # sample standard deviation across all rts
  mrt <- mean(data$mean_rt_c)                        # mean across all rts
  mpc <- mean(data$pc)                               # mean across all pcs
  zrt <- (data$mean_rt_c-mrt)/srt                    # standardized rts
  zpc <- (data$pc-mpc)/spc                           # z-standardized pcs
  data$bis <- zpc - zrt                              # Balanced Integration Score
  
  return(data)                                       # return data.frame with added variable 'bis'
}

#######################################################################################################
#######################################################################################################
# Example

#...either create an example dataframe (here called data)
mean_rt_c <- c(356.8,325.1,370.6,362.8,348.5,640.3,634.2,650.7,584.5,610.0)
pc <- c(0.71,0.73,0.67,0.73,0.72,0.97,0.99,0.99,0.97,0.97)
group <- c(1,1,1,1,1,2,2,2,2,2)
data <- data.frame(group,mean_rt_c,pc)

# call BIS with dataframe data as argument
data <- BIS(data)

glimpse(data2)
data2 = devcog_param %>%
  rename(mean_rt_c = rt_Inc, pc = resp_Con) %>%
  mutate(group = 1) %>% filter(!is.na(mean_rt_c)) %>%
  select(group, mean_rt_c, pc) 

n <- length(data2$group)    # sample size to correct var()-function result (which uses n-1)
srt <- sqrt( ((n-1)/n) * var(data2$mean_rt_c))     # sample standard deviation across all rts
spc <- sqrt( ((n-1)/n) * var(data2$pc) )            # sample standard deviation across all rts
mrt <- mean(data2$mean_rt_c)                        # mean across all rts
mpc <- mean(data2$pc)                               # mean across all pcs
zrt <- (data2$mean_rt_c-mrt)/srt                    # standardized rts
zpc <- (data2$pc-mpc)/spc                           # z-standardized pcs
data2$bis <- zpc - zrt   

BIS(data2)

modP = lm(Moral ~ edad_meses, 
          devcog_param)
jtools::summ(modP, digits = 3)

modP1 = lm(Moral ~ efficiency_incongruent +  bw_ctr + edad_meses, 
           devcog_param)
jtools::summ(modP1, digits = 3)

cor.test(~ backward + Textual, devcog_param, method = 'spearman')
cor.test(~ backward + Moral, devcog_param, method = 'spearman')

cor.test(~ efficiency_incongruent + Textual, devcog_param, method = 'spearman')
cor.test(~ efficiency_incongruent + Moral, devcog_param, method = 'spearman')

anova(modP, modP1)

supp_results2 %>%
  mutate(Textual = if_else(Textual < 0, 0, Textual),
         Moral = if_else(Moral < 0, 0, Moral)) %>%
  ggplot(aes(x = reorder(ResponseId, Moral-Textual))) + 
  geom_col(aes(y = Textual, color = group, alpha = Textual_p < .05), fill = 'grey',
           linewidth = .2, width = 1) +
  geom_col(aes(y = -Moral, color = group, alpha = Moral_p < .05),  fill = 'grey',
           linewidth = .2, width = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_alpha_manual(values = c(.2, .7)) +
  facet_grid(. ~ reorder(group, -Moral), space = 'free_y', scale = 'free_x') + 
  theme_classic()

d4 = supp_results2 %>%
  mutate(Textual = if_else(Textual < 0, 0, Textual),
         Moral = if_else(Moral < 0, 0, Moral), 
         age_order = case_when(group == 'peques' ~ 1,
                               group == 'adultos1' ~ 2,
                               group == 'adultos2' ~ 3),
         group_label = case_when(group == 'peques' ~ 'Children\n5-8',
                                 group == 'adultos1' ~ 'Young Adults\n18-25',
                                 group == 'adultos2' ~ 'Older Adults\n45-65')) %>%
  ggplot() + 
  geom_col(aes(x = reorder(ResponseId, Moral-Textual), y = Textual, alpha = Textual_p < .05), color = '#7570B3', fill = 'lightgrey',
           linewidth = .3, width = 1) +
  geom_col(aes(x = reorder(ResponseId, Moral-Textual), y = -Moral, alpha = Moral_p < .05), color = '#1B9E77',  fill = 'lightgrey',
           linewidth = .3, width = 1) +
  geom_hline(yintercept = 0, linetype = 1, linewidth = .3) +
  geom_rect(data = supp_group, aes(ymin = lower_ci, 
                                   ymax = upper_ci, fill = predictor), 
            xmin = -Inf, xmax = Inf, alpha = .2)+
  geom_hline(data = supp_group, aes(yintercept = Aggregate, color = predictor), linetype = 2)+
  scale_y_continuous(name = 'Correlation coefficients', breaks = seq(-1, 1, .25), 
                     labels = c("1", ".75", ".50", ".25", "0", ".25", ".50", ".75", "1"),
                     limits = c(-1, 1), expand = c(0, 0),
                     sec.axis = dup_axis(name = NULL, 
                                         breaks = c(-.5, .5), 
                                         labels = c('Moral\nreasoning', 'Textualist\nreasoning'))) +
  facet_grid(. ~ reorder(group_label, age_order), space = 'free_y', scale = 'free_x') + 
  theme_classic() + 
  scale_fill_manual(values = c("#1B9E77", "#7570B3")) +
  scale_color_manual(values = c("#1B9E77", "#7570B3")) +
  scale_alpha_manual(values = c(.1, .7)) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 10, angle = 0),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(),
        strip.background = element_blank(), 
        legend.position = 'none')

ggsave("dev_rules2.jpg", width = 10, height = 15, units = 'cm')

supp_results %>%
  mutate(Textual = if_else(Textual < 0, 0, Textual),
         Moral = if_else(Moral < 0, 0, Moral), 
         age_order = case_when(group == 'peques' ~ 1,
                               group == 'adultos1' ~ 2,
                               group == 'adultos2' ~ 3)) %>%
  ggplot(aes(x = reorder(ResponseId, Moral-Textual))) + 
  geom_col(aes(y = Textual, color = group), alpha = .7, fill = 'grey',
           linewidth = .2, width = 1) +
  geom_col(aes(y = -Moral, color = group), alpha = .3, fill = 'white',
           linewidth = .2, width = 1) +
  geom_hline(yintercept = 0, linetype = 2) + coord_flip() +
  facet_grid(reorder(group, -age_order) ~ ., space = 'free_x', scale = 'free_y') + 
  theme_classic()


devy_means = devy %>%
  select(ResponseId, age) %>%
  full_join(., supp_results, by = 'ResponseId') %>%
  gather(Intercept:Textual, key = 'parameter', value = 'value', na.rm = TRUE) %>%
  group_by(group, parameter) %>%
  summarise(value = mean(value, na.rm = TRUE), 
            age = mean(age, na.rm = TRUE))

mod_coefs = devy %>%
  select(ResponseId, age) %>%
  full_join(., supp_results, by = 'ResponseId') %>%
  gather(Intercept:Textual, key = 'parameter', value = 'value', na.rm = TRUE) %>%
  lm(value ~ parameter * group, data = .) %>%
  emmeans(., ~ parameter | group) %>% data.frame() %>%
  mutate(age = case_when(group == 'peques' ~ 6.6,
                         group == 'adultos1' ~ 52,
                         group == 'adultos2' ~ 22.9))

devy %>%
  select(ResponseId, age) %>%
  full_join(., supp_results, by = 'ResponseId') %>%
  gather(Intercept:Textual, key = 'parameter', value = 'value', na.rm = TRUE) %>%
  ggplot(aes(x = age, y = value, color = parameter)) + 
  geom_jitter(alpha = .3) + geom_line(data = mod_coefs, aes(y = emmean), linetype = 2) +
  geom_ribbon(data = mod_coefs, 
              aes(ymin = lower.CL, ymax = upper.CL, y = emmean, fill = parameter), 
              color = 'transparent', alpha = .2) +
  geom_point(data = mod_coefs, aes(y = emmean), size = 3, stroke = 2, shape = 21, fill = 'white') 

