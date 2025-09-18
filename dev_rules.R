library(tidyverse)
library(readr)
library(emmeans)
library(readxl)
library(lmerTest)
library(optimx)
library(logistf)

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

emtrends(mod1, pairwise ~ group, var = 'text')$contrasts %>%
  data.frame() %>%
  mutate(OR = exp(estimate))

emtrends(mod1, pairwise ~ group, var = 'purpose')$contrasts %>%
  data.frame() %>%
  mutate(OR = exp(estimate))

filename3 = "Data/supp_data.csv"

headers = read_csv(filename3, col_names = FALSE, n_max = 1)
headers
# Raw data (deleting 3 rows)
supp_rules = read_csv(filename3, skip = 3, col_names = FALSE)
# Naming raw date (with "headers")
colnames(supp_rules) = headers
glimpse(supp_rules)
colnames(supp_rules)[19:66] = c('c1t', 'c1m', 'n1t', 'n1m', 'u1t', 'u1m',  'o1t', 'o1m',
                                'c2t', 'c2m', 'n2t','n2m', 'o2t', 'o2m', 'u2t', 'u2m',
                                'n3t', 'n3m', 'c3t', 'c3m', 'o3t', 'o3m', 'u3t', 'u3m',
                                'c4t', 'c4m', 'n4t', 'n4m', 'u4t', 'u4m', 'o4t', 'o4m',
                                'c5t', 'c5m', 'n5t', 'n5m', 'o5t', 'o5m', 'u5t', 'u5m',
                                'n6t', 'n6m', 'c6t', 'c6m', 'o6t', 'o6m', 'u6t', 'u6m')

supp_rules %>%
  filter(Progress == 100) %>%
  summarise(mean(gender == 2, na.rm = TRUE), 
            n())

supp_rules_long = supp_rules %>%
  select(ResponseId, c1t:u6m) %>%
  gather(-ResponseId, key = 'key', value = 'value') %>%
  mutate(predictor = str_sub(key, -1, -1),
         condition = str_sub(key, 1, 1), 
         scenario = str_sub(key, 2, 2)) %>%
  select(-key) %>%
  pivot_wider(names_from = 'predictor', values_from = 'value') %>% 
  rename(literal = t, moral = m) %>%
  mutate(literal = 2 - literal, 
         moral = (5 - moral)/4,
         text = condition %in% c('c', 'o'),
         purpose = condition %in% c('c', 'u'))

mod_letter = glmer(literal ~ text + purpose + (1 | scenario), supp_rules_long, 
                   family = 'binomial', control = glmerControl(optimizer = "optimx", 
                                                             optCtrl = list(method = "bobyqa")))
car::Anova(mod_letter)
emmeans(mod_letter, pairwise ~ text, type = 'response')
emmeans(mod_letter, pairwise ~ purpose, type = 'response')
emmeans(mod_letter, ~ text * purpose, type = 'response')
jtools::summ(mod_letter, digits = 3, exp = TRUE)

mod_moral = lmer(moral ~  text + purpose + (1 | scenario) + (1 | ResponseId), supp_rules_long)
car::Anova(mod_moral)
emmeans(mod_moral, pairwise ~ text, type = 'response')
emmeans(mod_moral, pairwise ~ purpose, type = 'response')
jtools::summ(mod_moral, digits = 3)


supp_means = supp_rules %>%
  select(c1t:u6m) %>%
  gather() %>%
  mutate(predictor = str_sub(key, -1, -1),
         key = str_sub(key, 1, 2)) %>%
  group_by(key, predictor) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'predictor', values_from = 'mean_value') %>% 
  rename(literal = t, moral = m) %>%
  mutate(moral = (5 - mean(moral))/4, 
         literal = 2 - mean(literal))

supp_means %>%
  group_by(str_sub(key, 1, 1)) %>%
  summarise(mean(moral), 
            mean(literal))

supp_children <- read_excel("Data/data_reglas.xlsx") %>%
  filter(!is.na(CODIGO))

names(supp_children) = c('child_id', 'codigo2', 'grupo', 'sex',
                         'edad', 'edad_meses', 'date', 'birth_date',
                         'stroop_score', 'tom_1', 'tom_2', 'tom_3',
                         'tom_4', 'tom_5', 'tom_6', 'tom_7', 'tom_8',
                         'tom_9', 'tom_total', 'comments_tom', 'forward',
                         'fwd_score', 'span_1', 'backward', 'bwd_score',
                         'span_2', 'comments')

dev_long = left_join(dev_long, supp_children, by = 'child_id') %>%
  mutate(stroop_ctr = stroop_score - 22,
         meses_ctr = edad_meses - 85, # median age in months = 85
         tom_ctr = as.numeric(tom_total) - 7,
         fw_ctr = as.numeric(forward) - 6,
         bw_ctr = as.numeric(backward) - 6)

dev_long = full_join(dev_long, supp_means, by = 'key')

model_supp = glmer(violation ~ group * (moral + literal) + (1 | subj_id) + (1 | scen),
                data = dev_long, family = 'binomial', 
                control = glmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "nlminb")))
car::Anova(model_supp)

emtrends(model_supp, pairwise ~ group, var = 'moral')
emtrends(model_supp, pairwise ~ group, var = 'literal')$contrasts %>%
  data.frame() %>%
  mutate(OR = exp(estimate))

model_months = glmer(violation ~ meses_ctr * (text * purpose) + (1 | child_id) + (1 | scen),
                   data = subset(dev_long, !is.na(meses_ctr)), family = 'binomial', 
                   control = glmerControl(optimizer = "optimx", 
                                          optCtrl = list(method = "bobyqa")))
car::Anova(model_months)

emtrends(model_months, pairwise ~ text, var = 'meses_ctr')$contrasts %>%
  data.frame() %>%
  mutate(estimate = -estimate,
         z.ratio = -z.ratio,
        OR = exp(estimate))

emtrends(model_months, pairwise ~ purpose, var = 'meses_ctr')$contrasts %>%
  data.frame() %>%
  mutate(estimate = -estimate,
         z.ratio = -z.ratio,
         OR = exp(estimate))

model_supp_months = glmer(violation ~ meses_ctr * (moral + literal) + (1 | subj_id) + (1 | scen),
                   data = subset(dev_long, !is.na(meses_ctr)), family = 'binomial', 
                   control = glmerControl(optimizer = "optimx", 
                                          optCtrl = list(method = "nlminb")))
car::Anova(model_supp_months)

emtrends(model_supp_months, pairwise ~ moral, var = 'meses_ctr',
         at = list(moral = c(1,0)))$contrasts %>%
  data.frame() %>%
  mutate(OR = exp(estimate))
emtrends(model_supp_months, pairwise ~ literal, var = 'meses_ctr',
         at = list(literal = c(1,0)))$contrasts %>%
  data.frame() %>%
  mutate(OR = exp(estimate))

stroop_data = read_excel("Data/data_reglas.xlsx", sheet = "STROOP (TD)") %>%
  mutate(congruent = if_else(ITEM <= 12, 'Con', 'Inc')) %>%
  rename(child_id = CODIGO) %>%
  group_by(child_id, congruent) %>%
  summarise(rt = median(`RT ONSET (SECONDS,MS)`, na.rm = TRUE), 
            resp = mean(ERROR, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'congruent', values_from = c('rt', 'resp'))

wilcox.test(stroop_data$rt_Con, stroop_data$rt_Inc)
wilcox.test(stroop_data$resp_Con, stroop_data$resp_Inc)

dev_long = full_join(dev_long, stroop_data, by = 'child_id')

dev_long = dev_long %>% 
  mutate(efficiency_congruent = scale(rt_Con/resp_Con)[,1],
         efficiency_incongruent = scale(rt_Inc/resp_Inc)[,1], 
         meses_z = scale(edad_meses)[,1])

children_cog_measures = dev_long %>%
  select(child_id, edad_meses, tom_total, forward, backward, 
         rt_Con, resp_Con, rt_Inc, resp_Inc) %>%
  distinct() %>%
  filter(!is.na(child_id)) %>%
  mutate(across(edad_meses:resp_Inc, as.numeric), 
         efficiency_congruent = rt_Con/resp_Con,
         efficiency_incongruent = rt_Inc/resp_Inc) %>%
  select(-rt_Con, -resp_Con, -rt_Inc, -resp_Inc)

firth_params_all <- data.frame(
  group = character(),
  subj_id = character(),
  intercept = character(),
  text = character(),
  purpose = character()
)

for (i in levels(as.factor(dev_long$subj_id))) {
  temp = subset(dev_long, subj_id == i)
  
  mod_temp = logistf(violation ~ text + purpose,
                     data = temp)
  
  temp_results = summary(mod_temp)$coefficients
  new_row = c(as.character(temp$group[1]), i, temp_results) 
  names(new_row) = names(firth_params_all)
  
  firth_params_all = rbind(firth_params_all, new_row)
}

names(firth_params_all) = c('group', 'subj_id', 'intercept', 'text', 'purpose')

firth_params_all = firth_params_all %>%
  mutate(intercept = as.double(intercept),
         text = as.double(text),
         purpose = as.double(purpose))

firth_params_means = firth_params_all %>%
  rename(Text = text, Purpose = purpose) %>%
  gather(Text:Purpose, key = 'parameter', value = 'coef') %>%
  group_by(group, parameter) %>%
  summarise(coef = mean(coef, na.rm = TRUE))

firth_params_all %>%
  rename(Text = text, Purpose = purpose) %>%
  gather(Text:Purpose, key = 'parameter', value = 'coef') %>%
  ggplot(aes(x = reorder(group, coef),  y = coef, fill = parameter)) + 
  facet_grid(. ~ reorder(parameter, coef)) + coord_flip() + 
  geom_hline(yintercept = 0, linetype = 2, color = 'darkgrey') +
  geom_violin(color = 'darkgrey', alpha = .4, trim = FALSE, draw_quantiles = c(.25, .75)) + 
  geom_point(data = firth_params_means, aes(color = parameter),
             shape = 21, fill = 'white', size = 3, stroke = 1) +
  theme_classic() + 
  scale_fill_manual(values = c("#1B9E77", "#7570B3"), name = "Parameter") + 
  scale_color_manual(values = c("#1B9E77", "#7570B3")) + 
  scale_y_continuous(name = "Parameter", breaks = seq(-3, 6, 3), 
                     expand = c(0, 0), limits = c(-3, 7)) +
  scale_x_discrete(name = NULL) +
  theme(legend.position = 'none',
        text = element_text(size = 14, family = 'Helvetica Neue', 
                            color = 'black'),
        strip.background = element_blank(),
        panel.grid.major.x = element_line(), 
        axis.ticks = element_blank())

ggsave("Figures/SuppFigure1.jpg", width = 13, height = 9, units = 'cm')

firth_params_all %>%
  summarise(
    median(intercept), 
    median(text), 
    median(purpose)
  )

firth_params <- data.frame(
  child_id = character(),
  intercept = character(),
  text = character(),
  purpose = character()
)

for (i in levels(as.factor(dev_long$child_id))) {
  temp = subset(dev_long, child_id == i)
  
  mod_temp = logistf(violation ~ text + purpose,
               data = temp)
  
  temp_results = summary(mod_temp)$coefficients
  new_row = c(i, temp_results) 
  names(new_row) = names(firth_params)
  
  firth_params = rbind(firth_params, new_row)
}

names(firth_params) = c('child_id', 'intercept', 'text', 'purpose')

firth_params = firth_params %>%
  mutate(intercept = as.double(intercept),
         text = as.double(text),
         purpose = as.double(purpose))

firth_params %>%
  gather(-child_id, key = 'parameter', value = 'coef') %>%
  ggplot(aes(x = parameter,  y = coef, fill = parameter)) + 
  geom_violin(color = 'darkgrey', alpha = .4, trim = FALSE, draw_quantiles = c(.25, .75))

children_all_measures = full_join(children_cog_measures, firth_params)

cor.test(~ efficiency_congruent + edad_meses, children_cog_measures, method = 'spearman')
cor.test(~ efficiency_incongruent + edad_meses, children_cog_measures, method = 'spearman')
cor.test(~ tom_total + edad_meses, children_cog_measures, method = 'spearman')
cor.test(~ forward + edad_meses, children_cog_measures, method = 'spearman')
cor.test(~ backward + edad_meses, children_cog_measures, method = 'spearman')

apaTables::apa.cor.table(children_all_measures, filename = 'Analysis/Table1.doc')

mod3 = glmer(violation ~ (tom_ctr) * (text * purpose) + (1 | subj_id) + (1 | scen),
               data = dev_long, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa")))

mod3age = glmer(violation ~ (meses_ctr + tom_ctr) * (text + purpose) + (1 | subj_id) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

tom_results = bind_rows(emtrends(mod3, pairwise ~ text, var = 'tom_ctr')$contrasts %>%
            data.frame(), 
      emtrends(mod3age, pairwise ~ text, var = 'tom_ctr')$contrasts %>%
        data.frame(),
      emtrends(mod3, pairwise ~ purpose, var = 'tom_ctr')$contrasts %>%
        data.frame(),
      emtrends(mod3age, pairwise ~ purpose, var = 'tom_ctr')$contrasts %>%
        data.frame()) %>%
  mutate(z.ratio = -z.ratio, 
         OR = exp(-estimate), 
         moderator = 'ToM')

mod4 = glmer(violation ~ (fw_ctr) * (text * purpose) + (1 | subj_id) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

mod4age = glmer(violation ~ (meses_ctr + fw_ctr) * (text + purpose) + (1 | subj_id) + (1 | scen),
                data = dev_long, family = 'binomial', 
                control = glmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "bobyqa")))

fw_results = bind_rows(emtrends(mod4, pairwise ~ text, var = 'fw_ctr')$contrasts %>%
                          data.frame(), 
                        emtrends(mod4age, pairwise ~ text, var = 'fw_ctr')$contrasts %>%
                          data.frame(),
                        emtrends(mod4, pairwise ~ purpose, var = 'fw_ctr')$contrasts %>%
                          data.frame(),
                        emtrends(mod4age, pairwise ~ purpose, var = 'fw_ctr')$contrasts %>%
                          data.frame()) %>%
  mutate(z.ratio = -z.ratio, 
         OR = exp(-estimate), 
         moderator = 'Fwd Digit Span')

mod5 = glmer(violation ~ (bw_ctr) * (text * purpose) + (1 | subj_id) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

mod5age = glmer(violation ~ (meses_ctr + bw_ctr) * (text + purpose) + (1 | subj_id) + (1 | scen),
                data = dev_long, family = 'binomial', 
                control = glmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "bobyqa")))

bw_results = bind_rows(emtrends(mod5, pairwise ~ text, var = 'bw_ctr')$contrasts %>%
                         data.frame(), 
                       emtrends(mod5age, pairwise ~ text, var = 'bw_ctr')$contrasts %>%
                         data.frame(),
                       emtrends(mod5, pairwise ~ purpose, var = 'bw_ctr')$contrasts %>%
                         data.frame(),
                       emtrends(mod5age, pairwise ~ purpose, var = 'bw_ctr')$contrasts %>%
                         data.frame()) %>%
  mutate(z.ratio = -z.ratio, 
         OR = exp(-estimate), 
         moderator = 'Bwd Digit Span')


mod6 = glmer(violation ~ (efficiency_congruent) * (text * purpose) + (1 | subj_id) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

mod6age = glmer(violation ~ (meses_ctr + efficiency_congruent) * (text + purpose) + (1 | subj_id) + (1 | scen),
                data = dev_long, family = 'binomial', 
                control = glmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "bobyqa")))

con_results = bind_rows(emtrends(mod6, pairwise ~ text, var = 'efficiency_congruent')$contrasts %>%
                         data.frame(), 
                       emtrends(mod6age, pairwise ~ text, var = 'efficiency_congruent')$contrasts %>%
                         data.frame(),
                       emtrends(mod6, pairwise ~ purpose, var = 'efficiency_congruent')$contrasts %>%
                         data.frame(),
                       emtrends(mod6age, pairwise ~ purpose, var = 'efficiency_congruent')$contrasts %>%
                         data.frame()) %>%
  mutate(OR = exp(estimate), 
         moderator = 'Congruent Stroop')


mod7 = glmer(violation ~ (efficiency_incongruent) * (text * purpose) + (1 | subj_id) + (1 | scen),
             data = dev_long, family = 'binomial', 
             control = glmerControl(optimizer = "optimx", 
                                    optCtrl = list(method = "bobyqa")))

mod7age = glmer(violation ~ (meses_ctr + efficiency_incongruent) * (text + purpose) + (1 | subj_id) + (1 | scen),
                data = dev_long, family = 'binomial', 
                control = glmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "bobyqa")))

inc_results = bind_rows(emtrends(mod7, pairwise ~ text, var = 'efficiency_incongruent')$contrasts %>%
                          data.frame(), 
                        emtrends(mod7age, pairwise ~ text, var = 'efficiency_incongruent')$contrasts %>%
                          data.frame(),
                        emtrends(mod7, pairwise ~ purpose, var = 'efficiency_incongruent')$contrasts %>%
                          data.frame(),
                        emtrends(mod7age, pairwise ~ purpose, var = 'efficiency_incongruent')$contrasts %>%
                          data.frame()) %>%
  mutate(OR = exp(estimate), 
         moderator = 'Incongruent Stroop')

table1 = bind_rows(tom_results, fw_results, bw_results, con_results, inc_results) %>%
  mutate(effect = str_sub(contrast, 1, 1), 
         age_control = row_number()/2 == round(row_number()/2)) %>%
  select(effect, moderator, age_control, OR, z.ratio, p.value) %>%
  mutate(OR = round(OR, 2), z.ratio = round(z.ratio, 2), p.value = round(p.value, 3)) %>%
  pivot_wider(values_from = c('OR', 'z.ratio', 'p.value'), 
              names_glue = "{age_control}.{.value}",
              names_from = 'age_control') %>%
  select(effect, moderator, order(colnames(.)))

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
        legend.text = element_text(size = 9, family = 'Helvetica Neue', 
                                   color = 'black')) +
  guides(linetype = FALSE, fill = FALSE, 
         color=guide_legend(nrow=2,byrow=FALSE))


kid_means = dev_long %>%
  group_by(edad, condition2, text, purpose) %>%
  summarise(violate = mean(violation, na.rm = TRUE),
            lower_bound = DescTools::BinomCI(sum(violation, na.rm = TRUE),
                                             sum(!is.na(violation), na.rm = TRUE))[2],
            upper_bound = DescTools::BinomCI(sum(violation, na.rm = TRUE),
                                             sum(!is.na(violation), na.rm = TRUE))[3]) %>%
  mutate(edad_meses = edad*12 + 6) %>% filter(!is.na(edad_meses))

devfig1b = dev_long %>%
  group_by(child_id, condition2, text, purpose) %>%
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
                  widths = c(1.4, 1), labels = c('B', 'C'), 
                  hjust = c(-0.6, 1.6), vjust = c(2.2, 2.2))

ggsave("Figures/Figure2_ggplot.jpg", width = 22, height = 12, units = 'cm')


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

for (i in levels(dev_long$group)) {
  temp = pcor_ci.test(subset(dev_long, group == i)$violation, 
                      subset(dev_long, group == i)$literal, 
                      subset(dev_long, group == i)$moral)
  
  new_row = c(group = i, predictor = 'Text', 
              as.numeric(temp$estimate), 
              as.numeric(temp$conf.int[1]), 
              as.numeric(temp$conf.int[2]))
  
  names(new_row) = names(supp_group)
  supp_group = bind_rows(supp_group, new_row)
  
  temp = pcor_ci.test(subset(dev_long, group == i)$violation, 
                      subset(dev_long, group == i)$moral, 
                      subset(dev_long, group == i)$literal)
  
  new_row = c(group = i, predictor = 'Moral', 
              as.numeric(temp$estimate), 
              as.numeric(temp$conf.int[1]), 
              as.numeric(temp$conf.int[2]))
  
  names(new_row) = names(supp_group)
  supp_group = bind_rows(supp_group, new_row)
  
}

supp_group = supp_group %>% 
  mutate(age_order = case_when(group == 'Children' ~ 1,
                               group == 'Adults (18-25)' ~ 2,
                               group == 'Adults (45-65)' ~ 3),
         group_label = case_when(group == 'Children' ~ 'Children\n5-8',
                                 group == 'Adults (18-25)' ~ 'Young Adults\n18-25',
                                 group == 'Adults (45-65)' ~ 'Older Adults\n45-65'), 
         Aggregate = if_else(predictor == 'Text', as.numeric(Aggregate), -as.numeric(Aggregate)), 
         lower_ci = if_else(predictor == 'Text', as.numeric(lower_ci), -as.numeric(lower_ci)),
         upper_ci = if_else(predictor == 'Text', as.numeric(upper_ci), -as.numeric(upper_ci)))


supp_results <- data.frame(
  group = character(),
  ResponseId = character(),
  Intercept = numeric(),
  Moral = numeric(),
  Textual = numeric(),
  stringsAsFactors = FALSE  # avoids auto-conversion of characters to factors
)

for (i in levels(as.factor(dev_long$subj_id))) {
  temp = subset(dev_long, subj_id == i)
  model_s = lm(violation ~ moral + literal, 
               temp)
  
  group = temp %>% select(group) %>% unique()
  
  new_row = data.frame(group, i, model_s$coefficients[1], model_s$coefficients[2], model_s$coefficients[3])
  colnames(new_row) = colnames(supp_results)
  supp_results = bind_rows(supp_results, new_row)
}


supp_results2 <- data.frame(
  group = character(),
  subj_id = character(),
  Textual = numeric(),
  Moral = numeric(),
  Textual_p = numeric(),
  Moral_p = numeric(),
  stringsAsFactors = FALSE  # avoids auto-conversion of characters to factors
)

for (i in levels(as.factor(dev_long$subj_id))) {
  temp= subset(dev_long, subj_id == i)
  
  m_coef = cor.test(~ moral + violation, temp)$estimate
  t_coef = cor.test(~ literal + violation, temp)$estimate
  m_pval = cor.test(~ moral + violation, temp)$p.value
  t_pval = cor.test(~ literal + violation, temp)$p.value
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
  select(child_id, subj_id, backward, tom_total, edad_meses,
         rt_Con:resp_Inc) %>%
  unique(.) %>%
  mutate(efficiency_congruent = rt_Con/resp_Con,
         backward = as.numeric(backward),
         efficiency_incongruent = rt_Inc/resp_Inc) %>%
  left_join(supp_results2, by = 'subj_id')

cor.test(~ as.numeric(tom_total) + Textual, devcog_param, method = 'spearman')
cor.test(~ as.numeric(tom_total) + Moral, devcog_param, method = 'spearman')

cor.test(~ efficiency_incongruent + Textual, devcog_param)
cor.test(~ efficiency_incongruent + Moral, devcog_param)


modT = lm(Textual ~ edad_meses, 
          devcog_param)
jtools::summ(modT, digits = 3)

modT1 = lm(Textual ~ efficiency_incongruent +  backward + edad_meses, 
           devcog_param)
jtools::summ(modT1, digits = 3)

anova(modT, modT1)

## previous color schme
#scale_fill_manual(values = c('#009E73', '#E69F00')) +
# scale_color_manual(values = c('#009E73', '#E69F00')) +

d4 = supp_results2 %>%
  mutate(age_order = case_when(group == 'Children' ~ 1,
                               group == 'Adults (18-25)' ~ 2,
                               group == 'Adults (45-65)' ~ 3),
         group_label = case_when(group == 'Children' ~ 'Children\n5-8',
                                 group == 'Adults (18-25)' ~ 'Young Adults\n18-25',
                                 group == 'Adults (45-65)' ~ 'Older Adults\n45-65'))  %>%
  ggplot() + 
  geom_col(aes(x = reorder(subj_id, Moral-Textual), y = Textual, alpha = Textual_p < .05), color = '#7570B3', fill = 'lightgrey',
           linewidth = .3, width = 1) +
  geom_col(aes(x = reorder(subj_id, Moral-Textual), y = -Moral, alpha = Moral_p < .05), color = '#1B9E77',  fill = 'lightgrey',
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
                                         labels = c('Moral\nappraisal', 'Literal\nappraisal'))) +
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


ggsave("Figures/Figure3.jpg", width = 24, height = 14, units = 'cm')



modP = lm(Moral ~ edad_meses, 
          devcog_param)
jtools::summ(modP, digits = 3)

modP1 = lm(Moral ~ efficiency_incongruent +  backward + edad_meses, 
           devcog_param)
jtools::summ(modP1, digits = 3)

cor.test(~ backward + Textual, devcog_param, method = 'spearman')
cor.test(~ backward + Moral, devcog_param, method = 'spearman')

cor.test(~ efficiency_incongruent + Textual, devcog_param, method = 'spearman')
cor.test(~ efficiency_incongruent + Moral, devcog_param, method = 'spearman')
