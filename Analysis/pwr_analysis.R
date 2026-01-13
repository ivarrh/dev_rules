library(lmerTest)

boot_data = dev_long %>%
  select(group, subj_id, meses_ctr, scen, violation, text, purpose) %>%
  pivot_wider(names_from = c('scen', 'text', 'purpose'), values_from = 'violation')

supp_ratings = dev_long %>% select(scen, text, purpose, moral, literal) %>%
  mutate(text = as.character(text), 
         purpose = as.character(purpose)) %>%
  distinct()

n_sims = 1000

pwr_results1 <- data.frame(
  sim = character(),
  model = character(),
  group = character(),
  text = character(),
  purpose = character(),
  groupXtext = character(),
  groupXpurpose = character()
)

pwr_results2 <- data.frame(
  sim = character(),
  model = character(),
  group = character(),
  moral = character(),
  literal = character(),
  groupXmoral = character(),
  groupXliteral = character()
)

for (i in seq_along(1:n_sims)) {
  temp_dt = sample_n(boot_data, size = 151, replace = TRUE) %>%
    pivot_longer(
      cols = -c(group, subj_id, meses_ctr),          # keep these as id columns
      names_to = c('scen', 'text', 'purpose'),
      names_sep = "_" ,                    # or whatever separator pivot_wider used
      values_to = 'violation'
    )
  
  mod1 = glmer(violation ~ group * (text + purpose) + (1 | subj_id) + (1 | scen),
               data = temp_dt, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "nlminb")))
  
  temp_results = car::Anova(mod1)$`Pr(>Chisq)`
  new_row = c(i, 'Model1a_GroupTP', temp_results) 
  pwr_results1 = rbind(pwr_results1, new_row)
  
  temp_dt = left_join(temp_dt, supp_ratings, by = c('scen', 'text', 'purpose'))
  
  mod2 = glmer(violation ~ group * (moral + literal) + (1 | subj_id) + (1 | scen),
                            data = temp_dt, family = 'binomial', 
                            control = glmerControl(optimizer = "optimx", 
                                                   optCtrl = list(method = "nlminb")))
  
  temp_results = car::Anova(mod2)$`Pr(>Chisq)`
  new_row = c(i, 'Model1b_GroupLM', temp_results) 
  pwr_results2 = rbind(pwr_results2, new_row)
  
  temp_dt = boot_data %>% filter(group == 'Children') %>%
    sample_n(size = 49, replace = TRUE) %>%
    pivot_longer(
      cols = -c(group, subj_id, meses_ctr),          # keep these as id columns
      names_to = c('scen', 'text', 'purpose'),
      names_sep = "_" ,                    # or whatever separator pivot_wider used
      values_to = 'violation'
    )
  
  mod1 = try(glmer(violation ~ meses_ctr * (text + purpose) + (1 | subj_id) + (1 | scen),
               data = temp_dt, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa"))))
  if(typeof(mod1) == 'S4') {
  temp_results = car::Anova(mod1)$`Pr(>Chisq)`
  new_row = c(i, 'Model2a_DevTP', temp_results) 
  pwr_results1 = rbind(pwr_results1, new_row)
  }
  
  temp_dt = left_join(temp_dt, supp_ratings, by = c('scen', 'text', 'purpose'))
  
  mod2 = try(glmer(violation ~ meses_ctr * (moral + literal) + (1 | subj_id) + (1 | scen),
               data = temp_dt, family = 'binomial', 
               control = glmerControl(optimizer = "optimx", 
                                      optCtrl = list(method = "bobyqa"))))
  
  if(typeof(mod2) == 'S4') {
  temp_results = car::Anova(mod2)$`Pr(>Chisq)`
  new_row = c(i, 'Model2b_DevLM', temp_results) 
  pwr_results2 = rbind(pwr_results2, new_row)
  }
  
  message('Finished: iter. ', i)
  if(i == n_sims) 
  {
    message('~ COMPLETE ~')
  }
}

names(pwr_results1) = c('sim', 'model', 'group', 'text', 'purpose', 'groupXtext', 'groupXpurpose')
names(pwr_results2) = c('sim', 'model', 'group', 'moral', 'literal', 'groupXmoral', 'groupXliteral')

power_table = bind_rows(pwr_results1, pwr_results2) %>% 
  gather(group:groupXliteral, key = 'term', value = 'p_value', na.rm = TRUE) %>%
  mutate(p_value = as.numeric(p_value)) %>%
  group_by(term, model) %>%
  summarise(power = mean(p_value < .05, na.rm = TRUE), sims = n())

power_table

write.csv(power_table, 'Analysis/SuppTable2.csv', row.names= FALSE)
