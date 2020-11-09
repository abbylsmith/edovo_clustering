library(tidyverse)
library(skimr)
library(RecordLinkage)


#-- read in the data, basically do the SQL join I should have done before...
edovo_user_data <- read_csv('inmates_Edovo.csv')

edovo_id <- read_csv('user_IDs.csv')

skim(edovo_user_data)
nrow(edovo_user_data)

#--- overall duplicate rate
edovo_user_data %>%
  left_join(edovo_id, by='id') %>%
  select(first_name, last_name) %>%
  unique() %>% 
  count() %>% summarise(percent_dup = 1 - n/288777) #27.5 % clear duplicate rate

#--- for each **clear** duplicate user, what IDs are they using?
edovo_user_data %>% 
  group_by(first_name, last_name) %>% 
  filter(n()>1)%>% #where there are duplicates
  summarize(n=n()) %>% 
  drop_na() %>%
  arrange(desc(n)) %>% #John Doe... has 309 duplicates?
  left_join(edovo_user_data, by=c("first_name", "last_name")) %>%
  group_by(first_name, last_name) %>%
  summarise(id = paste(id, collapse = ","))

#-- among clear duplicates, how do the points stack up across the accounts?
edovo_user_data %>% 
  group_by(first_name, last_name) %>% 
  filter(n()>1)%>% #where there are duplicates
  summarize(n=n()) %>% 
  drop_na() %>%
  arrange(desc(n)) %>% #John Doe... has 309 duplicates?
  left_join(edovo_user_data, by=c("first_name", "last_name")) %>%
  group_by(first_name, last_name) %>%
  summarise(sum_points= sum(total_points))  %>% arrange(desc(sum_points))  




#-- we can do better! Let's use the RecordLinkage package in R
#--- removing "unique IDs" (inmate_id and id)

for_RL <- edovo_user_data %>% select(-id, 
                                     -inmate_id, 
                                     -hashed_password, 
                                     -points,
                                     -total_points, 
                                     -total_spent_points,
                                     -credit_balance, 
                                     -balance, 
                                     -created_at,
                                     -updated_at,
                                     -desk_id,
                                     -phone_number,
                                     -zendesk_id) %>%
  mutate(release_date = as.Date(release_date))

first_pass_RL <- RLBigDataDedup(for_RL, strcmp=c(1:2, 12), blockfld = c(3,11)) #block on BIRTH DATE, language
saveRDS(first_pass_RL,'first_pass_RL.RDS')


#--- look at all pair

RL_pairs <- epiWeights(first_pass_RL)
result <- epiClassify(RL_pairs, 0.65)

test <- getPairs(result, filter.link  = "link")

#---looking at FastLink, good for large datasets?

library(fastLink)

matches_FastLink <- fastLink(
  dfA = for_RL, dfB = for_RL, 
  varnames = colnames(for_RL)[-3] ,
  stringdist.match = c("first_name", "last_name",  "security_question_1", "security_answer_1", "security_question_2",
                       "security_answer_2", "email"),
  partial.match = c("first_name", "last_name", "email")
)
#--- looking across facilities
facility_ID_orig <- read_csv('facility_IDs.csv')



facilities_to_include <- c("Cleveland County","Kane County", 
                           "ME DOC Maine Correctional Center",
                           "ME DOC Maine State Prison",
                           "ME DOC Maine Mountain View",
                           "Mendocino County",
                           "Moore County",
                           "Steuben County",
                           "Yakima County",
                           "Yolo County")

facilities_ID_orig <- facilities_ID_orig %>%
  filter(str_detect(name, paste(facilities_to_include, collapse = "|")))

facility_ID <- read_csv('facility_IDs.csv') %>%
  select(id, state,name, goals_enabled)

edovo_user_and_facility <- edovo_user_data %>%
  left_join(facility_ID, by=c( 'facility_id' = 'id')) 

#--- duplicates per facility
edovo_user_data %>% 
  group_by(first_name, last_name) %>% 
  filter(n()>1)%>% #where there are duplicates 
  group_by(facility_id) %>%
  summarise(dup_per_facility = n())


#--- how many of the duplicate inmate accounts are from the same vs. different facility?
edovo_user_and_facility %>% 
  group_by(first_name, last_name) %>% 
  mutate(which_facil =paste(facility_id, collapse= ",")) %>%
  summarise(num_unique = length(strsplit(which_facil, ","))) %>% 
  arrange(desc(num_unique))

#--- clear duplicate accounts
edovo_user_data %>% 
  group_by(first_name, last_name) %>% 
  filter(n()>1)%>% #where there are duplicates
  summarize(n=n()) %>% 
  drop_na() %>%
  arrange(desc(n)) %>% #John Doe... has 309 duplicates?
  left_join(edovo_user_data, by=c("first_name", "last_name")) %>%
  group_by(first_name, last_name) %>%
  summarise(facility_id = paste(facility_id, collapse = ","))



# Now, let's look at duplicates among sponsors! --------------------------------------------




  