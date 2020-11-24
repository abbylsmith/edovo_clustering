##############################################################
# this takes in the json navmap with information about total_pages
# uses it in clustering_analysis.R
##############################################################

library(data.tree)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(xlsx)

#source(clustering_analysis.R)

default_map<- fromJSON('new_default_map.json', simplifyDataFrame = FALSE)


default_map<- as.Node(default_map)
#print(default_map, "id", "title")


parsed_json <- default_map %>%
  ToDataFrameTable(json_id='id', 'title', parent_ID = function(x) x$parentId, 
                   tag_ID = 'tags', course_type='type',
                   total_pages = 'total') %>% 
  distinct()


#---- function that handles NAs while joining
coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}


#--- course info with content tags
course_content_test <- read_csv('clustering_datasets/course_tags_with_IDS.csv')

#--- same as in clustering_analysis.R

course_tags <- course_content_test %>% 
  mutate(split_content_tags = str_split(content_tags, ",")) %>% 
  unnest(split_content_tags) %>%
  group_by(course_id) %>%
  separate(split_content_tags, c("name","id_number"), sep =  "-") %>% 
  mutate(name =toupper(name)) %>%
  #---- filter out non-relevant tags
  filter(name != "COURSE" & name != "ACADEMIC" & 
           name != "EARN" & name != "SPEND" & name !="INFORMATION" &
           name != "RESOURCES" & name != "NEW_CONTENT" & 
          !endsWith(name, 'ONLY')) %>% #-- _only are the facility specific tags we don't want
  mutate(N = paste0('tag', row_number())) %>%
  pivot_wider(names_from='N', values_from= c("name", "id_number")) %>%
  mutate_at(vars(starts_with("name")), list(~na_if(.,""))) %>%
  mutate_at(vars(starts_with('name')), function(x) as.factor(x)) %>%
  mutate_at(vars(starts_with('id_number')), function(x) as.factor(x))

#--- XLSX with tag information from Mike
content_tags_test <- read.xlsx('clustering_datasets/content_tags.xlsx', sheetIndex =1) %>%
  as_tibble() %>%
  mutate(name =toupper(name)) %>%
  #---- filter out non-relevant tags
  filter(name != "COURSE" & name != "ACADEMIC" & 
           name != "EARN" & name != "SPEND" & name !="INFORMATION" &
           name != "RESOURCES" & name != "NEW_CONTENT" & 
           !endsWith(name, 'ONLY')) %>% #-- _only are the facility specific tags we don't want
  mutate(tag_ID = id) %>% 
  select(-id)

course_tags_with_json <- course_tags %>% select(-starts_with('name')) %>%
  pivot_longer(cols=starts_with('id'), names_to='how_many_id', values_to ='tag_ID')%>%
  full_join(parsed_json , by= "tag_ID") %>% 
  mutate(tag_ID = as.numeric(tag_ID),
         name = title.y) %>% 
  select(-title.y)


for_clustering <-coalesce_join(course_tags_with_json,
                               content_tags_test , by= c('tag_ID')) %>%
  drop_na(name)



write.csv(for_clustering, 'clustering_datasets/new_joined_tags.csv')
