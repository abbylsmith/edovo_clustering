
library(tidyverse)
library(janitor)


library(cluster)
library(Rtsne) # shows our clustering in a 2D space

#-- more clustering related packages
library(clustMixType)
library(FeatureImpCluster)
library(PCAmixdata) #PCA -> clustering with mixed data


#--- this is a joined .csv with inmate_id, course_id and inmate information! 
# ---- TODO: when to use this?
courses <- read_csv('clustering_datasets/attempting_clustering.csv')

#----reshape course_content so that one tag per column
course_content <- read_csv('clustering_datasets/course_node_count_tags.csv')


#---- USE THIS: new CSV Mike sent over with the IDs attached to each tag
course_content_test <- read_csv('clustering_datasets/course_tags_with_IDS.csv')


course_tags <- course_content_test %>% 
  mutate(split_content_tags = str_split(content_tags, ",")) %>% 
  unnest(split_content_tags) %>%
  group_by(course_id) %>%
  separate(split_content_tags, c("name","id_number"), sep =  "-") %>% 
  mutate(name = toupper(name)) %>%
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




 
#---- clustering just the courses using PAM + gower distance
#---- gower distance good for mixed data
#--- BUT !! issue: how do we weight things ?
gower_dist <- daisy(course_tags %>%
                      select(lesson_count, total_page_count, total_page_item_count, 
                             !!paste0('id_number_tag', 1:32, sep="")), 'gower')

gower_mat <- as.matrix(gower_dist)

#' Print most similar courses
course_tags[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]


#print most dissimilar courses
course_tags[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


  sil_width <- c(NA)
  for(i in 2:20){  
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
    sil_width[i] <- pam_fit$silinfo$avg.width  
  }
  plot(1:20, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width")
  lines(1:20, sil_width)
  
  k <- 16
  pam_fit <- pam(gower_dist, diss = TRUE, k)
  pam_results <- course_tags %>%
    ungroup() %>% 
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary



#----- looking at lower dimensional space
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster))


# Course + Inmate Demographic Info ----------------------------------------

#---- joining with inmate information, perform clustering
  
  course_inmates <- courses %>% select(inmate_id, course_id, 
                                       quiz_score,
                                       language, 
                                       spent_time,
                                       total_points, 
                                       total_spent_points,
                                       facility_id, 
                                       created_at,
                                       updated_at_1)
  
  #--- only taking the top 5 tags
  course_tags <- course_tags %>% select(course_id, lesson_count, total_page_count,
                                        total_page_item_count,!!paste0('id_number_tag', 1:5, sep=""))
    
  inmate_courses <- course_tags %>%
    left_join(course_inmates, by=c("course_id")) %>% 
    select(!!paste0('id_number_tag', 1:5, sep=""), inmate_id, course_id, total_points, total_spent_points,
           language, updated_at_1) %>%
    group_by(inmate_id) %>%
    arrange(desc(updated_at_1)) %>%
    summarise_all(funs(toString(na.omit(.)))) %>%
    #select(!!paste0('tag', 1:5, sep="")) %>%
    as_tibble      %>% # convert to table
    purrr::modify(~replace(.x,lengths(.x)==0,list(NA))) %>% # replace empty elements by list(NA) so they have length 1 too
    modify_if(~all(lengths(.x)==1),unlist) %>%
    separate('id_number_tag1', paste("id_number_tag1", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
    separate('id_number_tag2', paste("id_number_tag2", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
    separate('id_number_tag3', paste("id_number_tag3", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
    separate('id_number_tag4', paste("id_number_tag4", 'course',1:10, sep="_"), sep=",", extra="drop") %>%
    separate('id_number_tag5', paste("id_number_tag5", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
    mutate_all(as.factor) 
  
  gower_dist <- daisy(inmate_courses %>%
                        select(-course_id, -inmate_id, -updated_at_1), 'gower')
  gower_mat <- as.matrix(gower_dist)
  
  #print most similar courses
  View(inmate_courses[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ] %>% select_if(~sum(!is.na(.)) > 0))
  
  
  #print most dissimilar courses
  inmate_courses[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


sil_width <- c(NA)
for(i in 2:20){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)

k <- 14
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- inmate_courses %>%
  ungroup() %>% 
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary



# looking at lower dimensional space
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
  


#--- more inspection of this; What do the inmates in cluster have in common?

  inmate_demo_info <- read_csv('general_datasets/inmates_edovo.csv')
  
  inmate_demo <- inmate_demo_info %>% 
    mutate(id = as.factor(id), inmate_id =as.factor(inmate_id)) %>%
    mutate(age = lubridate::time_length(Sys.Date() - birth_date, unit='days') / 365.25) %>%
    select(id,first_name,  last_name, age, facility_id, total_points, total_spent_points)
  
  
  inmate_demo_with_clustering <- inmate_courses %>%
    ungroup() %>% 
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    mutate(inmate_id = as.factor(inmate_id)) %>%
    select(-total_points, -total_spent_points) %>%
    inner_join(inmate_demo, by=c('inmate_id' = 'id' )) %>%
    ungroup()
   
  
  
  inmate_demo_with_clustering %>% 
    filter(cluster == 1)%>%
    select(first_name, last_name, age, facility_id, language, 
                                           total_points, total_spent_points)

  
  distinct_tags <-inmate_demo_with_clustering %>%
    select(starts_with('tag')) %>%
    rowwise() %>%
    do(data.frame(., count_distinct_tags = n_distinct(unlist(.))))
  
  inmate_demo_with_clustering $count_distinct_tags <- distinct_tags$count_distinct_tags
  
  #avg number of points + age per cluster
  inmate_demo_with_clustering %>% 
    #left_join(distinct_tags %>% select(inmate_id, count_distinct_tags)) %>%
    group_by(cluster)%>%
    summarise(mean_total_points = mean(total_points, na.rm=T),
              mean_spent_points= mean(total_spent_points, na.rm=T),
              point_diff = mean_total_points - mean_spent_points, # How many more points earned vs. spent?
              mean_age = mean(age, na.rm=T),
              mean_distinct_tags = mean(count_distinct_tags, na.rm=T),
              most_common_tag1_course1 = modeest::mlv(id_number_tag1_course_1, method=mfv),
              most_common_tag2_course1 = modeest::mlv(id_number_tag2_course_1, method=mfv),
              most_common_tag1_course2 = modeest::mlv(id_number_tag1_course_2, method=mfv))
  
  
  for_plot <-inmate_demo_with_clustering  %>% group_by(cluster,facility_id) %>%
    summarise(n= n()) %>%
    mutate(freq = n / sum(n))
  
  ggplot(for_plot, 
         aes(x=as.factor(facility_id), y=n)) + geom_col()+ facet_wrap(~cluster)
    
    
  
  #number of members in each cluster in a certain facility
  View(inmate_demo_with_clustering  %>% group_by(cluster) %>% count(facility_id))



# Clustering Based on Time as ACTIVE user ---------------------------------
  
  active_users<- inmate_demo_info %>% 
    mutate(id = as.factor(id), inmate_id =as.factor(inmate_id)) %>%
    mutate(age = lubridate::time_length(Sys.Date() - birth_date, unit='days') / 365.25,
           user_time =lubridate::time_length(Sys.Date() - lubridate::as_date(created_at), unit='days') / 365.25 ) %>%
    select(id,first_name,  last_name, age, facility_id, total_points, total_spent_points, 
           status, user_time) %>%
    filter(status=='ACTIVE')
  
  
  top_tags_per_inmate<- inmate_courses %>%
    ungroup() %>% 
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    mutate(inmate_id = as.factor(inmate_id)) %>%
    select(-total_points, -total_spent_points) %>%
    inner_join(active_users, by=c('inmate_id' = 'id' )) %>%
    ungroup()
  
  
  
  distinct_tags <-top_tags_per_inmate %>%
    select(starts_with('id_number_tag')) %>%
    rowwise() %>%
    do(data.frame(., count_distinct_tags = n_distinct(unlist(.))))
  
  top_tags_per_inmate$count_distinct_tags <- distinct_tags$count_distinct_tags
  
  #avg number of points + age per cluster
  top_tags_per_inmate %>% 
    #left_join(distinct_tags %>% select(inmate_id, count_distinct_tags)) %>%
    group_by(cluster)%>%
    summarise(mean_total_points = mean(total_points, na.rm=T),
              mean_spent_points= mean(total_spent_points, na.rm=T),
              point_diff = mean_total_points - mean_spent_points,
              mean_age = mean(age, na.rm=T),
              mean_user_time  =mean(user_time, na.rm=T),
              mean_distinct_tags = mean(count_distinct_tags, na.rm=T),
              most_common_tag1_course1 = modeest::mlv(id_number_tag1_course_1, method=mfv),
              most_common_tag2_course1 = modeest::mlv(id_number_tag2_course_1, method=mfv),
              most_common_tag1_course2 = modeest::mlv(id_number_tag1_course_2, method=mfv))
  
  
  
  k <- 15
  pam_fit <- pam(gower_dist, diss = TRUE, k)
  pam_results <- inmate_courses %>%
    ungroup() %>% 
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
  
  
  # looking at lower dimensional space
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster))


#----- Recommendation from John: divide data into quantiles based on time as active user 

  
  #-- List, each element a tibble representing a quantile
  course_inmates <- top_tags_per_inmate %>%
    mutate(quantile = ntile(user_time, 5)) %>% 
    group_by(quantile) %>% 
    group_map( ~ .x)
  
  course_tags <- course_tags %>% select(course_id, lesson_count, total_page_count,
                                        total_page_item_count,!!paste0('id_number_tag', 1:5, sep="")) %>%
    mutate(course_id = as.factor(course_id))
  
  
  
  #--- takes in a quantiles of active users + performs clustering
  #--- only looking at the top 5 tags 
perform_clustering <- function(quant_tib){
    inmate_courses <- course_tags %>%
      left_join(quant_tib, by=c("course_id")) %>% 
      select(!!paste0('id_number_tag', 1:5, sep=""), inmate_id, course_id, total_points, total_spent_points, user_time) %>%
      group_by(inmate_id) %>%
      mutate_at(vars(contains('tag')), funs(toString(na.omit((.))))) %>%
      purrr::modify(~replace(.x,lengths(.x)==0,list(NA))) %>% # replace empty elements by list(NA) so they have length 1 too
      modify_if(~all(lengths(.x)==1),unlist) %>%
      separate('id_number_tag1', paste("id_number_tag1", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
      separate('id_number_tag2', paste("id_number_tag2", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
      separate('id_number_tag3', paste("id_number_tag3", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
      separate('id_number_tag4', paste("id_number_tag4", 'course',1:10, sep="_"), sep=",", extra="drop") %>%
      separate('id_number_tag5', paste("id_number_tag5", 'course', 1:10, sep="_"), sep=",", extra="drop") %>%
      mutate_at(vars(contains("tag")), as.factor) %>%
      mutate_if(is.double, as.numeric)
    
    gower_dist <- daisy(inmate_courses %>%
                          select(-course_id, -inmate_id), 'gower')
    gower_mat <- as.matrix(gower_dist)
  
  
    k <- 10 #--- TODO: you could look at siloutte plot and pick out optimal k for each quantile?? 
    pam_fit <- pam(gower_dist, diss = TRUE, k)
    pam_results <- inmate_courses %>%
      ungroup() %>% 
      mutate(cluster = pam_fit$clustering) %>%
      group_by(cluster) %>%
      do(the_summary = summary(.))
  
    
    # looking at lower dimensional space
    tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pam_fit$clustering))
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster)) 
    
    
    
  }
  
  
  map(course_inmates, perform_clustering)
  
  
  


# Adding Navigation Map Information ---------------------------------------

  
#----- new Clustering based on total pages
#---- new_joined_tags.csv comes from parsing_json.R
  tags_with_parsed_json <- read_csv('clustering_datasets/new_joined_tags.csv') %>% select(-X1) %>%
    mutate(tag_ID = as.factor(tag_ID), 
           course_id = as.factor(course_id),
           how_many_id = as.factor(how_many_id)) 
  
  
  tags_with_parsed_json2 <- tags_with_parsed_json%>%
    #---- need to impute median values for any sort of numeric feature
    mutate_if(is.numeric, list(~ replace(., is.na(.), median(., na.rm = TRUE))))
  
  
  
  gower_dist <- daisy(tags_with_parsed_json2 %>%
                        select(course_id,lesson_count, total_page_count, tag_ID,
                               total_pages), 'gower')
  
  gower_mat <- as.matrix(gower_dist)
  
  # print most similar tags!
  tags_with_parsed_json[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), 
                               arr.ind = TRUE)[1, ],]
  
  
  
  sil_width <- c(NA)
  for(i in 2:20){  
    pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
    sil_width[i] <- pam_fit$silinfo$avg.width  
  }
  
  plot(1:20, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width")
  lines(1:20, sil_width)
  
  k <- 2
  pam_fit <- pam(gower_dist, diss = TRUE, k)
  pam_results <- tags_with_parsed_json2 %>%
    ungroup() %>% 
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary


  # looking at lower dimensional space
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster))



# Clustering and PCA ------------------------------------------------------

  #--- get principal components for mixed data types 
  
  tags_with_parsed_json <- tags_with_parsed_json %>% 
    pivot_wider(id_cols=c('course_id', 'total_page_count'),
                names_from="how_many_id", values_from=c('tag_ID')) #going to do PCA
  
  test_df <- as.data.frame(course_tags %>% select(lesson_count, total_page_count, total_page_item_count,starts_with("id_number")))
  test_df <- test_df[,colSums(is.na(test_df))<nrow(test_df)]
  test <-PCAmixdata::splitmix(test_df[,1:25])
  X1 <- test$X.quanti 
  X2 <- test$X.quali 
  res.pcamix <- PCAmix(X.quanti=X1, X.quali=X2,  rename.level=TRUE,
                       graph=FALSE)
  
  
  #---- uh this is really bad lol, like horrifically bad
  plot(res.pcamix,choice="sqload",coloring.var=T, leg=TRUE,
       posleg="topright", main="All variables")



#---- Kprototypes clustering!!

  tags_with_parsed_json <- read_csv('clustering_datasets/new_joined_tags.csv') %>% select(-X1) %>%
    mutate(tag_ID = as.factor(tag_ID), 
           course_id = as.factor(course_id),
           how_many_id = as.factor(how_many_id),
           parent_ID = as.factor(parent_ID)) %>%
    select(-course_type, -json_id, -name,
           -description, -created_at, -updated_at, -system)
  
  k_proto_object <- kproto(as.data.frame(tags_with_parsed_json), 12,
                           lambda = 10000) # bigger lambda: partitions that emphasize differences between the categorical features,
  k_proto_object$centers


#--- omit NAs-- attach column
  tags_with_parsed_json2 <- na.omit(tags_with_parsed_json)
  tags_with_parsed_json2$cluster <- k_proto_object$cluster
  
  par(mfrow=c(1,2))
  for(i in 1: 1:4){
    plot(as.data.frame(tags_with_parsed_json2%>% select_if(is.numeric))[,1:i], 
         col=tags_with_parsed_json2$cluster, main="K-prototypes")
  }
  
  #--- Elbow Method for finding the optimal number of clusters
    set.seed(123)
    
    # Compute and plot wss for k = 2 to k = 15.
    k.max <- 20
    wss <- sapply(1:k.max, 
                  function(k){kproto(as.data.frame(tags_with_parsed_json2), k)$tot.withinss})
    wss
    plot(1:k.max, wss,
         type="b", pch = 19, frame = FALSE, 
         xlab="Number of clusters K",
         ylab="Total within-clusters sum of squares")
    


#--- What Features are the most important?
FeatureImp_res <- FeatureImpCluster(k_proto_object,as.data.table(tags_with_parsed_json))
plot(FeatureImp_res,tags_with_parsed_json,color="type")
