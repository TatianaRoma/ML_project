library(jsonlite)
library(rlist)

json_reviews_file <- "~/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
reviews_all <- fromJSON(sprintf("[%s]", paste(readLines(json_reviews_file), 
                                              collapse=",")))

reviews <- cbind(reviews_all$votes,reviews_all$stars, reviews_all$text, reviews_all$business_id)

colnames(reviews) <- c("funny", "useful", "cool", "stars", "text", "business_id")

json_business_file <- "~/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"
businesses_all <- fromJSON(sprintf("[%s]", paste(readLines(json_business_file), collapse=",")))

businesses_all <- businesses_all[,c(1,5,11,15)]

rest_ids <- businesses_all['Restaurants' %in% businesses_all$categories,1]

rests <- as.list(as.data.frame(t(businesses_all)))
rests <- list.filter(rests,"Restaurants" %in% categories)
rest_ids <- unlist(list.select(rests, business_id), use.names = F)

rest_reviews <- reviews[reviews$business_id %in% rest_ids,]
rand.reviews <- rest_reviews[sample(nrow(rest_reviews)),]

rest_reviews_part1 <- rand.reviews[1:50000,1:5]
rest_reviews_part2 <- rand.reviews[50001:100000,1:5]
rest_reviews_part3 <- rand.reviews[100001:150000,1:5]

write.csv(rest_reviews_part1, file = "rest_reviews_1.csv", row.names = F)
write.csv(rest_reviews_part2, file = "rest_reviews_2.csv", row.names = F)
write.csv(rest_reviews_part3, file = "rest_reviews_3.csv", row.names = F)

