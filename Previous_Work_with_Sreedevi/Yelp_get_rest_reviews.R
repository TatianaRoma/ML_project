
##################################################
library(jsonlite)
library(rlist)
library(RColorBrewer)

json_reviews_file <- "~/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
reviews_all <- fromJSON(sprintf("[%s]", paste(readLines(json_reviews_file), 
                                              collapse=",")))

reviews <- cbind.data.frame(Rating = reviews_all$stars, 
                            Text = reviews_all$text, 
                            ID = reviews_all$business_id)

json_business_file <- "~/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"
businesses_all <- fromJSON(sprintf("[%s]", paste(readLines(json_business_file), collapse=",")))

businesses_all <- businesses_all[,c(1,5,11,15)]

rest_ids <- businesses_all['Restaurants' %in% businesses_all$categories,1]

rests <- as.list(as.data.frame(t(businesses_all)))
rests <- list.filter(rests,"Restaurants" %in% categories)
rest_ids <- unlist(list.select(rests, business_id), use.names = F)
selected <- reviews$ID %in% rest_ids
rest_reviews <- reviews[selected,]

stars <- ifelse(rest_reviews$stars==1, "one", 
                ifelse(rest_reviews$stars==2,"two",
                       ifelse(rest_reviews$stars==3, "three",
                              ifelse(rest_reviews$stars==4,"four",
                                     "five"))))
s <- prop.table(table(rest_reviews$stars))
#par(mar=c(5.1,4.1,6.1,2.1))
bp <- barplot(s, col = brewer.pal(3:8, "Greys"), 
              main = "Yelp: Distribution of ratings", 
              xlab = "Review Rating", 
              ylab = "Proportion", 
              names.arg = c("one","two","three","four","five"), 
              cex.names = .75, 
              legend.text = F
)
#title("Imbalanced distribution of the response", line = +3)
#labels <- c("one","two","three","four","five")
#text(bp, s, labels, cex=.6, pos=3)

rand.reviews <- rest_reviews[sample(nrow(rest_reviews)),]

rest_reviews_part1 <- rand.reviews[1:10000,1:5]
rest_reviews_part2 <- rand.reviews[10001:60000,4:5]
rest_reviews_part3 <- rand.reviews[20001:30000,4:5]

write.csv(rest_reviews_part1, file = "rest_reviews_1.csv", row.names = F)
write.csv(rest_reviews_part2, file = "rest_reviews_2.csv", row.names = F)
write.csv(rest_reviews_part3, file = "rest_reviews_3.csv", row.names = F)


Text <- rep(c("It was 
              great", "it was awesome", "Very bad service", 
              "Fantastic experience", "Great place and food!"), 2)
Ratings <- c(5,4,3,2,1,5,4,5,4,1)
df <- cbind.data.frame(Ratings, Text)
df$Text <- gsub('[\r\n]', '', df$Text)


reviews_save <- rand.reviews[,1:2]
reviews_save$Text <- gsub('[^[:alnum:] ]', ' ', reviews_save$Text)
write.csv(reviews_save, "reviews_all.csv", row.names = F)
write.table(reviews_save, "rest_reviews_all.csv", 
            sep=",", col.names = F, row.names = F)

