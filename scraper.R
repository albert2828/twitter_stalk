library(twitteR)

tuits <- userTimeline("bhherto95",n=3200, includeRts = T) 
tuits <- twListToDF(tuits)
tuits_df <- tuits %>% filter(!isRetweet) %>%
            select(text, favoriteCount, replyToSN, id, created, retweetCount)
tuits_df$created <- with_tz(tuits_df$created, "America/Mexico_City")

write.csv(tuits_df, "tuits_bhherto95.csv")
