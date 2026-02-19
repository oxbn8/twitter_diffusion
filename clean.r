data = data[,c("tid","sharers","seed_user",
               "selected","depth","t","inter","nexpose","sim",
               "sr","reciprocity",
               "rtb","friends_count.sharers","followers_count.sharers","statuses_count.sharers","tenure.sharers",
               "friends_count.seed","followers_count.seed","statuses_count.seed","tenure.seed")]
colnames(data) = c("tweet_id","retweeter_id","parent_id",
                   "retweeted","depth","time_elapsed","interaction_freq","number_exposures","interest_similarity",
                   "structural_redundancy","reciprocity",
                   "retweeting_inertia","friends_count_retweeter","followers_count_retweeter","statuses_count_retweeter","age_retweeter",
                   "friends_count_parent","followers_count_parent","statuses_count_parent","age_parent")
save(data,file="data_model.Rdata")
