# 运行模型
model1 <- glmer(retweeted ~ interest_similarity + interaction_freq + depth + 
                  (1 | tweet_id), 
                data = data_clean, 
                family = binomial)

# 查看结果
summary(model1)