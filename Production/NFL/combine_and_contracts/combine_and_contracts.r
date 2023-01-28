library(nflreadr)
library(dplyr)
library(ggplot2)

combine_data <- load_combine()

contract_data <- load_contracts()

df <- combine_data %>% 
  left_join(contract_data, by = c("player_name" = "player", 
                                  "pos" = "position",
                                  "draft_year",
                                  "draft_round"
                                  ))

feature_list <- c("value", "wt", "forty", "bench", "vertical", 
                  "broad_jump", "cone", "shuttle", "pos")

pos_list <- c("WR", "QB", "RB", "TE", "S", "CB", "C", "FB", "LB", "LS", "P"
)

# Get a player's highest-valued contract within 6 years of being drafted
best_contract <- df %>% 
  filter(year_signed - draft_year >= 5,
         draft_year <= 2017
         ) %>% 
  group_by(
    player_name,
    pos,
    draft_year,
    draft_round
  ) %>% 
  slice_max(order_by = value, n = 1) %>% 
  select(value, wt, forty, bench, vertical, broad_jump, cone, shuttle, pos)

# Fill NA with 0 for bench since most QBs don't bench
best_contract[is.na(best_contract$bench), "bench"] <- 0
best_contract[is.na(best_contract$cone), "cone"] <- mean(best_contract$cone, na.rm = TRUE)
best_contract[is.na(best_contract$shuttle), "shuttle"] <- mean(best_contract$shuttle, na.rm = TRUE)

best_contract <- na.omit(best_contract)

lmodel <- lm(value ~ wt + forty + bench + vertical + broad_jump + cone + shuttle + pos,
   data = best_contract
   )

lmodel

summary(lmodel)

best_contract$pred_value <- predict(lmodel)

best_contract %>% 
  ggplot(aes(x = value, y = pred_value)) +
  geom_point()

test <- combine_data %>% 
  filter(draft_year == 2022,
         pos %in% pos_list) %>% 
  select(player_name, pos, draft_round, feature_list[-1])

test[is.na(test$bench), "bench"] <- 0
test[is.na(test$cone), "cone"] <- mean(test$cone, na.rm = TRUE)
test[is.na(test$shuttle), "shuttle"] <- mean(test$shuttle, na.rm = TRUE)

test <- na.omit(test)

test$pred_value <- predict(lmodel, newdata = test)
