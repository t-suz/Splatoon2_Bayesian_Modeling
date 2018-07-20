library(checkpoint)
checkpoint("2018-07-09")
options(mc.cores = 4)

# require(tidyverse)
require(readr)
require(dplyr)
require(tidyr)
require(tibble)
require(lubridate)
require(stringr)
require(ggplot2)
require(rstan)
require(ggmcmc)
require(data.table)


# 関数定義 --------------------------------------------------------------------

read_data <- function(num = 1:7) {
  
  result <- data.frame()
  
  for (i in num) {
    dat <- fread(paste0("data/result_", i, ".csv"))
    result <- rbind(result, dat)
  }
  
  dim(result)
  result
}
detect_win <- function(team, win_my_team) {
  
  aa <- str_detect(team, "my")
  ab <- win_my_team == "win"
  
  ba <- str_detect(team, "his")
  bb <- win_my_team == "lose"
  
  a <- as.logical(aa * ab)
  b <- as.logical(ba * bb)
  
  a | b
}


# データロードと加工 ---------------------------------------------------------------
weapon_list <- 
  read_csv("data/weapon_list.csv")

df <-
  read_data(1:2) %>% 
  rename_all(funs(sub('/0', '', ., fixed = TRUE))) %>% 
  filter(lobby_key == "standard") %>% 
  filter(mode_key == "gachi") %>% 
  filter(rank == "x") %>% 
  filter(weapon != "", !is.na(weapon), length(weapon) >= 3) %>%
  mutate(is_top500 = if_else(is.na(.$is_top50), FALSE, .$is_top50)) %>% 
  # TimestampをUnixtimeからJSTに変換
  mutate(
    start_at = as.POSIXct(.$start_at, origin="1970-1-1", tz = "UTC") %>% with_tz(tzone = "Asia/Tokyo"),
    end_at = as.POSIXct(.$end_at, origin="1970-1-1", tz = "UTC") %>% with_tz(tzone = "Asia/Tokyo")
  ) %>% 
  mutate(win = if_else(detect_win(.$team, .$result_my_team), 1, 0)) %>% 
  select(
    battle_id, version, start_at, end_at, map_key, rule_key, estimate_x_power, x_power, # ゲーム情報
    splatnet_id, team, is_me, user_order, win, is_top500,# プレイヤー情報
    weapon, weapon_base, weapon_type, weapon_category, # ブキ情報
    kill, death, assist, special, point
  ) %>% 
  left_join(weapon_list, by = "weapon") %>% 
  as_tibble()

glimpse(df)

  
## エリアのみ

area_df <- df %>% filter(rule_key == "area")

# 3試合以上データがあるユーザーデータの抽出 ----------------------------------------------------

area_target_user_list <-
  area_df %>% 
  group_by(splatnet_id) %>% 
  count() %>% 
  filter(n >= 4) %>% 
  pull(splatnet_id)

length(area_target_user_list)

## memo
area_df %>% 
  filter(is_me == FALSE) %>% 
  group_by(splatnet_id) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(n) %>% 
  count() %>% 
  arrange(desc(nn)) %>% 
  filter(n >= 4) %>% 
  pull(nn) %>% 
  sum()
  

area_df_target <-
  area_df %>% 
  filter(splatnet_id %in% area_target_user_list) %>% 
  # Splatnet_idをFactorに変換し匿名化
  mutate(user_id = as.factor(.$splatnet_id) %>% as.integer() %>% as.character()) %>% 
  # WeaponをID化
  mutate(weapon = as.factor(origin_weapon)) %>% 
  select(-splatnet_id)

glimpse(area_df_target)

weapon_df <-
  data.frame(weapon = area_df_target$weapon %>% levels()) %>% 
  rownames_to_column('weapon_id')

stan_df <-
  area_df_target %>% 
  left_join(weapon_df, by = "weapon")

stan_df$user_id %>% as.integer %>% max()
stan_df$user_id %>% as.integer %>% min()

# Stanに渡すデータの作成 -----------------------------------------------------------

datasets <-
  list(
    N = dim(stan_df)[1],
    W = dim(weapon_df)[1],
    U = length(area_target_user_list),
    WeaponID = stan_df$weapon_id %>% as.integer(),
    UserID = stan_df$user_id %>% as.integer(),
    Y = as.integer(stan_df$win),
    Kill = stan_df$kill %>% as.integer() / max(stan_df$kill %>% as.integer())
  )


# Stan  ----------------------------------------------------------------

fit <- stan(file='stan/model.stan', data = datasets, seed = 71, iter = 3000)

result <- 
  summary(fit)$summary %>% 
  as.data.frame() %>% 
  rownames_to_column('param') %>% 
  as_tibble()

result %>% 
  filter(Rhat >= 1.1)

#  ----------------------------------------------------------------

param_b <-
  result %>% 
  filter(grepl("b", param)) %>% 
  mutate(weapon_id = (row_number()) %>% as.character()) %>% 
  filter(weapon_id > 0) %>% 
  left_join(weapon_df) 

result %>% 
  filter(grepl("b", param)) %>% 
  select(-contains("%")) %>% 
  as.data.frame() %>% 
  arrange(mean)

result %>% filter(grepl("s", param))

pairs(fit, pars = c("a", "lp__"), log = TRUE, las = 1)

# ggmcmc(ggs(fit), file = "output/model_04_0718.pdf")

result %>% 
  filter(grepl("a", param), grepl("mu", param)) 

result %>% 
  filter(grepl("b", param), !grepl("mu", param)) 

# ggplot ------------------------------------------------------------------

param_b %>% filter(weapon != "NA") %>% 
ggplot() +
  geom_pointrange(
    aes(x = reorder(weapon, mean), y = mean, ymax = `75%`, ymin = `25%`, colour = reorder(weapon, mean))
    ) +
  coord_flip() +
  ylab("b[w]") + xlab("ブキ種") +
  #ggtitle("b[w]_50%ベイズ予測区間") +
  theme(legend.position = 'none')

# 比較用_単純に勝率を計算 ------------------------------------------------------------

p_b <-
param_b %>% 
  filter(weapon != "") %>% 
  select(weapon, mean) 

weapon_win_rate <-
  area_df %>% 
  group_by(origin_weapon) %>% 
  summarize(win_rate = sum(win) / n()) %>% 
  arrange(desc(win_rate)) %>% 
  rename(weapon = origin_weapon)

rs_df <-
  p_b %>% 
  left_join(weapon_win_rate) %>% 
  mutate(b_rank = rank(desc(mean))) %>% 
  mutate(wr_rank = rank(desc(win_rate))) %>% 
  arrange(b_rank)

rs_df %>% clipr::write_clip()
