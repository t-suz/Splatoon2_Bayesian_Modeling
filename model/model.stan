data {
  int N; //サンプルサイズ
  int W; //ブキ種類
  int U; //ユーザー数
  int<lower=1, upper=W> WeaponID[N]; //ブキID
  int<lower=1, upper=U> UserID[N]; //ユーザーID
  int<lower=0, upper=1> Y[N]; //勝敗結果
  real<lower=0, upper=1> Kill[N]; //キル数
}

parameters {
  real a[U];
  real b[W];
  real<lower=0> s_a;
  real<lower=0> s_b;
  real<lower=0> s_kill;
}

model {
  for (u in 1:U) {
    a[u] ~ normal(0, s_a);
  }
  for (w in 1:W) {
    b[w] ~ normal(0, s_b);
  }
  for (n in 1:N) {
    Kill[n] ~ normal(a[UserID[n]], s_kill);
    Y[n] ~ bernoulli_logit(a[UserID[n]] + b[WeaponID[n]]);
  }
  
}
