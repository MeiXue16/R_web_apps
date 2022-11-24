# interest rate 利息
i <- 0.03

# 折扣因子
v <- 1/(1+i)

# list 不同时间点的折扣因子
discount_factors <- v^ (0:7)

# cash flow vector

cash_flows <- c(500, 400, 300, rep(200, 5))

# present value of cash flow vector
present_value <- sum(cash_flows * discount_factors)

present_value



# Define the cash flows
cash_flows <- c(rep(30,4), 130, 230)

# Define i and v
i <- 0.02
v <- 1 / (1 + i)

# Define the discount factors
discount_factors <- v ^ (0: 5)
 
# Calculate the present value  
present_value <- sum(discount_factors * cash_flows)

present_value



# Define the cash flows
cash_flows <- c(0, rep(3000, 3), rep(1000, 2))

# Define the discount factors
discount_factors <- (1 + 0.05) ^ - (0: 5)

# NPV  Calculate the net present value 
net_present_value <- sum(cash_flows * discount_factors) - 10000

net_present_value



# v(s, t) if s< t , v is discount factor
# if s >t, v is accumulation faactor
# value at time s of 1 euro paid at time t
i <- 0.03
v <- 1/(1+i)
s <- 2
t <- 4
value <- v ^(t-s)
value


# define discount function
discount <- function(s, t, i =0.03) {(1+i)^ (s-t)}
# 方法一：计算时间点3的值
value_at_time_3 <- 500*discount(3,0) + 300* discount(3,2) +200* discount(3,7)
value_at_time_3

# 方法二：计算时间点3的值
discount <- function(s, t, i =0.03) {(1+i)^ (s-t)}
cash_flows <- c(500,0, 300, rep(0,4), 200)
value_at_time_3 <- sum(cash_flows * discount(3, 0:7) )
value_at_time_3



# Define the discount function v
discount <- function(s, t, i = 0.02) {
  (1 + i) ^ - (t - s)
}

# Calculate the present value
present_value <- sum(cash_flows * discount(0, 0:5))
present_value

# Calculate the value at time 6
sum(cash_flows * discount(6, 0:5))

# Calculate the value at time 6, starting from present_value
present_value * discount(6, 0)



# 贷款购买一辆20000的车，免费到手，年末开始还贷，分4年付款
discount2 <- function(s, t, i =0.03){
  (1+i)^ -(t-s)
}

payment <- c(0, rep(1, 4))

pv <- sum(payment * discount2(0, 0:4))

# 计算每年支付的贷款额度
20000/pv



# Define the discount factors
discount_factors <- (1 + 0.03) ^ - (0:9)

# 定义存款模式（未知存款金额） Define the deposit pattern
deposits <- c(0, rep(1, 4), rep(0, 5))

# 定义支付模式 Define the university expenses
payments <- c(rep(0, 5), rep(3500,5))

# 计算（未知）*1元存款现值Calculate the present value of the deposits
PV_deposit <- sum(deposits * discount_factors) 

# 计算支付额现值 Calculate the present value of the payments
PV_payment <- sum(payments * discount_factors) 

# 计算出每年的存款额度 Calculate the yearly deposit K in the first 4 years
K <- PV_payment/PV_deposit
K




# 年利率 -> 月利率
i <- 0.03
month_interest <- (1+i)^ (1/12) -1

# 年利率 -> 月利率，贷款125000买房，20年还款，年利率 0.0304，
# 现在按月还，求月利率和每月还款金额
# Define the number of payments
number_payments <- 20*12

# Define the yearly interest rate
i <- 0.0304

# Calculate the monthly interest rate
monthly_interest <- (1+i) ^ (1/12) - 1
monthly_interest

# Define the discount factors
discount_factors <- (1+monthly_interest ) ^ - (1 : number_payments)

# Define the payment pattern
payments <- rep(1, number_payments)

# Calculate the monthly loan payment K
K <- 125000 / sum(payments * discount_factors)
K




# 变化的利率
interest <- c(0.04, 0.03, 0.02, 0.01)
yearly_discount_factors <- (1+ interest) ^-1
#折扣因子 cumprod返回向量的累积乘积向量
discount_factors <- c(1, cumprod(yearly_discount_factors))



# 辛西娅想向父母借钱去澳大利亚旅行。她今年需要 1000 欧元，明年还需要 5000 欧元。
# 在之后的10年内每年偿还816.86元，贷款协议中的利率不是恒定的，她能还清借款吗？
# Interest rates
interest <- c(rep(0.05, 3), rep(0.06, 3), rep(0.07, 5))

# Define the yearly discount factors
yearly_discount_factors <- (1 + interest) ^ ( - 1)

# Define the discount factors
discount_factors <- c(1 , cumprod(yearly_discount_factors))

# Define the cash flow vector
cash_flow <- c(1000, 5000, rep(-816.86, 10))

# Calculate the PV
PV <- sum(cash_flow * discount_factors)
PV
#PV = -0.03， 能还清

