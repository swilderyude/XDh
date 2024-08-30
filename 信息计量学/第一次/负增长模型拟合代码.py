import numpy as np
from scipy.optimize import curve_fit

# 数据
years = np.array([8, 9, 11,  13, 14, 15, 17, 18, 19,  21, 22, 23])
rates = np.array([3.38317757, 2.939252336,  2.934579439,  2.672897196, 2.317757009, 1.855140187,  1.76635514, 1.644859813, 1.46728972,  1.58411215, 1.191588785, 0.8411214953])

# 定义负指数模型函数
def exponential_decay(x, k, a):
    return k * np.exp(-a * x)

# 使用最小二乘法进行拟合
params, _ = curve_fit(exponential_decay, years, rates)

# 拟合参数
k = params[0]
a = params[1]

print(f"拟合参数：k = {k}, a = {a}")
