import numpy as np

# 输入年份和累积数量数据
years = np.array([0,1,2,3,4,5,6,7,8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23])
cumulative_counts = np.array([117, 211, 317, 434, 577, 707, 854, 999, 1176, 1373, 1520, 1682, 1852, 2031, 2290, 2545, 2805, 3093, 3430, 3889, 4409, 5086, 6072, 7314])

# 已知参数a
a = 117

# 对数变换
log_counts = np.log(cumulative_counts / a)

# 使用线性拟合计算参数b
b, _ = np.polyfit(years, log_counts, 1)

print("b =", b)