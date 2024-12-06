import numpy as np

# 给定数据
data = np.array([
    [3, 5],
    [2, 4],
    [5, 7],
    [4, 6],
    [6, 8],
    [8, 11],
    [1, 3],
    [7, 10],
    [9, 13],
    [10, 14]
])

target = np.array([9, 6, 12, 10, 14, 19, 5, 17, 22, 24])

# 根据先前的计算，获取权重 w
w = np.linalg.inv(data.T @ data) @ data.T @ target
print(w)

# 使用权重 w 进行预测
predictions = data @ w

# 计算 MSE
mse = np.mean((target - predictions) ** 2)

print(round(mse/2, 3))
