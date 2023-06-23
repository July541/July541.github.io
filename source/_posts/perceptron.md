---
title: '统计学习方法（一）：感知机'
tags:
  - machine learning
  - perceptron
categories:
  - 统计学习方法
date: 2020-07-04 00:58:28
mathjax: true
---
# 感知机

## 模型

感知机是一种二类分类的线性分类模型，属于判别模型。感知机的数据集要求线性可分。
$$
f(x) = sign(\omega \cdot x + b)
$$
其中
$$
sign(x) = \left\{\begin{matrix}
1, x \geq 0\\
-1, x < 0
\end{matrix} \right.
$$
$\omega \cdot x + b = 0$ 决定了将数据分为正负两类的超平面。

<!--more-->

## 学习策略

对于误分类的数据 $(x_i, y_i)$ 有
$$
-y_i(\omega \cdot x_i + b) > 0
$$
则误分类点 $x_i$ 到超平面的距离为
$$
-\frac{1}{||w||}y_i(\omega \cdot x_i + b)
$$
$||\omega||$ 为 $\omega$ 的 $L_2$ 范数，所有误分类点到超平面的总距离为
$$
-\frac{1}{||w||}\sum_{x_i\in M}{y_i(\omega \cdot x_i + b)}
$$
其中 $M$ 为所有误分类点到集合。

忽略$\frac{1}{||w||}$项，得感知机学习的经验风险函数
$$
L(\omega, b) = -\sum_{x_i\in M}{y_i(\omega \cdot x_i + b)}
$$
显然损失函数是非负的，若无误分类点，损失函数值为0，误分类点越少，损失函数值越小。

## 感知机学习算法

### 原始形式

由上文知感知机的学习目标为
$$
\underset{\omega, b}{min} L(\omega, b) = - \sum_{x_i \in M}{y_i(\omega \cdot x_i + b)}
$$
算法学习是误分类驱动的，采用随机梯度下降法，有
$$
\bigtriangledown_\omega L(\omega, b) = -\sum_{x_i\in M}{x_i y_i}\\
\bigtriangledown_b L(\omega, b) = -\sum_{x_i \in M} {y_i}
$$
则更新方式为
$$
\omega \leftarrow \omega + \eta x_i y_i
$$

$$
b \leftarrow b + \eta y_i
$$

该学习方法的直观解释为：当一个实例点被误分类，则调整 $\omega$ 和 $b$ 的值，使超平面向误分类点到一侧移动，以减少误分类点与超平面间的距离，知道超平面越过该误分类点使其被正确分类。

>  **感知机算法由于采用不同的初值或选取不同的误分类点，解可能会不同**

### 算法的收敛性

为便于叙述，令 $\hat{\omega} = (\omega^T, b)^T$，$\hat{x} = (x^T, 1)^T$，有 $\hat{\omega} \cdot \hat{x} = \omega \cdot x + b$。

1. 存在满足条件 $||\hat{\omega}_{opt}||=1$ 的超平面 $||\hat{\omega}_{opt} \cdot \hat{x}|| = \omega_{opt} \cdot x + b_{opt} = 0$ 将训练数据集完全正确分开，且存在 $\gamma > 0$，对所有 $i = 1,2,\cdots,N$

$$
y_i({\hat{\omega}_{opt}} \cdot \hat{x}_i) = y_i(\omega_{opt} \cdot x_i + b_{opt}) \geqslant \gamma
$$

2. 令 $R = \underset{1\leq i\leq N}{max}||\hat{x}_i||$，则该算法在训练数据集上误分类的次数 $k$ 满足不等式

$$
k \leqslant (\frac{R}{\gamma})^2
$$

证明如下：

(1) 由于训练数据集数线性可分的，则存在超平面可将训练数据集完全正确分开，任取符合条件的超平面 $||\hat{\omega}_{opt} \cdot \hat{x}|| = \omega_{opt} \cdot x + b_{opt} = 0$，通过缩放调整 $\hat{\omega}_{opt} = 1$。此时对于训练数据集，有
$$
y_i({\hat{\omega}_{opt}} \cdot \hat{x}_i) = y_i(\omega_{opt} \cdot x_i + b_{opt}) > 0
$$
所以存在
$$
\gamma = \underset{i}{min}\{y_i(\omega_{opt} \cdot x_i + b_{opt})\}
$$
使
$$
y_i({\hat{\omega}_{opt}} \cdot \hat{x}_i) = y_i(\omega_{opt} \cdot x_i + b_{opt}) \geqslant \gamma
$$
(2) 首先参数 $\hat{\omega}$ 对误分类点有如下更新方法 $\hat{\omega}_k = \hat{\omega}_{k - 1} + \eta \hat{x}_i y_i$，其中 $\hat{\omega}_{k - 1}$ 为第 $k$ 个误分类实例 (这里应理解为第 $k$ 次更新误分类而不是第 $k$ 个误分类点) 更新前的权重向量。对更新方法的两侧同乘 $\hat{\omega}_{opt}$，有
$$
\hat{\omega}_k \cdot \hat{\omega}_{opt} = \hat{\omega}_{k - 1} \cdot \hat{\omega}_{opt} + \eta y_i{\hat{\omega}_{opt}} \cdot \hat{x}_i \geqslant \hat{\omega}_{k - 1} \cdot \hat{\omega}_{opt} + \eta \gamma
$$
取 $\hat{\omega}_0 = 0$，递推得
$$
\hat{\omega}_k \cdot \hat{\omega}_{opt} \geqslant \hat{\omega}_{k-1} \cdot \hat{\omega}_{opt} + \eta \gamma \geqslant \hat{\omega}_{k-2} \cdot \hat{\omega}_{opt} + 2\eta \gamma \geqslant \cdots \geqslant k\eta \gamma \tag{1}
$$
对更新方法的范数两边平方有
$$
||\hat{\omega}_k||^2 = ||\hat{\omega}_{k-1}||^2 + 2 \eta \hat{x}_i y_i \hat{\omega}_{k-1} + \eta^2 ||\hat{x}_i||^2
$$
由于 $y_i \in \{+1, -1\}$ ，上式右边第三项的 $y_i^2 = 1$ 被省略，且对于误分类点有 $y_i(\hat{\omega}_{k-1} \cdot \hat{x}_i) \leqslant 0$**(这里为小于等于，不影响最后的结果)**，则上式满足
$$
\begin{equation}\nonumber
\begin{aligned}
||\hat{\omega}_k||^2 &= ||\hat{\omega}_{k-1}||^2 + 2 \eta \hat{x}_i y_i \hat{\omega}_{k-1} + \eta^2 ||\hat{x}_i||^2 \\
&\leqslant ||\hat{\omega}_{k-1}||^2 + \eta^2 ||\hat{x}_i||^2 \\
&\leqslant ||\hat{\omega}_{k-1}||^2 + \eta^2 R^2 \\
&\leqslant ||\hat{\omega}_{k-2}||^2 + 2\eta^2 R^2 \\
&\leqslant \cdots \\
&\leqslant k\eta^2 R^2
\end{aligned} \tag{2}
\end{equation}
$$
结合公式 (1) 和 (2)，有
$$
k\eta \gamma \leqslant \hat{\omega}_k \cdot \hat{\omega}_{opt} \leqslant ||\hat{\omega}_k||\cdot||\hat{\omega}_{opt}|| \leqslant\sqrt{k} \eta R
$$
即
$$
k^2 \gamma^2 \leqslant k R^2 \Rightarrow k \leqslant (\frac{R}{\gamma})^2
$$
于是我们可以得到误分类的次数 $k$ 是有上界的，算法可以经过有限次搜索将训练数据完全线性分开，即算法是收敛的。

