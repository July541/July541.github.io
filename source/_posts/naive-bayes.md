---
title: '统计学习方法（三）：朴素贝叶斯法'
tags:
  - machine learning
  - naive Bayes
categories:
  - 统计学习方法
date: 2020-07-05 06:38:12
mathjax: true
---

# 朴素贝叶斯法

朴素贝叶斯 (naive Bayes) 法是基于贝叶斯定理于特征条件独立假设的分类方法。对于给定的训练数据集，首先基于**特征条件独立假设**学习输入输出的联合概率分布，随后再利用此模型，对于给定的输入 $x$，利用贝叶斯定理求出后验概率最大的输出 $y$。朴素贝叶斯法属于**生成模型**。
<!--more-->

## 贝叶斯公式

$$
P(B|A) = \frac{P(B)P(A|B)}{P(A)}
$$

$$
P(B_i|A) = \frac{P(B)P(A|B_i)}{\sum_{j=1}^n{P(B_j)P(A|B_j)}}
$$

## 模型

朴素贝叶斯法通过训练数据集学习联合概率分布 $P(X, Y)$。

首先学习先验概率分布
$$
P(Y=c_k), k=1,2,\cdots,K
$$
其中 $c_k$ 是第 $k$ 个类标记。随后学习条件概率分布
$$
P(X=x|Y=c_k)=P(X^{(1)}=x^{(1)},\cdots,X^{(n)}=x^{(n)}|Y=c_k), k=1,2,\cdots,K
$$
于是便可学到联合概率分布 $P(X,Y)=P(X|Y)P(Y)$。

为了解决条件概率分布 $P(X=x|Y=c_k)$ 的参数过多的问题，朴素贝叶斯法假设训练数据由 $P(X,Y)$ 独立同分布产生，由于这是一个较强的假设，朴素贝叶斯法也由此得名。将条件独立性假设写成公式
$$
\begin{equation}\nonumber
\begin{aligned}
P(X=x|Y=c_k)&=P(X^{(1)}=x^{(1)},\cdots,X^{(n)}=x^{(n)}|Y=c_k) \\
&=\prod_{j=1}^n{P(X^{(j)}=x^{(j)}|Y=c_k)}
\end{aligned}
\end{equation}
$$
条件独立性假设使模型变简单，但有时会损失一定的分类准确率。

下面对模型进行推导。由贝叶斯定理有
$$
P(Y=c_k|X=x) = \frac{P(X=x|Y=c_k)P(Y=c_k)}{\sum_j{P(X=x|Y=c_j)P(Y=c_j)}}
$$
将独立性假设引入上式，得
$$
P(Y=c_k|X=x) = \frac{\prod_iP(X^{(i)}=x^{(i)}|Y=c_k)P(Y=c_k)}{\sum_j\prod_i{P(X^{(i)}=x^{(i)}|Y=c_j)P(Y=c_j)}}
$$
这是朴素贝叶斯法分类的基本公式，因此，朴素贝叶斯分类器可表示为
$$
y=f(x)=arg\space \underset{c_k}{max}\frac{\prod_iP(X^{(i)}=x^{(i)}|Y=c_k)P(Y=c_k)}{\sum_j\prod_i{P(X^{(i)}=x^{(i)}|Y=c_j)P(Y=c_j)}}
$$
注意到，对于所有的 $c_k$，上式的分母都是相同的，于是分类器可进一步被写为
$$
y=f(x)=arg\space \underset{c_k}{max}\prod_iP(X^{(i)}=x^{(i)}|Y=c_k)P(Y=c_k)
$$

### 对于后验概率最大化的理解

可以证明：朴素贝叶斯法后验概率最大化等价于期望风险最小化。

假设选择0-1损失函数
$$
L(Y,f(X)) = \left\{\begin{matrix}
1,Y\neq f(X)\\ 
0, y=f(X)
\end{matrix}\right.
$$
其中 $f(X)$ 是分类决策函数。这时，期望风险函数为
$$
R_{exp}(f)=E[L(Y,f(X))]
$$
期望是对联合分布 $P(X,Y)$ 取的。由此取条件期望
$$
R_{exp}(f)=E_X\sum_{k=1}^K{[L(c_k, f(X))]P(c_k|X)}
$$
为了使期望风险最小化，对 $X=x$ 逐个极小化
$$
\begin{equation}\nonumber
\begin{aligned}
f(x) &= arg \space \underset{y\in \mathcal{Y}}{min}\sum_{k=1}^{K}{L(c_k,y)P(c_k|X=x)} \\
&= arg \space \underset{y\in \mathcal{Y}}{min}\sum_{k=1}^{K}{P(y\neq c_k|X=x)} \\
&= arg \space \underset{y\in \mathcal{Y}}{min}\sum_{k=1}^{K}{(1-P(y\neq c_k|X=x))} \\
&= arg \space \underset{y\in \mathcal{Y}}{max}P(y=c_k|X=x)
\end{aligned}
\end{equation}
$$
由此，就根据期望风险最小化准则得到了后验概率最大化准则，也就是朴素贝叶斯法所采用的原理。

## 朴素贝叶斯法的参数估计

在朴素贝叶斯法中，模型的学习就是对 $P(Y=c_k)$ 和 $P(X^{(i)}=x^{(i)}|Y=c_k)$ 进行估计，估计的方法选择极大似然估计法。

对先验概率 $P(Y=c_k)$ 的极大似然估计为
$$
P(Y=c_k)=\frac{\sum_{i=1}^K{I(y_i=c_k)}}{N}
$$
$N$ 为训练样本个数。

设第 $j$ 个特征 $x^{(j)}$ 可能取值的集合为 $\{a_{j1},a_{j2},\cdots,a_{jS_j}\}$，条件概率 $P(X^{(i)}=a_{jl}|Y=c_k)$ 的极大似然估计为
$$
P(X^{(j)}=a_{jl}|Y=c_k)=\frac{\sum_{i=1}^N{I(x_i^{(j)}=a_{jl}},y_i=c_k)}{\sum_{i=1}^N{I(y_i=c_k)}}
$$
其中 $S_j = |x^{(j)}|$ 。

> 这里的联合概率直接利用训练数据中满足条件的进行计算即可。

### 利用贝叶斯估计改进估计效果

用极大似然估计可能会出现所要估计的概率值为 $0$ 的情况，影响后验概率的计算结果。为了解决这一问题可以采用贝叶斯估计。

条件概率的贝叶斯估计为
$$
P_\lambda(X^{(j)}=a_{jl}|Y=c_k)=\frac{\sum_{i=1}^N{I(x_i^{(j)}=a_{jl}},y_i=c_k)+\lambda}{\sum_{i=1}^N{I(y_i=c_k)+S_j \lambda}}
$$
先验概率的贝叶斯估计为
$$
P(Y=c_k)=\frac{\sum_{i=1}^K{I(y_i=c_k)+\lambda}}{N+K\lambda}
$$
其中 $\lambda \geqslant 0$，当 $\lambda = 0$ 时就是极大似然估计。常取 $\lambda = 1$，称为拉普拉斯平滑 (Laplacian smoothing)。
