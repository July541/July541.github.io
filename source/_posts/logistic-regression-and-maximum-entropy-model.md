---
title: '统计学习方法（五）：逻辑斯谛回归与最大熵模型'
tags:
  - machine learning
  - logistic regression
  - maximum entropy model
  - entropy
categories:
  - 统计学习方法
date: 2020-07-15 22:03:39
mathjax: true
---

# 逻辑斯谛回归与最大熵模型

logistic regression 是统计学习中的经典分类方法，其方法可简单理解为在 linear regression 上应用 sigmoid 进行映射, 将其转换为二分类问题。《统计学校方法》 一书通过 logistic distribution 进行推导。将 logistic 翻译成逻辑斯谛这种无意义的单词也太无聊了，后文将全部使用英文表述。

<!--more-->

## logistic regression model

首先介绍 logistic distribution 的定义。

设 $X$ 是连续随机变量，$X$ 服从 logistic distribution 是指 $X$ 具有下列分布函数和密度函数
$$
F(x) = P(X \leqslant x) = \frac{1}{1+e^{-(x-\mu)/\gamma}} \\
f(x) = F'(x) = \frac{e^{-(x-\mu)/\gamma}}{\gamma (1 + e^{-(x-\mu)/\gamma})^2}
$$

利用参数化的 logistic distribution 可以得到 logistic model。通常情况 logistic model 用于进行二值分类，也可以扩展到多值分类。对于二值分类，有如下概率分布
$$
P(Y = 1 | X) = \frac{e^{(w\cdot x+b)}}{1 + e^{(w \cdot x + b)}} = \frac{1}{1 + e^{-(w \cdot x + b)}} \\
P(Y = 0 | X) = \frac{1}{1 + e^{(w \cdot x + b)}} = \frac{e^{-(w \cdot x + b)}}{1 + e^{-(w \cdot x + b)}}
$$
现考察 logistic regression model 的特点，一个事件发生的几率 (odds) 是指该事件发生的概率与该事件不发生的概率的比值。如果时间发生的概率是 $p$，则该事件发生的几率是 $\frac{p}{1-p}$，该事件的对数几率 (log odds) 即 logit 函数为
$$
logit(p) = log\frac{p}{1-p}
$$
对 logistic regression，有
$$
log \frac{P(Y=1|X)}{1-P(Y=0|X)}=w \cdot x + b
$$
也就是在 logistic regression model 中，输出 $Y=1$ 的对数几率是输入 $x$ 的线性函数，即证明了 linear regression 上应用 sigmoid 的可行性。

### 模型参数估计

模型的参数估计可以用极大似然估计法估计模型参数。令 $h(x) = P(Y=1|X)$, $P(Y=0|X) = 1 - h(x)$，有似然函数
$$
\prod_{i=1}^N{[h(x)]^{y_i} [1-h(x)]^{1-y_i}}
$$
为了简化计算，两边取对数得到对数似然函数
$$
L(w) = \sum_{i=1}^N{[y_i\space log\space h(x_i) + (1-y_i) log(1-h(x_i))]}
$$
为了得到上述函数的极大值，可使用梯度上升求得 $w$。

## 最大熵模型

### 最大熵原理

设离散随机变量的概率分布时 $P(X)$，则其熵为
$$
H(P) = -\sum_{x}P(x) \space log \space P(x)
$$
熵的取值满足如下不等式
$$
0 \leqslant H(P) \leqslant log \space |X|
$$
其中，$|X|$ 是 $X$ 的取值个数，当且仅当 $X$ 的分布是均匀分布时右边的等号成立。也就是说，当 $X$ 服从均匀分布时，熵最大。

**举一个🌰**

设随机变量 $X$ 有五个取值 $\{A,B,C,D,E\}$，要估计取各个值当概率 $P(A)$，$P(B)$，$P(C)$，$P(D)$，$P(E)$。

首先有
$$
P(A) + P(B) + P(C) + P(D) + P(E) = 1
$$
在没有其他任何信息的情况下，一个对概率分布合理的估计方法就是假设这个分布中取各个值的概率是相等的，即
$$
P(A) = P(B) = P(C) + P(D) + P(E) = \frac{1}{5}
$$
等概率表示了对事实的无知。因为没有更多知识，这种判断是合理的。

若此时知道
$$
P(A) + P(B) = \frac{3}{10}
$$
则可认为 $A$ 与 $B$ 是等概率的，$C$，$D$ 与 $E$ 是等概率的，即
$$
P(A) = P(B) = \frac{3}{20} \\
P(C) = P(D) = P(E) = \frac{7}{30}
$$
这种概率的估计方法正是最大熵原理的应用。

### 最大熵模型的定义

最大熵模型利用最大熵原理进行分类。首先考虑模型应该满足的条件。给定训练数据集，联合分布 $P(X,Y)$ 的经验分布 $\tilde{P}(X,Y)$ 和边缘分布 $P(X)$ 的经验分布 $\tilde{P}(X)$ 分别为
$$
\tilde{P}(X=x,Y=y) = \frac{\nu(X=x,Y=y)}{N} \\
\tilde{P}(X=x) = \frac{\nu(X=x)}{N}
$$
其中，$\nu(X=x,Y=y)$ 表示训练数据中样本 $(x,y)$ 出现的频数，$\nu (X=x)$ 表示训练数据中输入 $x$ 出现的频数，$N$ 表示训练样本容量。

用特征函数 (feature function) $f(x,y)$ 描述输入 $x$ 和输出 $y$ 之间的某一个事实 (例如 $P(A) = \frac{3}{10}$)，其定义为
$$
f(x,y) = \left\{\begin{matrix}
\begin{align}
& 1 , \space \text{x and y satisfy a certain fact}\\ 
& 0 , \space otherwise
\end{align}
\end{matrix}\right.
$$
特征函数 $f(x,y)$ 关于经验分布 $\tilde{P}(X,Y)$ 的期望值用 $E_{\tilde{P}}(f)$ 表示，有
$$
E_{\tilde{P}}(f) = \sum_{x,y}\tilde{P}(x,y)f(x,y)
$$
特征函数 $f(x,y)$ 关于模型 $P(Y|X)$ 与经验分布 $\tilde{P}(X)$ 的期望值用 $E_P(f)$ 表示，有
$$
E_P(f) = \sum_{x,y}\tilde{P}(x)P(y|x)f(x,y)
$$
在模型能从训练数据中获得足够信息的前提下，可以假设这两个期望值相等 (由$P(A,B) = P(A|B)P(B)$)，即
$$
E_P(f) = E_{\tilde{P}}(f)
$$
以上式为约束条件，有最大熵模型的定义：

假设满足所有约束条件的模型集合为
$$
C \equiv \{P \in \wp | E_P(f_i)=E_\tilde{P}(f), i=1,2,\cdots,n\}
$$
定义在条件概率分布 $P(Y|X)$ 上的条件熵为
$$
H(P)=-\sum_{x,y}{\tilde{P}(x)P(y|x)logP(y|x)}
$$
则模型集合 $C$ 中的条件熵 $H(P)$ 最大的模型称为最大熵模型。式中的对数为自然对数 $e$。

### 最大熵模型的学习

最大熵模型的学习可以形式化为约束最优化问题
$$
\underset{P \in C}{max} H(P) = -\sum_{x,y}{\tilde{P}(x)P(y|x)logP(y|x))} \\
s.t. \quad E_p(f_i) = E_{\tilde{P}}(f_i), \space 1, 2, \cdots, n \\
\sum_{y}{p(y|x)} = 1
$$
按照最优化问题的习惯，将最大值求解问题改为等价的求最小值问题
$$
\underset{P \in C}{min} \space-H(P) = \sum_{x,y}{\tilde{P}(x)P(y|x)logP(y|x))} \\
s.t. \quad E_p(f_i) = E_{\tilde{P}}(f_i), \space 1, 2, \cdots, n \\
\sum_{y}{p(y|x)} = 1
$$
求解上述问题得出的解就是最大熵模型的解。

为了解决这个问题，首先利用拉格朗日乘数法将带约束的优化问题转化为无约束的优化问题，引入拉格朗日乘子 $w_0,w_1,\cdots,w_n$，定义拉格朗日函数 $L(P,w)$
$$
\begin{equation}\nonumber
\begin{aligned}
L(P,w) & \equiv -H(P) + w_0\left (1-\sum_{y}{P(y|x)} \right) + \sum_{i=1}^n{w_i(E_{\tilde{P}}(f_i) - E_P(f_i))} \\
&= \sum_{x,y}{\tilde{P}(x)P(y|x)logP(y|x)} + w_0\left (1 - \sum_{y}{P(y|x)}\right ) + \\
& \quad \sum_{i=1}^n{w_i\left (\sum_{x,y}{\tilde{P}(x,y)f_i(x,y)} - \sum_{x,y}{\tilde{P}(x)P(y|x)(f_i(x,y)}\right)}
\end{aligned}
\end{equation}
$$
有原始优化问题
$$
\underset{P\in C}{min}\space\underset{w}{max} \space L(P,w)
$$
则其对偶问题为
$$
\underset{w}{max} \space \underset{P\in C}{min} \space L(P,w)
$$
由我现在还是不懂的 KKT 规则可以知道原问题和对偶问题是等价的。先求解 $\underset{P\in C}{min}\space L(P,w)$，对 $P(y|x)$ 求偏导数
$$
\begin{equation}\nonumber
\begin{aligned}
\frac{\partial L(P,w)}{\partial P(y|x)} &= \sum_{x,y}{\tilde{P}(x)(logP(y|x)+1)} - \sum_y{w_0} - \sum_{x,y}{\left( \tilde{P}(x)\sum_{i=1}^n{w_i f_i(x,y)}\right)} \\
&= \sum_{x,y}{\tilde{P}(x)\left( logP(y|x) + 1 - w_0 - \sum_{i=1}^n{w_if_i(x,y)}\right)}
\end{aligned}
\end{equation}
$$
令偏导数为 $0$，由于 $\tilde{P}(x) > 0$，有
$$
P(y|x) = exp\left( \sum_{i=1}^n{w_i f_i(x,y) + w_0 - 1}\right)=\frac{exp\left( \sum_{i=1}^n{w_i f_i(x,y)}\right)}{exp(1-w_0)}
$$
又因为 $\sum_y{P(y|x) = 1}$，则有
$$
\sum_y{P(y|x)} = \sum_y{\frac{exp\left( \sum_{i=1}^n{w_i f_i(x,y)}\right)}{exp(1-w_0)}} = \frac{\sum_yexp\left( \sum_{i=1}^n{w_i f_i(x,y)}\right)}{exp(1-w_0)} = 1
$$
即
$$
exp(1-w_0) = \sum_yexp\left( \sum_{i=1}^n{w_i f_i(x,y)}\right)
$$
得最优解
$$
arg \space \underset{P\in C}{min} \space L(P,w) = P_w(y|x)= \frac{1}{Z_w(x)}exp\left( \sum_{i=1}^n{w_i f_i(x,y)}\right)
$$
其中
$$
Z_w(x) = \sum_yexp\left( \sum_{i=1}^n{w_i f_i(x,y)}\right)
$$
这里 $Z_w(x)$ 被称为规范化因子。

现在可以对上述最小值进行极大化，求 $\underset{w}{\bf{max}} \space \underset{P\in C}{min} \space L(P,w)$ 对应的 $w$，可直接求解，也可用梯度下降法等迭代法求解。
