---
title: 'Deriving 详解'
tags:
  - haskell extensions
  - deriving
categories:
  - Haskell
date: 2021-11-14 15:46:50
---

# Deriving 详解

本文介绍 Haskell 中 deriving 的详细用法。

## Deriving 的常规用法

对于如下代码：
```haskell
data A = MkA Int deriving (Show, Eq)
```
上面的类型 `A` 派生为了 `Show` 和 `Eq` 的实例，这使得我们可以打印 `A` 的实例，也可以对两个 `A` 类型的实例进行相等的比较，即：

```
λ> MkA 3
MkA 3
λ> MkA 3 == MkA 4
False
```

<!-- more -->

## StandaloneDeriving

Haskell 允许将类型的声明和派生分开写，这需要开启 `StandaloneDeriving` 扩展。

使用 `StandaloneDeriving` 有以下好处：
1. 可以使类型的定义和派生的实现不在同一个文件中
2. 可以实现更加具体的派生规则

例如：
```haskell
data A a = MkA a
deriving instance Eq a => Eq (A (Maybe a))
```
这样就允许 `MkA (Just 3) == MkA (Just 4)` 的比较，但是会拒绝 `MkA 3 == MkA 4`。

## 对 newtype 进行派生

看如下代码：

```haskell
newtype A = MkA Int deriving (Show)
```

在这里 `A` 实际上是对 `Int` 的一个包装，我们希望可以实现在 `A` 上的四则运算，当写下 `newtype A = MkA Int deriving (Show, Num)` 时，会发现编译器报错：

```
<interactive>:1:29: error:
    • Can't make a derived instance of ‘Num A’:
        ‘Num’ is not a stock derivable class (Eq, Show, etc.)
        Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
    • In the newtype declaration for ‘A’
```

因为编译器仍然在类型 `A` 上去实现对 `Num` 的推导，而 `A` 本身并不是 `Num` 类型的实例，所以无法执行。

此时可以启用 `GeneralisedNewtypeDeriving` 扩展，当普通的派生在 newtype 上无法实现时，会自动考虑使用 newtype 包装的类型。开启扩展后便可以实现在 `A` 上的四则运算了：

```haskell
λ> {-# LANGUAGE GeneralisedNewtypeDeriving #-}
λ> newtype A = MkA Int deriving (Show, Num)
λ> MkA 3 + MkA 4
MkA 7
```

## 派生任意 type class

```haskell
{-# LANGUAGE DeriveAnyClass #-}

class F a where
    f :: a
    f = undefined

data A = MkA deriving (Show, F)
```

在开启 `DeriveAnyClass` 扩展后，我们可以对任意的 class 进行派生。

## 派生策略

考虑上文提到的

```haskell
λ> {-# LANGUAGE GeneralisedNewtypeDeriving #-}
λ> newtype A = MkA Int deriving (Show, Num)
λ> MkA 3 + MkA 4
MkA 7
```

既然 `Num` 可以在 newtype 上派生，`Show` 是不是也可以？答案是肯定的，通过开启 `DerivingStrategies` 扩展，可以手动指定派生的规则：

``` haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class F a where
    f :: a
    f = undefined

newtype A = MkA Int
    deriving newtype (Show)
    deriving stock (Eq)
    deriving anyclass (F)
```

在上述代码中 `deriving newtype (Show)` 表示 `Show` 的派生依据 newtype 的策略，即 `MkA 3` 会被直接显示为 `3`，`deriving stock (Eq)` 表示用普通的策略派生 `Eq`，而 `deriving anyclass (F)` 则表明使用 *anyclass* 的策略进行派生。

## Deriving via

Deriving via 是一种更广泛的表明派生策略的方式，它允许依照已经是某一 typec class 的实例来进行派生，使用 Deriving via 需要开启 `DerivingVia` 扩展。

```haskell
newtype A = MkA Int
    deriving Show via Int
```

这里的 via 表明 `A` 类型根据 `Int` 的 `Show` 的方式进行派生。

via 可以更复杂，详见[官方文档](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html)。