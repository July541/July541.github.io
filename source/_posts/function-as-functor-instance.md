---
title: '函数作为函子实例的理解'
tags:
  - functor
  - monad
  - applicative
categories:
  - Haskell
date: 2021-09-13 20:29:00
---

# 函数作为函子实例的理解

`((->) r)`，或者说函数，也是函子 (Functor) 的实例，这个概念理解起来通常有一点困难，让我们通过一个例题来体验一下。

题目是判断一个给定的字符串是否为回文串。思路很简单，对于给定的字符串 `str`，只需要判断其反转后的结果是否与原串相同即可。

<!-- more -->

## Naive
最普通的写法如下：
```haskell
palindrome :: String -> Bool
palindrome str = str == reverse str
```

## Applicative
更进一步，考虑 `((->) r)` 是 Applicative 函子的实例，有
```
λ> :info Applicative
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘GHC.Base’
instance Applicative (Either e) -- Defined in ‘Data.Either’
instance Applicative [] -- Defined in ‘GHC.Base’
instance Applicative Maybe -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Applicative ((->) r) -- Defined in ‘GHC.Base’ -- 在这里
instance (Monoid a, Monoid b, Monoid c) =>
         Applicative ((,,,) a b c)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b) => Applicative ((,,) a b)
  -- Defined in ‘GHC.Base’
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
```

`((->) r)` 作为 Applicative 的实例实现如下

```haskell
instance Applicative ((->) r) where
  pure x = (\_ -> x) -- 即 pure = const
  f <*> g = \x -> f x (g x)
```

这里一定要弄清楚，`pure` 的签名是 `pure :: a -> (r -> a)`，因为 `((->) r)` 本身是函子，将 `a` 类型的东西应用于这个函子仍应该返回 `a` 类型本身。

`f` 的类型是 `f :: r -> (a -> b)`，即 `a -> b` 在 `((->) r)` 的上下文中，类似地，有 `g :: r -> a`，根据 `<*>` 的定义，我们需要返回 `r -> b`，即上述的实现（`x` 为 `r` 类型）。

此时考虑有 `((->) String)` 的函子，对其应用 `(==) :: String -> String -> Bool` 和 `reverse :: String -> String`，得到

```haskell
palindrome :: String -> Bool
palindrome = (==) <*> reverse
```

这里的 `(==) :: String -> String -> Bool` 处在 `((->) String)` 的上下文中，构成了新的类型 `String -> String -> String -> Bool`。让我们根据 `<*>` 的定义将其展开，有

```haskell
   (==) <*> reverse
== \str -> (==) str (reverse str)
== \str -> str == reverse str -- 熟悉吗 (doge)
```

以上就是回文数判断的另一种写法。

在研究 Applicative 的时候一定不要忘了 `liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c`，对于我们的题目来说，它的签名应该是 `liftA2 :: (String -> String -> Bool) -> (String -> String) -> (String -> String) -> (String -> Bool)`，即

```haskell
palindrome = liftA2 (==) id reverse -- 本题 id 和 reverse 的顺序可以互换
```

## Functor
当然也可以用 Funtor 去考虑，`((->) r)` 也是 Funtor 的实例。Functor 由于不带上下文，所以我们要手工添加一些上下文

```haskell
palindrome str = (== str) <$> reverse
```

此处 `(== str) :: String -> Bool`，`(<$>) :: (String -> Bool) -> (String -> String) -> (String -> Bool)`，值得注意的是 `<$>` 的两个参数中，第一个的 `String` 是函数本身的 `String`，而第二个参数 (`String -> String`) 的参数才是 `((->) r)` 中的 `r`。

## Monad
没错，`((->) r)` 也是 Monad 的实例，参考之前的想法，将 `((->) String)` 看做 `Monad m` 的 `m`，对于 `(>>=) :: (Monad m) => m a -> (a -> m b) -> m b`，第一个参数应该是一个 `r -> String` 类型的东西，而第二个参数应该是 `String -> (String -> Bool)`，有
```haskell
palindrome = reverse >>= (==)
```
