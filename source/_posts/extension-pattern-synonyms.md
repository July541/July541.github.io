---
title: 'Haskell 扩展 -- PatternSynonyms'
tags:
  - haskell extensions
categories:
  - Haskell
date: 2021-02-23 22:21:45
---

# PatternSynonyms

PatternSynonyms 用来处理在进行模式匹配 (pattern matching) 时模式名称过长的问题，通过给模式一个简化的名称来实现降低代码量和增加易理解程度的功能。PatternSynonyms 开启需要显式指明 `{-# LANGUAGE PatternSynonyms #-}`。

<!--more-->

例如有如下实现:
```haskell
newtype Type = T String [Exp]

collectTarget :: Exp -> [Exp]
collectTarget (T "refer" [t1, t2]) = t1 : collectTarget t2
collectTarget _ = []
```

在匹配我们想要的特殊的模式时，可以开启 PatternSynonyms 扩展来减少匹配中的代码量。

```haskell
pattern Refer :: Type -> Type -> Type
pattern Refer t1 t2 = T "refer" [t1, t2]
```

这里 Refer 是接收两个 Type 的 pattern synonyms，pattern 的签名可以省略。有了 Refer 后上面的 `collectTarget` 就可以改为

```haskell
collectTarget :: Type -> [Type]
collectTarget (Refer t1 t2) = t1 : collectTarget t2
collectTarget _ = []
```

## Uni-directional & bidirectional
上面的 pattern 还可以写为
```haskell
pattern Refer t1 t2 <- T "refer" [t1, t2]
```

区别是将 `=` 替换为了 `<-`。这两种写法的区别在于 `=` 是 bidirectional pattern synonyms，而 `<-` 是 Uni-directional 的。bidirectional 允许将创建的 pattern 作为一个具体的 Constructor 来使用，uni-directional 则不行。

```haskell
refers :: [Type] -> Type -> Type
refers = flip $ foldr Refer

>>> refers [App "Int" [], App "Bool" []] (App "Double" [])
-- App "->" [App "Int" [],App "->" [App "Bool" [],App "Double" []]]
```

在使用 `=` 后，可以将 Refer 作为表达式的一部分进行使用。


## References

1. [pattern synonyms](https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms)
2. [24 Days of GHC Extensions: Pattern Synonyms](https://ocharles.org.uk/posts/2014-12-03-pattern-synonyms.html)