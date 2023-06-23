---
title: 'Haskell 的 Kind 系统入门'
tags:
  - type system
  - kind system
categories:
  - Haskell
date: 2021-03-20 22:05:30
mathjax: true
---

# Haskell 的 Kind 系统入门

本文是对[这篇](https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/)文章的翻译，文章通过与 type 的对比帮助读者入门 kind 理论。

> 文中所有的代码基于 `GHC 8.4.3`。

<!-- more -->

## Types 和 Kinds

简单来说

> 正如表达式 (values/terms) 都有类型 (types)，types 也有自己对应的 kinds。

 例如 `"hello"` 和 `"world"` 都属于 `String` 类型，`True` 和 `False` 都属于 `Bool` 类型。相似地，`String` 类型和 `Bool` 类型都属于 `*`（读作 "star"）kind。
![State type](/imgs/haskells-kind-system-a-primer/kind-system001.svg)

GHCi 中可以使用 `:t` 或 `:type` 来查看某个表达式的类型，用 `:k` 或 `:kind` 可以查看某个类型对应的 kind。

``` haskell
λ> :t True
True :: Bool

λ> :k Bool
Bool :: *
```

在标准的 Haskell 中所有的原始 (inhabited) 类型 (即至少有一个取值的类型) 都属于 `*` 的 kind。因此 `Int`、`Int -> String`、`[Int]`、`Maybe Int`、`Either Int Int` 这些至少拥有一个取值的类型都属于 `*` 的 kind。

> 一个明显无法构造的例外是 `data Void`，这种类型没有取值，其只能由无法成功完成的语句来产生，例如 `undefined` 或无限循环 `f x = f x`。

不是所有的类型都是原始类型，例如 `Maybe` 和 `Either` 就不属于，对于 `Maybe` 来说，没有任何值可以取，即没有任何表达式 (或值) 属于 `Maybe` 类型，不同于 `Just True` 属于 `Maybe Bool` 类型，`Just 'C'` 属于 `Maybe Char` 类型，我们无法构造一个属于 `Maybe` 的表达式。

``` haskell
λ> x = undefined :: Maybe
<interactive>:9:18: error
    • Expecting one more argument to ‘Maybe’

λ> f x = f x :: Maybe
<interactive>:10:14: error:
    • Expecting one more argument to ‘Maybe’
```

那么 `Maybe` 和 `Either` 属于什么呢？它们实际是类型构造器 (type constructor)。

## Data constructors 和 Type constructors

正如可以利用数据构造器 (data constructor) 来构造一个 data，我们也可以利用类型构造器 (type constructor) 来创建 types。

``` haskell
λ> data Person = MkPerson { name :: String, age :: Int }

λ> :t MkPerson
MkPerson :: String -> Int -> Person
```

如上面的代码所示，`MkPerson` 是一个数据构造器，通过接受类型为 `String` 的 `name` 和类型为 `Int` 的 `age` 来构造一个 `Person` 类型的值，换句话说，`MkPerson` 具有 `String -> Int -> Person` 的类型。

``` haskell
λ> data Either a b = Left a | Right b

λ> :k Either
Either :: * -> * -> *
```

相似地，`Either` 是一个接受两个 kind 为 `*` 的类型来构造一个新的类型为 `*` 的类型构造器，即 `Either` 是一个具有 `* -> * -> *` 的 kind 的类型。

> 这里要注意，`Either` 接受类型 `a` 和类型 `b` 来构造一个新的类型，`Left` 接受一个具有 `a` 类型的值来构造一个新的值。

数据构造器可以科里化 (currying)，类型构造器同样也可以。

``` haskell
λ> :t MkPerson
MkPerson :: String -> Int -> Person
λ> :t MkPerson "Diogo"
MkPerson "Diogo" :: Int -> Person
λ> :t MkPerson "Diogo" 29
MkPerson "Diogo" 29 :: Person
```
![](/imgs/haskells-kind-system-a-primer/kind-system002.svg)

``` haskell
λ> :k Either
Either :: * -> * -> *
λ> :k Either String
Either String :: * -> *
λ> :k Either String Int
Either String Int :: *
```
![](/imgs/haskells-kind-system-a-primer/kind-system003.svg)

## Type signatures 和 Kind signatures

GHC 不仅可以正确推断出一个表达式的类型，也可以推断出一个类型对应的 kind。

``` haskell
-- `x` 被推断为`Bool` 类型
x = True

-- `y` 被推断为 `String -> IO ()` 类型
y = putStrLn
```

``` haskell
-- `a` 被推断为 `*` kind
data List a = Cons a (List a) | Nil

-- `f` 被推断为 `* -> *` kind
-- `a` 和 `b` 被推断为 `*` kind
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
```

正如我们可以手动标明某个值的类型，我们也可以开启 `KindSignatures` 扩展来手动标明某个类型的 kind。

``` haskell
x :: Bool
x = True

y :: String -> IO ()
y = putStrLn
```

``` haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

data List (a :: *) = Cons a (List a) | Nil

class Functor (f :: * -> *) where
  fmap :: forall (a :: *) (b :: *). (a -> b) -> (f a -> f b)
```

这里启用了 `ExplicitForAll` 扩展来显示地标明每个类型的 kind。

## HOFs 和 HKTs

正如我们拥有高阶函数 (higher-order functions, HOFs) 这种可以接受其他函数作为参数的函数，我们也可以拥有高阶类型 (higher-kinded types, HKTs) 这种接受其他类型构造器作为参数的类型构造器。

``` haskell
λ> :t map
map :: (a -> b) -> [a] -> [b]
```

这里 `map` 是接受两个参数的函数，这两个参数分别是一个 `a -> b` 类型的函数和一个 `a` 类型的 list。

``` haskell
λ> data NonEmpty f a = MkNonEmpty { head :: a, tail :: f a }

λ> :k NonEmpty
NonEmpty :: (* -> *) -> * -> *
```

相似地，`NonEmpty` 是接受两个参数的类型构造器，这两个参数分别是一个具有 `* -> *` 的 kind 的类型构造器和一个具有 `*` 的 kind 的类型。

当给 `NonEmpty` 应用 `[]` 和 `Bool` 两个类型时，可以得到 `NonEmpty [] Bool` 类型，即一个保证至少有一个元素存在的 `Bool` 类型的 list。

``` haskell
λ> :t MkNonEmpty True [False, True]
MkNonEmpty True [False, True] :: NonEmpty [] Bool
```

我们可以将任何满足 kind 约束的类型应用在 `NonEmpty` 上构造我们需要的类型，例如：`NonEmpty [] Int`、`NonEmpty Tree String` 和 `NonEmpty Vector Char`。

## kind 的其他内容

### Unboxed/unlifted types

在文章的开头说过所有的原始类型都具有 kind `*`，更标准的表达应该是：在标准 Haskell 中，`*` 是所有装箱的 (boxed) [或升格 (lifted)] 的原始类型的 kind。然而，在 GHC 对 Haskell 的实现中，有一些原始类型是未装箱 (unboxed) [或未升格 (unlifted)] 的。它们是 `ghc-prim` 包 (package) 中的 `GHC.Prim` 模块 (module)。为了方便起见，所有的未升格的类型都用 `#` 结尾，它们被称为 *magic hash*，需要开启 `MagicHash` 扩展。例如 `Char#` 和 `Int#`，甚至可以拥有未装箱的 tuple `(# a, b #)` 和未装箱的 `(# a | b #)`。

每一个未升格的类型都拥有一个描述其运行时状态的 kind。这个状态标明该类型是否是一个指向某个堆的指针、是否是一个有符号的 *word-size* 值。编译器随后会利用类型的 kind 信息来确定生成对应的机器码，这一过程叫做类型指向的编译 (kind-directed compilation)。

下图是一个具体的例子：
![Unboxed types and kinds](/imgs/haskells-kind-system-a-primer/kind-system007.svg)

后面会对 `TYPE` 做介绍。

### Constraint

`Constraint` kind 是用来描述所有需要受到约束的类型的 kind，它通常出现在 $\Rightarrow$ 的左边，包括 typeclass 的限制、多个 typeclass tuple 组成的限制等。

``` haskell
λ> :t show
show :: Show a => a -> String

λ> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

如上所示，`show` 函数接受的 `a` 类型的参数需要是 `Show` 类型的实例，即 `Show` 一个具有 `Constraint` kind 的类型，`fmap` 同理。

```haskell
λ> :k Show
Show :: * -> Constraint

λ> :k Show Int
Show Int :: Constraint

λ> :k Functor
Functor :: (* -> *) -> Constraint

λ> :k Functor IO
Functor IO :: Constraint
```

利用 `ConstraintKinds` 扩展可以将约束作为一等公民使用达到令人惊奇的效果。例如 `Set` 不是 `Functor` 的实例，原因是 `Set.map` 拥有 `Ord` 的约束，而 `fmap :: (a -> b) -> f a -> f b` 并不满足。

``` haskell
λ> :t Data.Set.map
Data.Set.map :: Ord b => (a -> b) -> Set a -> Set b
```

可以看到 `Set.map` 需要 `b` 是 `Ord` 的实例。

通过 `ConstraintKinds` 我们可以实现一个既可以拥有约束也可以没有约束的 `Functor` typeclass。

``` haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Kind (Constraint)
import qualified Data.Set as Set
import qualified Data.List as List

class GFunctor (c :: * -> Constraint) (f :: * -> *) | f -> c where
  gfmap :: c b => (a -> b) -> (f a -> f b)

instance GFunctor Ord Set.Set where
  gfmap = Set.map

instance GFunctor EmptyConstraint [] where
  gfmap = List.map
```

其中 `EmptyConstraint` 是一个满足所有类型的约束。

``` haskell
{-# LANGUAGE FlexibleInstances #-}

class EmptyConstraint a
instance EmptyConstraint a
```

### Datatype promotion

在标准 Haskell 中，可以利用 `data` 关键字定义由一个或多个数据构造器定义的类型或类型构造器。

``` haskell
data ConnectionStatus = Open | Closed
```

![ConnectionStatus type](/imgs/haskells-kind-system-a-primer/kind-system004.svg)
当开启 GHC 的 `DataKinds` 扩展后，`data` 关键字会在原有的基础上增加一个自定义的 `kind` 和满足该 `kind` 的类型或类型构造器。

``` haskell
{-# LANGUAGE DataKinds #-}

data ConnectionStatus = Open | Closed
```

上述代码会创建一个新的 `ConnectionStatus` kind，该 kind 拥有两个非原始的类型，分别是 `'Open` 和 `'Close`。
![ConnectionStatus kind](/imgs/haskells-kind-system-a-primer/kind-system005.svg)

此时，我们说 `ConnectionStatus` 已经被提升 (promoted) 为了拥有 `Open` 和 `Close` 两个类型的 kind，注意到这里 `Open` 和 `Close` 都省略了 `'`前缀，该前缀在大多数情况下可以省略，只有在[极少数情况下](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#distinguishing-between-types-and-constructors)需要写明以避免歧义。

通过利用新生成的 kind，我们可以定义**只**接受被实例化为 `Open` 和 `Closed` 的类型变量。

``` haskell
{-# LANGUAGE KindSignatures #-}

data Connection (s :: ConnectionStatus) = MkConnection ...

newConnection     :: Address           -> Connection Closed
openConnection    :: Connection Closed -> Connection Open
closeConnection   :: Connection Open   -> Connection Closed
connectionAddress :: Connection s      -> Address
```

``` haskell
λ> :k Connection Int
<interactive>:1:12: error:
    • Expected kind ‘ConnectionStatus’, but ‘Int’ has kind ‘*’
```

通过连接状态，我们可以在编译期实现例如 `closeConnection` 只能在开启的连接中被调用，而不能在关闭的连接中被调用的限制。

这种功能并不仅限于我们自定义的 `data`，例如在开启 `DataKinds` 扩展后，`Bool` 类型也可以被提升为 kind。

``` haskell
λ> data F (b :: Bool) = MkF deriving Show

λ> MkF :: F 'True
MkF

λ> MkF :: F 'False
MkF

λ> MkF :: F 'Open
<interactive>:30:10: error:
    • Expected kind ‘Bool’, but ‘ 'Open’ has kind ‘ConnectionStatus’
```

### GHC.TypeLits

GHC 还提供了两个方便的未装箱的 kind， 它们被定义在 *base* 中的 `GCH.TypeLits` 模块下。

第一个是 `Symbol`，`Symbol` 是类型层面 (type-level) 的字符串表示的 kind。例如我们可以将 `"hello"` 字符串作为一种类型使用，这时 `"Hello"` 就拥有 `Symbol` 的 kind。
![Symbol kind](/imgs/haskells-kind-system-a-primer/kind-system006.svg)

这样我们就可以用字符串字面量来表示一种类型了。

``` haskell
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol)
import Data.Ratio ((%))

newtype Money (currency :: Symbol) = Money Rational

fivePence :: Money "GBP"
fivePence = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros = Money 2
```

将货币在类型层面表示，而不是在表达式层面 (term-level) 上表示 (例如 `data Money = Money String Rational`) 可以静态地有效避免将不同货币弄混，犯下例如将英镑和欧元直接相加这样的错误。

``` haskell
add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)
```

``` haskell
λ> add fivePence fivePence
Money (1 % 10)

λ> add fivePence twoEuros
<interactive>:8:15: error:
    • Couldn't match type ‘"EUR"’ with ‘"GBP"’
```

如果只在表达式层面进行定义，那么在相加货币时必须对货币的类型进行运行时检查，相关的函数也要添加类似 `Maybe` 、`Either` 或 `MonadError e m` 这种表示失败的返回结果。

> 一个有趣的事实：上面的例子不是仅仅只为演示而虚构的，这是真实存在的 *safe-money* 库的[表示方式](https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Dense)。

此外，如果我们不需要对金钱进行数学运算的话，将货币值用 `Rational` 保存显得过于浪费了一点。因此 *safe-money* 库还提供了另一种对 `Integer` 简单包装的 [Discrete](https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Discrete-39-) 类型来满足不需要进行运算的场景。要注意的是，根据 *safe-money* 库官方的[解释](https://ren.zone/articles/safe-money#scales)，用 `Integer` 表示货币值时仅仅保存货币值本身还不够，还应该记录货币的单位间的换算关系。例如表示美元时，这种关系就是 1 比 1，而表示美分时，这种关系就变成了 100 比 1 (100 美分换 1 美元)。

为了在类型层面表示这种换算关系，需要用到在 `GHC.TypeLits` 里定义的另一个在类型层面表示自然数的 kind `Nat`。它的用法和 `Symbol` 相似，只需要将字符串字面量换成数字就可以了。

``` haskell
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol, Nat)

newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1)
oneDollarThirtyCents = Discrete 130
```

其中 `scale :: (Nat, Nat)` 中的 tuple `(,)` 是通过 `DataKinds` 提升为 kind 的，同时 `'(,)` 这一类型构造器也是由原始的数据构造器提升来的。

### Kind polymorphism

参数多态性 (parametric polymorphism) 广泛存在于 Haskell 中，它可以帮助我们对类型进行抽象。通过开启 `PolyKinds` 扩展可以达到对 kind 更高程度的抽象。

让我们来看一个例子：

``` haskell
data Proxy a = MkProxy
```

这里 `Proxy` 是接受一个类型变量的类型构造器，它拥有一个不带参数的数据构造器。在我们手边没有任何数据时，使用 `Proxy` 传递类型十分方便。

> `Proxy` 已经被定义在 *base* 中的 `Data.Proxy` 模块中，本文中的实现是为了演示 kind 的逻辑，要更深入地了解 `Data.Proxy` 及其使用的例子，可以阅读 Kwang Seo 的[这篇](https://kseo.github.io/posts/2017-01-15-data-proxy.html)博客。

``` haskell
λ> intRepresentative = MkProxy :: Proxy Int

λ> stringRepresentative = MkProxy :: Proxy String
```

有一个问题是默认情况下，GHC 会假设 a 具有 `*` 的 kind，这意味着 `Proxy` 仅适用于具有 `*` 的 kind 的类型，无法适用于所有的类型。

``` haskell
λ> :k Proxy
Proxy :: * -> *

λ> maybeRepresentative = MkProxy :: Proxy Maybe
<interactive>:9:38: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
```

正常情况下，我们需要应该为所有可能的 kind 创建下面这样属于其的 `Proxy` 类型。

``` haskell
data Proxy a = MkProxy
data Proxy1 (a :: * -> *) = MkProxy1
data Proxy2 (a :: * -> * -> *) = MkProxy2
data HKProxy1 (a :: (* -> *) -> * -> *) = MkHKProxy1

stringRepresentative   = MkProxy    :: Proxy String
maybeRepresentative    = MkProxy1   :: Proxy1 Maybe
eitherRepresentative   = MkProxy2   :: Proxy2 Either
nonEmptyRepresentative = MkHKProxy1 :: HKProxy1 NonEmpty
```

很明显上面这种写法不具有扩展性，但是在开启 `PolyKinds` 扩展后我们可以创建适用于所有具有 `k` 的 kind 类型 的 `Proxy`。

``` haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

data Proxy (a :: k) = MkProxy

-- Now `a` can be anything at all
maybeRepresentative    = MkProxy :: Proxy Maybe
nonEmptyRepresentative = MkProxy :: Proxy NonEmpty
functionRepresentative = MkProxy :: Proxy (->)
helloRepresentative    = MkProxy :: Proxy "hello"
showRepresentative     = MkProxy :: Proxy Show
functorRepresentative  = MkProxy :: Proxy Functor
openRepresentative     = MkProxy :: Proxy 'Open
```

事实上，我们可以省略 `a :: k` 由 GHC 自动推断。

``` haskell
λ> :set -XPolyKinds
λ> data Proxy a = MkProxy

λ> :k Proxy
Proxy :: k -> *
```

*[Servant](https://docs.servant.dev/en/stable/)* 是另一个使用 kind polymorphism 的例子，*Servant* 用于编写类型安全的 web APIs。其中有一个名为 `:>` 的类型用于将一个 API 的各个组件 (component) 组合成一个拥有完整类型信息的用例。这些组件可能拥有不同的类型和不同的 kind。

```  haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

data (:>) (a :: k) (b :: *)
infixr 4 :>
```

通过这样可以描述请求的响应，例如 `POST /books`，通过使用 `:>` 将 `"books" :: Symbol`、`ReqBody '[JSON] Book :: *` 和 `Post '[JSON] () :: *` 结合在一起。

``` haskell
type BooksAPI = "books" :> ReqBody '[JSON] Book :> Post '[JSON] ()
```

### Levity polymorphism

在一些情况下，我们可能需要对升格的类型和未升格的类型做统一的抽象。例如 `error` 函数接受一个错误信息并抛出异常：

``` haskell
{-# LANGUAGE ExplicitForAll #-}

error :: forall a. String -> a
```

由于 `error` 的返回值具有多态性，它可以应用于大多数的场景：

``` haskell
increment :: Int -> Int
increment x = error "oops"
```

但是因为 `a` 会被默认推断为具有 `*` 的 kind，因此我们无法将其应用于未升格的类型：

``` haskell
incrementUnlifted :: Int# -> Int#
incrementUnlifted x = error "oops"
```

``` haskell
<interactive>:22-40: error
   • Couldn't match a lifted type with an unlifted type
```

在现有的知识下，我们可能会想到开启 `PolyKinds` 并引入 kind 变量 `k` 来实现上面的目的，但是这并不能成功：

``` haskell
error :: forall k (a :: k). String -> a
```

``` haskell
/Playground.hs:15:40: error:
    • Expected a type, but ‘a’ has kind ‘k’
    • In the type signature: error :: forall k (a :: k). String -> a
```

原因是 `a :: k` 对原始类型以外的其他类型没有意义。例如 `k` 是 `Symbol`，`a` 是 `"hello"`，当 `"hello"` 不是原始类型时，`error` 应该返回什么呢？

为了让 `error` 可以适用于所有的原始类型 (升格的和未升格的)，我们需要 *levity polymorphism*。

秘诀在于我们之前提到的 `TYPE r` kind。这种 kind 是 `r :: RuntimeRep` 的参数化的结果，描述了类型的运行时状态，其取值可以为下列之一：

``` haskell
data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | LiftedRep       -- ^ lifted; represented by a pointer
                | UnliftedRep     -- ^ unlifted; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | WordRep         -- ^ unsigned, word-sized value
                | Int64Rep        -- ^ signed, 64-bit value (on 32-bit only)
                | Word64Rep       -- ^ unsigned, 64-bit value (on 32-bit only)
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number
```

我们之前已经见过 `TYPE 'IntRep` 是未升格的 integer 的 kind，`TYPE 'FloatRep` 是未升格的 float 的kind。对于 `TYPE 'LiftRep` 则是所有升格的类型的 kind，事实上，`*` 就是 `TYPE 'LiftRep` 的[同义词](https://hackage.haskell.org/package/ghc-prim-0.5.2.0/docs/GHC-Types.html#t:-42-)。

所以，实际上我们可以用 `TYPE r` 表示未升格的类型和已经升格的类型。

``` haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

import GHC.Exts (TYPE, RuntimeRep)

error :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
```

此时，`error` 就可以同时被应用于 `increment` 和 `incrementUnlifted` 了。

levity polymorphism 也有其自身的限制，有些地方并不能使用，[这里](https://www.youtube.com/watch?v=lSJwXZ7vWBw)可以了解到具体不能使用的地方和不能使用的原因。

## 总结

到目前为止读者可能会好奇：上面的这些东西有什么意义？为什么要有 kind 系统？为什么我们会关心 kind？实际上，在 Java 或 C# 等语言中所有的泛型类型 `<T>` 都属于 `*` 的 kind，我们无法得到例如 `Functor<List>` 的这种拥有 `* -> *` 的 kind 的类型。如果没有 kind 系统，我们无法定义 functor 和 monad，甚至之前提到的 `NonEmpty f a` 都无法做到。更进一步，利用 levity polymorphism，我们可以[实现](https://gist.github.com/dcastro/a7f9730981fa404415588224350dc918)同时适用于升格类型和未升格类型的抽象，而 Java 只能实现适用于升格类型的泛型变量。

同时，kind 系统打开了通向类型层面编程的大门，类似于 `Connection s` 和 `Money c` 等可扩展的记录 (record)、*Servant* 的类型层面的 web API都可以通过 kind 实现。Simon Peyton Jones 整合前沿研究，讨论了在 Haskell 中的 practical linearity，通过这种方式可以给函数类型 `(->) a b` 一个额外的拥有 `Linearity` 的 kind 类型变量 `l` ，这里 `Linearity` 是类型 `Omega` 和 `One` 的整合。

另一个读者可能会问的问题是：如果所有的表达式都有类型、所有的类型都有 kind，那么所有的 kind 会有什么？在 GHC 7 及其之前的版本中，kind 拥有的是 *sorts*，所有的 kind 都有一个[唯一的 sort `Box`](https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/promotion.html)，这些 sort 仅存在与 GHC 的内部，对于我们开发人员来说是不可见的。

在 GHC 8 中，情况有了一点变化。读者可能注意到，本文尝试比较类型和 kind 之间的相似程度：包括都可以实现高阶、都可以有多态性、都可以被推断、都可以进行科里化。这并不是巧合！随着 GHC 8 中 `TypeInType` 扩展的到来，类型 (type) 和 kind (包括 sort) 都称为了同一个东西。如之前类型属于 kind，现在类型可以属于其他的类型。`3` 属于类型 `Int`，`Int` 属于类型 `*`，`*` 属于类型 `*`。这种统一为 Haskell 实现完全类型依赖铺平了道路。

在最近的论文中，读者可能会看到 kind `*` 被认为是 Type (和前文的 `TYPE r` **不是**同一个东西)，~~目前 (2018 年) 这两个词是[同义词](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Kind.html#t:Type)~~，(2021 年已[取消](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Kind.html#t:Type)) ，已逐步过渡为 `Type`。

> 如果想要了解更多关于类型依赖的知识，可以阅读 *Type-Driven Development with Idris* [这本书](https://www.manning.com/books/type-driven-development-with-idris) 或最新发布的 *The Little Typer* [这本书](https://mitpress.mit.edu/books/little-typer)。
