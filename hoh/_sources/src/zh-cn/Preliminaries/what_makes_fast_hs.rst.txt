.. _sec-lethargy:

拖慢程序运行速度的原因
======================

本章会展示一些特定条件下导致 Haskell 代码运行变慢的程序，这些程序具有很典型的特征，因此也被称为样板(canonical)程序。在阅读本章后，读者应该对 Haskell 程序运行变慢的原因有基本的了解。本章提到的这些程序只是一个简单的介绍，对于具体的细节将会在书中的后续部分进行详细解释。

.. _canonical-inlining:

内联
----

什么是内联
^^^^^^^^^^

内联 [#]_ 是一个几乎所有编译器都会使用的简单的优化技巧。内联核心的想法是将函数 ``f`` 的调用点展开为 ``f`` 本身的实现。例如：

.. code-block:: haskell
   :caption: 未执行内联

   > let f x = x * 3
   --- somewhere else
   > f (a + b) - c

上述代码首先定义了一个函数 ``f``，随后函数 ``f`` 应用于 ``f (a + b) - c``。内联机制会将 ``f`` 在调用点进行展开，``f (a + b)`` 会被替换为 ``f`` 本身的实现。

.. code-block:: haskell
   :caption: 执行内联

   > let f x = x * 3
   -- somewhere else
   > (a + b) * 3 - c

可以看到 ``f`` 的调用被替换为 ``x * 3``，其中 :math:`x \mapsto (a + b)`。

为什么要进行内联
^^^^^^^^^^^^^^^^

内联也被称为“所有优化的起点”。首先，内联减少了函数调用的开销，这对频繁调用的函数有很大益处。其次，通过将函数的调用点替换为函数本身的实现，编译器在后续可以对内联之后的结果做进一步的优化来生成运行速度更快的代码。这种性能上的优化与其说是计算机科学的应用，比如说是艺术的展现，任何微小的变化都可能会产生出意想不到的效果。

什么场景下内联会影响性能
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

内联操作本身并不会降低运行速度，而缺乏内联会影响后续可能的优化操作的执行，从而导致运行速度降低。然而，这并不意味着我们需要永远让 GHC 执行内联操作或手动强制内联，相反，我们有时可以通过禁止内联来实现性能优势。在后续的 :ref:`内联
<Inlining Chapter>` 一章中会对内联的性能效果做出分析，并展示 GHC 如何执行内联。

.. _canonical-fusion:

融合
----

什么是融合
^^^^^^^^^^

融合 [#]_ [#]_ (Fusion，也被称为 Deforestation，这里参考 Andy  Gill 论文中的例子，感谢 Andy Gill 的工作！) 是一种消除函数调用之间的中间状态和临时数据结构的优化技术。由于 Haskell 惯用的函数组合的写法，融合成为了一种关键的优化方法。例如：

.. code-block:: haskell

   all p xs = and (map p xs)

函数 ``all`` 通过 ``map`` 函数将 ``p`` 应用于 ``xs`` 的每个元素，在这个过程中产生了一个 bool 类型的 list。产生的 list 只是短暂地存在于计算的过程中，随后被应用于 ``and`` 产生最后的 bool 类型的结果。不幸的是，使用这个临时的 list 是十分昂贵的，需要在堆上为 list 的每个元素都申请内存、分配一个 ``Cons``，并用 thunk 填充，然后赋一个具体的值，随后被 ``and`` 访问，最后再释放。完整的流程需要需要额外的 CPU 周期和垃圾回收机制。

融合可以将 ``all`` 这类函数转化为不需要中间 list 的形式。

.. code-block:: haskell

   all' p xs = h xs
     where h []     = True
           h (x:xs) = p x && h xs

这个版本的 ``all`` *不会* 产生任何的中间 list，在对 ``xs`` 的每一步计算都会产生一个 bool 值，这个值会被直接应用到 ``&&``，产生一个由 ``&&`` 连接到函数调用链（事实上这个调用链就是 ``and`` 函数的实现方式）。因为这一版本的实现成功地消除了中间状态的 list，也就是 ``and`` 和 ``map`` *融合* 了。

为什么需要融合
^^^^^^^^^^^^^^

正如 Andy Gill 所说：

   We want to eat our cake and have it too. That is, we would like to write
   programs in the style of ``all`` but have the compiler automatically
   transform this into the more efficient version ``all'``.

大致翻译如下：

  鱼与熊掌，不可兼得。我们既想编写 ``all`` 风格的代码，也希望编译器可以自动将我们的代码转化为更高效的 ``all'`` 形式。

什么场景下融合会影响性能
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

与内联类似，融合本身并不会降低系统的运行速度，反而缺乏融合会造成系统运行速度降低。这是由于系统会为那些本可以融合却并没有这样做的函数分配额外的内存，从而导致性能降低。

对融合来说最困难的地方莫过于发现融合是使你性能降低的根本原因和告诉 GNHC 如何融合你的代码。:ref:`融合 <Fusion Rules Chapter>` 会展示如何识别你的代码是否需要融合，同时也会介绍如何使 GHC 进行融合。

.. _canonical-pointer-chasing:

过度的 Pointer Chasing
----------------------

什么是过度的 Pointer Chasing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

过度的 Pointer Chasing 是一种多余的计算形式；我们的程序在计算结果时做了比实际需要更多的工作。每当我们的程序解引用指针以检索值而不是直接引用值本身时，就会发生这种情况，这导致访问过程创建了一个不必要的中间过程。

在 Haskell 程序中，这种情况最常发生在我们编写程序时没有考虑它们的内存表示方式，并且尤其涉及到惰性计算时这种情况更稳严重。大部分这些情况都是众所周知的，并且在社区中已经存在一段时间了。

过度的 Pointer Chasing 为什么会影响性能
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

经典例子是 fold 的惰性求值引起的内存泄漏，例如 [#]_：

.. code-block:: haskell
   :caption: 惰性的 foldl

   mean :: [Double] -> Double
   mean xs = s / fromIntegral ln
     where (s, ln)        = foldl step (0,0) xs
           step (s, ln) a = (s + a, ln + 1)

.. code-block:: haskell
   :caption: 立即求值的 foldl'

   mean' :: [Double] -> Double
   mean' xs = s / fromIntegral ln
     where (s, ln)        = foldl' step (0,0) xs
           step (s, ln) a = (s + a, ln + 1)

``mean`` 和 ``mean'`` 展示了两种常见的内存泄露形式。``mean`` 使用惰性的 ``foldl`` 导致大量内存堆积； ``mean'`` 的问题在于，虽然 ``foldl'`` 是严格形式，但是 ``foldl'`` 只会将表达式求值到 :term:`WHNF`。在本例中 ``foldl'`` 中的 ``step`` 只会将 tuple 求值到它的 constructor，即 ``(,)``，而不是 ``s + a`` 或者 ``ln + 1``。由于惰性计算的原因，这些 ``(,)`` 会被存储在堆上，因此我们必须通过这些指向 ``(,)`` 的指针来进行计算。

另一种常见的过度 Pointer Chasing 的形式是在 data constructor 中过度使用惰性字段，特别是当该字段永远都会被求值时，使用惰性并没有任何优势，甚至会影响程序的性能。考虑如下例子，``step`` 不再通过 tuple 计算，而是使用自定义类型：

.. code-block:: haskell

   data Step = Step Double Double
   ...

   -- mean rewritten with Step instead of (,)
   mean'' :: [Double] -> Double
   mean'' xs = s / fromIntegral ln
     where (Step s  ln)       = foldl' step (0,0) xs
           step (Step s ln) a = Step (s + a) (ln + 1)

与 ``mean'`` 堆积了大量的 ``(,)`` 类似，由于 ``Step`` 和 ``Double`` 是惰性的，``mean''`` 同样也有大量的 ``(s + a)`` 和 ``(ln + 1)`` 的 thunk。但是在我们的目标：计算平均值中，我们并没有从这种惰性中获得任何好处，因为我们的程序不需要惰性求值。因此在这种场景下应该立即对 ``Step`` 进行求值以消除中间的计算结果，避免不必要的内存分配。

还有一种场景是，在 data constructor 可以使用 :term:`Unboxed` 的字段时反而使用了 :term:`Boxed` 的版本。考虑如下例子，``Counter`` 是对某个特定领域 Int 的包装：

.. code-block:: haskell

   data Counter = Counter Int

.. note::
   通常情况下，使用 ``-O2`` 编译选项编译时，GHC 会识别这种场景并对 ``Counter`` 进行优化。

``Int`` 是 ``Counter`` 中的一个 :term:`Boxed` 且 :term:`Lifted` 的类型。这意味着每个 ``Counter`` 都会储存一个指针，指向一个保存在堆上的 ``Int`` 对象，而不是 ``Int`` 本身的值。我们可以使用 `unpack
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html?highlight=unpack#unpack-pragma>`_ 和 bang pattern 来告诉 GHC 移除堆中的间接引用。

.. code-block:: haskell

   data Counter = Counter {-# UNPACK -#} !Int

上面的代码告诉 GHC 将 ``Int`` 的值直接储存在 ``Counter`` 中，而不是储存一个指向 ``Int`` 的指针。在 :ref:`Unboxing` 一章中将会对这种方法做进一步的介绍。

.. _canonical-closure-alloc:

过度的闭包分配
--------------

什么是过度的闭包分配
^^^^^^^^^^^^^^^^^^^^

过度的闭包分配（Excessive closure allocation）是另一种多余的计算和内存分配的形式；这意味着我们的程序在计算结果时进行了更多的内存分配和更多的计算。过度的闭包分配的形成有以下两种原因：首先是由于 GHC 通过 :term:`Let Floating` 可以得到很好的优化结果，因此大多数 Haskell 用户从来没有意识到有这个问题存在（这也侧面说明了 GHC 的质量很高=。=）；另一方面，为了观察到过度的闭包分配，程序员必须大量跟踪程序在函数（functions）、模块（modules）和包（packages）中的内存分配，在大多数 Haskell 开发场景中是不会这样做的。

我们将给出一些示例，GHC 可以轻松找到这些例子存在的问题并给出优化。:ref:`Impact of seq Removal on SBV's cache <SBV572>` 会展示在一个广泛应用的库中存在的大量内存申请的例子。

.. todo::
   Not yet written, see `#18 <https://github.com/input-output-hk/hs-opt-handbook.github.io/issues/18>`_

虽然 GHC 对这些场景的优化表现很好，但熟悉这些代码的变换原因是有益的；这会使得用户在阅读或编写 Haskell 代码时开始以内存分配为基础进行思考，并逐渐掌握如何在 GHC 无法进行优化时手动执行这些优化。

过度的闭包分配为什么会影响性能
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

考虑如下例子 [#]_：

.. code-block:: haskell

   let x = y + 1
   in case tail zs of
           [] -> x * x
           _  -> 1

这是 ``Let Floating inwards`` 一个例子，注意到 ``x`` 只在 case 的一个分支中用到，另一个分支则不需要。因此 GHC 可以将 ``x`` 移动到第一个分支中（floating inward）：

.. code-block:: haskell

   case tail zs of
        [] -> let x = y + 1
              in x * x
        _  -> 1

在这种场景中，``let x = ...`` 从必定执行变成了在特定条件下执行（译者注：这里的执行指的是分配一个 ``x`` 的 thunk），取决于 ``tail zs`` 的结果。因此，第二种写法虽然在语义上与第一种相同，但是却更高效，因为第二种写法可以在某些场景下避免 ``let x = ...``，从而避免额外的堆分配。

.. note::
   Let Floating 可能会改变 :term:`Thunk` 的大小。

考虑 :cite:t:`peytonjones1997a` 7.1 节中的一个例子：

.. code-block:: haskell

   let x = v + w     -- v 和 w 是 x 中的自由变量（free variables）
       y = ...x...x  -- y 使用到了 x
   in B              -- B 没有使用 x

对 ``x`` 进行移动：

.. code-block:: haskell

   let y = let x = v + w -- v 和 w 变成了 y 的自由变量
           in ...x...x
   in B

现在 ``v`` 和 ``w`` 变成了 ``y`` 而不是 ``x`` 的自由变量。``x`` 是 ``y`` 中的变量（会被内联）。因此如果 ``v`` 和 ``w`` 是自由变量，那么 ``y`` 的 thunk 大小将会保持不变。然而如果 ``v`` 和 ``w`` 是新的自由变量，那么 ``y`` 的大小会进行对应的增加。

``Let bindings`` 也可以向外移动（outwards）。在 ``case`` 表达式附近移动 ``let bindings`` 有不止一种方法，这里会介绍一种非常有效的移动方法，叫做 :term:`Full Laziness transformation`。`Full Laziness transformation` 会将 ``bindings`` 完全从 lambda 表达式中抽离出来。

考虑如下代码：

.. code-block:: haskell

   f = \xs -> let
                g = \y -> let n = length xs  -- 计算 n
                          in ...g...n        -- 使用 n 而不是 xs
              in ...g...

我们有一个外部函数 ``f``，它定义了一个紧密的内部循环 ``g``。请注意，``g`` 的每次递归调用都会为 ``length xs`` 分配空间，并进行计算，因为 ``let n = ...`` 是在 ``g`` 的函数体内部，而且 ``n`` 也在 ``g`` 中使用。但这显然是浪费的，因为在 ``g`` 的函数体内 ``xs`` 并没有改变，所以我们只需要计算一次 ``n`` 就足够了。幸运的是，``g`` 除了计算 ``n`` 之外不使用 ``xs``，所以 ``let n = ...`` 可以从 ``g`` 中提出来：

.. code-block:: haskell

   f = \xs -> let n = length xs          -- n 只计算一次
              in let g = \y -> ...g...n  -- 使用上面定义的 n
                 in ...g...

这个版本是完全惰性版本，因为我们将 ``let n = ..`` 移出了 ``g`` 函数体内的 lambda 表达式。通过利用惰性求值并避免对 ``n`` 的重复、无效计算，这个版本效率更高。在 ``g`` 的第一次迭代中，``n`` 将是一个惰性求值的表达式（thunk），但在后续的每次迭代中，``n`` 都将被评估为一个值，从而节省了时间和空间。

.. _canonical-domain-modeling:

糟糕的领域建模
------------

定义
^^^^

Poor domain modeling is a catch all phrase for constructing a program that has a
high impedance to the problem domain. The problem domain is the abstract domain
that dictates the computation that the program must do, the logical sequence of
steps it must take and the invariants it must uphold. The program domain is the
implementation, it is the code that is tasked with performing the computation
and upholding the invariants in the problem domain. This is a one-to-many
relationship; for any given problem domain there are many possible
implementations.

For example, imagine our task is to implement a program that sorts some data. We
can list the concepts, invariants and properties this problem domain specifies:
the domain has the concepts of a datum; which is a single unit of information, a
partial order on that data; there are many sequences of data, but for a given
set of data only two sequences have the property sorted, a datum must have an
ordinal property; or else we would not be able to sort, and the sorted
invariant; that defines what the property sorted means: for a sort from low to
high, a given datum that is less than another datum must precede the greater
datum in the output sequence. Note that every possible implementation must
somehow represent and abide by these ideas for the program to be considered
correct and for the implementation to be considered an implementation at all.

Therefore poor domain modeling occurs when the implementation makes it difficult
to express the computation, properties and invariants required by the problem
domain. If this is the case then we say there is a high impedance between the
problem domain and the program domain. Obviously this is problem specific and we
cannot provide a canonical example, instead we'll provide a set of guidelines to
describe when you know you have high impedance and how to fix it.

如何知道程序是否存在糟糕的领域建模
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

不幸的是，这更多的是一种艺术而非科学，需要主观的判断。Haskell 中典型的场景是所有 instance 的实现做了很多不必要的工作。

过度使用 Data.List
"""""""""""""""""

下列 ``Data.List`` 中的函数，可以对任意无序列表进行处理，或是需要遍历整个列表。

#. ``length``
#. ``reverse``
#. ``splitAt``
#. ``takeWhile``
#. ``dropWhile``
#. ``elem``
#. ``notElem``
#. ``find``
#. ``filter``
#. ...其他的索引

请记住，Haskell 中的列表是流；不将它们视为流不仅会在问题领域和你的程序之间产生阻抗，还会降低运行时性能（并且很容易创建一个平方时间复杂度的程序）。然而，包含少量元素的小型临时列表是可以的，因为它们的构建和遍历所需的时间比更复杂的数据结构要少。

函数组合的不易
""""""""""""

组合性和可组合性是代码最有价值的属性之一。它是模块化的关键，也是代码重用的关键，能够创建更易于测试、更易于理解且通常更紧凑的代码。当你的程序领域中的函数不易组合时，你会经常发现自己在不断地打包、解包和重新打包领域元素，只为了完成任何事情。你将被迫深入到程序领域对象的*实现*中，以便在问题领域中表达意义，而不是通过函数表达这种意义。

当程序领域缺乏可组合性时，函数会变得过于庞大，并且过于关注实现细节；这就是在实现中表现出的高阻抗。

.. todo::
   下一个例子可以参考 `#20 <https://github.com/input-output-hk/hs-opt-handbook.github.io/issues/20>`_

问题领域的不变量难以表达
""""""""""""""""""""

这通常表现为使用了多余的 guard。许多函数采取如下形式：

.. code-block:: haskell

   -- | 一个关于 Foo 的示例函数，通过根据多个谓词测试 Foo
   myFunction :: Foo -> Bar
   myFunction foo | predicate0 foo = ...do something ...
                  | predicate1 foo = ...do another thing...
                  | ...
                  | predicateN foo = ...do N thing...

当这种模式在代码库中变得无处不在时，就会成为一个问题。当程序中的许多函数使用守卫时，程序将遭受冗余检查和糟糕的分支预测。例如：


.. code-block:: haskell

   -- | another function on Foo, this function doesn't learn much about Foo
   -- because it only tests Foo against one predicate.
   myOtherFunction :: Foo -> Baz
   myOtherFunction foo | predicate1 foo = ...do some another thing...
                       | otherwise      = ...

   main :: IO ()
   main = do foo <- getFoo          -- we get a Foo
             myFunction foo         -- we learn a lot about Foo
             myOtherFunction foo    -- nothing we've learned is propagated forward
                                    --  from myFunction to myOtherFunction, and so
                                    --  we redundantly check predicate1 on foo.

References
----------
.. [#] https://wiki.haskell.org/Inlining_and_Specialisation
.. [#] https://www.sciencedirect.com/science/article/pii/030439759090147A?via%3Dihub
.. [#] https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf
.. [#] This code adapted from Johan Tibell slides on Haskell `optimization
       <https://www.slideshare.net/tibbe/highperformance-haskell>`_.
.. [#] This code adapted from :cite:t:`peytonjones1997a` Section 7.
