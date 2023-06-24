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

什么场景下内联会降低运行速度
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

References
----------
.. [#] https://wiki.haskell.org/Inlining_and_Specialisation
.. [#] https://www.sciencedirect.com/science/article/pii/030439759090147A?via%3Dihub
.. [#] https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf
