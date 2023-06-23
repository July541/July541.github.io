.. _How_to_use:

如何使用本书
====================

目标
----

本书是为勇敢的 Haskell 开发人员编写的一本实用手册。不同于互联网上流传的各种神秘的优化方法，全书通过阐述 GHC 编译器的各个方面来演示一些可行的性能优化。

目标读者
--------

本书面向于所有日常使用 Haskell 的开发人员，假设读者已经熟练掌握函数式编程的基本概念，包括但不限于递归(recursion)，纯函数与非纯函数(purity)，高阶函数的 functor，applicative functors，monad 等。同时读者需要熟悉基本的 Haskell 开发工具套，能够使用 cabal 或 stack 编译运行代码。

环境准备
--------

理想情况下，读者可以准备一个自己的项目，一边阅读本书一边在项目上进行优化练习。如果读者没有合适的项目进行练习，可以在 hackage 上选择自己感兴趣的项目尝试进行优化（这些项目大部分已经进行了足够的优化，所以可能没有足够的空间可以优化）。

本书假设读者使用 GHC |ghcVersion|，Linux 系统（kernal 版本大于等于 ``5.8``）（译者使用 macos）。某些章节在旧版本 GHC 仍有效，例如 :doc:`Using EventLog
</src/Measurement_Observation/Heap_Ghc/eventlog>`; 可以在 ``GHC 8.8`` 及以上的版本使用，:doc:`Using Cachegrind
</src/Measurement_Observation/Heap_Third/cachegrind>`; 可以在 ``GHC 8.10`` 及以上的版本使用，该章中用到的 :term:`DWARF` 符号在 ``GHC 8.10`` 被首次引入。类似地，某些章节例如 :doc:`Using perf
</src/Measurement_Observation/Measurement_Third/linux_perf>` 会使用到一些只有在 Linux 操作系统上才有的功能。

符号介绍
--------

若无其他说明，所有样例代码均为 Haskell 语言。其他符号遵循编程语言和计算机科学学术标准。

1. :math:`\perp` 代表底(bottom)
2. :math:`Foo[x \rightarrow e]` 表示将 :math:`Foo` 中的 :math:`x` 替换为 :math:`e`。
3. 大 O 标记：:math:`\mathcal{O}(n\log{}n)` 代表算法的渐进运行时间。例如某个算法的运行时长为 :math:`\mathcal{O}(n\log{}n)` ，那么对于有 :math:`n` 个元素的输入结合，该算法最长运行时间为 :math:`c*n \log{} n`，其中 :math:`c` 为一个固定的常数。

阅读顺序
------------

为了更好地发挥手册的作用，全书被划分为各自独立的章节，*每个章节解决一个特定的问题*，读者可按照自己的需求在章节间任意跳跃，无需按顺序依次阅读。

全书可大致分为两部分，每个部分的章节均按照预计阅读时间从短到长排列。

第一部分目标是帮助程序员定位代码中的性能问题，因此该部分涉及的主要内容为测量(measurement)、观察(observation)、可重复性(repeatability)和进行测试(testing)，同时也会介绍一些直接观察(direct observation)方法，例如对 ``Core`` 语言和 ``Stg`` 语言的观察分析（译者注：``Core`` 和 ``Stg`` 都是 GHC 编译过程中的中间表示）。

在定位到性能问题出现的地方之后，第二部分介绍如何进行优化。章节顺序按照从易到难排序，最简单的方法例如：选择正确的库，最难的方法例如：利用 ``bakpack`` 活动细粒度的 :term:`Unboxed` 数据类型或利用 :term:`Levity Polymorphism` 来控制类型在运行时的表示。

本书特点
--------

本书(Haskell Optimization Handbook, HOH)有如下特点

#. 在网上完全免费公开。
#. 书中介绍的方法完全具有可操作性，读者可以选择任意章节进行阅读，并将该章节中提到的方法应用于实际的问题中。
#. 书中的例子均来自于现实中的代码，并非特意构造。
#. 书中的内容是实用的。对于书中提到的各种技巧及方法，会介绍如何实现、何时使用，更进一步，也会介绍如何观察该技巧/方法在代码中的实际效果。例如，介绍 GHC 堆性能分析(heap profiling)的章节会介绍什么是 profiling，如何进行 profiling，profiling 的输出是什么样的，同时会通过例子来讲解性能差的堆长什么样子并解释为什么这样的堆状态会造成性能变差。
#. 全书完全由社区驱动。
#. 本书会不断进行维护和更新，面向版本从 ``GHC 8.10`` 开始(:term:`DWARF` 引入的版本)。
#. 所有中级到高级的 Haskell 开发人员都应能顺利理解本书中提到的内容。

书中不会涉及到的内容
--------------------

#. 对于仅有入门级函数式编程能力的程序员，可能并不合适阅读本书。
#. 本书不会介绍如何初始化一个 Haskell 项目、如何使用编译工具、如何设置 shell 环境、如何使用 shell；本书也不会介绍如何安装一些特定平台的第三方工具。
#. 本书不会对使用到的算法或数据结构进行科普解释。因此相关的内容会是“如果你有 XX 问题，请考虑使用 unordered-containers”，而不是“这是一个银行家队列的 XX”。
#. 同时本书也不是 monad 或 monad transformer 的教程，读者被假定已掌握这些知识。
