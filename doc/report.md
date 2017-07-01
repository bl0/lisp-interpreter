## 编程语言的解释器/编译器的实现-实验报告
#### 小组成员： 马子俊 刘斌 李肇阳

### 执行方法
见README。

### 目标

#### 功能实现

* __独立主程序__：实现两个独立主程序`ki, kc`。

* __REPL__：执行`ki --repl`或`ki -r`即可进入REPL模式。该模式支持`:i :t :q`三种命令，详细用法见下：
  - _用法_: `ki -repl`
    _解释_: 进入REPL模式，该模式下用户输入一段指令，回车后解释器输 出执行结果到`stdout`，并等待处理用户的下一条指令。
  - _用法_: `:i <statement>`
    _解释_: 在REPL模式下，解释器执行`statement`，并将执行后的`Memory`输出到`stdout`。
  - _用法_: `:f <function>`
    _解释_: 在REPL模式下，定义一个函数，将该函数插入`Memory`中，并将执行后的`Memory`输出到`stdout`。
  - _用法_: `:e <expression>`
    _解释_: 在REPL模式下，解释器执行该表达式，并将执行后的结果输出到`stdout`。
  - _用法_: `:q`
    _解释_: 在REPL模式下，该指令退出解释器程序。
  - _用法_: `:t`
    _解释_: 在REPL模式下，该指令输出上一段程序的抽象语法树。如果没有上一段程序，输出错误信息。

* __解释器__：实现了解释执行给定语言的解释器`ki`。具体用法如下：
  - _用法_: `ki [-i|--input] <file> [-o|--output] <file>`
    _解释_: `[-i|--input]`指定输入文件的路径。`[-o|--output]`指定将解释的结果输出到哪个文件。如果不指定`[-o|--output]`选项，那么结果将输出到`stdout`。
  - _用法_: `ki [-t|--tree] <file> [-o|--output] <file>`
    _解释_: `[-t|--tree]`后跟程序文件的路径。该命令输出指定程序的抽象语法树至指定文件或`stdout`。
  - _用法_: `ki [-h|--help]`
    _解释_： 显示帮助信息。

* __编译器__：实现了将给定语言转化为python的语言翻译器`kc`。具体用法如下：
  - _用法_: `kc <file> -o <file>`
    _解释_: 第一个参数为输入程序的路径；-o指定输出的可执行文件的路径，若不指定则输出到`stdout`。

* __文法解析__：实现了具体文法的解析器，从字符串解析到抽象语法树。语言特性列表中所有特性的文法均基于S表达式设计，不含左递归构造， 比较简单，方便使用parsing组合子进行解析。

* __Pretty-printer__：实现了语法树和`Mem`（`Map Var Val`，即从变量名到变量值的映射）的`pretty printer`。使得输出的语法树和`Mem`具有很好的可读性。

* __错误处理__：在解析器和解释器实现中，处理了可能出现的错误情况(如类型不匹配、 显式抛出异常等)，并返回了有一定信息量的错误信息。

* __代码测试__：基于`HUnit`和`QuickCheck`，实现了`Eval`和Parser部分的大量单元测试和功能测试。

* __代码风格__：在代码中使用了`Monad`组织代码。同时具有如下特性：
  * 可组合：项目中共包含5个模块：`Eval, Parser, Gen, AST, Memory, FileIO`，其中`Eval, Parser, Gen`又作了细分。模块与模块间耦合较小，方便组装。

#### 语言特性
* __逻辑表达式__: 实现了`True, False, not, and, or`等布尔常量和逻辑运算。

* __浮点算术表达式__: 实现了科学计数，`+, -, *, /`四种浮点算术运算，`=, <=, >=, <, >`等比较运算。

* __字符串与列表__: 实现了cons，car，cdr和字符常量，字符串常量。

* __While语言__: 实现了`set, if, skip, while`四种语言特性。同时，对于变量，支持以字母或`_`开头，变量名中可有字母，数字，和`_`。

* __数组__: 实现了`vector-ref`，`make-vector`，`vector-set!`。

* __一阶函数__: 实现了函数定义，函数调用，函数返回等操作。

* __文法作用域__: 实现了`let`语句。

* __高阶函数__: 实现无类型$\lambda$演算，支持定义和使用匿名函数。需要指出的是我们实现了多参数的匿名函数。

* __注释__: 实现了基于`;`的行注释。

### 思路
下面分模块来介绍开发过程中的思路。
* Memory模块对内存（`Mem`)、变量（`Var`）、值（`Val`）进行了抽象，并实现了内存的`Pretty printer`（`mempp`）。变量类型定义为`String`，`Val`定义为`BoolVal Bool | ScientificVal Scientific`，内存定义为从`Var`到`Val`的`Map`。而`mempp`使用一些trick（详见代码）来将`Mem`以表格的形式输出。

* `AST`模块对`Expression, Statement, Program`进行了抽象，完整的定义了包含所有待实现语法特性的抽象语法树。

* `Parser`模块实现了将字符串转换为抽象语法树的功能。在具体实现方面，使用了`Data.Attoparsec.Text`构造各种`parser`进行分析，并使用`Control.Applicative`将不同`parser`组装成`Expr Parser`和`Statement Parser`。

* `Eval`模块实现了对抽象语法树求值的功能。这部分是平凡而繁琐的，不再赘述。

* `Gen`模块实现了根据抽象语法树生成`python`代码的功能。实现方法较为暴力，直接依据抽象语法树进行硬编码。

* `Ki`模块为解释器的主程序。可分为参数解析，`repl`，指令执行，语法树输出四部分。

  - 参数解析部分解析输入参数，以决定进入解释器哪种模式。值的一提的是参数解析部分使用了`optparse-applicative`，使得`Ki`可同时支持长参数（如`--help`）和短参数（如`-h`），并能提供用户友好的帮助信息。

  - `repl`模式下，接受用户输入一条指令然后输出结果。下面对`repl`所支持的三种指令分别进行解释。

    * `:i <statement>`：依此调用`parser`和`eval`得出Statement的运行结果并使用`Pretty printer`输出，最后更新内存状态、上条指令、使用更新后的状态再次调用repl。

    * `:f <function>`: 定义函数，后续的statement和expression中可调用该函数。

    * `:e <expression>`: 对表达式进行求值，并输出计算结果。

    * `:t`：直接使用`pretty printer`输出上一条输入指令（function，statement或expression）`parse`后的结果并再次调用`repl`。

    * `:q`：直接退出程序。

    * `:h`: 显示帮助信息。

  - 指令执行模式类似于`repl`模式下的`:i`指令，不再赘述。

  - 语法树输出模式类似于`repl`模式下的`:t`指令，不再赘述。

### 亮点

* 测试部分使用了`Hunit`和`QuickCheck`，使得我们可以同时支持`testcase`和`property`两种测试方法。此外，对于`parser`和`eval`两部分，我们编写了大量的单元测试和部分功能测试，保证了项目基础程序的正确性。覆盖率测试结果如下(说明：由于lib中含有部分将在Ki,Ks中调用的定义和语句，因此该部分顶层定义和语句无法被测试，因此代码覆盖率并不是很高)：
  ```
   75% expressions used (981/1303)
   33% boolean coverage (7/21)
        33% guards (7/21), 10 always True, 4 always False
       100% 'if' conditions (0/0)
       100% qualifiers (0/0)
   70% alternatives used (71/101)
   79% local declarations used (38/48)
   49% top-level declarations used (47/95)
  ```

* 内存(`Mem`)的输出使用了一些小技巧，将`Mem`以表格的形式输出，效果见下：

  ```
  variable name |        value
  --------------+---------------------
  a             | ScientificVal 456.0
  b             | BoolVal True
  variable      | ScientificVal 2333.0
  ```

* 解释器`ki`的参数解释部分使用了`optparse-applicative`，使得参数支持长参数和短参数，并能提供用户友好的帮助信息。效果见下：
  ```
  Usage: ki [-i|--instructions INPUT_PATH] [-o|--output OUTPUT_PATH]
            [-t|--tree INPUT_PATH] [-r|--repl]

  Available options:
    -h,--help                Show this help text
    -i,--instructions INPUT_PATH
                             path of instructions to execute.
    -o,--output OUTPUT_PATH  path to output
    -t,--tree INPUT_PATH     path of program to parse
    -r,--repl                REPL mode
  ```

* `repl`模式使用了`haskeline`，支持以下常用快捷键，更多功能参见[haskeline KeyBindings](http://trac.haskell.org/haskeline/wiki/KeyBindings)。

  | 快捷键       | 备注        |
  | --------- | --------- |
  | 左右箭头      | 左右移动光标    |
  | Backspace | 向左删除一个字符  |
  | delete    | 向右删除一个字符  |
  | Up/down   | 上/下一条命令历史 |
  | ctr+L     | 清屏        |
  | ctr+R     | 向后搜索历史记录  |
  | ctr+S     | 向前搜索历史记录  |
  | ctr+A     | 移动光标至行首   |
  | ctr+E     | 移动光标至行尾   |

* 使用了`Scientific`表示浮点数，能表示任意精度的浮点数，且空间利用率较高。当然，由于`haskell`对`Scientific`的支持不高，因此许多操作不完善，在开发过程中需要自己手写许多轮子，如将`Scientific`实现为`Out`类型的实例以使`Scientific`支持`Pretty printer`。

* 浮点数解析支持负数，科学计数(如`1e10`)，16进制数(如`0x34`)。

* 解释器`ki`支持较多指令。

* StingLit的Parer支持转义（如可定义`"S\"tring"`）,同时StringLit的show函数行为与系统行为一致（即字符串列表会以String的形式展示出来）。

* 支持lisp原生行注释`;`

### 挑战与解决方案

在整个项目开发过程中，遇到了很多困难，下面根据我的回忆列举一些：

* `var`的`parser`较难完美的实现。原本的设想是变量名应该像c语言一样，支持数字，字母，下划线等，但首字母必须是字母或下划线，但如若这么定义，那`var`的`parser`将会比较复杂，由于精力有限，最终我们决定只支持字母。

* `Mem`的`Pretty printer`较难实现。由于`Map`的默认输出为`FromList [()]`，可读性很差，因此需要采用可读性更好的方式展现`Mem`。经过大量的探索，最终我们采用的方案是将`Mem`以表格的形式展现出来。

* 参数解析是一个较麻烦的事情。在写解释器`ki`时，我找了许多参数解析的库，以免像助教提供的代码那样写很冗长的代码，可是我找到的库要么已经很久没有更新不在能用，要么很难用，花了整整一天我终于找到了`optparse-applicative`，它能支持长/短参数，能生成用户友好的帮助信息，使用时代码量很少。可是，在写编译器`ks`时，发现`optparse-applicative`无法处理`：kc <file> -o <file>`中前面一个`<file>`，最终，在`ks`中，我只能放弃使用任何库，手动解析参数。

* StringLit的显示比较困难，起初只能使用list的方式显示（如`"['a','b']"`），在查阅了大量资料后想到可以使用map将形为[CharVal c]的列表转换为形为[c]的列表，然后调用系统的show即可实现StringLit的显示。


### 参考
1. stack官网: https://docs.haskellstack.org/en。由于框架使用了stack，因此在项目初期，查阅stack文档较多。
2. haskell wiki: https://wiki.haskell.org/。在开发过程中需要不时查阅haskell文档。
3. ltc: https://github.com/scvalex/ltc/blob/master/ltc.cabal。借鉴了其测试部分。
4. 助教所给代码：https://github.com/SnowOnion/ThuHaskell2016Autumn。遇到瓶颈时会去从助教所给代码中寻找灵感。
