### 执行方法

1. 安装`stack`，参见[stack install_and_upgrade](https://docs.haskellstack.org/en/stable/install_and_upgrade/)。

2. 运行解释器。有多种方法可供选择。

    2.1 先编译后执行： `stack build && stack exec ki -- ARGS`。其中`ARGS`为解释器ki的参数。详见下列提示信息。

    ```
    Usage: ki [-i|--instructions INPUT_PATH] [-o|--output OUTPUT_PATH]
              [-t|--tree INPUT_PATH] [-r|--repl]

    Available options:
      -h,--help                Show this help text
      -i,--instructions INPUT_PATH
                               path of instructions to excute.
      -o,--output OUTPUT_PATH  path to output
      -t,--tree INPUT_PATH     path of program to parse
      -r,--repl                REPL mode
    ```

    2.2 直接执行：`stack runhaskell -- app/ki.hs ARGS`。`ARGS`的含义见上条。

3. 运行编译器

    3.1 先编译后执行：`stack build && stack exec kc -- ARGS`。其中`ARGS`为编译器`kc`的参数。详见下列提示信息。

    ```
    Usage: kc [INPUT] [-o OUTPUT] [-h]
    example:
      kc input.txt -o output.py
    Avaliable options:
      -o   path to output
      -h   show this help message
    ```

    3.2 直接执行：`stack runhaskell -- app/kc.hs ARGS`。`ARGS`的含义见上条。

4. 运行测试： `stack test`。如需单独测试parser或eval，则执行`stack test project:parser-test`或`stack test project:eval-test`。

5. 运行测试覆盖率分析：`stack build; stack clean; stack test --coverage`

### 例程

见examples目录。
