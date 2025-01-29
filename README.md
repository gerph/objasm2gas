# objasm2gas

<div align="center">
<p>
      <a href="https://github.com/gerph/objasm2gas"><img src="https://img.shields.io/badge/objasm2gas-v1.1-brightgreen"></a>
      <a href="https://github.com/gerph/objasm2gas/blob/main/LICENSE"><img src="https://img.shields.io/badge/License-GPLv3.0-blue" alt="LICENSE"></a>
</p>
</div>
Migrate legacy ObjASM syntax assembly (as used on RISC OS) to GNU syntax (GAS).

## Usage

`objasm2gas.pl [<options>] <file1> [<file2>...]`

### Options

| Switch                  | Descriptions                                            |
| :---------------------- | :------------------------------------------------------ |
| `-c, --compatible`      | Keeps compatibility with armclang assembler             |
| `-h, --help`            | Show this help text                                     |
| `-i, --verbose`         | Show a message on every non-trivial convertions         |
| `-n, --no-comment`      | Discard all the comments in output                      |
| `-o, --output=<file>`   | Specify the output filename                             |
| `-r, --return-code`     | Print return code definitions                           |
| `-s, --strict`          | Error on directives that have no equivalent counterparts |
| `-v, --version`         | Show version info                                       |
| `-w, --no-warning`      | Suppress all warning messages                           |
| `-x, --suffix=<string>` | Suffix of the output filename [default: '`.out`']       |
| `--inline`              | Process the `GET` and `INCLUDE` inline as objasm, rather than generating a GNU as include |

## Supported conversions

- [X] [Comments](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Comments?lang=en)
- [X] [Labels](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Labels?lang=en)
- [X] [Numeric local labels](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Numeric-local-labels?lang=en)
- [X] [Functions](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Functions?lang=en)
- [X] [Sections](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Sections?lang=en)
- [X] [Symbols with special characters](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Symbol-naming-rules?lang=en)
- [X] [Numeric literals](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Numeric-literals?lang=en)
- [X] [Operators](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Operators?lang=en)
- [X] [Aligment](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Alignment?lang=en)
- [X] [PC-relative addressing](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/PC-relative-addressing?lang=en)
- [X] [Directives: Conditional](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Conditional-directives?lang=en)
- [X] [Directives: Data definition](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Data-definition-directives?lang=en)
- [X] [Directives: Instruction set](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Instruction-set-directives?lang=en)
- [X] [Directives: Symbol definition](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Symbol-definition-directives?lang=en)
- [X] [Directives: Miscellaneous](https://developer.arm.com/documentation/dui0742/g/Migrating-ARM-syntax-assembly-code-to-GNU-syntax/Miscellaneous-directives?lang=en)

- [X] `GET` and `INCLUDE` inline
- [X] RISC OS format filename handling
- [X] Macro expansion (tentative)
- [X] Local (numeric) labels
- [X] Conditionals with expressions


## Demo

Conversion result of `demo.s` (with option `-i -c`)

![](./demo/demo.png)

Command-line output:

```bash
$ ./objasm2gas.pl -i -c demo/demo.s
WARN: demo/demo.s:6 -> demo/demo.s.out:6: Numeric local label with scope '2routA' is not supported in GAS, converting to '2'
WARN: demo/demo.s:7 -> demo/demo.s.out:7: Scope of numeric local label is not supported in GAS, removing ROUT directives
WARN: demo/demo.s:8 -> demo/demo.s.out:8: Numeric local label with scope '3routB' is not supported in GAS, converting to '3'
WARN: demo/demo.s:12 -> demo/demo.s.out:12: Can't specify label's search level 't' in GAS, dropping
WARN: demo/demo.s:13 -> demo/demo.s.out:13: Can't specify label's search level 'a' in GAS, dropping
WARN: demo/demo.s:14 -> demo/demo.s.out:14: Can't specify label's search level 't' in GAS, dropping
WARN: demo/demo.s:14 -> demo/demo.s.out:14: Can't specify label's scope 'routC' in GAS, dropping
Argument "    " isn't numeric in exponentiation (**) at ./objasm2gas.pl line 799, <$f_in> line 23.
WARN: demo/demo.s:27 -> demo/demo.s.out:30: Implicit shift is not supported in GAS, converting to explicit shift
WARN: demo/demo.s:28 -> demo/demo.s.out:32: Implicit shift is not supported in GAS, converting to explicit shift
INFO: demo/demo.s:29 -> demo/demo.s.out:34: Converting hexadecimal '&10AF' to '0x10AF'
INFO: demo/demo.s:30 -> demo/demo.s.out:35: Converting '2_11001010' to hexadecimal literal '0xCA'
INFO: demo/demo.s:31 -> demo/demo.s.out:36: Converting '-2_1101' to hexadecimal literal '-0x0D'
INFO: demo/demo.s:32 -> demo/demo.s.out:37: Converting '8_27' to hexadecimal literal '0x17'
WARN: demo/demo.s:46 -> demo/demo.s.out:51: Unsupported operator :ROR:, need a manual check
WARN: demo/demo.s:54 -> demo/demo.s.out:59: Conversion containing strings needs a manual check
WARN: demo/demo.s:55 -> demo/demo.s.out:60: Conversion containing strings needs a manual check
WARN: demo/demo.s:68 -> demo/demo.s.out:73: Conversion containing strings needs a manual check
WARN: demo/demo.s:75 -> demo/demo.s.out:80: Local variable 'var1' is not supported, using static declaration
WARN: demo/demo.s:76 -> demo/demo.s.out:81: Local variable 'var2' is not supported, using static declaration
WARN: demo/demo.s:81 -> demo/demo.s.out:90: Conversion containing strings needs a manual check

```



## Cautions

By default (without `--strict`), for those directives that have no equivalent GNU format, `objasm2gas` will try best to convert and generate warning information on the specific line. Therefore, a 'warning' does **NOT** necessarily mean no issue, please check the conversion result to ensure it works as expected.

Note that `objasm2gas` *assumes that the input file is in the **correct** syntax*, otherwise, the conversion result is **UNEXPECTED**

## TODO

- [ ] Loops
