# objasm2gas

<div align="center">
<p>
      <a href="https://github.com/gerph/objasm2gas"><img src="https://img.shields.io/badge/objasm2gas-v1.10-brightgreen"></a>
      <a href="https://github.com/gerph/objasm2gas/blob/main/LICENSE"><img src="https://img.shields.io/badge/License-GPLv3.0-blue" alt="LICENSE"></a>
</p>
</div>
Migrate legacy ObjASM syntax assembly (as used on RISC OS) to GNU syntax (GAS).

## Usage

`objasm2gas.pl [<options>] <file1> [<file2>...]`

### Options

| Switch                  | Descriptions                                            |
| :---------------------- | :------------------------------------------------------ |
| `--compatible`          | Keeps compatibility with armclang assembler             |
| `-h, --help`            | Show this help text                                     |
| `-v, --verbose`         | Show a message on every non-trivial conversion          |
| `-n, --no-comment`      | Discard all the comments in output                      |
| `-o, --output=<file>`   | Specify the output filename                             |
| `-r, --return-code`     | Print return code definitions                           |
| `-s, --strict`          | Error on directives that have no equivalent counterparts |
| `-V, --version`         | Show version info                                       |
| `-w, --no-warning`      | Suppress all warning messages                           |
| `-x, --suffix=<string>` | Suffix of the output filename [default: '`.out`']       |
| `--inline`              | Process the `GET` and `INCLUDE` inline as objasm, rather than generating a GNU `as` include |
| `--simple-conditions`   | Leave conditions in GNU `as` form (default is to interpret `objasm` conditionals |
| `--test-expr=<expr>`    | Diagnostics for exercising the expression parser        |
| `--gas=<gas-binary>`    | GNU `as` to invoke to create ELF file, after conversion; output file will be the output from GNU `as` |
| `--bin`                 | When used with `--gas`, create a binary file (default is to produce an ELF file) |
| `--predefine=<asm>`     | Pre-execute a SETA, SETL or SETS directive.             |
| `--line-map=<file>`     | Output a map of output file line numbers to source lines. |
| `--while-limit=<count>` | Maximum iterations per WHILE loop (default 100000). |
| `-i <paths>`            | Comma separated list of paths to search for inclues     |
| `--32`                  | Select 32 bit mode                                      |
| `--64`                  | Select 64 bit mode                                      |
| `--util`                | Create a Utility file, using `riscos64-link` |
| `--aif`                 | Create a AIF Absolute file, using `riscos64-link` |
| `--rmf`                 | Create a Relocatable Module file, using `riscos64-link` |
| `--debug=<opts>`        | Selective enabling of debug options |



### ENTRY in AArch64 output

With `--64`, `ENTRY` defines `_entry` at the current position and exports it,
equivalent to an `_entry` label followed by `EXPORT _entry`. The GNU output is:

```asm
        .global _entry
_entry:
```

It emits no bytes. A label on the same source line remains an alias for that
position. Place `ENTRY` once, in the executable code area where execution should
begin; do not also define `_entry` elsewhere. Conditional assembly still controls
whether it is emitted. The linker must select `_entry` as its entry symbol:
exporting a symbol alone does not set an ELF header's entry address.

In `--32` mode, `ENTRY` retains its previous ignored behaviour.

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
- [X] Macro expansion
- [X] Local variables within macros
- [X] Conditionals with expressions
- [X] `WHILE` loops with re-evaluated conditions

### WHILE and WEND

`WHILE` tests its expression before each iteration. The converter caches the
body and replays it through the normal expression, directive and macro handlers.
`SETA`, `SETL` and `SETS` changes affect the next test; a false initial condition
emits nothing. Loops nest and work within macros, retaining their existing local
variable scope. Inline includes can change loop state; use `--inline` when the
included source contains ObjAsm directives to interpret during replay.

```asm
        GBLA count
count   SETA 0
        WHILE count < 3
        DCD count
count   SETA count + 1
        WEND
```

This emits three words, 0, 1 and 2. It is not GAS `.rept`, which repeats a fixed
count without re-evaluating an ObjAsm condition. WHILE requires the default
interpreted conditional mode; `--simple-conditions` reports an unsupported-mode
error rather than generating misleading output.

Missing WEND, stray WEND, unbalanced body conditionals and an exceeded iteration
limit are processing errors. A loop has no new variable scope; repeated ordinary
labels must therefore be made unique, or use numeric local labels. Replayed output
retains original source locations and iteration information in `--line-map`.
The default iteration limit is 100000 per loop; raise it explicitly with
`--while-limit` for a larger finite expansion. Nested cached loops are limited to
64 levels. These bounds detect accidental nontermination; they are not ObjAsm
instruction-set limits.

The loop regression suite runs as part of `make tests`. It can also run using
installed native cross-assemblers without fetching Docker wrappers:

```sh
make tests-while AS=arm-unknown-riscos-as AS64=aarch64-unknown-linux-gnu-as
```

Substitute the assembler commands available on your host. The converter and these
tests run on host Perl 5.14 or later, not the RISC OS Perl 5.001 interpreter.

## Extensions

In addition to the standard operations, the conversion includes a few
extensions to make using ObjAsm syntax a little easier.

Expression parsing:

* `$regs :REGLISTLEFT: $num` - a binary operator which returns the left most `$num` registers from the supplied register list.
* `$regs :REGLISTRIGHT: $num` - a binary operator which returns the right most `$num` registers from the supplied register list.
* `$regs :REGLISTSKIP: $num` - a binary operator which returns the supplied register list without the first `$num` registers.
* `$regs :REGLISTTRIM: $num` - a binary operator which returns the supplied register list without the last `$num` registers.


## Demo

Conversion result of `demo.s`:

![](./demo/demo.png)

Command-line output:

```bash
$ /objasm2gas.pl -v demo/demo.s
INFO: demo/demo.s:33 -> demo/demo.s.out:37: Converting '2_11001010' to hexadecimal literal '0xCA'
```



## Cautions

By default (without `--strict`), for those directives that have no equivalent GNU format, `objasm2gas` will try best to convert and generate warning information on the specific line. Therefore, a 'warning' does **NOT** necessarily mean no issue, please check the conversion result to ensure it works as expected.

Note that `objasm2gas` *assumes that the input file is in the **correct** syntax*, otherwise, the conversion result is **UNEXPECTED**

Use of `.` or `{PC}` may be unreliable in some circumstances. Try to simplify your expressions if
possible.

## TODO

- [ ] Loops
