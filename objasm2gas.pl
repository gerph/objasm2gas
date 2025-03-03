#!/usr/bin/perl
##
# Convert ObjAsm-like syntax to somethng that can be assembled by GNU assembler.
# Based on https://github.com/yxnan/arm2gas
# Releasd under GPL-3 license.
# See https://github.com/gerph/objasm2gas for source code.
#

use strict;
use warnings;
use feature ":5.14";
no warnings qw(portable);
use Getopt::Long qw(:config no_ignore_case bundling);

my $toolname = 'objasm2gas';
my $ver = '1.4';
my $helpmsg = <<"USAGE";
$toolname (v$ver) - Convert legacy ARM assembly syntax (used by objasm) to GNU syntax (GAS)

Usage: $toolname [<options>] <file1> [<file2>...]
Options:
        --compatible            Keeps compatibility with armclang assembler
    -h, --help                  Show this help text
    -v, --verbose               Show a message on every non-trivial conversion
    -n, --no-comment            Discard all the comments in output
    -o, --output=<file>         Specify the output filename
    -r, --return-code           Print return code definitions
    -s, --strict                Error on directives that have no equivalent counterpart
    -V, --version               Show version info
    -w, --no-warning            Suppress all warning messages
    -x, --suffix=<string>       Suffix of the output filename [default: '.out']
        --inline                Process GET and INCLUDE inline
        --simple-conditions     Only process conditions as simple statements
        --test-expr=<expr>      Test the expression processor
        --gas=<as-executable>   GNU Assembler to invoke to create ELF file, after
                                conversion
        --bin                   When used with --gas, create a binary file
        --util                  Create a Utility file, using 'riscos64-link'
        --aif                   Create a AIF Absolute file, using 'riscos64-link'
        --rmf                   Create a Relocatable Module file, using 'riscos64-link'
        --predefine=<statement> Pre-execute a SETA, SETL or SETS directive.
        --line-map=<file>       Specify a mapping file for output lines to source lines
    -i <paths>                  Comma separated list of paths to include
    --32                        Select 32bit mode
    --64                        Select 64bit mode
    --debug <opts>              Enable debugging options (expr, macros, filename)

Cautions:
    By default (without --strict), for those directives that have no equivalent
    in GNU format, the tool will try best to convert and generate warning information
    on the specific line. Therefore, a 'warning' does NOT necessarily mean no issue,
    please check the conversion result to ensure it works as expected.

    Note that the tool will assume that the input file is in the correct syntax,
    otherwise, the conversion result is UNEXPECTED.

Issues and Bugs:
    https://github.com/gerph/objasm2gas
    mailto:gerph\@gerph.org
    (original version: https://github.com/yxnan/arm2gas)
USAGE

#--------------------------------
# return code definitions
#--------------------------------
my $ERR_ARGV        = 1;
my $ERR_IO          = 2;
my $ERR_UNSUPPORT   = 3;
my $ERR_SYNTAX      = 4;

my $rvalmsg = <<"RETVAL";
$toolname may return one of several error code if it encounters problems.

    0       No problems occurred.
    1       Invalid or conflicting command-line args.
    2       File I/O error.
    3       Unsupported conversion.
    4       Syntax or processing error.
    255     Generic error code.

RETVAL


#--------------------------------
# global definitions
#--------------------------------

# stack
our @symbols = ();

# Stack of labels that we know.
our @label_stack = ({'backward'=>{}, 'forward'=>{}, 'rout'=>'rout'});
# The most recent entry is the highest numbered in the stack (ie $label_stack[-1])
# The stack will contain a dictionary of:
#   backward => mapping the local label number to the actual label to use
#   forward => mapping a supplied label number in the future to the label that should be assigned
#   rout => the routine name that we're in ('rout' if undefined)
#
# See: https://developer.arm.com/documentation/101754/0623/armasm-Legacy-Assembler-Reference/Symbols--Literals--Expressions--and-Operators-in-armasm-Assembly-Language/Syntax-of-numeric-local-labels?lang=en
# F searches the forward stack (or assigns)
# B searches the backward stack
# Neither searches backwards, then forwards.

# The label sequence will be incremented on every label creation.
our $label_sequence = 1;

# command-line switches
our $opt_compatible  = 0;
our $opt_verbose     = 0;
our $opt_strict      = 0;
our $opt_nocomment   = 0;
our $opt_nowarning   = 0;
our $opt_inline      = 0;
our $opt_simplecond  = 0;
our $opt_testexpr    = 0;
our $opt_gastool     = undef;
our $opt_gasbin      = 0;
our $opt_gasbintype  = undef;
our $opt_linemap     = undef;

# Debug options
our $debug_macros    = 0;
our $debug_filename  = 0;
our $debug_expr      = 0;

# directives that don't have any labels
our %cmd_withoutlabel = map { $_ => 1  } (
    "THUMB", "REQUIRE8", "PRESERVE8", "CODE16", "CODE32", "ELSE", "ENDIF",
    "ENTRY", "ENDP","LTORG", "MACRO", "MEND", "MEXIT", "NOFP", "WEND"
);
# directives that aren't really using labels
our %cmd_notlabel = map { $_ => 1  } (
    "RN", "QN", "DN", "ENTRY",
    "*", "#", "PROC", "FUNCTION",
    'SETA', 'SETL', 'SETS',
);

# FIXME: Old style operators - remove
# operators
our %operators = (
    ":OR:"   => "|",
    ":EOR:"  => "^",
    ":AND:"  => "& ",
    ":NOT:"  => "~",
    ":MOD:"  => "%",
    ":SHL:"  => "<<",
    ":SHR:"  => ">>",
    ":LOR:"  => "||",
    ":LAND:" => "&&",
);
our $operators_re = "(?:" . (join '|', keys %operators) . ")";
# FIXME: Old style operators - remove to here

##
# Convert a string (in quotes to its simple form).
sub unstr
{
    my ($arg) = @_;
    if ($arg =~ /^"([^"]*)"$/)
    {
        return $1;
    }
    return "$arg";
}

##
# Convert a unsigned number to a signed one.
sub signed
{
    my ($arg) = @_;
    our $datawidth;
    our $datatbs;
    our $datalimit;
    $arg = $arg & $datawidth;
    if ($arg & $datatbs)
    {
        $arg = $arg - $datalimit;
    }
    return $arg;
}

##
# Convert a signed number to an unsigned one.
sub unsigned
{
    my ($arg) = @_;
    our $datawidth;
    $arg = $arg & $datawidth;
    return $arg;
}

##
# Check if the parameters are strings
sub isstring
{
    for my $arg (@_)
    {
        if ($arg =~ /[^0-9\-]/ || $arg eq '')
        {
            return 1;
        }
    }
    return 0;
}

##
# Return a boolean value.
sub bool
{
    my ($arg) = @_;
    return $arg ? 1 : 0;
}


# operators
our %operators_binary = (
    # Arithmetic
    "+"      => sub { my ($left, $right) = @_; return $left + $right },
    "-"      => sub { my ($left, $right) = @_; return $left - $right },
    "*"      => sub { my ($left, $right) = @_; return $left * $right },
    "/"      => sub { my ($left, $right) = @_; return int($left / $right) },
    ":MOD:"  => sub { my ($left, $right) = @_; return $left % $right },
    # Binary
    ":OR:"   => sub { my ($left, $right) = @_; return $left | $right },
    ":EOR:"  => sub { my ($left, $right) = @_; return $left ^ $right },
    ":AND:"  => sub { my ($left, $right) = @_; return $left & $right },
    ":SHL:"  => sub { my ($left, $right) = @_; return $left << $right },
    ":SHR:"  => sub { my ($left, $right) = @_; return $left >> $right },
    # Boolean
    ":LOR:"  => sub { my ($left, $right) = @_; return bool($left || $right) },
    ":LAND:" => sub { my ($left, $right) = @_; return bool($left && $right) },
    # Comparison
    "<"      => sub { my ($left, $right) = @_;
                      return bool(isstring($left, $right)
                                ? ("$left" lt "$right")
                                : (unsigned($left) < unsigned($right))) },
    ">"      => sub { my ($left, $right) = @_;
                      return bool(isstring($left, $right)
                                ? ("$left" gt "$right")
                                : (unsigned($left) > unsigned($right))) },
    ">="     => sub { my ($left, $right) = @_;
                      return bool(isstring($left, $right)
                                ? ("$left" ge "$right")
                                : (unsigned($left) >= unsigned($right))) },
    "<="     => sub { my ($left, $right) = @_;
                      return bool(isstring($left, $right)
                                ? ("$left" le "$right")
                                : (unsigned($left) <= unsigned($right))) },
    "!="     => sub { my ($left, $right) = @_;
                      return bool(isstring($left, $right)
                                ? ("$left" ne "$right")
                                : (unsigned($left) != unsigned($right))) },
    "=="     => sub { my ($left, $right) = @_;
                      return bool(isstring($left, $right)
                                ? (unstr($left) eq unstr($right))
                                : (unsigned($left) == unsigned($right))) },
    # String
    ":LEFT:" => sub { my ($left, $right) = @_; $right = signed($right); return '"'. ($right > 0 ? substr(unstr($left), 0, $right) : '') .'"' },
    ":RIGHT:" => sub { my ($left, $right) = @_; $right = signed($right); return '"'. ($right > 0 ? substr(unstr($left), -$right) : '') .'"' },
    ":CC:"   => sub { my ($left, $right) = @_; return '"'. unstr($left) . unstr($right) .'"' },
    # FIXME: We don't support:
    #   ?symbol
    #   :BASE:
    #   :INDEX:
    #   :LNOT:
    #   :RCONST:
    #   :CC_ENCODING:
    #   :REVERSE_CC:
    #   :MOD:
    #   :ROR:
    #   :ROL:
    # See: https://developer.arm.com/documentation/dui0801/g/Symbols--Literals--Expressions--and-Operators/Unary-operators?lang=en
);
sub splitregs
{
    my ($rlist) = @_;
    my @list;
    our $regnames_re;
    if ($rlist =~ /^"([^"]*)"$/)
    {
        $rlist = $1;
    }
    $rlist =~ s/^\s*//;
    while ($rlist)
    {
        last if ($rlist eq '');
        if ($rlist =~ s/^([a-zA-Z])(\d+)\s*-\s*\1(\d+)//)
        {
            my $rn = lc($1);
            my $rl = $2 + 0;
            my $rh = $3 + 0;
            if ($rh <= $rl)
            {
                exit_error($ERR_SYNTAX,
                           "Register list parse failed (low must be below high): $rn$rl-$rn$rh");
            }
            push @list, map "$rn$_", ($rl..$rh);
        }
        elsif ($rlist =~ s/^([a-zA-Z])(\d+)//)
        {
            push @list, "$1$2";
        }
        elsif ($rlist =~ s/^($regnames_re)//i)
        {
            my $rn = lc $1;
            push @list, $rn;
        }
        else
        {
            exit_error($ERR_SYNTAX,
                       "Register list parse failed (unknown registers): $rlist");
        }

        $rlist =~ s/^\s*//;
        last if ($rlist eq '');
        if ($rlist =~ s/^,\s*//)
        {
            # All's well
        }
        else
        {
            exit_error($ERR_SYNTAX,
                       "Register list parse failed (cannot parse trailing text): $rlist");
        }
    }
    #print "SplitRegs: ".join(', ', @list)."\n";
    return @list;
}
our %operators_binary_extensions = (
    ":REGLISTLEFT:" => sub {
        my ($left, $right) = @_; $right = signed($right);
        my @regs = splitregs(unstr($left));
        if (scalar(@regs) <= $right)
        {
            return '"' . join(",", @regs) . '"';
        }
        #print "RLLEFT: ".join(",", @regs[0..$right-1])."\n";
        return '"' . join(",", @regs[0..$right-1]) . '"';
    }, # First $right items

    ":REGLISTSKIP:" => sub {
        my ($left, $right) = @_; $right = signed($right);
        my @regs = splitregs(unstr($left));
        if (scalar(@regs) <= $right)
        {
            return '""';
        }
        return '"' . join(",", @regs[$right..$#regs]) . '"';
    }, # Skip $right items, and return the rest


    ":REGLISTRIGHT:" => sub {
        my ($left, $right) = @_; $right = signed($right);
        my @regs = splitregs(unstr($left));
        if (scalar(@regs) <= $right)
        {
            return '"' . join(",", @regs) . '"';
        }
        return '"' . join(",", @regs[-$right..-1]) . '"';
    }, # Last $right items

    ":REGLISTTRIM:" => sub {
        my ($left, $right) = @_; $right = signed($right);
        my @regs = splitregs(unstr($left));
        if (scalar(@regs) <= $right)
        {
            #print "RLTRIM: <none>\n";
            return "";
        }
        #print "RLTRIM: ".join(",", @regs[0..$#regs-$right])."\n";
        return '"' . join(",", @regs[0..$#regs-$right]) . '"';
    }, # Trim off the last $right items
);
# FIXME: For now we always support the extensions
%operators_binary = (%operators_binary, %operators_binary_extensions);
# Aliases
$operators_binary{'%'} = $operators_binary{':MOD:'};
$operators_binary{'&'} = $operators_binary{':AND:'};
$operators_binary{'|'} = $operators_binary{':OR:'};
$operators_binary{'^'} = $operators_binary{':EOR:'};
$operators_binary{'<<'} = $operators_binary{':SHL:'};
$operators_binary{'>>'} = $operators_binary{':SHR:'};
$operators_binary{'='} = $operators_binary{'=='};
$operators_binary{'<>'} = $operators_binary{'!='};
$operators_binary{'/='} = $operators_binary{'!='};
our $operators_binary_re = "(?:" . (join '|', map { "\Q$_\E" } sort { length($b) cmp length($a) } keys %operators_binary) . ")";
our %operators_monadic = (
    # Arithmetic
    "+"      => sub { my ($right) = @_; return (0+$right) },
    "-"      => sub { my ($right) = @_; return (0-$right) },
    # Binary
    "~"      => sub { my ($right) = @_; our $datawidth; return ($right) ^ $datawidth },
    ":NOT:"  => sub { my ($right) = @_; our $datawidth; return ($right) ^ $datawidth },
    # String
    ":LEN:"  => sub { my ($right) = @_; return length(unstr($right)) },
    ":UPPERCASE:" => sub { my ($right) = @_; return '"'. uc(unstr($right)) .'"' },
    ":LOWERCASE:" => sub { my ($right) = @_; return '"'. lc(unstr($right)) .'"' },
    ":STR:"  => sub { my ($right) = @_; return sprintf "\"%08x\"", (0+$right); },
    ":CHR:"  => sub { my ($right) = @_; return sprintf "\"%c\"", (0+$right); },
);
our $operators_monadic_re = "(?:" . (join '|', map { "\Q$_\E" } keys %operators_monadic) . ")";

# simple replace
our %misc_op = (
    "ARM"       =>  ".arm",
    "THUMB"     =>  ".thumb",
    "REQUIRE8"  =>  ".eabi_attribute 24, 1",
    "PRESERVE8" =>  ".eabi_attribute 25, 1",
    "CODE16"    =>  ".code16",
    "CODE32"    =>  ".code32",
    "ALIGN"     =>  ".balign",
    "LTORG"     =>  ".ltorg",
    "INCBIN"    =>  ".incbin",
    "INCLUDE"   =>  ".include",  # Only processed when no --inline given
    "END"       =>  ".end",
    "WHILE"     =>  ".rept",
    "WEND"      =>  ".endr",
    "IMPORT"    =>  ".global",  # check before (weak attr)
    "EXTERN"    =>  ".global",
    "GLOBAL"    =>  ".global",
    "EXPORT"    =>  ".global",
    "RN"        =>  ".req",
    "QN"        =>  ".qn",
    "DN"        =>  ".dn",
    "ENTRY"     =>  "",
    "SUBT"      =>  "//",
);
our $misc_op_re = "(?:" . (join "|", keys %misc_op) . ")";

# Directives that take expressions
our %miscexpression_op = (
    "DCB"       =>  ".byte",
    "DCW"       =>  ".hword",
    "DCWU"      =>  ".hword",
    "DCD"       =>  ".word",
    "DCDU"      =>  ".word",
    "DCQ"       =>  ".quad",
    "DCQU"      =>  ".quad",
    "DCFS"      =>  ".single",
    "DCFSU"     =>  ".single",
    "DCFD"      =>  ".double",
    "DCFDU"     =>  ".double",
);
our $miscexpression_op_re = "(?:" . (join "|", keys %miscexpression_op) . ")";

# simple replace with quoted strings
our %miscquoted_op = (
    );

# variable initial value
our %init_val = (
    "A" => '0',         # arithmetic
    "L" => 'F',         # logical
    "S" => '""'         # string
);

# Built in variable values
# https://developer.arm.com/documentation/100069/0611/Using-armasm/Built-in-variables-and-constants
our %builtins = (
    'ARCHITECTURE' => '7',          # Might be a lie.
    'AREANAME' => 'NONE',           # No current area initially
    'ARMASM_VERSION' => 600000,     # Absolutely a lie
    'CODESIZE' => sub { our %builtins; return $builtins{'CONFIG'} },   # alias for CONFIG
    'CONFIG' => 32,                 # Need to update this from CLI.
    'COMMANDLINE' => '',            # No point in mapping this?
    'CPU' => 'Generic ARM',         # We'll just give it a generic name
                                    # https://developer.arm.com/documentation/100069/0611/armasm-Command-line-Options/--cpu-name?lang=en
    'ENDIAN' => 'little',           # RISC OS is always little endian
    'FPU' => 'FPA',                 # Not sure what this should be?
    'INPUTFILE' => sub {
            our $in_file;
            return $in_file // 'none';
        },
    'INTER' => 0,                   # Might need updating from CLI?
    'LINENUM' => sub {
            our $linenum_input;
            return $linenum_input;
        },
    'LINENUMUP' => 0,               # We could obtain this, but it's harder
    'LINENUMUPPER' => 0,            # We could obtain this, but it's harder
    'OPT' => 0,                     # We don't really support option values
    'PC' => 0,                      # We might be able to derive this
    'PCSTOREOFFSET' => 4,           # Might be a lie
    'ROPI' => 0,                    # Might need updating from CLI?
    'RWPI' => 0,                    # Might need updating from CLI?
    'VAR' => sub {
            our $mapping_base;
            return $mapping_base;
        },

    # Definitions
    'TRUE' => 1,
    'FALSE' => 0,

    # Target options
    'TARGET_ARCH_AARCH32' => 1,     # Might be a lie
    'TARGET_ARCH_AARCH64' => 0,     # Might be a lie
    'TARGET_ARCH_ARM' => 8,         # Might be a lie
    'TARGET_ARCH_THUMB' => 5,       # Might be a lie
    # We don't support any of the TARGET_FEATURE_* defines
    'TARGET_PROFILE_A' => 1,        # RISC OS only uses profile A
    'TARGET_PROFILE_R' => 0,        # RISC OS only uses profile A
    'TARGET_PROFILE_M' => 0,        # RISC OS only uses profile A
);

our %cmp_number = (
    '=' => sub { my ($a, $b) = @_; ($a == $b) ? 1 : 0; },
    '>' => sub { my ($a, $b) = @_; ($a > $b) ? 1 : 0; },
    '<' => sub { my ($a, $b) = @_; ($a < $b) ? 1 : 0; },
    '<=' => sub { my ($a, $b) = @_; ($a <= $b) ? 1 : 0; },
    '>=' => sub { my ($a, $b) = @_; ($a >= $b) ? 1 : 0; },
    '<>' => sub { my ($a, $b) = @_; ($a != $b) ? 1 : 0; },
);

# Names of all the general purpose registers we can use
our @regnames32 = (
    (map { "r$_" } (0..15)),
    'sp',
    'lr',
    'pc'
);
our @regnames64 = (
    (map { "x$_" } (0..30)),
    (map { "w$_" } (0..30)),
    'sp',
    'wsp',
    'lr',
    'xlr',
    'wlr',
    'xzr',
    'wzr',
    'pc'
);

# Data width
our $datawidth32 = 0xFFFFFFFF;
our $datawidth64 = 0xFFFFFFFFFFFFFFFF;
our $datatbs32 = (1<<31);
our $datatbs64 = (1<<63);
our $datalimit32 = (1<<32);
our $datalimit64 = (1<<64);

# Current data width
our $datawidth;
our $datatbs;
our $datalimit;

# Regular expression to match any of the registers
our $regnames32_re = "(?:" . (join "|", sort { length($a) <=> length($b) } @regnames32) . ")";
our $regnames64_re = "(?:" . (join "|", sort { length($a) <=> length($b) } @regnames64) . ")";

# Current regnames
our @regnames;
our $regnames_re;

# Configuration for the bitness
sub set_config {
    my ($bitness) = @_;
    our @regnames;
    our $regnames_re;
    if ($bitness == 64)
    {
        our @regnames64;
        our $regnames64_re;
        @regnames = @regnames64;
        $regnames_re = $regnames64_re;
        $builtins{'TARGET_ARCH_AARCH64'} = 1;
        $builtins{'TARGET_ARCH_AARCH32'} = 0;
        $builtins{'CONFIG'} = 64;
        $datawidth = $datawidth64;
        $datatbs = $datatbs64;
        $datalimit = $datalimit64;
    }
    else
    {
        our @regnames32;
        our $regnames32_re;
        @regnames = @regnames32;
        $regnames_re = $regnames32_re;
        $builtins{'TARGET_ARCH_AARCH64'} = 0;
        $builtins{'TARGET_ARCH_AARCH32'} = 1;
        $builtins{'CONFIG'} = 32;
        $datawidth = $datawidth32;
        $datatbs = $datatbs32;
        $datalimit = $datalimit32;
    }
}


# RISC OS extensions that we transform
my $extensions_dir_re = "s|hdr|c|h|cmhg|s_c|o|aof|bin|x";


#--------------------------------
# function definitions
#--------------------------------

# @args: exit_status, line, file, err_msg
sub exit_error {
    print STDERR "\e[01;31mERROR\e[0m: $_[1]\n";
    exit($_[0]);
}

sub msg_info {
    if ($opt_verbose) {
        print STDERR "\e[01;34mINFO\e[0m: $_[0]\n";
    }
}

# @args: no_exact_conv?, msg
sub msg_warn {
    if ($opt_strict && $_[0]) {
        exit_error($ERR_UNSUPPORT, $_[1]);
    }
    elsif (! $opt_nowarning) {
        print STDERR "\e[00;33mWARN\e[0m: $_[1]\n";
    }
}


#--------------------------------
# command-line arguments parsing
#--------------------------------
my @input_files     = ();
my @output_files    = ();
my $output_suffix   = '.out';

# Paths to search for files (in native format)
our @include_paths  = ('.');

# The variables are set with SET[ALS].
# The stack contains the global variables at the top,
# with local entries below that.
# Each set of entries is a dictionary containing the variable names.
# The value of each entry is a dictionary containing:
#   value => the value of the variable
#   context => the context it was created in
#   type => the variable type (A, L, S)
our @variable_stack = ({}, {});

# Default config
set_config(32);

GetOptions(
    "o|output=s"    => \@output_files,
    "x|suffix=s"    => \$output_suffix,
    "h|help"        => sub { print $helpmsg; exit },
    "return-code"   => sub { print $rvalmsg; exit },
    "V|version"     => sub { print "$ver\n"; exit },
    "compatible"    => \$opt_compatible,
    "v|verbose"     => \$opt_verbose,
    "s|strict"      => \$opt_strict,
    "n|no-comment"  => \$opt_nocomment,
    "w|no-warning"  => \$opt_nowarning,
    "inline"        => \$opt_inline,
    "simple-conditions" => \$opt_simplecond,
    "test-expr=s"   => \$opt_testexpr,
    "line-map=s"    => \$opt_linemap,
    "gas=s"         => \$opt_gastool,
    "bin"           => \$opt_gasbin,
    "rmf"           => sub { $opt_gasbintype = 'rmf'; },
    "util"          => sub { $opt_gasbintype = 'util'; },
    "aif"           => sub { $opt_gasbintype = 'aif'; },
    "predefine=s"   => sub {
            my ($switch, $arg) = @_;
            if ($arg =~ /^(\w+)\s+SET([ALS])\s+(.*)$/)
            {
                my ($var, $type, $values) = ($1, $2, $3);
                my ($val, $trail) = expression($values);
                if ($trail)
                {
                    exit_error($ERR_SYNTAX, "Trailing text '$trail' not recognised on predefine '$arg'");
                }
                declare_variable($var, 'G', $type);
                set_variable($var, $val, $type);
            }
            else
            {
                exit_error($ERR_SYNTAX, "Unrecognised predefine: '$arg'");
            }
        },
    "i|include=s"   => sub {
            my ($switch, $arg) = @_;
            # Ensure that the paths are passed in with an extension
            for my $path (split /,/, $arg)
            {
                my $exp = expand_paths($path);
                if (!defined $exp)
                {
                    # Referred to a RISC OS variable but none set; skip.
                    next;
                }
                if (scalar(@$exp) == 0)
                {
                    # There wasn't any path present, so just append raw
                    push @include_paths, $path;
                }
                else
                {
                    # Multiple paths present, so append them all
                    push @include_paths, @$exp;
                }
            }
        },
    "64" => sub { set_config(64); },
    "32" => sub { set_config(32); },
    "debug=s" => sub {
            my ($switch, $arg) = @_;
            my @list = split / *, */, $arg;
            for my $a (@list)
            {
                if ($a eq 'expr')
                { $debug_expr = 1; }
                elsif  ($a eq 'filename')
                { $debug_filename = 1; }
                elsif  ($a eq 'macro')
                { $debug_macros = 1; }
                else
                {
                    exit_error($ERR_SYNTAX, "Unrecognised debug option: '$a'");
                }
            }
        },
) or die("Conversion of ObjASM source to GAS source failed\n");

@input_files = @ARGV;

# validate input
if (scalar(@input_files) == 0 && !$opt_testexpr) {
    exit_error($ERR_ARGV, "$toolname".
        ": No input file");
}
elsif (scalar(@output_files) > 0 && $#input_files != $#output_files && !$opt_testexpr) {
    exit_error($ERR_ARGV, "$toolname".
        ": Input and output files must match one-to-one");
}
elsif ($output_suffix !~ /^\.*\w+$/ && !$opt_testexpr) {
    exit_error($ERR_ARGV, "$toolname".
        ": Invalid suffix '$output_suffix'");
}

if (defined $opt_gasbintype)
{
    $opt_gasbin = 1;
}

# pair input & output files
if (scalar(@output_files) == 0 && !$opt_testexpr) {
    @output_files = map {"$_$output_suffix"} @input_files;
}
my %in_out_files;
@in_out_files{@input_files} = @output_files;
my $writing;

if (!$opt_inline)
{
    %miscquoted_op = (
        %miscquoted_op,
        "GET"       =>  ".include",
        "INCLUDE"   =>  ".include",
    );
}
our $miscquoted_op_re = "(?:" . (join "|", keys %miscquoted_op) . ")";



# Variable definitions
our $mapping_base       = 0;
my $mapping_register    = undef;
my %mapping             = ();
my %constant            = ();

# Macros
our %macros;
our $macroname;

# Count of the nesting of the macros used
our $macro_nesting = 0;
our $macro_nesting_limit = 64;

our %linemapping;

# Conditional stack.
my @cond_stack = (1,);


END {
    # If we failing whilst writing a file, delete it.
    if (defined $writing)
    {
        unlink $writing;
    }
}


# file processing
foreach (keys %in_out_files) {
    # global vars for diagnosis
    our $in_file  = $_;
    our $out_file = $in_out_files{$_};
    our $linenum_output  = 1;
    our $context;
    my $elf_file = undef;

    %linemapping = ();

    open(my $f_in, "<", $_)
        or exit_error($ERR_IO, "$toolname: Cannot read input '$in_file': $!");

    our $f_out;
    if ($out_file eq '-')
    {
        if ($opt_gastool)
        {
            exit_error($ERR_SYNTAX, "$toolname: Cannot write to stdout when assembling output")
        }

        # Write to the stdout stream
        open($f_out, ">&", STDOUT)
            or exit_error($ERR_IO, "$toolname: <STDOUT>: $!");
    }
    else
    {
        if ($opt_gastool)
        {
            $elf_file = $out_file;
            $out_file = "$in_file.gas";
        }
        open($f_out, ">", $out_file)
            or exit_error($ERR_IO, "$toolname: Cannot create output '$out_file': $!");
        $writing = $out_file;
    }

    process_file($f_in, $in_file, undef);
    $context = "$in_file";

    if (defined $macroname)
    {
        exit_error($ERR_SYNTAX, "$toolname: $in_file: Macro '$macroname' not ended at end of file")
    }
    if ($builtins{'AREANAME'} eq 'NONE')
    {
        msg_warn(1, "$context".
            ": AREA directive not used");
    }

    close $f_in  or exit_error($ERR_IO, "$toolname: $in_file: $!");
    close $f_out or exit_error($ERR_IO, "$toolname: $out_file: $!");

    if ($opt_linemap)
    {
        # Produce a line mapping file
        open(my $f_lm, ">", $opt_linemap) || exit_error($ERR_IO, "$toolname: Cannot write line mapping file '$opt_linemap': $!");
        our $linemapping;
        for my $line (sort { $a <=> $b } keys %linemapping)
        {
            my $lineis = $linemapping{$line};
            print $f_lm "$line: $lineis\n";
        }
        close($f_lm);
    }

    if ($opt_gastool && $out_file ne '-')
    {
        assemble_file($opt_gastool, $out_file, $elf_file, $in_file);
        # If we were successful, remove the temporary file
        unlink($out_file);
    }

    $writing = undef;
}

if ($opt_testexpr)
{
    our $context = "Expression";
    my ($value, $tail) = expression($opt_testexpr);
    print "Test expression: '$opt_testexpr'\n";
    print "Gave: '$value'\n";
    print "Tail: '$tail'\n";
}

sub assemble_file
{
    my ($gas, $file, $output, $infile) = @_;
    my $elfoutput = $output;
    my $binoutput;

    if ($opt_gasbin)
    {
        # If they asked for a binary, then we replace the elf with a temporary file.
        $binoutput = $output;
        $elfoutput = "tmp-gas-elf.$$";
    }

    my $cmd = "$gas $file -o $elfoutput < /dev/null 2>&1";
    msg_info("Assembling with: $cmd");

    my $fh;
    if (!open($fh, '-|', $cmd))
    {
        unlink $elfoutput;
        exit_error($ERR_IO, "$toolname: $output: Failed to assemble with GNU as: $!");
    }

    # Process all the lines of the input
    while (<$fh>)
    {
        s!(\Q$file\E):(\d+)!my $l = "$1:$2"; if ($1 eq $file) { $l = $linemapping{$2} // "<generated>:$2" } $l;!ge;
        s!(\Q$file\E): !($infile // "<generated>") . ": ";!ge;
        print;
    }
    close($fh);
    my $rc=$?;
    if ($rc)
    {
        unlink $elfoutput;
        exit_error($ERR_IO, "$toolname: $output: Failed to assemble with GNU as: rc=".($rc>>8));
    }

    if ($binoutput)
    {
        # after assembling the ELF they want to link as a binary.
        if (defined $opt_gasbintype)
        {
            # They wanted a specific type; which means we need to use the
            # linker.
            my $linker = "riscos64-link -$opt_gasbintype";
            $cmd = "$linker -o $binoutput $elfoutput";
        }
        else
        {
            my $objcopy = $gas;
            if ($objcopy =~ s/as$/objcopy/)
            {}
            else
            {
                unlink $elfoutput;
                exit_error($ERR_IO, "$toolname: $output: Cannot infer 'objcopy' command");
            }
            $cmd = "$objcopy -O binary $elfoutput $binoutput";
        }
        msg_info("Linking with: $cmd");
        if (system($cmd))
        {
            unlink $elfoutput;
            exit_error($ERR_IO, "$toolname: $output: Failed to link into a binary");
        }
        unlink $elfoutput;
    }
}


sub process_file {
    my ($f_in, $filename) = @_;

    our $in_file;
    our $out_file;
    our $linenum_input;
    our $linenum_output;
    our $context;
    our $f_out;
    # Remember the caller's context
    my @caller_context = ($in_file, $linenum_input, $context);
    my $our_context = '';
    # Strip the last output line reference
    $context =~ s/ -> [^ ]+$// if ($context);
    if ($context)
    {
        $our_context = "$context -> ";
    }

    my $end_used;

    $end_used = 0;
    $in_file = $filename;
    $linenum_input = 1;
    while (my $line = <$f_in>) {
        chomp $line;
        my $linecontext = "$our_context$in_file:$linenum_input";
        $context = "$linecontext -> $out_file:$linenum_output";
        $linenum_input++;
        if ($line =~ /^\s+END\s*$/)
        {
            # Handle explicit file end here.
            $end_used = 1;
            last;
        }
        my $outputline = single_line_conv($line);
        if (defined $outputline)
        {
            print $f_out "$outputline\n";
            my @nlines = split /\n/, $outputline, -1;
            @nlines = ('') if ($outputline eq '');
            #print "$linenum_output : ".(scalar(@nlines)) . "\n";
            for my $i (0..scalar(@nlines)-1)
            {
                #print "$i => $linecontext\n";
                $linemapping{$linenum_output + $i} = $linecontext; # . " : $nlines[$i]";
            }
            $linenum_output += scalar(@nlines);
        }
    }
    print $f_out "\n";  # required by as
    $linenum_output += 1;
    $context = "$in_file";

    if (!$end_used)
    {
        msg_warn(1, "$context".
            ": END directive not used at end of file");
    }

    ($in_file, $linenum_input, $context) = @caller_context;
}


sub expand_paths {
    my ($filename) = @_;

    my @paths;

    # Expand the filename given to multiple path
    if ($filename =~ /^([a-zA-Z][a-zA-Z0-9_]*):(.*)$/ ||
        $filename =~ /^<([a-zA-Z][a-zA-Z0-9_]*\$[Dd][Ii][Rr])>(?:\.(.*))?$/)
    {
        # This is a RISC OS style path/dir variable.
        my $pathvar = $1;
        my $suffix = $2 // '';
        if ($pathvar =~ /\$/)
        {
            # This is actually a RISC OS directory variable
            $pathvar =~ s/\$/_/;
            my $val = $ENV{uc($pathvar)};
            if (!defined $val)
            {
                return undef;
            }
            @paths = ($val);
        }
        else
        {
            my $val = $ENV{uc($pathvar)};
            if (!defined $val)
            {
                return undef;
            }

            @paths = split /,/, $val;
        }

        $suffix =~ s/^[\.\/]+//;
        # We now have a list of paths to search, and a suffix path
        # We need the suffix path to be in host format so that we can add
        # to the paths
        if ($suffix =~ /\./)
        {
            $suffix =~ s!\.!/!g;
        }
        @paths = map { my $path = "$_/$suffix/";
                    $path =~ s!//+!/!g;
                    $path;
                 } @paths;
        @paths = grep { -d $_ } @paths;
    }
    return \@paths;
}


sub resolve_filename {
    my ($filename) = @_;
    our $in_file;
    print "Resolving: $filename\n" if ($debug_filename);
    return $filename if (-f $filename);

    my @paths = @include_paths;
    if ($filename =~ /^([a-zA-Z][a-zA-Z0-9_]*):(.*)$/ ||
        $filename =~ /^<([a-zA-Z][a-zA-Z0-9_]*\$[Dd][Ii][Rr])>(?:\.(.*))?$/)
    {
        # This is a RISC OS style path/dir variable.
        my $pathvar = $1;
        my $suffix = $2 // '';

        my $pathref = expand_paths($pathvar);
        if (!defined $pathref)
        {
            exit_error($ERR_IO, "$toolname: $filename: Cannot expand RISC OS path/directory variables")
        }
        @paths = @$pathref;
        $filename = $suffix;
        # Remove any leading . or /.
        $filename =~ s/^[\/\.]//;
    }
    print "Resolving against paths: ". (join ", ",@paths) ."\n" if ($debug_filename);


    # Apply the filename to the relative location of the current source file.
    my $basedir = $in_file;
    $basedir =~ s/[^\/]+$//;   # Trim leafname
    if ($basedir ne '')
    {
        unshift @paths, $basedir;
    }

    for my $pathdir (@paths)
    {
        my $newfilename;
        my $path = $pathdir;
        if ($path eq '.' || $path eq '@' || $path eq '')
        {
            $path = '';
        }
        else
        {
            $path =~ s!([^/])$!$1/!;
        }
        print "Path prefix: $path\n" if ($debug_filename);

        if ($filename =~ m!^/! && $pathdir ne '')
        {
            # If this is an explicit path and the path is rooted, we can skip this.
            next;
        }

        if ($path ne '')
        {
            $newfilename = "$path$filename";
            print "  Trying $newfilename\n" if ($debug_filename);
            return $newfilename if (-f $newfilename);
        }

        # If the path is in unix form of the RISC OS extensions, strip it.
        # Ie if the source file was <dirs>/s/<leaf> the base directory is <dirs>
        if ($path =~ /(^|.*\/)($extensions_dir_re)\/?$/)
        {
            $path = $1;
        }

        # We'll try to apply the file that was supplied again
        if ($path ne '')
        {
            $newfilename = "$path$filename";
            return $newfilename if (-f $newfilename);
        }

        # Now let's try applying the filename given as a RISC OS style filename
        my $unixised = $filename;
        #print "Unixised: $unixised\n";
        # Convert (<dirs>.)?s.<leaf> to <dirs>/s/<leaf>
        if ($unixised =~ /(^|.*\.)($extensions_dir_re)\.([^\.]+)$/)
        {
            $unixised =~ tr!/.!./!;
            $newfilename = "$path$unixised";
            print "  Trying $newfilename\n" if ($debug_filename);
            return $newfilename if (-f $newfilename);
        }

        # Let's try it with the names transformed
        # For example convert hdr.foo to foo.hdr
        if ($unixised =~ /(^|.*\/)($extensions_dir_re)\/([^\/]+)$/)
        {
            my $newfilename = "$path$1$3.$2";
            print "  Trying $newfilename\n" if ($debug_filename);
            return $newfilename if (-f $newfilename);
        }

        # Try it the other way around.
        # If they gave us foo.hdr check this as hdr/foo.
        if ($unixised =~ /(^|.*\/)([^\.]+)\.($extensions_dir_re)$/)
        {
            my $newfilename = "$path$1$3/$2";
            print "  Trying $newfilename\n" if ($debug_filename);
            return $newfilename if (-f $newfilename);
        }
    }

    exit_error($ERR_IO, "$toolname: $filename: Cannot find file (searched paths: " . (join ", ", @paths) . ")");
}


sub include_file {
    my ($filename) = @_;

    $filename = resolve_filename($filename);

    open(my $f_in, "<", $filename)
        or exit_error($ERR_IO, "$toolname: $filename: $!");

    process_file($f_in, $filename);

    close $f_in  or exit_error($ERR_IO, "$toolname: $filename: $!");
}


sub expand_macro {
    my ($macroname, $label, $values) = @_;
    our $macros;
    my $macrodef = $macros{$macroname};
    my %macrovars;
    our $context;
    my @valuelist;
    my $valueparse=$values;
    my $comment = undef;

    msg_info($context.
        ": Expanding macro '$macroname' with args: $values");

    $macro_nesting += 1;
    if ($macro_nesting > $macro_nesting_limit)
    {
        exit_error($ERR_SYNTAX, "$context".
            ": Macro nesting levels exceeded at macro '$macroname': $values");
    }

    print "Expand macro '$macroname' args: $values\n" if ($debug_macros);
    while ($valueparse ne '')
    {
        next if ($valueparse =~ s/^\s+//);
        if ($valueparse =~ s/^((?:"[^"]*"[^,]*)+?)\s*(,|$)//)
        {
            # Quoted string.
            print "Quoted value '$1'\n" if ($debug_macros);
            my ($val, $tail) = expression($1);
            if ($val =~ /^"(.*)"$/)
            {
                $val = $1;
            }
            push @valuelist, $val;
        }
        elsif ($valueparse =~ s/^([\-0-9a-zA-Z_]+)\s*(;|,|$)/$2/)
        {
            print "Simple value '$1'\n" if ($debug_macros);
            push @valuelist, $1;
        }
        elsif ($valueparse =~ s/^('.')\s*(;|,|$)/$2/)
        {
            print "Simple char value '$1'\n" if ($debug_macros);
            push @valuelist, $1;
        }
        elsif ($valueparse =~ s/^,/,/)
        {
            print "Empty value\n" if ($debug_macros);
            push @valuelist, '';
        }
        elsif ($valueparse =~ s/^([^,;]+)(;|,|$)/$2/)
        {
            my $val = $1;
            print "Literal value: $val\n" if ($debug_macros);
            push @valuelist, $val;
        }
        # Remove any training spaces
        $valueparse =~ s/^\s+//;
        if ($valueparse =~ s/^,//)
        {
            # There is another parameter present.
        }
        elsif ($valueparse =~ s/^;//)
        {
            # There is another parameter present.
            $comment = $valueparse;
            print "Comment: $comment\n" if ($debug_macros);
            last;
        }
        else
        {
            # No more parameters supplied, so we're done.
            print "Giving up at: $valueparse\n" if ($debug_macros);
            last;
        }
    }

    if (!defined $macrodef)
    {
        exit_error(128, "$context".
            ": Internal consistency failure: Macro '$macroname' being expanded which isn't known");
    }

    #print "Value list: ", (join ', ', @valuelist), " / params: ", join( ', ', @{ $macrodef->{'params'} }), "\n";
    if (scalar(@valuelist) > scalar(@{ $macrodef->{'params'} }))
    {
        msg_warn(1, "$context".
            ": Macro '$macroname' supplied with too many arguments (params: '$values')");
    }
    for my $index (0.. scalar(@{ $macrodef->{'params'} }) -1)
    {
        my $varname = $macrodef->{'params'}->[$index];
        last if (!defined $varname);
        my $value = $valuelist[$index];
        if (!defined $value or $value eq '|')
        {
            $value = $macrodef->{'defaults'}->[$index] // "";
        }
        $macrovars{$varname} = $value;
        #print "Macro param #$index '$varname' => $value\n";
    }
    if (defined $label && $label ne '')
    {
        if (!defined $macrodef->{'label'})
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Macro '$macroname' supplied with a label but none supported by the macro");
        }
        $macrovars{ $macrodef->{'label'} } = $label;
    }
    else
    {
        if (defined $macrodef->{'label'})
        {
            # If the label isn't given, the result is empty string
            $macrovars{ $macrodef->{'label'} } = "";
            #exit_error($ERR_SYNTAX, "$context".
            #    ": Macro '$macroname' requires a label");
        }
    }

    our $in_file;
    our $out_file;
    our $linenum_input;
    our $linenum_output;
    our $f_out;
    # Remember the caller's context
    my @caller_context = ($in_file, $linenum_input, $context);
    # Strip the last output line reference
    $context =~ s/ -> [^ ]+$// if ($context);
    my $our_context = '';
    if ($context)
    {
        $our_context = "$context -> ";
    }
    $our_context = "${our_context}[Macro $macroname] ";

    # We're in a macro
    # ... so set up the label stack
    my $inrout = $label_stack[-1]->{'rout'};
    push @label_stack, {'backward'=>{}, 'forward'=>{}, 'rout'=>"_${inrout}_macro_$macroname"};
    # ... and the local variable stack
    push @variable_stack, {};

    if ($comment)
    {
        my $outputline = single_line_conv("        ;$comment");
        if (defined $outputline)
        {
            print $f_out "$outputline\n";
        }
    }

    $in_file = $macrodef->{'base_file'};
    $linenum_input = $macrodef->{'base_line'};
    for my $line (@{ $macrodef->{'lines'} })
    {
        my $linecontext = "$our_context$in_file:$linenum_input";
        $context = "$linecontext -> $out_file:$linenum_output";
        $linenum_input++;
        my $macroline = "$line";

        # Perform the substitutions
        if ($macroline =~ /\$/)
        {
            $macroline =~ s/(\$[A-Za-z_][A-Za-z0-9_]*)\.?/$macrovars{$1} \/\/ "$1"/ge;
        }

        #print "Macroline: $macroline\n";
        my $outputline = single_line_conv($macroline);
        if (defined $outputline)
        {
            print $f_out "$outputline\n";
            my @nlines = split /\n/, $outputline, -1;
            @nlines = ('') if ($outputline eq '');
            for my $i (0..scalar(@nlines)-1)
            {
                #print "$i => $linecontext\n";
                $linemapping{$linenum_output + $i} = $linecontext; # . " : $nlines[$i]";
            }
            $linenum_output += scalar(@nlines);
        }
    }

    # Leaving the macro
    # ... so pop the label stack
    my $macro_labels = pop @label_stack;
    if (scalar( keys %{ $macro_labels->{'backward'} }) > 0)
    {
        # If any labels were defined, increment the sequence number, so that we don't clash.
        $label_sequence++;
    }
    # ... and variable stack
    pop @variable_stack;

    # Restore the old context
    ($in_file, $linenum_input, $context) = @caller_context;

    $macro_nesting -= 1;
}


sub single_line_conv {
    our $in_file;
    our $out_file;
    our $linenum_input;
    our $linenum_output;
    our $context;
    our $macroname;
    our %macros;
    my $line = shift;

    # empty line
    if ($line =~ m/^\s*$/) {
        if ($cond_stack[-1])
        {
            return "";    # just keep it
        }
        else
        {
            return undef;
        }
    }

    # warn if detect a string
    #if ($line =~ m/"+/) {
    #    msg_warn(0, "$context".
    #        ": Conversion containing strings needs a manual check");
    #}

    # ------ Recording macros ------
    if ($line =~ /^\s+MACRO/)
    {
        # Marker to show we're in a macro
        if (defined $macroname)
        {
            msg_warn(1, "$context".
                ": Macro '$macroname' contains a nested macro");
        }

        $macroname = '1';
        return undef;
    }
    if (defined $macroname)
    {
        if ($macroname eq '1')
        {
            # This is the first line after the MACRO, which defines what the macro is.
            if ($line =~ /^(?:(\$[a-zA-Z_]\w*))?\s*(\w+)\s*(.*?)(\/\/.*)?$/) {
                my ($label, $name, $args, $comment) = ($1, $2, $3, $4);
                $macroname = $name;
                my @arglist = split /\s*,\s*/, $args;
                my @defaultlist;
                for my $arg (@arglist)
                {
                    if ($arg !~ /^\$/)
                    {
                        msg_warn(1, "$context".
                            ": Macro '$name' argument '$arg' does not start with \$'");
                    }
                    if ($arg =~ /^(.*?)=(.*)$/)
                    {
                        push @defaultlist, $2;
                        $arg = $1; # Note: Modifies the value in the arglist
                    }
                    else
                    {
                        push @defaultlist, undef;
                    }
                }
                #print "Defining macro '$macroname'\n";
                $macros{$macroname} = {
                        'lines' => [],
                        'label' => $label,
                        'params' => \@arglist,
                        'defaults' => \@defaultlist,
                        'comment' => $comment,
                        # Remember the file and line we are in
                        'base_file' => $in_file,
                        'base_line' => $linenum_input,  # Start *after* the definition line (we already incremented)
                    };
            }
            else
            {
                msg_warn(1, "$context".
                    ": Macro prototype is not recognised ($line)");
            }
        }
        else
        {
            # We're inside a macro definition
            if ($line =~ /^([\$\w]+)?\s+MEND(\/\/.*)?$/) {
                # We're leaving a macro
                if (!defined $macroname)
                {
                    msg_warn(1, "$context".
                        ": MEND used when not in a macro");
                }
                $macroname = undef;
            }
            else
            {
                push @{ $macros{$macroname}->{'lines'} }, $line;
            }
        }
        return undef;
    }

    # ------ Conversion: comments ------
    if ($line =~ m/(^|\s+);/) {
        if ($opt_nocomment) {
            $line =~ s/(^|\s+);.*$//;
        }
        else {
            $line =~ s/(^|\s+);/$1\/\//;
        }
        if ($line =~ /^\s*\/\//)
        {
            # If the line is ONLY comments, we're done
            if ($cond_stack[-1])
            {
                return $line;
            }
            else
            {
                return undef;
            }
        }
    }

    # Try to decode the line into its components.
    my $label;
    my $lspcs;
    my $cmd;
    my $cspcs;
    my $values;
    my $vspcs;
    my $comment;

    $line =~ s/(\$([A-Za-z_][A-Za-z0-9_]*))\.?/
                my ($s, $v) = ($1, $2);
                my $var = find_variable($v);
                my $val;
                if (defined $var)
                { $val = unstr($var->{'value'}); }
                $val \/\/ $s/ge;

    # FIXME: This parse doesn't handle strings with // in.
    if ($line =~ /^((?:[A-Z_a-z0-9][A-Z_a-z0-9]*|[0-9]+|\|[^\|]+\|)?)?(\s+)([^\s]*)(\s*)(.*?)(\s*)(\/\/.*)?$/ ||
        $line =~ /^((?:[A-Z_a-z0-9][A-Z_a-z0-9]*|[0-9]+|\|[^\|]+\|)?)?()()()()(\s*)(\/\/.*)?$/)
    {
        $label=$1;
        $lspcs=$2;
        $cmd=$3;
        $cspcs=$4;
        $values=$5;
        $vspcs=$6;
        $comment=$7 // '';

        if ($label =~ /^\|(.*)\|$/)
        {
            $label = $1;
        }
    }
    elsif ($line =~ /^(\s*)(\/\/.*)$/)
    {
        $label = '';
        $lspcs = $1;
        $cmd = '';
        $cspcs = '';
        $values = '';
        $vspcs = '';
        $comment = $2;
    }
    else
    {
        msg_warn(1, $context.
            ": Unrecognised line format '$line'");
        return undef;
    }
    #print "LINE: label='$label', cmd='$cmd', values='$values', comment='$comment'\n";

    # ------ Conversion: conditional directives ------
    if ($cmd eq 'IF' || $cmd eq '[')
    {
        if ($opt_simplecond)
        {
            if ($values =~ /^\s*:NOT:\s*:DEF:\s*(.*)/)
            {
                $cmd = '.ifndef';
                $values = $1;
            }
            elsif ($values =~ /^\s*:DEF:\s*(.*)/)
            {
                $cmd = '.ifdef';
                $values = $1;
            }
            else
            {
                $cmd = '.if';
                $values = evaluate($values);
            }
            goto reconstruct;
        }
        else
        {
            # Interpreted conditionals
            #print ''. (" " x scalar(@cond_stack)) . "IF '$values'\n";
            my $cond = $cond_stack[-1];
            if ($cond)
            {
                my ($value, $trail) = expression($values);
                if ($trail ne '')
                {
                    exit_error($ERR_SYNTAX, $context.
                        ": Trailing text '$trail' not recognised on $cmd");
                }
                $cond = ($value ne '0');
                $cond = 0 if ($cond_stack[-1] == 0);
            }
            push @cond_stack, $cond;

            # We ignore this line in all cases
            return undef;
        }
    }
    elsif ($cmd eq 'ELSE' || $cmd eq '|')
    {
        if ($opt_simplecond)
        {
            $cmd = '.else';
            goto reconstruct
        }
        else
        {
            # Interpreted conditionals
            if (scalar(@cond_stack) == 1)
            {
                exit_error($ERR_SYNTAX, $context.
                    ": Invalid conditional: $cmd specified outside of conditional");
            }
            my $cond = (!$cond_stack[-1]);
            $cond = 0 if ($cond_stack[-2] == 0);
            $cond_stack[-1] = $cond;

            # We ignore this line in all cases
            return undef;
        }
    }
    elsif ($cmd eq 'ELSEIF')
    {
        if ($opt_simplecond)
        {
            exit_error($ERR_SYNTAX, $context.
                ": Conditional ELSEIF not supported");
        }
        else
        {
            # Interpreted conditionals
            if (scalar(@cond_stack) == 1)
            {
                exit_error($ERR_SYNTAX, $context.
                    ": Invalid conditional: $cmd specified outside of conditional");
            }
            #print ''. (" " x (scalar(@cond_stack)-1)) . "ELSEIF $values\n";

            my $cond = $cond_stack[-2];
            $cond_stack[-1] = $cond;

            # This isn't right - we'll actually execute *all* the ELSEIF clauses that are true, not just first
            if ($cond)
            {
                my ($value, $trail) = expression($values);
                if ($trail ne '')
                {
                    exit_error($ERR_SYNTAX, $context.
                        ": Trailing text '$trail' not recognised on $cmd");
                }
                my $cond = ($value ne '0');
                $cond_stack[-1] = $cond;
            }

            # We ignore this line in all cases
            return undef;
        }
    }
    elsif ($cmd eq 'ENDIF' || $cmd eq ']')
    {
        if ($opt_simplecond)
        {
            $cmd = '.endif';
            goto reconstruct
        }
        else
        {
            #print ''. (" " x (scalar(@cond_stack)-1)) . "ENDIF\n";
            # Interpreted conditionals
            if (scalar(@cond_stack) == 1)
            {
                exit_error($ERR_SYNTAX, $context.
                    ": Invalid conditional: $cmd specified outside of conditional");
            }
            pop @cond_stack;

            # We ignore this line in all cases
            return undef
        }
    }

    if (!$opt_simplecond)
    {
        if ($cmd ne 'END')
        {
            # END is always processed
            if (!$cond_stack[-1])
            {
                # Not true condition, so ignore the line.
                return undef;
            }
        }
    }

    # ------ Conversion: Macro expansion ------
    if ($cmd && defined $macros{$cmd})
    {
        # Macro expansion requested.
        #print "Macro expansion '$cmd' of '$label', '$values'\n";
        expand_macro($cmd, $label, $values);
        return undef;
    }

    # ------ Conversion: space reservation ------
    if ($cmd eq 'SPACE' || $cmd eq '%')
    {
        $cmd = '.space';
        # FIXME: Doesn't support the <expr>, <fill>, <fillsize> form
        my $trail;
        ($values, $trail) = expression($values);
        if ($trail ne '')
        {
            exit_error($ERR_SYNTAX, $context.
                ": Trailing text '$trail' not recognised on $cmd");
        }
        goto reconstruct;
    }

    # ------ Conversion: local label assignment ------
    if ($label =~ /^\d+$/)
    {
        # A numeric label.
        my $forward = $label_stack[-1]->{'forward'};
        my $symbol;
        if (defined $forward->{$label})
        {
            # This is a previously mentioned label, so the one we created can be used
            $symbol = $forward->{$label};
            delete $forward->{$label};
        }
        else
        {
            # This is a new label.
            # Only increment the sequence if this replaces a prior label name
            if (defined $label_stack[-1]->{'backward'}->{$label})
            {
                $label_sequence++;
            }

            my $label_rout = $label_stack[-1]->{'rout'};
            $symbol = "L${label_rout}__local_${label}_$label_sequence";
        }
        # Remember the symbol so that we can look up what it means when we go backwards.
        $label_stack[-1]->{'backward'}->{$label} = $symbol;

        $line =~ s/^\Q$label\E(\s?)\s*/$symbol$1/;
        $lspcs = $1;
        $label = $symbol;
    }

    # ------ Conversion: label usage ------
    if ($values =~ /(?<![0-9A-Za-zA-Z_)])%([FB]?)([TA]?)(\d+)/i && $values !~ /"/)
    {
        # There is a label usage present, which we need to expand
        my $direction = uc $1;
        my $scoping = uc $2;
        my $number = $3;
        # If no direction is given, we search backwards then forwards
        # T searches in this scope; A searches all levels; nothing should search upwards
        # We're not going to support the 'nothing' for scoping because it's a lot more complex.

        my $symbol;
        # Backwards search
        if ($direction eq 'B' or $direction eq '')
        {
            if ($scoping eq 'T')
            {
                # Search backward in current scope
                $symbol = $label_stack[-1]->{'backward'}->{$number};
            }
            else
            {
                # Search backward in all scopes
                for my $stack (reverse @label_stack)
                {
                    $symbol = $stack->{'backward'}->{$number};
                    last if (defined $symbol);
                }
            }
        }

        # Forwards search, or search if the backwards failed and no direction given
        if ($direction eq 'F' or ($direction eq '' and !defined $symbol))
        {
            if ($scoping eq 'T')
            {
                # Search backward in current scope
                $symbol = $label_stack[-1]->{'forward'}->{$number};
            }
            else
            {
                # Search backward in all scopes
                for my $stack (reverse @label_stack)
                {
                    $symbol = $stack->{'forward'}->{$number};
                    last if (defined $symbol);
                }
            }

            # IF the symbol was not found we need to create the symbol name we will use.
            my $label_rout = $label_stack[-1]->{'rout'};
            $symbol = "L${label_rout}__local_${number}_$label_sequence";
            $label_stack[-1]->{'forward'}->{$number} = $symbol;
        }

        if (!defined $symbol)
        {
            exit_error($ERR_SYNTAX, "$context".
                       ": Search for local label '$number' failed");
        }

        # Now replace the value
        $values =~ s/%([FB]?)([TA]?)(\d+)/$symbol/i;

        # Update the line as well
        $line =~ s/%([FB]?)([TA]?)(\d+)/$symbol/i;

        #print "Converted label in values to $values\n";
        #print "Converted label in line to $line\n";
    }

    # Routine name declaration
    if ($cmd eq 'ROUT')
    {
        # Clear out the current context in the label stack
        pop @label_stack;
        # FIXME: We could mark any forward operation that hadn't been used with an error?
        push @label_stack, {'backward'=>{}, 'forward'=>{}, 'rout'=>'rout'};
        $label_sequence += 1;

        if ($label eq '')
        {
            # There's nothing else to do here
            return undef;
        }
        $label_stack[-1]->{'rout'} = $label;
        $line = "$label: $lspcs$cspcs$vspcs$comment";
        $line =~ s/:\s*$/:/;
        return $line;
    }

    # Label splitting
    if ($label ne '' and not defined $cmd_notlabel{$cmd})
    {
        my $newline = '';
        # Labelled statement seen.
        if ($cmd ne '')
        {
            #print "COMMAND: $cmd\n";
            if (defined $cmd_withoutlabel{$cmd})
            {
                exit_error($ERR_SYNTAX, "$context".
                           ": Directive '$cmd' is not allowed to have a label");
            }
            $line =~ s/^(\|?\Q$label\E\|?)/' ' x length($1)/e;
            $newline = single_line_conv($line);
        }
        #print "Got back $newline\n";
        if (defined $newline)
        {
            if ($label =~ /[^A-Za-z0-9_]/)
            {
                # Label contains special characters
                $line = ".set \"$label\", .\n$newline";
            }
            else
            {
                $line = "$label:\n$newline";
            }
            $line =~ s/\s+$//;
            return $line;
        }
    }

    # Byte/String constants
    if ($cmd eq '=' || $cmd eq 'DCB' || $cmd =~ /^$miscexpression_op_re$/)
    {
        my $op = $miscexpression_op{$cmd} // '.byte';
        # The constants can be things like:
        # = "hello"
        # = "hello", "there"
        # = 1, 2, 3
        # = "hello", 32, "there"
        # = "hello", 0
        # We will decode this as a sequence of regex matched strings
        my @lines = ();
        my @num_accumulator = ();
        my $is_fp = ($op eq '.single' || $op eq '.double');
        my $and = 0;
        $and = 0xFFFFFFFF if ($op eq '.word');
        $and = 0xFFFF if ($op eq '.hword');
        $and = 0xFF if ($op eq '.byte');
        while ($values ne '')
        {
            my ($value, $nextvalues) = expression($values, $is_fp);
            if (!defined $value)
            {
                exit_error($ERR_SYNTAX, "$context".
                    ": Literal sequences cannot interpret '$values'");
            }


            if ($value =~ /^"(.*)"$/)
            {
                if ($op ne '.byte')
                {
                    exit_error($ERR_SYNTAX, "$context".
                        ": Non-byte literal sequences cannot use strings for $value");
                }

                my $str = $1;
                if ($nextvalues =~ s/^\s*,\s*0(?=\s|$)//)
                {
                    # This is a zero-terminated string, so dump it out raw.
                    if (@num_accumulator)
                    {   # Flush the number accumulator
                        push @lines, "$op " . join ", ", @num_accumulator;
                        @num_accumulator = ();
                    }
                    push @lines, ".asciz$cspcs\"$str\"";
                }
                else
                {
                    # This is a string, so dump it out
                    if (@num_accumulator)
                    {   # Flush the number accumulator
                        push @lines, "$op " . join ", ", @num_accumulator;
                        @num_accumulator = ();
                    }
                    push @lines, ".ascii$cspcs\"$str\"";
                }
            }
            elsif (defined $value)
            {
                # This is a number, so we want to accumulate it.
                if (!$is_fp && !is_pc_relative($value))
                {
                    if ($and)
                    {
                        $value = $value & $and;
                    }
                    push @num_accumulator, gas_number($value, 1);
                }
                else
                {
                    # FP numbers are left alone
                    push @num_accumulator, $value;
                }
            }
            else
            {
                msg_warn(1, "$context".
                    ": Literal byte sequence cannot interpret '$values'".
                    ", need a manual check");
                last;
            }
            $values = $nextvalues;
            if ($values =~ /^,/ || $values eq '')
            {
                # Trim any commas between parameters
                $values =~ s/^(,\s*)+//;
            }
            else
            {
                msg_warn(1, "$context".
                    ": Literal byte sequence cannot interpret '$values'".
                    ", need a manual check");
                last;
            }
        }

        if (@num_accumulator)
        {   # Flush the number accumulator
            push @lines, "$op$cspcs" . join ", ", @num_accumulator;
            @num_accumulator = ();
        }

        if (@lines)
        {
            if ($comment)
            {
                $lines[-1] .= "$vspcs$comment";
            }
        }

        my $unlabelled = 0;
        my $sprefix = (' ' x length($label)) . $lspcs;
        $line = join "\n", map { $unlabelled++ ? "$sprefix$_" : "$label$lspcs$_" } @lines;
        return $line;
    }
    elsif ($cmd eq 'SPACE')
    {
        $cmd = '.space';
        my $trail;
        ($values, $trail) = expression($values);
        if ($trail)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Trailing text '$trail' at the end of SPACE reservation not supported");
        }
        goto reconstruct;
    }

    # ------ Conversion: mappings ------
    if ($cmd eq '^') {
        our $mapping_base;
        my $value = $values;
        if ($values =~ m/^(.*),\s*($regnames_re)$/)
        {
            $value = $1;
            $mapping_register = $2;
        }
        else
        {
            $mapping_register = undef;
        }
        my $trail;
        ($mapping_base, $trail) = expression($value);
        if ($trail)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Trailing text '$trail' at the end of mapping definition");
        }
        return undef;
    }
    if ($cmd eq '#') {
        our $mapping_base;
        if (!defined $mapping_base)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Attempt to define field mapping for '$label' without setting up base ('^' not used)");
        }
        my ($size, $trail) = expression($values);
        if ($trail)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Trailing text '$trail' at the end of field definition '$label'");
        }
        $mapping{$label} = [$mapping_base, $mapping_register, $size];
        $comment //= '';
        $cspcs = '' if (!$comment);
        $line = ".set $label, " . gas_number($mapping_base) . $cspcs . $comment;
        $mapping_base += $size;
        return $line;
    }

    # ------ Conversion: constants ------
    if ($cmd eq '*' || $cmd eq 'EQU') {
        $constant{$label} = evaluate($values);
        $comment //= '';
        $cspcs = '' if (!$comment);
        $line = ".set $label, " . gas_number($constant{$label}) . $cspcs . $comment;
        return $line;
    }

    # ------ Conversion: includes ------
    if ($opt_inline && ($cmd eq 'GET' or $cmd eq 'INCLUDE')) {
        my $file = $values;
        msg_info($context.
            ": Inline inclusion of '$file'");

        include_file($file);

        return undef;
    }

    # ------ Conversion: functions ------
    if ($cmd eq 'PROC' or $cmd eq 'FUNCTION') {
        my $func_name = $label;
        if ($opt_compatible) {
            push @symbols, $func_name;
            if ($values)
            {
                $line = ".type $label, \"function\"$values\n$label:$cspcs$comment";
            }
            else
            {
                $line = "$label:$cspcs$comment";
            }
        }
        else {
            $line = ".func $label\n$label:$cspcs$comment";
        }
        return $line;
    }
    elsif ($cmd eq 'ENDP' or $cmd eq 'ENDFUNC') {
        if ($opt_compatible) {
            my $func_name = pop @symbols;
            my $func_end  = ".L$func_name"."_end";
            $line = "$func_end:${lspcs}\n$1.size $func_name, $func_end-$func_name$cspcs$comment";
        }
        else {
            $line = ".endfunc$cspcs$comment";
        }
    }

    # ------ Conversion: symbol definition ------
    if ($cmd =~ m/^(LCL|GBL)([A|L|S])$/) {
        my $scope = ($1 eq 'LCL') ? 'L' : 'G';
        my $type = $2;
        my $var = $values;
        declare_variable($var, $scope, $type);
        if ($scope eq 'G')
        {
            $line = "$lspcs.set$cspcs$var, $init_val{$type}$vspcs$comment";
            $line = "$lspcs.global $var\n" . $line;
            return $line;
        }
        else
        {
            return undef;
        }
    }
    elsif ($cmd =~ /^SET([A|L|S])$/i) {
        my $var = $label;
        my $type = $1;
        my ($val, $trail) = expression($values);
        if ($trail)
        {
            exit_error($ERR_SYNTAX, $context.
                ": Trailing text '$trail' not recognised on $cmd");
        }
        my $v = set_variable($var, $val, $type);
        if ($v->{'scope'} eq 'G')
        {
            $line = ".set $var, $val";
            return $line;
        }
        return undef;
    }

    # ------ Conversion: sections ------
    if ($cmd eq 'AREA' && ($values =~ /^\|*([.\w\$]+)\|*(.*)$/))
    {
        my $sec_name = $1;
        my @options  = split /,/, $2;

        my $flags = "a";
        my $args  = "";
        my @warnings = ();
        my $align = undef;
        my $marked_readonly = 0;
        for (@options) {
            s/^ *//;
            s/ *$//;
            $args .= ", \%progbits" if (s/DATA//i);
            $args .= ", $1" if (m/MERGE\s*=\s*(\d+)/i);
            $args .= ", $1" if (m/GROUP\s*=\s*\|*(\w+)\|*/i);

            $flags .= "x"   if (s/CODE//i);
            $flags .= "w"   if (s/READWRITE//i);
            $flags =~ s/a// if (s/NOALLOC//i);
            $flags .= "M"   if (s/MERGE\s*=\s*(\d+)//i);
            $flags .= "G"   if (s/GROUP\s*=\s*\|*(\w+)\|*//i);

            if (s/ALIGN\s*=\s*(\d+)//i)
            {
                $align = $1;
            }

            # Sections are readonly by default in ELF
            $marked_readonly = 1 if (s/READONLY//i);

            if ($_ ne "")
            {
                push @warnings, $_;
            }
        }
        if (!$marked_readonly && $args =~ /\%progbits/ && $flags !~ /w/)
        {
            # It's data, and they didn't mark as readonly, and didn't say READWRITE
            # So it must be readwrite (ObjAsm defaults to read-write if you don't say)
            $flags .= 'w';
        }
        if (scalar(@warnings) > 0)
        {
            # Only generate a warning when there was something we didn't understand
            msg_warn(1, "$context".
                ": Not all AREA attributes are supported".
                " (ignored: " . join(', ', @warnings) . ")");
        }

        # Fix up the section name
        if ($sec_name =~ /^\./)
        {
            # The section starts with a '.' so we assume it is literal and use as is.
        }
        else
        {
            # The section name does not start with a '.' so probably this is a raw
            # ObjAsm file with the RISC OS style C$$code or similar. Let's map it
            # to something sane.
            $sec_name =~ s/\$\$?(code|data)//i;
            $sec_name =~ s/\$/_/;
            my $sec = '.text';
            if ($args =~ /\%progbits/)
            {
                if ($flags =~ /w/)
                {
                    $sec = '.data';
                }
                else
                {
                    $sec = '.rodata';
                }
            }

            if ($sec_name eq 'C')
            {
                # C code, so just put it in a .text / .rodata / .data area
                $sec_name = "$sec";
            }
            else
            {
                $sec_name = "$sec.$sec_name";
            }
        }

        # Should the AREANAME be the section name, or the literal area they supplied?
        $builtins{'AREANAME'} = "$sec_name";

        msg_info($context.
            ": Starting section '$sec_name', flags '$flags'");

        $line = "${label}${lspcs}.section${cspcs}$sec_name, \"$flags\"$args${vspcs}${comment}";

        if (defined($align)) {
            $line .= "\n${lspcs}.balign " . (2**$align);
        }
        return $line;
    }

    # ------ Conversion: expression constants ------
    if ($values =~ /#/)
    {
        my $orig = $values;
        my @acc = ();
        while ($values ne '')
        {
            if ($values =~ s/^([^"#]+)//)
            {
                push @acc, $1;
            }
            elsif ($values =~ s/^("[^"]*")//)
            {
                push @acc, $1;
            }
            elsif ($values =~ s/^#//)
            {
                # We've found a constant expression, so we evaluate
                push @acc, '#';
                my ($value, $trail) = expression($values);
                if (!defined $value)
                {
                    exit_error($ERR_SYNTAX, $context.
                        ": Cannot parse expression: '$values'");
                }
                push @acc, gas_number($value);
                $values = $trail;
            }
            elsif ($values =~ /^"[^"]$/)
            {
                exit_error($ERR_SYNTAX, $context.
                    ": Unbalanced quotes in operands '$orig'");
            }
            else
            {
                exit_error($ERR_SYNTAX, $context.
                    ": Internal consistency error parsing operands '$orig'");
            }
        }
        $values = join "", @acc;
        goto reconstruct;
    }

    # ------ Conversion: numeric literals ------
    if ($line =~ m/(\s*)\w+\s*(\w+).*([A|L]S[L|R])\s*(#\d+)(\s*\/\/.*)?$/) {
        my $indent    = $1;
        my $reg       = $2;
        my $op        = $3;
        my $imp_shift = $4;
        msg_warn(0, "$context".
            ": Implicit shift is not supported in GAS".
            ", converting to explicit shift");
        $line =~ s/,\s*$op\s*$imp_shift//i;
        $line .= $indent . "$op $reg, $reg, $imp_shift\n";
    }

    # Expand numeric literals
    $line = expand_literals($line, "$context");
    #print "Expand literals gave: $line\n";

    # ------ Conversion: references to field mappings ------
    if ($line =~ m/(\s)(ADR|LDR|STR)([A-Z]*)(\s+)($regnames_re)(\s*,\s*)(\w+)/)
    {
        my ($space1, $instbase, $extra, $space2, $reg1, $comma, $symbol) = ($1, $2, $3, $4, $5, $6, $7);
        if (defined $mapping{$symbol})
        {
            my ($value, $reg) = @{ $mapping{$symbol} };
            if (!defined $reg)
            {
                exit_error($ERR_SYNTAX, "$context".
                    ": Attempt to use field mapping for '$symbol' without a register base in definition");
            }
            my $replacement;
            if ($instbase eq 'ADR')
            {
                $replacement = "$reg, #$value";
                $instbase = 'ADD';
            }
            else
            {
                $replacement = "[$reg, #$value]";
            }
            $line =~ s/(\s)(ADR|LDR|STR)([A-Z]*)(\s+)($regnames_re)(\s*,\s*)(\w+)/$space1$instbase$extra$space2$reg1$comma$replacement/;
        }
    }
    #print "About to finish: $line\n";

    # ------ Conversion: misc directives ------
    # weak declaration
    if ($line =~ m/(EXPORT|GLOBAL|IMPORT|EXTERN)(\s+)(\w+|\|[^\|]+\|).+\[\s*WEAK\s*\]/i) {
        my $drctv = $1;
        my $spcs = $2;
        my $sym = $3;
        if ($sym =~ /^\|(.*)\|$/)
        {
            $sym = $1;
        }
        if ($sym =~ /[^a-zA-Z0-9_]/)
        {
            $sym = "\"$sym\"";
        }
        $line = ".weak $sym";
        return $line;
    }
    # INFO directive
    if ($line =~ m/(INFO\s+\d+\s*,)\s*".*"/i) {
        my $prefix = $1;
        $line =~ s/$prefix/.warning /i;
    }

    if ($line =~ s/\b($misc_op_re)\b/$misc_op{$1}/eg)
    {
        my $op = $misc_op{$1};
        $line = expand_variables($line);
        if ($line !~ /\s+\.balign\s+[^0-9]/)
        {
            $line =~ s/(\s+\.balign)(\s+[^0-9]|\s*$)/$1 4$2/;
        }
        if ($op eq '.global')
        {
            $line =~ s/\|([^\|]+)\|/"$1"/;
        }
    }

    # ------ Conversion: labels on instructions ------
    if ($line =~ m/^([a-zA-Z_][a-zA-Z_0-9]*)(\s+)([A-Z])/) {
        my $label = $1;
        my $spaces = $2;
        my $inst1 = $3;
        my $prefix = (' ' x length($label)) . $spaces;
        $line =~ s/$label$spaces$inst1/$label:\n$prefix$inst1/;
    }

    # postprocess
    if ($line =~ m/^\s*$/) {
        # delete empty line
        return undef;
    }
    return $line;


reconstruct:
    # Reconstruct the line from the components
    if ($label ne '' && $label !~ /:$/)
    {
        $lspcs = (' ' x length($label)) . $lspcs;
        $label = "$label:\n";
    }
    $line = "$label$lspcs$cmd$cspcs$values$vspcs$comment";
    $line =~ s/\s*$//;
    return $line;
}

sub dec2hex {
    my $decnum = shift;
    my $hexnum = "";
    my $tempval;

    while ($decnum != 0) {
        $tempval = $decnum % 16;
        $tempval = chr($tempval + 55) if ($tempval > 9);
        $hexnum = $tempval . $hexnum ;
        $decnum = int($decnum / 16);

        if ($decnum < 16) {
            $decnum = chr($decnum + 55) if ($decnum > 9);
            $hexnum = $decnum . $hexnum;
            $decnum = 0;
        }
    }
    return $hexnum;
}


##
# Find a variable
#
# @param[in] $var:      The variable to set
#
# @return:  The variable declaration hashref, or undef if not found
sub find_variable
{
    my ($var, $val) = @_;
    for my $vars (reverse @variable_stack)
    {
        my $v = $vars->{$var};
        if (defined $v)
        {
            return $v;
        }
    }
    return undef;
}


##
# Declare a variable
#
# @param[in] $var:      The variable to declare
# @param[in] $scope:    G or L for global or local scope
# @param[in] $type:     Type of variable (L, A, S)
sub declare_variable
{
    my ($var, $scope, $type) = @_;
    my $vlist;
    our $context;
    if ($scope eq 'G')
    {
        $vlist = $variable_stack[0];
    }
    else
    {
        $vlist = $variable_stack[-1];
    }
    my $v = {
            'value' => $init_val{$type},
            'type' => $type,
            'context' => $context,
            'scope' => $scope,
        };
    $vlist->{$var} = $v;
}


##
# Set a variable
#
# @param[in] $var:      The variable to set
# @param[in] $val:      The value to set
# @param[in] $type:     Type to set as
sub set_variable
{
    my ($var, $val, $vtype) = @_;
    our $context;
    for my $vars (reverse @variable_stack)
    {
        my $v = $vars->{$var};
        if (defined $v)
        {
            # Variable exists at this level of the stack.
            my $type = $v->{'type'};
            if ($vtype ne $type)
            {
                # FIXME: Should we warn that you tried to set it with the wrong type? Or Error?
            }
            if ($type eq 'L')
            {
                if ($val eq 'T' || $val eq 'TRUE' || $val eq '1')
                {
                    $val = 'T';
                }
                elsif ($val eq 'F' || $val eq 'FALSE' || $val eq '0')
                {
                    $val = 'F';
                }
                else
                {
                    $val = (!!$val) ? 'T' : 'F';
                }
            }
            elsif ($type eq 'A')
            {
                if ($val eq 'T' || $val eq 'TRUE')
                {
                    $val = '1';
                }
                elsif ($val eq 'F' || $val eq 'FALSE' || $val eq '')
                {
                    $val = '0';
                }
                else
                {
                    $val = 0 + $val;
                }
            }
            elsif ($type eq 'S')
            {
                $val = "$val";
            }
            $v->{'value'} = $val;
            # The value was assigned
            return $v;
        }
    }
    exit_error($ERR_SYNTAX, "$context".
        ": Variable '$var' cannot be set to type $vtype, value '$val' as it has not been declared with GBL or LCL");
}


##
# Expand the literals into values that can be evaluated in GNU AS (or in Perl)
#
# @param[in] $line:     The line to process.
#
# @return Line with literals replaced so that they can be processed by GNU AS.
sub expand_literals
{
    my ($line) = @_;
    our $context;
    while (1)
    {
        if ($line =~ m/(-?)&([\dA-F]+)/i) {
            my $sign    = $1;
            my $hex_lit = $2;
            #msg_info($context.
            #    ": Converting hexadecimal '&$hex_lit' to '0x$hex_lit'");
            $line =~ s/${sign}&$hex_lit/${sign}0x$hex_lit/;
        }
        elsif ($line =~ m/(-?)(?<![A-Za-z_0-9])([2|8])_(\d+)/) {
            my $sign = $1;
            my $base = $2;
            my $lit  = $3;
            my $cvt  = dec2hex (($base eq "2") ?
                oct("0b".$lit): oct($lit));
            msg_info($context.
                ": Converting '$sign${base}_$lit' to hexadecimal literal '${sign}0x$cvt'");

            $line =~ s/$sign${base}_$lit/${sign}0x$cvt/;
        }
        else
        {
            last;
        }
    }
    return $line;
}

##
# Expand variables
#
# @param[in] $expr:     Expression to evaluate
#
# @return:  Expanded expression with variables handled
sub expand_variables
{
    my ($expr) = @_;

    # Unknown builtin will be evaluated as 0.
    $expr =~ s/\{([A-Za-z_][A-Za-z0-9_]*)\}/
               my $val = defined $builtins{$1} ? (ref($builtins{$1}) eq 'CODE' ? $builtins{$1}->() : $builtins{$1}) : 0;
               if ($val =~ m![^0-9]!)
               { $val = '"' . $val . '"'; }
               $val;
              /ge;
    return $expr;
}


##
# Evaluate an expression, if we can.
#
# @param[in] $expr:     Expression to evaluate
#
# @return:  (value as a number if possible or string if not,
#            trailing string)
sub expression
{
    my ($expr, $is_fp) = @_;
    my $orig = $expr;
    our $context;
    $is_fp = 0 if (!defined $is_fp);

    # This is not going to be a proper parser for numeric expressions; it's going
    # to be very simple just to parse expressions left to right.
    my $left = undef;
    my $operator = undef;
    my $monadic = undef;
    my $monadicstr = undef;

    # Trim the leading spaces so that we can expand things easier
    $expr =~ s/^\s+//;

    # Try to catch PC-relative expressions that we cannot handle.
    if ($expr eq '.' || $expr eq '{PC}')
    {
        return ('.', '');
    }
    if (is_pc_relative($expr))
    {
        $expr =~ s/\{PC}/ . /g;
        while (1)
        {
            my $oldexpr = $expr;
            $expr =~ s/\(([^).]+)\)/my $bracket = $1;
                                    my ($result, $tail) = expression($1);
                                    if ($tail)
                                    {
                                       "($bracket)";
                                    }
                                    else
                                    {
                                       "$result";
                                    }
                                   /ge;
            if ($expr eq $oldexpr)
            {
                last;
            }
        }
        $expr =~ s/^\s+//;
        $expr =~ s/\s+$//;
        $expr =~ s/\s\s+/ /;
        return ($expr, '');
    }

    while ($expr ne '')
    {
        print "Expression parse: '$expr'\n" if ($debug_expr);
        if (!defined $left || defined $operator)
        {
            # Monadic operators can only happen if there is no left parameter, or
            # an operator has been given (start of line or after operator).
            if ($expr =~ s/^($operators_monadic_re)//)
            {
                my $monoop = $operators_monadic{$1};
                $monadicstr = defined $monadicstr ? "$monadicstr$1" : $1;
                print "Monadic operator: $1 (now: $monadicstr)\n" if ($debug_expr);
                if (defined $monadic)
                {
                    # A monadic op has already been seen, so we need to chain these
                    # operators.
                    my $lastmonadic = $monadic;
                    $monadic = sub { my ($right) = @_; return $monoop->($lastmonadic->($right)) };
                    goto next_token;
                }
                $monadic = $monoop;
                goto next_token;
            }
        }

        if (defined $left && !defined $monadic && !defined $operator)
        {
            # We're expecting an operator here.
            if ($expr =~ s/^($operators_binary_re)//)
            {
                my $binop = $operators_binary{$1};
                print "Binary operator: $1\n" if ($debug_expr);
                $operator = $binop;
                goto next_token;
            }
        }

        # We are now expecting an expression, either for the left or the right.
        my $value;
        if ($expr =~ s/^\(//)
        {
            # Bracketted expression, so we need to extract from that expression the value.
            ($value, $expr) = expression($expr, $is_fp);
            if ($expr =~ s/^\)//)
            {
                # All is well, they ended with a bracket
            }
            else
            {
                exit_error($ERR_SYNTAX, "$context".
                    ": Evaluation of variables in '$orig' was missing a closing bracket, instead got '$expr'");
            }
        }
        # Override for the assembler special '.' or '{PC}'.
        elsif ($expr =~ s/^(\.(?![0-9])|\{PC})//)
        {
            $value = '.';
        }
        # Builtins check
        elsif ($expr =~ s/^\{([A-Za-z_][A-Za-z0-9_]*)\}//)
        {
            print "Builtin: $1\n" if ($debug_expr);
            $value = defined $builtins{$1} ? (ref($builtins{$1}) eq 'CODE' ? $builtins{$1}->() : $builtins{$1}) : 0;
            if ($value =~ m![^0-9]!)
            { $value = '"' . $value . '"'; }
        }
        # Simple strings and values
        elsif ($expr =~ s/^("[^"]*")//)
        {
            my $str_lit = $1;
            $value = $str_lit;
        }
        elsif ($expr =~ s/^2_([01]+)//)
        {
            my $bin_lit = $1;
            $value = oct("0b$bin_lit");
        }
        elsif ($expr =~ s/^8_([0-7]+)//)
        {
            my $oct_lit = $1;
            $value = oct($oct_lit);
        }
        elsif ($expr =~ s/^(?:&|0x)([\dA-Fa-f]+)//)
        {
            my $hex_lit = $1;
            $value = hex($hex_lit);
        }
        elsif ($expr =~ s/^'((?:[^\\]|\\.)*?)'//)
        {
            my $char_lit = $1;
            $char_lit =~ s/^\\(.)/$1/g;
            $value = ord($char_lit);
        }
        elsif ($expr =~ s/^((?:\.\d+|\d*\.\d+|\d+)(?:E-?\d+)?)//)
        {
            my $dec_lit = $1;
            $value = $dec_lit + 0;
        }
        # Special checks
        elsif ($expr =~ s/^:DEF:\s*([A-Za-z_][A-Za-z0-9_]*)//)
        {
            my $varname = $1;
            my $var = find_variable($varname);
            $value = $var ? 1 : 0;
        }
        # Mappings and constants
        elsif ($expr =~ s/^([A-Za-z_][A-Za-z0-9_]*)//)
        {
            my $name = $1;
            if (defined $mapping{$name})
            {
                $value = $mapping{$name}->[0];  # We use the value of the mapping (ignore register and size)
            }
            elsif (defined $constant{$name})
            {
                $value = $constant{$name};
            }
            else
            {
                # Not a mapping or a constant, so it could be an SET value.
                my $var = find_variable($name);
                if (defined $var)
                {
                    if ($var->{'type'} eq 'A' || $var->{'type'} eq 'S')
                    {
                        $value = $var->{'value'};
                    }
                    elsif ($var->{'type'} eq 'L')
                    {
                        $value = $var->{'value'} eq 'T' ? 1 : 0;
                    }
                    else
                    {
                        exit_error($ERR_SYNTAX, "$context".
                            ": Internal consistency failure during evaluation of expression in '$orig'; unrecognised SET type");
                    }
                }
                else
                {
                    if (0)
                    {
                        # Variable isn't recognised.
                        exit_error($ERR_SYNTAX, "$context".
                            ": Evaluation of variables in '$orig' could not find a value for '$name' at '$expr'");
                    }
                    else
                    {
                        # The variable is probably a label. We will need to pass through this
                        # expression unmodified.
                        return ($orig, '');
                    }
                }
            }
        }
        else
        {
            # Something we don't understand
            last;
        }

        if (defined $monadic)
        {
            # Call the monadic functions to operate on the value;
            $value = $monadic->($value);
            $monadic = undef;
            if ($monadicstr =~ /[+-]$/ && !defined $operator)
            {
                $operator = $operators{'+'};
            }
        }

        if (!defined $left)
        {
            $left = $value;
        }
        elsif (defined $operator)
        {
            my $right = $value;
            $value = $operator->($left, $right);
            print " ($left, $right) => $value\n" if ($debug_expr);
            $left = $value;
            $operator = undef;
        }
        else
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Internal consistency failure during evaluation of expression in '$orig'; no operator defined");
        }

next_token:
        $expr =~ s/^\s+//;
    }

    if (defined $left && defined $operator)
    {
        exit_error($ERR_SYNTAX, "$context".
            ": Evaluation of variables in '$orig' was missing a right hand operator at '$expr'");
    }

    if (defined $monadic)
    {
        exit_error($ERR_SYNTAX, "$context".
            ": Evaluation of variables in '$orig' failed at a monadic expansion due to missing right hand value (at '$expr')");
    }

    return ($left, $expr);
}


##
# Evaluate an expression, if we can.
#
# @param[in] $expr:     Expression to evaluate
#
# @return:  value as a number if possible.
sub evaluate
{
    my ($expr) = @_;
    my $orig = $expr;
    our $context;
    our %cmp_number;

    #print "Expression: $expr\n";

    $expr = expand_literals($expr);

    $expr =~ s/($operators_re)/$operators{$1}/ig;

    $expr =~ s/\b([A-Za-z_][A-Za-z0-9_]*)\b/defined $mapping{$1} ? $mapping{$1}->[0] : $1/ge;
    $expr =~ s/\b([A-Za-z_][A-Za-z0-9_]*)\b/defined $constant{$1} ? $constant{$1} : $1/ge;

    $expr = expand_variables($expr);

    $expr =~ s/(&|0x)([A-Fa-f0-9]+)/hex($2)/ge;

    # Manual replacement of the string operators where we can.
    # FIXME: We only support the simplest of operator forms here
    while ($expr =~ /[:<=>\|&+\-*\/%]/)
    {
        my $before = $expr;
        #print "Before: $before\n";
        $expr =~ s/:LEN:\s*"([^"]*)"/length($1)/ge;
        $expr =~ s/"([^"]*)"\s*:LEFT:\s*(-?\d+)/'"' . ($2 > 0 ? substr($1, 0, $2) : '') . '"'/ge;
        $expr =~ s/"([^"]*)"\s*:RIGHT:\s*(-?\d+)/'"' . ($2 > 0 ? substr($1, -$2) : ''). '"'/ge;
        $expr =~ s/"([^"]*)"\s*:CC:\s*"([^"]*)"/'"' . $1 . $2 . '"'/ge;
        $expr =~ s/:UPPERCASE:\s*"([^"]*)"/'"' . uc($1) . '"'/ge;
        $expr =~ s/:LOWERCASE:\s*"([^"]*)"/'"' . lc($1) . '"'/ge;
        $expr =~ s/:STR:\s*(\d+)/sprintf "\"%08x\"", $1/ge;
        $expr =~ s/:CHR:\s*(\d+)/sprintf "\"%c\"", $1/ge;
        # FIXME: We don't support:
        #   ?symbol
        #   :BASE:
        #   :INDEX:
        #   :DEF:
        #   :LNOT:
        #   :RCONST:
        #   :CC_ENCODING:
        #   :REVERSE_CC:
        #   :MOD:
        #   :ROR:
        #   :ROL:
        # See: https://developer.arm.com/documentation/dui0801/g/Symbols--Literals--Expressions--and-Operators/Unary-operators?lang=en

        # Simple expressions
        if ($expr =~ /[+*\/%\-<>\|&~]/)
        {
            $expr =~ s/~(-?\d+)/~ (0+$1)/ge;
            $expr =~ s/(-?\d+)\s*<<\s*(-?\d+)/$1 << $2/ge;
            $expr =~ s/(-?\d+)\s*>>\s*(-?\d+)/$1 >> $2/ge;
            $expr =~ s/(-?\d+)\s*\*\s*(-?\d+)/$1 * $2/ge;
            $expr =~ s/(-?\d+)\s*\/\s*(-?\d+)/$1 \/ $2/ge;
            $expr =~ s/(-?\d+)\s*\%\s*(-?\d+)/$1 % $2/ge;
            $expr =~ s/(-?\d+)\s*\+\s*(-?\d+)/$1 + $2/ge;
            $expr =~ s/(-?\d+)\s*-\s*(-?\d+)/$1 - $2/ge;
            $expr =~ s/(-?\d+)\s*\|\s*(-?\d+)/(0+$1) | (0+$2)/ge;
            $expr =~ s/(-?\d+)\s*&\s+(-?\d+)/(0+$1) & (0+$2)/ge;
        }

        if ($expr =~ /[<>=]/)
        {
            $expr =~ s/(-?\d+)\s*(=|>|<|<=|>=|<>)\s*(-?\d+)/$cmp_number{$2}->($1, $3)/ge;
        }

        # Brackets around a bare number means that it can be handled alone
        $expr =~ s/\(\s*(-?\d+)\s*\)/$1/g;
        # And same for brackets around a string
        $expr =~ s/\(\s*("[^"]*")\s*\)/$1/g;
        #print "After: $expr\n";

        last if ($before eq $expr);
    }

    if ($expr =~ m/(:[A-Z]+:)/i) {
        msg_warn(1, "$context".
            ": Unsupported operator $1".
            ", need a manual check");
    }

    my $value = $expr;
    #my $value = eval $expr;
    #if ($@)
    #{
    #    # Something went wrong in the evaluation; let's just fault this.
    #    exit_error($ERR_SYNTAX, "$context".
    #        ": Evaluation of variables in '$orig' produced '$expr' which failed: $@");
    #}

    return $value;
}

##
# Value is PC relative?
#
# @param[in] $value:        Value to check
#
# @return:  True if this is PC relative.
sub is_pc_relative
{
    my ($value) = @_;
    if ($value eq '.' || $value =~ /\.\s*[-+]/ || $value =~ /[+-]\s*\./ || $value =~ /\{PC}/)
    {
        return 1;
    }
    return 0;
}


##
# Convert a numeric value to a GAS value
#
# @param[in] $expr:         Value to present
# @param[in] $positive:     1 to ignore negative numbers
#
# @return:  The string that can be used in GNU AS for the value, but is humanly readable.
sub gas_number
{
    my ($expr, $positive) = @_;
    my $value;
    $positive = $positive // 0;

    if (!defined $expr)
    {
        our $context;
        my ($pkg, $file, $line) = caller;
        exit_error(128, "$context".
            ": Internal consistency failure at gas_number: Called with undefined value from $file:$line");
    }
    if (is_pc_relative($expr))
    {
        # Pass a PC-relative string through
        return $expr;
    }

    my $sign = '';
    if ($expr =~ /[^0-9]/)
    {
        $value = evaluate($expr);
    }
    else
    {
        $value = 0 + $expr;
    }

    if ($positive)
    {
        $value = $value & $datawidth;
    }
    else
    {
        if ($value & $datatbs)
        {
            #$value = ($value & $datawidth) - $datalimit;
            # 64bit calculations in perl don't really work, so this should
            # be good enough.
            $value = ($value & 0xFFFFFFFFFFFF) - 0x1000000000000;
        }
        if ($value < 0)
        {
            $sign = '-';
            $value = -$value;
        }
    }
    if ($value >= 0 && $value < 1024)
    {
        return "$sign$value";
    }
    if ($value < 0x10000)
    {
        return sprintf("${sign}0x%04x", $value);
    }
    if ($value < 0x1000000)
    {
        return sprintf("${sign}0x%06x", $value);
    }
    if ($value < 0x100000000)
    {
        return sprintf("${sign}0x%08x", $value);
    }
    if ($value < 0x1000000000000)
    {
        return sprintf("${sign}0x%012x", $value);
    }
    return sprintf("0x%016x", $value);
}
