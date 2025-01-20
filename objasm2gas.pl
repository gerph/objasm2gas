#!/usr/bin/perl
##
# Convert ObjAsm-like syntax to somethng that can be assembled by GNU assembler.
# Based on https://github.com/yxnan/arm2gas
# Releasd under GPL-3 license.

use strict;
use warnings;
use feature ":5.14";
no warnings qw(experimental);
no warnings qw(portable);
use Getopt::Long;

my $toolname = 'objasm2gas';
my $ver = '1.1';
my $helpmsg = <<"USAGE";
$toolname (v$ver) - Convert legacy ARM assembly syntax (used by objasm) to GNU syntax (GAS)

Usage: $toolname [<options>] <file1> [<file2>...]
Options:
    -c, --compatible            Keeps compatibility with armclang assembler
    -h, --help                  Show this help text
    -i, --verbose               Show a message on every non-trivial conversion
    -n, --no-comment            Discard all the comments in output
    -o, --output=<file>         Specify the output filename
    -r, --return-code           Print return code definitions
    -s, --strict                Error on directives that have no equivalent counterpart
    -v, --version               Show version info
    -w, --no-warning            Suppress all warning messages
    -x, --suffix=<string>       Suffix of the output filename [default: '.out']
    --inline                    Process GET and INCLUDE inline
    --test-expr=<expr>          Test the expression processor

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
    1       Invalid or conflict command-line args.
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
our $opt_testexpr    = 0;

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
    if ($arg & (1<<31))
    {
        $arg = ($arg & 0xFFFFFFFF) - (1<<32);
    }
    return $arg;
}

# operators
our %operators_binary = (
    # Arithmetic
    "+"      => sub { my ($left, $right) = @_; return $left + $right },
    "-"      => sub { my ($left, $right) = @_; return $left - $right },
    "*"      => sub { my ($left, $right) = @_; return $left * $right },
    "/"      => sub { my ($left, $right) = @_; return int($left / $right) },
    "%"      => sub { my ($left, $right) = @_; return $left % $right },
    ":MOD:"  => sub { my ($left, $right) = @_; return $left % $right },
    # Binary
    "|"      => sub { my ($left, $right) = @_; return $left | $right },
    ":OR:"   => sub { my ($left, $right) = @_; return $left | $right },
    ":EOR:"  => sub { my ($left, $right) = @_; return $left ^ $right },
    "&"      => sub { my ($left, $right) = @_; return $left & $right },
    ":AND:"  => sub { my ($left, $right) = @_; return $left & $right },
    "<<"     => sub { my ($left, $right) = @_; return $left << $right },
    ":SHL:"  => sub { my ($left, $right) = @_; return $left << $right },
    ">>"     => sub { my ($left, $right) = @_; return $left >> $right },
    ":SHR:"  => sub { my ($left, $right) = @_; return $left >> $right },
    # Boolean
    ":LOR:"  => sub { my ($left, $right) = @_; return ($left || $right) ? 1 : 0 },
    ":LAND:" => sub { my ($left, $right) = @_; return ($left && $right) ? 1 : 0 },
    # String
    ":LEFT:" => sub { my ($left, $right) = @_; return '"'. (signed($right) > 0 ? substr(unstr($left), 0, $right) : '') .'"' },
    ":RIGHT:" => sub { my ($left, $right) = @_; return '"'. (signed($right) > 0 ? substr(unstr($left), -$right) : '') .'"' },
    ":CC:"   => sub { my ($left, $right) = @_; return '"'. unstr($left) . unstr($right) .'"' },
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
);
our $operators_binary_re = "(?:" . (join '|', map { "\Q$_\E" } keys %operators_binary) . ")";
our %operators_monadic = (
    # Arithmetic
    "+"      => sub { my ($right) = @_; return (0+$right) & 0xFFFFFFFF },
    "-"      => sub { my ($right) = @_; return (0-$right) & 0xFFFFFFFF },
    # Binary
    "~"      => sub { my ($right) = @_; return ($right) ^ 0xFFFFFFFF },
    ":NOT:"  => sub { my ($right) = @_; return ($right) ^ 0xFFFFFFFF },
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
    "ENTRY"     =>  ""
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
    "SPACE"     =>  ".space",
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
    'CODESIZE' => 32,               # alias for CONFIG
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
    'TARGET_ARCH_AARCH32' => 0,     # Might be a lie
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
our @regnames = (
    (map { "r$_" } (0..15)),
    'sp',
    'lr',
    'pc'
);

# Regular expression to match any of the registers
our $regnames_re = "(?:" . (join "|", sort { length($a) <=> length($b) } @regnames) . ")";

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

GetOptions(
    "output=s"      => \@output_files,
    "x|suffix=s"    => \$output_suffix,
    "help"          => sub { print $helpmsg; exit },
    "return-code"   => sub { print $rvalmsg; exit },
    "v|version"     => sub { print "$ver\n"; exit },
    "compatible"    => \$opt_compatible,
    "i|verbose"     => \$opt_verbose,
    "s|strict"      => \$opt_strict,
    "n|no-comment"  => \$opt_nocomment,
    "w|no-warning"  => \$opt_nowarning,
    "inline"        => \$opt_inline,
    "test-expr=s"   => \$opt_testexpr,
) or die("Conversion of ObjASM source to GAS source failed\n");

@input_files = @ARGV;

# validate input
if (@input_files == 0 && !$opt_testexpr) {
    exit_error($ERR_ARGV, "$0:".__LINE__.
        ": No input file");
}
elsif (@output_files > 0 && $#input_files != $#output_files && !$opt_testexpr) {
    exit_error($ERR_ARGV, "$0:".__LINE__.
        ": Input and output files must match one-to-one");
}
elsif ($output_suffix !~ /^\.*\w+$/ && !$opt_testexpr) {
    exit_error($ERR_ARGV, "$0:".__LINE__.
        ": Invalid suffix '$output_suffix'");
}

# pair input & output files
if (@output_files == 0 && !$opt_testexpr) {
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
our %macros;
our $macroname;

# The variables are set with SET[ALS].
# The stack contains the global variables at the top,
# with local entries below that.
# Each set of entries is a dictionary containing the variable names.
# The value of each entry is a dictionary containing:
#   value => the value of the variable
#   context => the context it was created in
#   type => the variable type (A, L, S)
our @variable_stack = ({}, {});


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

    our $f_out;
    if ($out_file eq '-')
    {
        # Write to the stdout stream
        open($f_out, ">&", STDOUT)
            or exit_error($ERR_IO, "$0:".__LINE__.": <STDOUT>: $!");
    }
    else
    {
        open($f_out, ">", $out_file)
            or exit_error($ERR_IO, "$0:".__LINE__.": $out_file: $!");
        $writing = $out_file;
    }

    open(my $f_in, "<", $_)
        or exit_error($ERR_IO, "$0:".__LINE__.": $in_file: $!");

    process_file($f_in, $in_file, undef);

    close $f_in  or exit_error($ERR_IO, "$0:".__LINE__.": $in_file: $!");
    close $f_out or exit_error($ERR_IO, "$0:".__LINE__.": $out_file: $!");
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

    $in_file = $filename;
    $linenum_input = 1;
    while (my $line = <$f_in>) {
        $context = "$our_context$in_file:$linenum_input -> $out_file:$linenum_output";
        $linenum_input++;
        if ($opt_inline && $line =~ /^\s+END\s*$/)
        {
            # Handle explicit file end here.
            last;
        }
        my $outputline = single_line_conv($line);
        if (defined $outputline)
        {
            print $f_out $outputline;
            my @nlines = ($outputline =~ m/\n/g);
            $linenum_output += scalar(@nlines);
        }
    }
    print $f_out "\n";  # required by as
    $linenum_output += 1;

    ($in_file, $linenum_input, $context) = @caller_context;
}


sub resolve_filename {
    my ($filename) = @_;
    our $in_file;
    #print "Resolving: $filename\n";
    return $filename if (-f $filename);

    my $newfilename;
    my $basedir;

    # Apply the filename to the relative location of the current source file.
    $basedir = $in_file;
    $basedir =~ s/[^\/]+$//;   # Trim leafname
    if ($basedir ne '')
    {
        $newfilename = "$basedir$filename";
        return $newfilename if (-f $newfilename);
    }

    # If the base directory is in unix form of the RISC OS extensions, strip it.
    # Ie if the source file was <dirs>/s/<leaf> the base directory is <dirs>
    if ($basedir =~ /(^|.*\/)($extensions_dir_re)\/?$/)
    {
        $basedir = $1;
    }

    # We'll try to apply the file that was supplied again
    if ($basedir ne '')
    {
        $newfilename = "$basedir$filename";
        return $newfilename if (-f $newfilename);
    }

    # Now let's try applying the filename given as a RISC OS style filename
    my $unixised = $filename;
    #print "Unixised: $unixised\n";
    #print "Basedir: $basedir\n";
    # Convert (<dirs>.)?s.<leaf> to <dirs>/s/<leaf>
    if ($unixised =~ /(^|.*\.)($extensions_dir_re)\.([^\.]+)$/)
    {
        $unixised =~ tr!/.!./!;
        $newfilename = "$basedir$unixised";
        return $newfilename if (-f $newfilename);
    }

    # Let's try it with the names transformed
    # For example convert hdr.foo to foo.hdr
    if ($unixised =~ /(^|.*\/)($extensions_dir_re)\/([^\/]+)$/)
    {
        my $newfilename = "$basedir$1$3.$2";
        return $newfilename if (-f $newfilename);
    }

    # Try it the other way around.
    # If they gave us foo.hdr check this as hdr/foo.
    if ($unixised =~ /(^|.*\/)([^\.]+)\.($extensions_dir_re)$/)
    {
        my $newfilename = "$basedir$1$3/$2";
        return $newfilename if (-f $newfilename);
    }

    exit_error($ERR_IO, "$0:".__LINE__.": $filename: Cannot find file")
}


sub include_file {
    my ($filename) = @_;

    $filename = resolve_filename($filename);

    open(my $f_in, "<", $filename)
        or exit_error($ERR_IO, "$0:".__LINE__.": $filename: $!");

    process_file($f_in, $filename);

    close $f_in  or exit_error($ERR_IO, "$0:".__LINE__.": $filename: $!");
}

sub expand_macro {
    my ($macroname, $label, $values) = @_;
    our $macros;
    my $macrodef = $macros{$macroname};
    my %macrovars;
    our $context;
    my @valuelist;
    my $valueparse=$values;

    while ($valueparse ne '')
    {
        next if ($valueparse =~ s/^\s+//);
        if ($valueparse =~ s/^((?:"[^"]*"[^,]*)+?)\s*//)
        {
            # Quoted string.
            # FIXME: Strip the quotes?
            #print "Quoted value '$1'\n";
            push @valuelist, $1;
        }
        elsif ($valueparse =~ s/^([^,\s]+)//)
        {
            # Should be either an empty string or a value
            #print "Simple value '$1'\n";
            push @valuelist, $1;
        }
        else
        {
            #print "Empty value\n";
            push @valuelist, '';
        }
        # Remove any training spaces
        $valueparse =~ s/^\s+//;
        if ($valueparse =~ s/^,//)
        {
            # There is another parameter present.
        }
        else
        {
            # No more parameters supplied, so we're done.
            #print "Giving up at: $valueparse\n";
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
            $value = $macrodef->{'defaults'}->[$index];
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
    my $inrout = $label_stack[-1]->{'rout'};
    push @label_stack, {'backward'=>{}, 'forward'=>{}, 'rout'=>"_${inrout}_macro_$macroname"};

    $in_file = $macrodef->{'base_file'};
    $linenum_input = $macrodef->{'base_line'};
    for my $line (@{ $macrodef->{'lines'} })
    {
        $context = "$our_context$in_file:$linenum_input -> $out_file:$linenum_output";
        $linenum_input++;
        my $macroline = "$line";

        # Perform the substitutions
        if ($macroline =~ /\$/)
        {
            $macroline =~ s/(\$[A-Za-z_][A-Za-z0-9_]*)\.?/$macrovars{$1} \/\/ ''/ge;
        }

        my $outputline = single_line_conv($macroline);
        if (defined $outputline)
        {
            print $f_out $outputline;
            my @nlines = ($outputline =~ m/\n/g);
            $linenum_output += scalar(@nlines);
        }
    }
    print $f_out "\n";  # required by as
    $linenum_output += 1;

    my $macro_labels = pop @label_stack;
    if (scalar( keys %{ $macro_labels->{'backward'} }) > 0)
    {
        # If any labels were defined, increment the sequence number, so that we don't clash.
        $label_sequence++;
    }
    ($in_file, $linenum_input, $context) = @caller_context;
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
        return "\n";    # just keep it
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
        $macroname = '1';
        return undef;
    }
    if (defined $macroname)
    {
        if ($macroname eq '1')
        {
            # This is the first line after the MACRO, which defines what the macro is.
            if ($line =~ /^(?:(\$[a-zA-Z_]\w*))?\s*(\w*)\s+(.*?)(\/\/.*)?$/) {
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
                        'base_line' => $linenum_input + 1,  # Start *after* the definition line
                    };
            }
        }
        else
        {
            # We're inside a macro definition
            if ($line =~ /^([\$\w]+)?\s+MEND(\/\/.*)?$/) {
                # We're leaving a macro
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
    # if has comments
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
            return $line;
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

    # FIXME: This parse doesn't handle strings with // in.
    if ($line =~ /^((?:[A-Z_a-z0-9][A-Z_a-z0-9]*|[0-9]+)?)?(\s+)([^\s]*)(\s*)(.*?)(\s*)(\/\/.*)?$/)
    {
        $label=$1;
        $lspcs=$2;
        $cmd=$3;
        $cspcs=$4;
        $values=$5;
        $vspcs=$6;
        $comment=$7 // '';
        if (defined $macros{$cmd})
        {
            # Macro expansion requested.
            #print "Macro expansion '$cmd' of '$label', '$values'\n";
            expand_macro($cmd, $label, $values);
            return undef;
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
        chomp $line;
        msg_warn(1, $context.
            ": Unrecognised line format '$line'");
        return undef;
    }
    #print "LINE: label='$label', cmd='$cmd', values='$values', comment='$comment'\n";

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

    # ------ Conversion: conditional directives ------
    if ($cmd eq 'IF' || $cmd eq '[')
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
    elsif ($cmd eq 'ELSE' || $cmd eq '|')
    {
        $cmd = '.else';
        goto reconstruct
    }
    elsif ($cmd eq 'ELSEIF')
    {
        exit_error($ERR_SYNTAX, $context.
            ": Conditional ELSEIF not supported");
    }
    elsif ($cmd eq 'ENDIF' || $cmd eq ']')
    {
        $cmd = '.endif';
        goto reconstruct
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
        $line = "$label: $lspcs$cspcs$vspcs$comment\n";
        $line =~ s/:\s*\n/:\n/;
        return $line;
    }

    # Label splitting
    if ($label ne '' and not defined $cmd_notlabel{$cmd})
    {
        # Labelled statement seen.
        if ($cmd ne '')
        {
            #print "COMMAND: $cmd\n";
            if (defined $cmd_withoutlabel{$cmd})
            {
                exit_error($ERR_SYNTAX, "$context".
                           ": Directive '$cmd' is not allowed to have a label");
            }
            $line =~ s/^\Q$label\E/' ' x length($label)/e;
            my $newline = single_line_conv($line);
            #print "Got back $newline\n";
            if (defined $newline)
            {
                return "$label:\n$newline";
            }
        }
        goto reconstruct;
    }

    # Byte/String constants
    if ($cmd eq '=') {
        # The constants can be things like:
        # = "hello"
        # = "hello", "there"
        # = 1, 2, 3
        # = "hello", 32, "there"
        # = "hello", 0
        # We will decode this as a sequence of regex matched strings
        my @lines = ();
        my @num_accumulator = ();
        while ($values ne '')
        {
            my ($value, $nextvalues) = expression($values);

            if ($value =~ /^"(.*)"$/)
            {
                my $str = $1;
                if ($nextvalues =~ s/^\s*,\s*0(?=\s|$)//)
                {
                    # This is a zero-terminated string, so dump it out raw.
                    if (@num_accumulator)
                    {   # Flush the number accumulator
                        push @lines, ".byte " . join ", ", @num_accumulator;
                        @num_accumulator = ();
                    }
                    push @lines, ".asciz \"$str\"";
                }
                else
                {
                    # This is a string, so dump it out
                    if (@num_accumulator)
                    {   # Flush the number accumulator
                        push @lines, ".byte " . join ", ", @num_accumulator;
                        @num_accumulator = ();
                    }
                    push @lines, ".ascii \"$str\"";
                }
            }
            elsif (defined $value)
            {
                # This is a number, so we want to accumulate it.
                push @num_accumulator, $value;
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
            push @lines, ".byte " . join ", ", @num_accumulator;
            @num_accumulator = ();
        }

        my $unlabelled = 0;
        my $sprefix = (' ' x length($label)) . $lspcs;
        $line = join "", map { $unlabelled++ ? "$sprefix$_\n" : "$label$lspcs$_\n" } @lines;
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

    # ------ Conversion: labels ------
    given ($line) {
        # FIXME: This needs to be implemented properly.
        # branch jump
        when (m/^\s*B[A-Z]*\s+(%([FB]?)([AT]?)(\d+)(\w*))/i) {
            my $label        = $1;
            my $direction    = $2;
            my $search_level = $3;
            my $num_label    = $4;
            my $scope        = $5;
            ($search_level eq "")
                or msg_warn(1, "$context".
                ": Can't specify label's search level '$search_level' in GAS".
                ", dropping");
            ($scope eq "")
                or msg_warn(1, "$context".
                ": Can't specify label's scope '$scope' in GAS".
                ", dropping");
            $line =~ s/$label/$num_label$direction/;
        }
    }

    # ------ Conversion: functions ------
    if ($cmd eq 'PROC' or $cmd eq 'FUNCTION') {
        my $func_name = $label;
        if ($opt_compatible) {
            push @symbols, $func_name;
            if ($values)
            {
                $line = ".type $label, \"function\"$values\n$label:$cspcs$comment\n";
            }
            else
            {
                $line = "$label:$cspcs$comment\n";
            }
        }
        else {
            $line = ".func $label\n$label:$cspcs$comment\n";
        }
        return $line;
    }
    elsif ($cmd eq 'ENDP' or $cmd eq 'ENDFUNC') {
        if ($opt_compatible) {
            my $func_name = pop @symbols;
            my $func_end  = ".L$func_name"."_end";
            $line = "$func_end:${lspcs}\n$1.size $func_name, $func_end-$func_name$cspcs$comment\n";
        }
        else {
            $line = ".endfunc$cspcs$comment\n";
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
            $line =~ s/GBL$type/.set/i;
            $line =~ s/$var/"$var, ".$init_val{$type}/ei;
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
    if ($line =~ m/^\s*AREA\s+\|*([.\w\$]+)\|*([^\/]*?)(\s*\/\/.*)?$/i) {

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

        $line =~ s/^(\s*)AREA[^\/]+[^\/\s]/$1.section $sec_name, "$flags"$args/i;

        my $indent = $1;
        if (defined($align)) {
            $line .= "$indent.balign " . (2**$align) . "\n";
        }
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

    # ------ Conversion: mappings ------
    if ($line =~ m/^(\s*)\^(\s*)(.*?)(\s*\/\/.*)?$/) {
        my ($spaces, $spaces2, $value, $comment) = ($1, $2, $3, $4, $5);
        our $mapping_base;
        if ($value =~ m/^(.*),\s*($regnames_re)/)
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
        $line = '';
    }
    elsif ($line =~ m/^(\w+)(\s*)\#(\s*)(.*?)(\s*\/\/.*)?$/) {
        my ($symbol, $spaces, $spaces2, $value, $comment) = ($1, $2, $3, $4, $5);
        our $mapping_base;
        if (!defined $mapping_base)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Attempt to define field mapping for '$symbol' without setting up base ('^' not used)");
        }
        $comment //= '';
        my ($size, $trail) = expression($value);
        if ($trail)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Trailing text '$trail' at the end of field definition '$symbol'");
        }
        $mapping{$symbol} = [$mapping_base, $mapping_register, $size];
        $line = ".set $symbol, " . gas_number($mapping_base) . "$comment\n";
        $mapping_base += $size;
    }

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


    # ------ Conversion: constants ------
    if ($line =~ m/^(\w+)(\s*)(?:\*|EQU)(\s*)(.*?)(\s*\/\/.*)?$/) {
        my ($symbol, $spaces, $spaces2, $value, $comment) = ($1, $2, $3, $4, $5);
        $constant{$symbol} = evaluate($value);
        $comment //= '';
        $line = ".set $symbol, " . gas_number($value) . "$comment\n";
    }


    # ------ Conversion: misc directives ------
    # weak declaration
    if ($line =~ m/(EXPORT|GLOBAL|IMPORT|EXTERN)\s+\w+.+\[\s*WEAK\s*\]/i) {
        my $drctv = $1;
        $line =~ s/$drctv/.weak/i;
        $line =~ s/,\s*\[\s*WEAK\s*\]//i;
    }
    # INFO directive
    if ($line =~ m/(INFO\s+\d+\s*,)\s*".*"/i) {
        my $prefix = $1;
        $line =~ s/$prefix/.warning /i;
    }

    if ($line =~ s/\b($misc_op_re)\b/$misc_op{$1}/eg)
    {
        $line = expand_variables($line);
    }
    $line =~ s/(\s+)($miscexpression_op_re)(\s+)(.*?)(\s*(\/\/.*)?)$/$1 . $miscexpression_op{$2} . $3 . gas_number(evaluate($4)) . $5/eg;
    if (scalar(%miscquoted_op) and $line =~ /\b$miscquoted_op_re\s/)
    {
        $line =~ s/\b($miscquoted_op_re)(\s+)([a-zA-Z_0-9\.\-\/]+)/$miscquoted_op{$1}$2"$3"/;
        $line =~ s/\b($miscquoted_op_re)(\s+)("[a-zA-Z_0-9\.\-\/]+")/$miscquoted_op{$1}$2$3/;
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
    $line =~ s/ +\n?$//;
    if ($line !~ /\n$/)
    {
        $line .= "\n";
    }
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
    my ($expr) = @_;
    my $orig = $expr;
    our $context;
    our %cmp_number;
    my $expr_debug = 0;

    # This is not going to be a proper parser for numeric expressions; it's going
    # to be very simple just to parse expressions left to right.
    my $left = undef;
    my $operator = undef;
    my $monadic = undef;
    my $monadicstr = undef;

    # Trim the leading spaces so that we can expand things easier
    $expr =~ s/^\s+//;

    while ($expr ne '')
    {
        print "Expression parse: '$expr'\n" if ($expr_debug);
        if (!defined $left || defined $operator)
        {
            # Monadic operators can only happen if there is no left parameter, or
            # an operator has been given (start of line or after operator).
            if ($expr =~ s/^($operators_monadic_re)//)
            {
                my $monoop = $operators_monadic{$1};
                $monadicstr = defined $monadicstr ? "$monadicstr$1" : $1;
                print "Monadic operator: $1 (now: $monadicstr)\n" if ($expr_debug);
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
                print "Binary operator: $1\n" if ($expr_debug);
                $operator = $binop;
                goto next_token;
            }
        }

        # We are now expecting an expression, either for the left or the right.
        my $value;
        if ($expr =~ s/^\(//)
        {
            # Bracketted expression, so we need to extract from that expression the value.
            ($value, $expr) = expression($expr);
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
        # Builtins check
        elsif ($expr =~ s/^\{([A-Za-z_][A-Za-z0-9_]*)\}//)
        {
            print "Builtin: $1\n" if ($expr_debug);
            $value = defined $builtins{$1} ? (ref($builtins{$1}) eq 'CODE' ? $builtins{$1}->() : $builtins{$1}) : 0;
            if ($value =~ m![^0-9]!)
            { $value = '"' . $value . '"'; }
        }
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
        elsif ($expr =~ s/^(\d+)//)
        {
            my $dec_lit = $1;
            $value = $dec_lit + 0;
        }
        elsif ($expr =~ s/^([A-Za-z_][A-Za-z0-9_]*)//)
        {
            my $name = $1;
            if (defined $mapping{$name})
            {
                $value = $mapping{$name};
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
                    # Variable isn't recognised.
                    exit_error($ERR_SYNTAX, "$context".
                        ": Evaluation of variables in '$orig' could not find a value for '$name' at '$expr'");
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
            ": Evaluation of variables in '$orig' failed at a monadic expansion due to missing right hand operator (at '$expr')");
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
# Convert a numeric value to a GAS value
#
# @param[in] $expr:     Value to present
#
# @return:  The string that can be used in GNU AS for the value, but is humanly readable.
sub gas_number
{
    my ($expr) = @_;
    my $value;

    my $sign = '';
    if ($expr =~ /[^0-9]/)
    {
        $value = evaluate($expr);
    }
    else
    {
        $value = 0 + $expr;
    }

    if ($value < 0)
    {
        $sign = '-';
        $value = -$value;
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
