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
my $ver = '1.0';
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

Cautions:
    By default (without --strict), for those directives that have no equivalent
    in GNU format, the tool will try best to convert and generate warning information
    on the specific line. Therefore, a 'warning' does NOT necessarily mean no issue,
    please check the conversion result to ensure it works as expected.

    Note that the tool will assume that the input file is in the correct syntax,
    otherwise, the conversion result is UNEXPECTED.

Issues and Bugs:
    https://github.com/gerph/arm2gas
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

# command-line switches
our $opt_compatible  = 0;
our $opt_verbose     = 0;
our $opt_strict      = 0;
our $opt_nocomment   = 0;
our $opt_nowarning   = 0;
our $opt_inline      = 0;

# directives for validation only
our @drctv_nullary = (
    "THUMB", "REQUIRE8", "PRESERVE8", "CODE16", "CODE32", "ELSE", "ENDIF",
    "ENTRY", "ENDP","LTORG", "MACRO", "MEND", "MEXIT", "NOFP", "WEND"
);

# operators
our %operators = (
    ":OR:"   => "|",
    ":EOR:"  => "^",
    ":AND:"  => "&",
    ":NOT:"  => "~",
    ":MOD:"  => "%",
    ":SHL:"  => "<<",
    ":SHR:"  => ">>",
    ":LOR:"  => "||",
    ":LAND:" => "&&",
    "EQU"    => "="
);

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
    "DCB"       =>  ".byte",
    "DCWU?"     =>  ".hword",
    "DCDU?"     =>  ".word",
    "DCQU?"     =>  ".quad",
    "DCFSU?"    =>  ".single",
    "DCFDU?"    =>  ".double",
    "SPACE"     =>  ".space",
    "ENTRY"     =>  ""
);
# simple replace with quoted strings
our %miscquoted_op = (
    );


# variable initial value
our %init_val = (
    "A" => '0',         # arithmetic
    "L" => 'FALSE',     # logical
    "S" => '""'         # string
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
    print "\e[01;31mERROR\e[0m: $_[1]\n";
    exit($_[0]);
}

sub msg_info {
    if ($opt_verbose) {
        print "\e[01;34mINFO\e[0m: $_[0]\n";
    }
}

# @args: no_exact_conv?, msg
sub msg_warn {
    if ($opt_strict && $_[0]) {
        exit_error($ERR_UNSUPPORT, $_[1]);
    }
    elsif (! $opt_nowarning) {
        print "\e[00;33mWARN\e[0m: $_[1]\n";
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
    "inline"        => \$opt_inline
) or die("Conversion of ObjASM source to GAS source failed\n");

@input_files = @ARGV;

# validate input
if (@input_files == 0) {
    exit_error($ERR_ARGV, "$0:".__LINE__.
        ": No input file");
}
elsif (@output_files > 0 && $#input_files != $#output_files) {
    exit_error($ERR_ARGV, "$0:".__LINE__.
        ": Input and output files must match one-to-one");
}
elsif ($output_suffix !~ /^\.*\w+$/) {
    exit_error($ERR_ARGV, "$0:".__LINE__.
        ": Invalid suffix '$output_suffix'");
}

# pair input & output files
if (@output_files == 0) {
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


# Variable definitions
my $mapping_base        = 0;
my $mapping_register    = undef;
my %mapping             = ();
my %constant            = ();
our %macros;
our $macroname;

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
    if ($basedir =~ /(^|.*\/)($extensions_dir_re)$/)
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
    my @valuelist = ($values =~ m/(?:"[^"]*"[^,]*)+|(?<=^|,)[^,]*/g);
    our $macros;
    my $macrodef = $macros{$macroname};
    my %macrovars;
    our $context;

    if (!defined $macrodef)
    {
        exit_error(128, "$context".
            ": Internal consistency failre: Macro '$macroname' being expanded which isn't known");
    }

    if (scalar(@valuelist) > scalar(@{ $macrodef->{'params'} }))
    {
        msg_warn(1, "$context".
            ": Macro '$macroname' supplied with too many arguments (params: '$values')");
    }
    #print "Value list: ", (join ', ', @valuelist), " / params: ", join( ', ', @{ $macrodef->{'params'} }), "\n";
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
            exit_error($ERR_SYNTAX, "$context".
                ": Macro '$macroname' requires a label");
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

    $in_file = $macrodef->{'base_file'};
    $linenum_input = $macrodef->{'base_line'};
    for my $line (@{ $macrodef->{'lines'} })
    {
        $context = "$our_context$in_file:$linenum_input -> $out_file:$linenum_output";
        $linenum_input++;
        my $macroline = "$line";

        # Perform the substitutions
        $macroline =~ s/\Q$_\E/$macrovars{$_}/i foreach (keys %macrovars);

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

    ($in_file, $linenum_input, $context) = @caller_context;}


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
    if ($line =~ m/"+/) {
        msg_warn(0, "$context".
            ": Conversion containing strings needs a manual check");
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
    }

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
    if ($line =~ /^([A-Z_a-z0-9]*)?(\s+)([^ \t]*)\s*(.*?)(\s*\/\/.*)?$/)
    {
        my $label=$1;
        my $lspcs=$2;
        my $cmd=$3;
        my $values=$4;
        my $comment=$5;
        if (defined $macros{$cmd})
        {
            # Macro expansion requested.
            #print "Macro expansion '$cmd' of '$label', '$values'\n";
            expand_macro($cmd, $label, $values);
            return undef;
        }
    }

    # ------ Conversion: includes ------
    if ($opt_inline && $line =~ m/\s+(GET|INCLUDE)\s+([_a-zA-Z0-9\/\.]*)\s*(\/\/.*)?$/) {
        my $file = $2;
        my $comment = $3 // '';
        msg_info($context.
            ": Inline inclusion of '$file'");

        include_file($file);

        return undef;
    }

    # remove special symbol delimiter
    $line =~ s/\|//;

    # ------ Conversion: labels ------
    given ($line) {
        # single label
        when (m/^([a-zA-Z_]\w*)\s*(\/\/.*)?$/) {
            my $label = $1;
            $line =~ s/$label/$label:/ unless ($label ~~ @drctv_nullary);
        }
        # numeric local labels
        when (m/^\d+\s*(\/\/.*)?$/) {
            $line =~ s/(\d+)/$1:/;
        }
        # scope is not supported in GAS
        when (m/^((\d+)[a-zA-Z_]\w+)\s*(\/\/.*)?$/) {
            my $full_label = $1;
            my $num_label  = $2;
            msg_warn(1, "$context".
                ": Numeric local label with scope '$1' is not supported in GAS".
                ", converting to '$2'");
            $line =~ s/$full_label/$num_label:/;
        }
        # delete ROUT directive
        when (m/^(\w+\s*ROUT\b)/i) {
            my $rout = $1;
            msg_warn(1, "$context".
                ": Scope of numeric local label is not supported in GAS".
                ", removing ROUT directives");
            $line =~ s/$rout//;
        }
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
    if ($line =~ m/^\s*(\w+)\s+PROC\b/) {
        my $func_name = $1;
        if ($opt_compatible) {
            push @symbols, $func_name;
            $line =~ s/$func_name\s+PROC(.*)$/.type $func_name, "function"$1\n$func_name:/i;
        }
        else {
            $line =~ s/$func_name\s+PROC/.func $func_name/i;
        }
    }
    elsif ($line =~ m/^(\s*)ENDP\b/i) {
        if ($opt_compatible) {
            my $func_name = pop @symbols;
            my $func_end  = ".L$func_name"."_end";
            $line =~ s/^(\s*)ENDP(.*)$/$func_end:$2\n$1.size $func_name, $func_end-$func_name/i;
        }
        else {
            $line =~ s/ENDP/.endfunc/i;
        }
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


        $line =~ s/^(\s*)AREA[^\/]+[^\/\s]/$1.section $sec_name, "$flags"$args/i;

        my $indent = $1;
        if (defined($align)) {
            $line .= "$indent.balign " . (2**$1) . "\n";
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
        if ($value =~ m/^(.*), ($regnames_re)/)
        {
            $value = $1;
            $mapping_register = $2;
        }
        else
        {
            $mapping_register = undef;
        }
        $mapping_base = evaluate($value);
        $line = '';
    }
    elsif ($line =~ m/^(\w+)(\s*)\#(\s*)(.*?)(\s*\/\/.*)?$/) {
        my ($symbol, $spaces, $spaces2, $value, $comment) = ($1, $2, $3, $4, $5);
        if (!defined $mapping_base)
        {
            exit_error($ERR_SYNTAX, "$context".
                ": Attempt to define field mapping for '$symbol' without setting up base ('^' not used)");
        }
        $comment //= '';
        $mapping{$symbol} = [$mapping_base, $mapping_register];
        $line = ".set $symbol, " . gas_number($mapping_base) . "$comment\n";
        $mapping_base += evaluate($value);
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
    if ($line =~ m/^(\w+)(\s*)\*(\s*)(.*?)(\s*\/\/.*)?$/) {
        my ($symbol, $spaces, $spaces2, $value, $comment) = ($1, $2, $3, $4, $5);
        $constant{$symbol} = evaluate($value);
        $comment //= '';
        $line = ".set $symbol, " . gas_number($value) . "$comment\n";
    }

    # ------ Conversion: conditional directives ------
    $line =~ s/IF\s*:DEF:/.ifdef /i;
    $line =~ s/IF\s*:LNOT:\s*:DEF:/.ifndef /i;
    $line =~ s/\bIF\b/.if/i;
    $line =~ s/(ELSE\b|ELSEIF|ENDIF)/'.'.lc($1)/ei;

    # ------ Conversion: operators ------
    $line =~ s/$_/$operators{$_}/i foreach (keys %operators);
    if ($line =~ m/(:[A-Z]+:)/i) {
        msg_warn(1, "$context".
            ": Unsupported operator $1".
            ", need a manual check");
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
    # Byte/String constants
    if ($line =~ m/^(\s+)=\s+(.*)/i) {
        my $prefix = $1;
        my $const = $2;
        # The constants can be things like:
        # = "hello"
        # = "hello", "there"
        # = 1, 2, 3
        # = "hello", 32, "there"
        # = "hello", 0
        # We will decode this as a sequence of regex matched strings
        my @lines = ();
        $const =~ s/\s+$//;
        my @num_accumulator = ();
        while ($const ne '')
        {
            $const =~ s/^\s//;
            if ($const eq '')
            {
                # We're done.
                last;
            }
            elsif ($const =~ /^\/\//)
            {
                # We've found a comment, so just append this as a bare line
                if (@num_accumulator)
                {   # Flush the number accumulator
                    push @lines, ".byte " . join ", ", @num_accumulator;
                    @num_accumulator = ();
                }
                push @lines, $const;
                last;
            }
            elsif ($const =~ s/^("[^"]*"),\s*0(?=\s|$)//)
            {
                # This is a zero-terminated string, so dump it out raw.
                if (@num_accumulator)
                {   # Flush the number accumulator
                    push @lines, ".byte " . join ", ", @num_accumulator;
                    @num_accumulator = ();
                }
                push @lines, ".asciz $1";
            }
            elsif ($const =~ s/^("[^"]*")//)
            {
                # This is a string, so dump it out
                if (@num_accumulator)
                {   # Flush the number accumulator
                    push @lines, ".byte " . join ", ", @num_accumulator;
                    @num_accumulator = ();
                }
                push @lines, ".ascii $1";
            }
            elsif ($const =~ s/^((?:0x|&)[0-9a-f]+|[0-9]+)//i)
            {
                # This is a number, so we want to accumulate it.
                my $value = $1;
                $value =~ s/&/0x/;
                push @num_accumulator, $value;
            }
            else
            {
                msg_warn(1, "$context".
                    ": Literal byte sequence cannot interpret '$const'".
                    ", need a manual check");
                last;
            }
            # Trim any commas between parameters
            $const =~ s/^(,\s*)+//;
        }

        if (@num_accumulator)
        {   # Flush the number accumulator
            push @lines, ".byte " . join ", ", @num_accumulator;
            @num_accumulator = ();
        }

        $line = join "", map { "$prefix$_\n" } @lines;
    }

    $line =~ s/\b$_\b/$misc_op{$_}/ foreach (keys %misc_op);
    $line =~ s/\b$_(\s+)([a-zA-Z_0-9\.\-\/]+)/$miscquoted_op{$_}$1"$2"/ foreach (keys %miscquoted_op);

    # ------ Conversion: labels on instructions ------
    if ($line =~ m/^([a-zA-Z_][a-zA-Z_0-9]*)(\s+)([A-Z])/) {
        my $label = $1;
        my $spaces = $2;
        my $inst1 = $3;
        my $prefix = (' ' x length($label)) . $spaces;
        $line =~ s/$label$spaces$inst1/$label:\n$prefix$inst1/;
    }

    # ------ Conversion: symbol definition ------
    if ($line =~ m/LCL([A|L|S])\s+(\w+)/i) {
        my $var_type = $1;
        my $var_name = $2;
        msg_warn(1, "$context".
            ": Local variable '$var_name' is not supported".
            ", using static declaration");
        $line =~ s/LCL$var_type/.set/i;
        $line =~ s/$var_name/"$var_name, ".$init_val{$var_type}/ei;
    }
    elsif ($line =~ m/^(\s*)GBL([A|L|S])\s+(\w+)/i) {
        my $indent   = $1;
        my $var_type = $2;
        my $var_name = $3;
        $line =~ s/GBL$var_type/.set/i;
        $line =~ s/$var_name/"$var_name, ".$init_val{$var_type}/ei;
        $line = "$indent.global $var_name\n" . $line;
    }
    elsif ($line =~ m/^(\w+)\s*(SET[A|L|S])/i) {
        my $var_name = $1;
        my $drctv    = $2;
        $line =~ s/$var_name/.set/;
        $line =~ s/$drctv/$var_name,/;
    }

    # postprocess
    if ($line =~ m/^\s*$/) {
        # delete empty line
        return undef;
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
            msg_info($context.
                ": Converting hexadecimal '&$hex_lit' to '0x$hex_lit'");
            $line =~ s/${sign}&$hex_lit/${sign}0x$hex_lit/;
        }
        elsif ($line =~ m/(-?)([2|8])_(\d+)/) {
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

    $expr = expand_literals($expr);

    $expr =~ s/$_/$operators{$_}/i foreach (keys %operators);
    $expr =~ s/$_/$mapping{$_}->[0]/i foreach (keys %mapping);
    $expr =~ s/$_/$constant{$_}/i foreach (keys %constant);

    my $value = eval $expr;
    if ($@)
    {
        # Something went wrong in the evaluation; let's just fault this.
        exit_error($ERR_SYNTAX, "$context".
            ": Evaluation of variables in '$orig' produced '$expr' which failed: $@");
    }

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
    my $value = evaluate($expr);
    my $sign = '';

    $sign = '-' if ($value < 0);
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
