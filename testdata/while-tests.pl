#!/usr/bin/perl
# Copyright (c) 2026 Charles Ferguson. SPDX-License-Identifier: GPL-3.0-or-later
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin;
use Text::ParseWords qw(shellwords);

my $tool = "$FindBin::Bin/../objasm2gas.pl";
my $directory = tempdir(CLEANUP => 1);
my $sequence = 0;

sub convert
{
    my ($body, @options) = @_;
    my $base = "$directory/case" . ++$sequence;
    open(my $source, '>', "$base.s") or die $!;
    print $source "        AREA Test, DATA, READONLY\n$body\n        END\n";
    close($source);
    my $pid = fork();
    die $! if (!defined $pid);
    if (!$pid)
    {
        open(STDOUT, '>', "$base.log") or die $!;
        open(STDERR, '>&', STDOUT) or die $!;
        exec($^X, $tool, '-o', "$base.out", '--line-map', "$base.map", @options, "$base.s");
        die $!;
    }
    local $SIG{ALRM} = sub { kill 'KILL', $pid; die "converter timed out\n"; };
    alarm(10);
    waitpid($pid, 0);
    alarm(0);
    my $rc = $? >> 8;
    my @files;
    for my $suffix ('out', 'log', 'map')
    {
        my $text = '';
        if (open(my $input, '<', "$base.$suffix"))
        {
            local $/;
            $text = <$input>;
            close($input);
        }
        push @files, $text;
    }
    return ($rc, @files, "$base.out");
}

my $basic = <<'ASM';
        GBLA count
count   SETA 0
        WHILE count < 3 ; re-evaluate this condition
        DCD count
count   SETA count + 1
        WEND
        DCD count
ASM

for my $mode ('--32', '--64')
{
    subtest "replay in $mode" => sub {
        my ($rc, $output, $log, $map, $path) = convert($basic, $mode);
        is($rc, 0, 'successful conversion');
        my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
        is_deeply(\@words, [0, 1, 2, 3], 'condition is re-evaluated and final variable survives');
        unlike($output, qr/\.rept|\.endr/, 'no fixed-count GAS repetition');
        like($map, qr/case\d+\.s:5 \[WHILE iteration 2\]/, 'replayed line retains its source position');
        my $as = $mode eq '--64' ? ($ENV{'AS64'} // 'aarch64-unknown-linux-gnu-as')
                                : ($ENV{'AS'} // 'arm-unknown-riscos-as');
        is(system(shellwords($as), $path, '-o', "$directory/assembled.o"), 0, 'converted output assembles');
    };
}

subtest 'nested loops and fresh inner condition' => sub {
    my ($rc, $output) = convert(<<'ASM');
        GBLA outer
        GBLA inner
outer   SETA 0
        WHILE outer < 2
inner   SETA 0
        WHILE inner < 2
        DCD outer * 10 + inner
inner   SETA inner + 1
        WEND
outer   SETA outer + 1
        WEND
ASM
    is($rc, 0, 'nested loops convert');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is_deeply(\@words, [0, 1, 10, 11], 'inner loop restarts for each outer iteration');
};

subtest 'zero iterations and inactive conditionals' => sub {
    my ($rc, $output) = convert(<<'ASM');
        WHILE {FALSE}
        GET definitely_missing_file
        WHILE unknown_symbol
        DCD 99
        WEND
        WEND
        [ {FALSE}
        WHILE unknown_symbol
        DCD 98
        WEND
        ]
        DCD 7
ASM
    is($rc, 0, 'skipped bodies are not evaluated');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is_deeply(\@words, [7], 'only live output is emitted');
};

subtest 'macros, local variables and mutable substitutions' => sub {
    my ($rc, $output) = convert(<<'ASM');
        MACRO
        Emit $limit
        LCLA i
i       SETA 0
        WHILE $i < $limit
        DCD $i
i       SETA i + 1
        WEND
        MEND
        Emit 3
        Emit 2
ASM
    is($rc, 0, 'loop inside macro converts');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is_deeply(\@words, [0,1,2,0,1], 'macro parameters and per-call locals remain correct');
};

subtest 'conditional body and logical state' => sub {
    my ($rc, $output) = convert(<<'ASM');
        GBLA i
        GBLL active
i       SETA 0
active  SETL {TRUE}
        WHILE active
        [ i = 1
        DCD 11
        |
        DCD i
        ]
i       SETA i + 1
active  SETL i < 3
        WEND
ASM
    is($rc, 0, 'SETL and IF/ELSE are processed during replay');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is_deeply(\@words, [0,11,2], 'conditional state changes per iteration');
};

subtest 'inline include changes loop state' => sub {
    open(my $include, '>', "$directory/loop-body.s") or die $!;
    print $include "        DCD i\ni       SETA i + 1\n        END\n";
    close($include);
    my ($rc, $output) = convert("        GBLA i\ni       SETA 0\n        WHILE i < 3\n        GET $directory/loop-body.s\n        WEND", '--inline');
    is($rc, 0, 'include replays in the existing scope');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is_deeply(\@words, [0,1,2], 'included assignments affect the next condition');
};

subtest 'repeated local labels and macro calls' => sub {
    my ($rc, $output, $log, $map, $path) = convert(<<'ASM', '--64');
        MACRO
        Word $n
        DCD $n
        MEND
        GBLA i
i       SETA 0
        WHILE i < 3
1       NOP
        B %BT1
        Word i
i       SETA i + 1
        WEND
ASM
    is($rc, 0, 'local labels and macro calls replay');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is_deeply(\@words, [0,1,2], 'macro sees each iteration value');
    my $as = $ENV{'AS64'} // 'aarch64-unknown-linux-gnu-as';
    is(system(shellwords($as), $path, '-o', "$directory/labels.o"), 0, 'repeated labels assemble without duplicates');
};

subtest 'source builtins and string conditions' => sub {
    my ($rc, $output) = convert(<<'ASM');
        GBLA i
        GBLS text
i       SETA 0
text    SETS "semi;colon"
        WHILE text = "semi;colon"
        DCD {LINENUM}
        [ i = 1
text    SETS "done"
        ]
i       SETA i + 1
        WEND
ASM
    is($rc, 0, 'quoted semicolon does not start a comment');
    my @words = $output =~ /^\s*\.word\s+(\d+)\s*$/mg;
    is(scalar(@words), 2, 'SETS changes the next condition');
    is($words[0], $words[1], 'LINENUM is the cached source line on both passes');
};

subtest 'bounded processing errors and output cleanup' => sub {
    for my $case (
        ['        WEND', [], 4, qr/WEND outside WHILE/],
        ['        WHILE {TRUE}', [], 4, qr/WHILE missing WEND at end of file/],
        ["        WHILE\n        WEND", [], 4, qr/WHILE requires a condition/],
        ["        WHILE {TRUE}\n        DCD 1\n        WEND", ['--while-limit', 3], 4, qr/WHILE iteration limit 3 exceeded/],
        ["        WHILE 1 garbage\n        WEND", [], 4, qr/WHILE condition cannot be evaluated/],
        ["        WHILE {TRUE}\n        WEND garbage", [], 4, qr/WEND does not take/],
        ["        WHILE {TRUE}\n        [ {FALSE}\n        WEND", [], 4, qr/WHILE body has unbalanced conditionals/],
        ["        MACRO\n        Bad\n        WHILE {TRUE}\n        MEND\n        Bad", [], 4, qr/WHILE missing WEND at end of macro/],
        ["        WHILE {FALSE}\n        WEND", ['--simple-conditions'], 3, qr/WHILE requires interpreted conditions/],
        [$basic, ['--while-limit', 0], 1, qr/positive integer/])
    {
        my ($rc, $output, $log, $map, $path) = convert($case->[0], @{ $case->[1] });
        is($rc, $case->[2], 'expected failure code');
        like($log, $case->[3], 'specific processing failure');
        ok(!-f $path, 'partial output is removed');
    }
};

done_testing();
