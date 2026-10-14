#!/usr/bin/perl
# Copyright (c) 2026 Charles Ferguson. SPDX-License-Identifier: GPL-3.0-or-later
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin;
use Text::ParseWords qw(shellwords);

# Relative fixtures are accessible to the Dockcross-mounted working directory.
my $directory = tempdir('entry-tests-XXXXXX', DIR => '.', CLEANUP => 1);
my $tool = "$FindBin::Bin/../objasm2gas.pl";
my $as = $ENV{'AS64'} // 'aarch64-unknown-linux-gnu-as';
my $sequence = 0;

sub convert
{
    my ($body, @options) = @_;
    my $base = "$directory/case" . ++$sequence;
    open(my $source, '>', "$base.s") or die $!;
    print $source "        AREA Test, CODE, READONLY\n$body\n        END\n";
    close($source);
    is(system($^X, $tool, @options, '-o', "$base.out", "$base.s"),
       0, 'source converts');
    open(my $output, '<', "$base.out") or die $!;
    local $/;
    my $text = <$output>;
    close($output);
    return ($base, $text);
}

for my $case (
    ['plain', "        ENTRY ; entry comment", '--64'],
    ['labelled', "start   ENTRY ; entry comment", '--64'],
    ['conditional', "        IF 0\n        ENTRY\n        ENDIF\n        ENTRY", '--64'],
    ['GAS conditional', "        IF 0\n        ENTRY\n        ENDIF\n        ENTRY", '--64', '--simple-conditions'],
)
{
    subtest $case->[0] => sub {
        my ($name, $entry, @options) = @$case;
        my ($base, $text) = convert("        DCD 0\n$entry\n        DCD 1", @options);
        like($text, qr/\.global\s+_entry\s*\n_entry:/, 'entry is defined and exported');
        like($text, qr/start:/, 'same-line label retained') if ($name eq 'labelled');
        like($text, qr/entry comment/, 'comment retained') if ($name eq 'plain');
        is(system(shellwords($as), "$base.out", '-o', "$base.o"), 0, 'AArch64 output assembles');
        open(my $symbols, '-|', 'readelf', '--wide', '--symbols', "$base.o") or die $!;
        local $/;
        my $table = <$symbols>;
        close($symbols);
        is($? >> 8, 0, 'ELF symbol table is readable');
        like($table, qr/\b0000000000000004\s+0\s+NOTYPE\s+GLOBAL\s+DEFAULT\s+\d+\s+_entry\b/,
             'global entry is at byte offset four, in a defined section');
        like($table, qr/\b0000000000000004\s+0\s+NOTYPE\s+LOCAL\s+DEFAULT\s+\d+\s+start\b/,
             'same-line label aliases the entry position') if ($name eq 'labelled');
    };
}

subtest 'unchanged AArch32 output' => sub {
    my ($base, $text) = convert("        DCD 0\n        ENTRY\n        DCD 1", '--32');
    unlike($text, qr/_entry|ENTRY/, '32-bit ENTRY remains ignored');
    my ($other, $without_entry) = convert("        DCD 0\n        DCD 1", '--32');
    is($text, $without_entry, '32-bit output is identical without ENTRY');
};

done_testing();
