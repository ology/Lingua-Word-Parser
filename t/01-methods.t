#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;

use_ok 'Lingua::Word::Parser';

my $p = eval { Lingua::Word::Parser->new };
isa_ok $p, 'Lingua::Word::Parser';
ok !$@, 'created with no arguments';
ok !ref $p->{lex}, 'no lex';

$p = Lingua::Word::Parser->new(
    file  => 'eg/lexicon.dat',
);
is ref $p->{lex}, 'HASH', 'lex';

done_testing();
