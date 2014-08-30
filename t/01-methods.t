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
    file => 'eg/lexicon.dat',
    word => 'abioticaly',
);
is ref $p->{lex}, 'HASH', 'lex';

my ($known) = $p->knowns;
is keys %$known, 10, 'known';
my $power = $p->power;
is @$power, 215, 'power';
my $score = $p->score;
is @{ $score->{ [ sort keys %$score ]->[-1] } }, 2, 'score';

done_testing();
