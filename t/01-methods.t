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
my $score = $p->score( '[', ']' );
my $mask = '1111111111';
is @{ $score->{$mask} }, 2, 'score N';
is $score->{$mask}[0]{score},
    '5:8 chunks / 10:40 chars',
    'score';
is $score->{$mask}[0]{familiarity},
    '1.00 chunks / 1.00 chars',
    'familiarity';
is $score->{$mask}[0]{partition},
    '[a]bioticaly, a[bio]ticaly, abio[tic]aly, abiotic[a]ly, abiotica[ly]',
    'partition';
is $score->{$mask}[0]{definition},
    'opposite, life, possessing, opposite, like',
    'definition';

is Lingua::Word::Parser::_rle('01'), 'u1k1', '_rle';
is Lingua::Word::Parser::_rle('0011'), 'u2k2', '_rle';

done_testing();
