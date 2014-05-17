#!/usr/bin/env perl
use strict;
use warnings;

use Test::More;

use_ok 'Lingua::Word::Parser';

my $p = eval { Lingua::Word::Parser->new };
isa_ok $p, 'Lingua::Word::Parser';
ok !$@, 'created with no arguments';
is ref $p->{lex}, '', 'no lex';

$p = Lingua::Word::Parser->new(
    file  => 'eg/lexicon.dat',
    store => 't/lexicon.store',
);
is ref $p->{lex}, 'HASH', 'lex';
$p->write_store;
ok -e $p->{store}, 'store';

done_testing();
