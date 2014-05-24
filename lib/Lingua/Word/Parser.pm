package Lingua::Word::Parser;

use strict;
use warnings;

use Bit::Vector;
use Data::PowerSet;
use IO::File;
use Storable;

our $VERSION = '0.01';

=head1 NAME

Lingua::Word::Parser - Parse a word into known and unknown parts

=head1 SYNOPSIS

 use Lingua::Word::Parser;
 my $p = Lingua::Word::Parser->new(
    word => shift || 'abioticaly',
    file => 'eg/lexicon.dat',
 );
 my ($known) = $p->knowns; #warn Dumper $known;
 my $combos  = $p->power;  #warn Dumper $combos;
 my $scored  = $p->score;  #warn Dumper $score;
 warn Dumper $scored->{ [ sort keys $score ]->[-1] };


=head1 DESCRIPTION

A C<Lingua::Word::Parser> breaks a word into known affixes.

=cut

=head1 METHODS

=head2 new()

  $x = Lingua::Word::Parser->new(%arguments);

Create a new C<Lingua::Word::Parser> object.

Arguments and defaults:

  word: undef
  lex:  undef

=cut

sub new {
    my $class = shift;
    my %args  = @_;
    my $self  = {
        file   => $args{file} || undef,
        lex    => $args{lex}  || undef,
        store  => $args{store} || undef,
        word   => $args{word} || undef,
        known  => {},
        masks  => {},
        combos => [],
        score  => {},
    };
    bless $self, $class;
    $self->_init(%args);
    return $self;
}
sub _init {
    my ($self, %args) = @_;

    # Set the length of our word.
    $self->{wlen} = length $self->{word};

    # Set lex if given data.
    if ( $self->{store} && -e $self->{store} ) {
        $self->{lex} = retrieve( $self->{store} );
    }
    elsif ( $self->{file} && -e $self->{file} ) {
        $self->fetch_lex;
    }
}

=head2 fetch_lex()

Populate word-part => regular-expression lexicon.

=cut

sub fetch_lex {
    my $self = shift;

    # Open the given file for reading...
    my $fh = IO::File->new();
    $fh->open( "< $self->{file}" ) or die "Can't read file: '$self->{file}'";
    for ( <$fh> ) {
        # Split space-separated entries.
        chomp;
        my ($re, $defn) = split /\s+/, $_, 2;
        # Add the entry to the lexicon.
        $self->{lex}{$re} = { defn => $defn, re => qr/$re/ };
    }
    $fh->close;

    return $self->{lex};
}

=head2 knowns()

Fingerprint the known word parts.

=cut

sub knowns {
    my $self = shift;

    # TODO What is this?
    my $id = 0;

    for my $i (values %{ $self->{lex} }) {
        while ($self->{word} =~ /$i->{re}/g) {
            # Match positions.
            my ($m, $n) = ($-[0], $+[0]);
            # Get matched word-part.
            my $part = substr $self->{word}, $m, $n - $m;

            # Create the part-of-word bitmask.
            my $mask = 0 x $m;                      # Before known
            $mask   .= 1 x (($n - $m) || 1);        # Known part
            $mask   .= 0 x ($self->{wlen} - $n);    # After known

            # Output our progress.
#            warn sprintf "%s %s - %s, %s (%d %d), %s\n",
#                $mask,
#                $i->{re},
#                substr($self->{word}, 0, $m),
#                $part,
#                $m,
#                $n - 1,
#                substr($self->{word}, $n),
#            ;

            # Save the known as a member of a list keyed by starting position.
            $self->{known}{$id} = {
                part => $part,
                span => [$m, $n - 1],
                defn => $i->{defn},
                mask => $mask,
            };
            # Save the relationship between mask and id.
            $self->{masks}{$mask} = $id++;
        }
    }

    return $self->{known}, $self->{masks};
}

=head2 power()

Find the "non-overlapping powerset."

=cut

sub power {
    my $self = shift;

    # Get a new powerset generator.
    my $power = Data::PowerSet->new(sort keys %{ $self->{masks} });

    # Consider each member of the powerset.. to save or skip?
    while (my $collection = $power->next) {
#        warn "C: @$collection\n";

        # Save this collection if it has only one item.
        if (1 == @$collection) {
#            warn "\t\tE: only 1 mask\n";
            push @{ $self->{combos} }, $collection;
            next;
        }

        # Compare each mask against the others.
        LOOP: for my $i (0 .. @$collection - 1) {

            # Set the comparison mask.
            my $compare = $collection->[$i];

            for my $j ($i + 1 .. @$collection - 1) {

                # Set the current mask.
                my $mask = $collection->[$j];
#                warn "\tP:$compare v $mask\n";

                # Skip this collection if an overlap is found.
                if (not $self->does_not_overlap($compare, $mask)) {
#                    warn "\t\tO:$compare v $mask\n";
                    last LOOP;
                }

                # Save this collection if we made it to the last pair.
                if ($i == @$collection - 2 && $j == @$collection - 1) {
#                    warn "\t\tE:$compare v $mask\n";
                    push @{ $self->{combos} }, $collection;
                }
            }
        }
    }

    # Hand back the "non-overlapping powerset."
    return $self->{combos};
}

=head2 score()

Score the known vs unknown word part combinations into ratios of characters and
chunks or parts or "spans of adjacent characters."

=cut

sub score {
    my $self = shift;

    # Visit each combination...
    my $i = 0;
    for my $c (@{ $self->{combos} }) {
        $i++;
        my $together = $self->or_together(@$c);

        # Breakdown knowns vs unknowns and knowncharacters vs unknowncharacters.
        my %count = (
            knowns   => 0,
            unknowns => 0,
            knownc   => 0,
            unknownc => 0,
        );
        my $val = '';
        for my $x ( reverse sort @$c ) {
            # Run-length encode an "un-digitized" string.
            my $y = rle($x);
            my ( $knowns, $unknowns, $knownc, $unknownc ) = grouping($y);
#            $val .= "$x ($y)[$knowns/$unknowns | $knownc/$unknownc] ";
            # Accumulate the counters!
            $count{knowns}   += $knowns;
            $count{unknowns} += $unknowns;
            $count{knownc}   += $knownc;
            $count{unknownc} += $unknownc;
        }
        $val .= "$count{knowns}:$count{unknowns} chunks, $count{knownc}:$count{unknownc} chars => "
          . join( ', ', @{ reconstruct( $self->{word}, @$c ) } );

        push @{ $self->{score}{$together} }, $val;
    }

    # 
    return $self->{score};
}

sub write_store {
    my $self = shift;
    store $self->{lex}, $self->{store};
}

=head2 grouping()

Make groups of "un-digitized" strings where B<k>nown and B<u>nknown.

=cut

sub grouping {
    my $scored = shift;
    my @groups = $scored =~ /([ku]\d+)/g;
    my ( $knowns, $unknowns ) = ( 0, 0 );
    my ( $knownc, $unknownc ) = ( 0, 0 );
    for ( @groups ) {
        if ( /k(\d+)/ ) {
            $knowns++;
            $knownc += $1;
        }
        if ( /u(\d+)/ ) {
            $unknowns++;
            $unknownc += $1;
        }
    }
    return $knowns, $unknowns, $knownc, $unknownc;
}

=head2 rle()

Compress B<k>/B<u> strings into cntiguous chunks.

=cut

sub rle {
    my $scored = shift;
    # Run-length encode an "un-digitized" string.
    $scored =~ s/1/k/g; # Undigitize
    $scored =~ s/0/u/g; # "
    # Count contiguous chars.
    $scored =~ s/(.)\1*/$1. length $&/ge;
    return $scored;
}

=head2 does_not_overlap()

Compute whether the given masks overlap.

=cut

sub does_not_overlap {
    my $self = shift;

    # Get our masks to check.
    my ($mask, $check) = @_;

    # Create the bitstrings to compare.
    my $bitmask  = Bit::Vector->new_Bin($self->{wlen}, $mask);
    my $orclone  = Bit::Vector->new_Bin($self->{wlen}, $check);
    my $xorclone = Bit::Vector->new_Bin($self->{wlen}, $check);

    # Compute or and xor for the strings.
    $orclone->Or($bitmask, $orclone);
    $xorclone->Xor($bitmask, $xorclone);

    # Return the "or & xor equivalent sibling."
    return $xorclone->equal($orclone) ? $orclone->to_Bin : 0;
}

=head2 or_together()

Combine a list of bitmasks.

=cut

sub or_together {
    my $self = shift;

    # Get our masks to score.
    my @masks = @_;

    # Initialize the bitmask to return, to zero.
    my $result = Bit::Vector->new_Bin($self->{wlen}, (0 x $self->{wlen}));

    for my $mask (@masks) {
        # Create the bitstrings to compare.
        my $bitmask = Bit::Vector->new_Bin($self->{wlen}, $mask);

        # Get the union of the bit strings.
        $result->Or($result, $bitmask);
    }

    # Return the "or sum."
    return $result->to_Bin;
}

=head2 reconstruct()

Reconstruct the word, with delimiters around known combinations.

=cut

sub reconstruct {
    my ( $word, @masks ) = @_;

    my $strings = [];

    for my $mask (reverse sort @masks) {
        my $i = 0;
        my $last = 0;
        my $string  = '';
        for my $m ( split //, $mask ) {
            if ( $m ) {
                $string .= '<' unless $last;
                $string .= substr( $word, $i, 1 );
                $last = 1;
            }
            else {
                $string .= '>' if $last;
                $string .= substr( $word, $i, 1 );
                $last = 0;
            }
            $i++;
        }
        $string .= '>' if $last;
        push @$strings, $string;
    }

    return $strings;
}

1;
__END__

=head1 SEE ALSO

L<Lingua::TokenParse> - The predecessor of this module.

L<http://en.wikipedia.org/wiki/Affix> is the tip of the iceberg...

=cut
