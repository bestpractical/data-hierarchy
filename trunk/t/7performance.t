#!/usr/bin/env perl

use Test::More tests => 2;
use Benchmark;

BEGIN {
    use_ok 'Data::Hierarchy';
}

use constant ITERATIONS => 5;
use constant MULTIPLIER => 3;
use constant N => 5_000;

# Returns the power of n that $code grows by.
sub order_of_growth {
    my $n = shift;
    my $code = shift;

    my $time_small = timeit(ITERATIONS, sub { $code->($n) });
    my $time_large = timeit(ITERATIONS, sub { $code->($n * MULTIPLIER) });

    # use 'user' time
    my $ratio = $time_large->[1]/$time_small->[0];

    return log($ratio)/log(MULTIPLIER);
}

my $growth = order_of_growth(N, sub {
                                 my $n = shift;
                                 my $d = Data::Hierarchy->new;
                                 my $kv = { foo => 'bar' };
                                 while ($n > 0) {
                                     $n--;
                                     $d->store("/A/$n", $kv);
                                 }
                                 $d->store("/A", $kv);
                             });

ok ($growth < 1.4, "Data::Hierarchy scales poorly: $growth");
