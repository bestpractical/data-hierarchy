#!/usr/bin/perl
use Test::More tests => 8;
use Test::Exception;

use strict;
use warnings;
BEGIN {
use_ok 'Data::Hierarchy';
}

my $t = Data::Hierarchy->new;
$t->store('/foo', { A => 1 });
$t->store('/foo/bar', { A => 3 });
$t->store('/foo/bar/baz', { A => 4 });

my $rel = $t->to_relative('/foo');

my $tnew = $rel->to_absolute('/beep');

is($tnew->get('/beep')->{A}, 1);
is($tnew->get('/beep/bar/baz')->{A}, 4);

my $saved = $tnew->save;
my $t_very_new = $saved->load;

is($t_very_new->get('/beep')->{A}, 1);
is($t_very_new->get('/beep/bar/baz')->{A}, 4);


throws_ok { $t->to_relative('/fo') } qr!/foo is not a child of /fo!;

# Test backwards compatibility: if we accidentally load an old DH,
# which looks like a Savable, and do stuff to it

bless $saved, 'Data::Hierarchy';
is($saved->get('/beep')->{A}, 1);
is($saved->get('/beep/bar/baz')->{A}, 4);
