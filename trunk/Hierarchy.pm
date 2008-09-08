package Data::Hierarchy;
$VERSION = '0.20';
use strict;
use Clone qw(clone);

=head1 NAME

Data::Hierarchy - Handle data in a hierarchical structure

=head1 SYNOPSIS

    my $tree = Data::Hierarchy->new();
    $tree->store ('/', {access => 'all'});
    $tree->store ('/private', {access => 'auth',
                               '.sticky' => 'this is private});

    $info = $tree->get ('/private/somewhere/deep');

    # return actual data points in list context
    ($info, @fromwhere) = $tree->get ('/private/somewhere/deep');

    my @items = $tree->find ('/', {access => qr/.*/});

    # override all children
    $tree->store_recursively ('/', {access => 'all'});

    my $hashref = $tree->dump;

=head1 DESCRIPTION

Data::Hierarchy provides a simple interface for manipulating
inheritable data attached to a hierarchical environment (like
filesystem).

=cut

sub new {
    my $class = shift;
    # allow shorthand of ->new({...}) to mean ->new(hash => {...})
    unshift @_, 'hash' if @_ % 2;

    my $self = bless {@_}, $class;
    $self->{sep} ||= '/';
    $self->{hash} ||= {};
    $self->{sticky} ||= {};
    return $self;
}

sub key_safe {
    use Carp;
    confess 'key unsafe'
	if length ($_[1]) > 1 and rindex($_[1], $_[0]->{sep}) == length ($_[1]);
    $_[1] =~ s/\Q$_[0]->{sep}\E+$//;
}

sub store_single {
    my ($self, $key, $value) = @_;
    $self->key_safe ($key);
    $self->{hash}{$key} = $value;
}

sub _store {
    my ($self, $key, $value) = @_;
    $self->key_safe ($key);

    my $oldvalue = $self->{hash}{$key} if exists $self->{hash}{$key};
    my $hash = {%{$oldvalue||{}}, %$value};
    for (keys %$hash) {
	if (index($_, '.') == 0) {
	    defined $hash->{$_} ?
		$self->{sticky}{$key}{$_} = $hash->{$_} :
		delete $self->{sticky}{$key}{$_};
	    delete $hash->{$_};
	}
	else {
	    delete $hash->{$_}
		unless defined $hash->{$_};
	}
    }

    $self->{hash}{$key} = $hash;
    delete $self->{hash}{$key} unless %{$self->{hash}{$key}};
    delete $self->{sticky}{$key} unless keys %{$self->{sticky}{$key}};
}

sub merge {
    my ($self, $other, $path) = @_;
    my %datapoints = map {$_ => 1} ($self->descendents ($path),
				    $other->descendents ($path));
    for my $key (reverse sort keys %datapoints) {
	my $value = $self->get ($key);
	my $nvalue = $other->get ($key);
	for (keys %$value) {
	    $nvalue->{$_} = undef
		unless defined $nvalue->{$_};
	}
	$self->store ($key, $nvalue);
    }
}

sub _descendents {
    my ($self, $hash, $key) = @_;
    return sort grep {index($_.$self->{sep}, $key.$self->{sep}) == 0}
	keys %$hash;
}

sub descendents {
    my ($self, $key) = @_;
    use Carp;
    my $both = {%{$self->{hash}}, %{$self->{sticky} || {}}};

    # If finding for everything, don't bother grepping
    return sort keys %$both unless length($key);

    return sort grep {index($_.$self->{sep}, $key.$self->{sep}) == 0}
	keys %$both;
}

# empty the overridden values on descendents for hash
sub _store_recursively {
    my ($self, $key, $value, $hash) = @_;

    $self->key_safe ($key);
    my @datapoints = $self->_descendents ($hash, $key);

    for (@datapoints) {
	my $vhash = $hash->{$_};
	delete $vhash->{$_} for keys %$value;
	delete $hash->{$_} unless %{$hash->{$_}};
    }
}

# use store_fast to avoid trimming duplicated value with ancestors
sub store_fast {
    my $self = shift;
    $self->store (@_, 1);
}

sub store {
    my ($self, $key, $value, $fast) = @_;

    unless ($fast) {
	my $ovalue = $self->get ($key);
	for (keys %$value) {
	    next unless defined $value->{$_};
	    delete $value->{$_}
		if exists $ovalue->{$_} && $ovalue->{$_} eq $value->{$_};
	}
    }
    return unless keys %$value;
    $self->_store_recursively ($key, $value, $self->{hash})
	unless $fast;
    $self->_store ($key, $value);
}

sub store_override {
    my ($self, $key, $value) = @_;

    my ($ovalue, @datapoints) = $self->get ($key);
    for (keys %$value) {
	next unless defined $value->{$_};
	if (exists $ovalue->{$_} && $ovalue->{$_} eq $value->{$_}) {
	    # if the parent has the property already
	    if ($#datapoints > 0 && exists $self->{hash}{$datapoints[-1]}{$_}) {
		$value->{$_} = undef;
	    }
	    else {
		delete $value->{$_};
	    }
	}
    }
    return unless keys %$value;
    $self->_store ($key, $value);
}

# Useful for removing sticky properties.
sub store_recursively {
    my ($self, $key, $value) = @_;

    $self->_store_recursively ($key, $value, $self->{hash});
    $self->_store_recursively ($key, $value, $self->{sticky});
    $self->_store ($key, $value);
}

sub find {
    my ($self, $key, $value) = @_;
    $self->key_safe ($key);
    my @items;
    my @datapoints = $self->descendents($key);

    for my $entry (@datapoints) {
	my $matched = 1;
	for (keys %$value) {
	    my $lookat = (index($_, '.') == 0) ?
		$self->{sticky}{$entry} : $self->{hash}{$entry};
	    $matched = 0
		unless exists $lookat->{$_}
			&& $lookat->{$_} =~ m/$value->{$_}/;
	    last unless $matched;
	}
	push @items, $entry
	    if $matched;
    }
    return @items;
}

sub get_single {
    my ($self, $key) = @_;
    return clone ($self->{hash}{$key} || {});
}

sub get {
    my ($self, $key, $rdonly) = @_;
    $self->key_safe ($key);
    my $value = {};
    # XXX: could build cached pointer for fast traversal
    my @datapoints = sort grep {index($key.$self->{sep}, $_.$self->{sep}) == 0}
	 keys %{$self->{hash}};

    for (@datapoints) {
	my $newv = $self->{hash}{$_};
	$newv = clone $newv unless $rdonly;
	$value = {%$value, %$newv};
    }
    if (exists $self->{sticky}{$key}) {
	my $newv = $self->{sticky}{$key};
	$newv = clone $newv unless $rdonly;
	$value = {%$value, %$newv}
    }
    return wantarray ? ($value, @datapoints) : $value;
}

sub dump {
    my ($self) = @_;
    return $self->{hash};
}

1;

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2003 by Chia-liang Kao E<lt>clkao@clkao.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
