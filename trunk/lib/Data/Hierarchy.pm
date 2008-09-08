package Data::Hierarchy;
$VERSION = '0.40';
use strict;
use Storable qw(dclone);

use Carp ();
# XXX consider using Moose

=head1 NAME

Data::Hierarchy - Handle data in a hierarchical structure

=head1 SYNOPSIS

    my $tree = Data::Hierarchy->new();
    $tree->store ('/', {access => 'all'});
    $tree->store ('/private', {access => 'auth',
                               '.note' => 'this is private});

    $info = $tree->get ('/private/somewhere/deep');

    # return actual data points in list context
    ($info, @fromwhere) = $tree->get ('/private/somewhere/deep');

    my @items = $tree->find ('/', {access => qr/.*/});

    # override all children
    $tree->store ('/', {'.note' => undef}, {override_sticky_descendents => 1});

=head1 DESCRIPTION

L<Data::Hierarchy> provides a simple interface for manipulating
inheritable data attached to a hierarchical environment (like
a filesystem).

One use of L<Data::Hierarchy> is to allow an application to annotate
paths in a real filesystem in a single compact data
structure. However, the hierarchy does not actually need to correspond
to an actual filesystem.

Paths in a hierarchy are referred to in a Unix-like syntax; C<"/"> is
the root "directory". (You can specify a different separator character
than the slash when you construct a Data::Hierarchy object.)  With the
exception of the root path, paths should never contain trailing
slashes. You can associate properties, which are arbitrary name/value
pairs, with any path.  (Properties cannot contain the undefined value.)
By default, properties are inherited by child
paths: thus, if you store some data at C</some/path>:

    $tree->store('/some/path', {color => 'red'});

you can fetch it again at a C</some/path/below/that>:

    print $tree->get('/some/path/below/that')->{'color'};
    # prints red

On the other hand, properties whose names begin with dots are
uninherited, or "sticky":

    $tree->store('/some/path', {'.color' => 'blue'});
    print $tree->get('/some/path')->{'.color'};            # prints blue
    print $tree->get('/some/path/below/that')->{'.color'}; # undefined

Note that you do not need to (and in fact, cannot) explicitly add
"files" or "directories" to the hierarchy; you simply add and delete
properties to paths.

=cut

=head1 CONSTRUCTOR

Creates a new hierarchy object.  Takes the following options:

=over

=item sep

The string used as a separator between path levels. Defaults to '/'.

=back

=cut

sub new {
    my $class = shift;
    my %args = (
                sep => '/',
                @_);

    my $self = bless {}, $class;
    $self->{sep} = $args{sep};
    $self->{root} = Data::Hierarchy::Node->new;
    return $self;
}

=head1 METHODS

=head2 Instance Methods

=over

=cut

=item C<store $path, $properties, {%options}>

Given a path and a hash reference of properties, stores the properties
at the path.

Unless the C<override_descendents> option is given with a false value,
it eliminates any non-sticky property in a descendent of C<$path> with
the same name.

If the C<override_sticky_descendents> option is given with a true
value, it eliminates any sticky property in a descendent of C<$path>
with the same name.

A value of undef removes that value; note, though, that
if an ancestor of C<$path> defines that property, the ancestor's value
will be inherited there; that is, with:

    $t->store('/a',   {k => 'top'});
    $t->store('/a/b', {k => 'bottom'});
    $t->store('/a/b', {k => undef});
    print $t->get('/a/b')->{'k'};

it will print 'top'.

=cut

sub store {
    my $self = shift;
    $self->_autoupgrade;
    my $path = shift;
    my $props = shift;
    my $opts = shift || {};

    my %args = (
               override_descendents => 1,
               override_sticky_descendents => 0,
                %$opts);


    my $node = $self->_get_subtree($path);
    my $node_is_new = 0;
    if ($node) {
        $self->_remove_matching_properties_recursively($node, $props, \%args, 1);
    } else {
        $node = Data::Hierarchy::Node->new;
        $node_is_new = 1;
    }

    my $current_props = $self->get($path, "don't clone");

    while (my ($key, $val) = each %$props) {
        if (defined $val) {
            if (not defined $current_props->{$key} or
                $val ne $current_props->{$key}) {
                $node->set_key($key, $val);
            }
        } else {
            $node->delete_key($key);
        }
    }

    if ($node_is_new or $node->is_empty) {
        $self->_set_subtree($path, $node);
    }
}

=item C<get $path, [$dont_clone]>

Given a path, looks up all of the properteies (sticky and not) and
returns them in a hash reference.  The values are clones, unless you
pass a true value for C<$dont_clone>.

If called in list context, returns that hash reference followed by all
of the ancestral paths of C<$path> which contain non-sticky properties
(possibly including itself).

=cut

sub _add_all_to_hash {
    my($to, $from, $dont_clone) = @_;

    $from = dclone $from unless $dont_clone;
    @{$to}{keys %$from} = values %$from;
}

sub get {
    my ($self, $path, $dont_clone) = @_;
    $self->_autoupgrade;

    my $parts = $self->_path_parts($path);
    my $node = $self->{root};

    my $props = {};
    my @datapoints;

    if (keys %{ $node->inherited }) {
        push @datapoints, '';
        _add_all_to_hash($props, $node->inherited, $dont_clone);
    }

    my $current_path = '';
    for my $part (@$parts) {
        $current_path .= $self->{sep} . $part;

        $node = $node->kids->{$part};
        unless ($node) {
            return wantarray ? ($props, @datapoints) : $props;
        }

        if (keys %{ $node->inherited }) {
            push @datapoints, $current_path;
            _add_all_to_hash($props, $node->inherited, $dont_clone);
        }
    }

    # Every part existed, so $node is now precisely what they asked
    # for.
    _add_all_to_hash($props, $node->uninherited, $dont_clone);

    return wantarray ? ($props, @datapoints) : $props;
}

=item C<find $path, $property_regexps>

Given a path and a hash reference of name/regular expression pairs,
returns a sorted list of all paths which are descendents of C<$path>
(including itself) and define B<at that path itself> (not inherited)
all of the properties in the hash with values matching the given
regular expressions.  (You may want to use C<qr/.*/> to merely see if
it has any value defined there.)  Properties can be sticky or not.

=cut

sub find {
    my ($self, $path, $prop_regexps) = @_;
    $self->_autoupgrade;

    my $node = $self->_get_subtree($path);

    return unless $node;

    my $items = [];

    my $recursive;
    $recursive = sub {
        my($subpath, $subnode) = @_;

        my $matched = 1;
        for (keys %$prop_regexps) {
            my $lookat = _is_inherited($_) ?
              $subnode->inherited : $subnode->uninherited;
            $matched = 0
              unless exists $lookat->{$_}
                && $lookat->{$_} =~ m/$prop_regexps->{$_}/;
            last unless $matched;
        }
        push @$items, $self->_root_without_sep($subpath) if $matched;

        my $kids = $subnode->kids;
        while (my ($name, $subsubnode) = each %$kids) {
            $recursive->($subpath . $self->{sep} . $name,
                         $subsubnode);
        }
    };

    $recursive->($self->_root_without_sep($path), $node);

    return sort @$items;
}

=item C<defines $path, $property>

Given a path and a property name, returns whether or not
that property (possibly sticky) is defined B<at C<$path> itself>
(not in an ancestor).

=cut

sub defines {
    my ($self, $path, $property) = @_;
    $self->_autoupgrade;

    my $node = $self->_get_subtree($path);

    return unless $node;
    return unless $node->get_key($property);
    return 1;
}

# Internal method.
#
# Returns whether a given property name is inherited.

sub _is_inherited {
    my $name = shift;
    return index($name, '.') != 0;
}

# Internal method.
#
# Returns its argument, unless it's just a sep, in which case it
# returns the empty string.

sub _root_without_sep {
    my ($self, $path) = @_;
    return $path eq $self->{sep} ? '' : $path;
}

# Internal method.
#
# Returns the Data::Hierarchy::Node for the given path, if it exists.

sub _get_subtree {
    my ($self, $path) = @_;

    my $parts = $self->_path_parts($path);

    my $node = $self->{root};
    for my $part (@$parts) {
        $node = $node->kids->{$part};
        return unless $node;
    }

    return $node;
}

# Internal method.
#
# Sets the Data::Hierarchy::Node for the given path, autovivifying
# parents.  Note that this essentially overwrites the entire subtree
# at the path.  $new_node may be undefined (or empty), in which case
# it deletes the subtree.

sub _set_subtree {
    my ($self, $path, $new_node) = @_;
    my $parts = $self->_path_parts($path);

    unless (@$parts) {
        $self->{root} = $new_node || Data::Hierarchy::Node->new;
        return;
    }

    my $recursive;
    $recursive = sub {
        my $node = shift;
        my $kid_name = shift @$parts;

        if (@$parts) {
            my $kid = $node->kids->{$kid_name} ||= Data::Hierarchy::Node->new;

            $recursive->($kid);

            if ($kid->is_empty) {
                delete $node->kids->{$kid_name};
            }
        } else {
            if (defined $new_node and not $new_node->is_empty) {
                $node->kids->{$kid_name} = $new_node;
            } else {
                delete $node->kids->{$kid_name};
            }
        }
    };

    $recursive->($self->{root});
}

=item C<merge $other_hierarchy, $path>

Given a second L<Data::Hierarchy> object and a path, copies all the
properties from the other object at C<$path> or below into the
corresponding paths in the object this method is invoked on.  All
properties from the object this is invoked on at C<$path> or below are
erased first.

=cut

sub merge {
    my ($self, $other, $path) = @_;
    $self->_autoupgrade;

    $self->_path_safe($path);

    my $node = dclone($other->_get_subtree($path));

    # We need to make sure that things that are inherited onto $path
    # in the other tree end up on $path in our tree.
    my $props_on_merge_root = $other->get($path);
    while (my ($k, $v) = each %$props_on_merge_root) {
        $node->set_key($k, $v);
    }

    $self->_set_subtree($path => $node);

    return;
}

=item C<move $from, $to>

Moves all of the properties under the path C<$from> to the path C<$to>
Given a second L<Data::Hierarchy> object and a path, copies all the
properties from the other object at C<$path> or below into the
corresponding paths in the object this method is invoked on.  All
properties from the object this is invoked on at C<$path> or below are
erased first.

=cut


sub move {
    my ($self, $from, $to) = @_;
    $self->_autoupgrade;

    $self->_path_safe($from);
    $self->_path_safe($to);

    my $node = dclone($self->_get_subtree($from));

    # We need to make sure that things that are inherited onto $from
    # end up on $to.
    my $props_on_copy_root = $self->get($from);
    while (my ($k, $v) = each %$props_on_copy_root) {
        $node->set_key($k, $v);
    }

    $self->_set_subtree($from, undef);
    $self->_set_subtree($to, $node);

    return;
}

=item C<to_relative $base_path>

Given a path which B<every> element of the hierarchy must be contained
in, returns a special Data::Hierarchy::Relative object which
represents the hierarchy relative that path. The B<only> thing you can
do with a Data::Hierarchy::Relative object is call
C<to_absolute($new_base_path)> on it, which returns a new
L<Data::Hierarchy> object at that base path. For example, if
everything in the hierarchy is rooted at C</home/super_project> and it
needs to be moved to C</home/awesome_project>, you can do

    $hierarchy = $hierarchy->to_relative('/home/super_project')->to_absolute('/home/awesome_project');

(Data::Hierarchy::Relative objects may be a more convenient
serialization format than Data::Hierarchy objects, if they are
tracking the state of some relocatable resource.  The (explicitly
undocumented) format of Data::Hierarchy::Relative objects will not
change with new versions of Data::Hierarchy.)

=cut

sub to_relative {
    my $self = shift;
    $self->_autoupgrade;
    my $base_path = shift;

    return Data::Hierarchy::Relative->new($self, $base_path);
}

=item C<save>

Returns a special Data::Hierarchy::Savable object which represents the
same data as this object.  While the internal representation of
Data::Hierarchy may change from version to version of this module, the
representation of Data::Hierarchy::Savable (which is explicitly
undocumented) will not change; thus, you may safely serialize a
Data::Hierarchy::Savable object and reload it even if Data::Hierarchy
has been upgraded.

The B<only> thing you can do with a Data::Hierarchy::Savable object is
call C<load> on it, which returns a new L<Data::Hierarchy>.

=cut

sub save {
    my $self = shift;
    $self->_autoupgrade;

    return Data::Hierarchy::Savable->new($self);
}

# Internal method.
#
# Dies if the given path has a trailing slash and is not the root.

sub _path_safe {
    my $self = shift;
    my $path = shift;

    return if $path eq $self->{sep};

    my $location_of_last_separator = rindex($path, $self->{sep});
    return if $location_of_last_separator == -1;

    my $potential_location_of_trailing_separator = (length $path) - (length $self->{sep});

    return unless $location_of_last_separator == $potential_location_of_trailing_separator;

    Carp::confess('non-root path has a trailing slash!');
}

# Internal method.
#
# Returns an array reference containing the parts of the path.
# Trailing slashes are ignored.
#
# Dies if the given path has a trailing slash and is not the root.
#
# Examples:
# ''          => []
# '/'         => []
# '/foo/bar'  => [qw/foo bar/]
# '/foo/bar/' => [qw/foo bar/]

sub _path_parts {
    my $self = shift;
    my $path = shift;

    return [] if $path eq '' or $path eq $self->{sep};

    $self->_path_safe($path);

    my $parts = [ split m{\Q$self->{sep}}, $path ];

    # Remove empty part at the front.
    Carp::confess("Must pass an absolute path to _path_parts") if length $parts->[0];
    shift @$parts;

    return $parts;
}

# Internal method.
#
# Given a node, a hash reference of properties, and args
# override_descendents and override_sticky_descendents, removes all
# properties from the hashes at the path or its descendents with the
# same name as a name in the given property hash. (The values in the
# property hash are ignored.)  It always removes everything at the top
# level.

sub _remove_matching_properties_recursively {
    my ($self, $node, $remove_props, $args, $top_level) = @_;

    for my $k (keys %$remove_props) {
        if (_is_inherited($k)) {
            delete $node->inherited->{$k}
              if $args->{override_descendents} or $top_level;
        } else {
            delete $node->uninherited->{$k}
              if $args->{override_sticky_descendents} or $top_level;
        }
    }

    return unless $args->{override_descendents} or
      $args->{override_sticky_descendents};

    my $kids = $node->kids;
    while (my ($name, $subnode) = each %$kids) {
        $self->_remove_matching_properties_recursively($subnode,
                                                       $remove_props,
                                                       $args);
        if ($subnode->is_empty) {
            delete $kids->{$name};
        }
    }
}

# These are for backwards compatibility only.

sub store_recursively { my $self = shift; $self->store(@_, {override_sticky_descendents => 1}); }
sub store_fast        { my $self = shift; $self->store(@_, {override_descendents => 0}); }
sub store_override    { my $self = shift; $self->store(@_, {override_descendents => 0}); }

# Internal method.
#
# Checks to see if this is an old (pre-0.4) Data::Hierarchy loaded via
# YAML or some such, and modernizes it if it is.  Note that such old
# objects had the same format that Data::Hierarchy::Savable does now.

sub _autoupgrade {
    my $self = shift;
    return unless $self->{hash} or $self->{sticky};

    # Aha!  This is an old object!
    bless $self, 'Data::Hierarchy::Savable';
    my $new_self = $self->load;
    bless $self, 'Data::Hierarchy';
    %$self = %$new_self;
    return;
}

package Data::Hierarchy::Relative;

sub new {
    my $class = shift;
    my $dh = shift;
    my $base_path = shift;

    my $self = bless { sep => $dh->{sep},
                       hash => {},
                       sticky => {}
                     }, $class;

    my $base_length = length $base_path;

    my $get_relative_path = sub {
        my $path = shift;
        unless ($path eq $base_path or index($path, $base_path . $self->{sep}) == 0) {
            Carp::confess("$path is not a child of $base_path");
        }
        return substr $path, $base_length;
    };

    my $recursive;
    $recursive = sub {
        my ($node, $path) = @_;

        if (%{ $node->inherited }) {
            my $rel_path = $get_relative_path->($path);
            $self->{hash}{$rel_path} = { %{ $node->inherited } };
        }

        if (%{ $node->uninherited }) {
            my $rel_path = $get_relative_path->($path);
            $self->{sticky}{$rel_path} = { %{ $node->uninherited } };
        }

        for my $kid (sort keys %{ $node->kids }) {
            $recursive->($node->kids->{$kid}, $path . $self->{sep} . $kid);
        }
    };
    $recursive->($dh->{root}, '');

    return $self;
}

sub to_absolute {
    my $self = shift;
    my $base_path = shift;

    my $tree = Data::Hierarchy->new( sep => $self->{sep} );

    for my $item (qw/hash sticky/) {
        my $original = $self->{$item};

        while (my ($path, $props) = each %$original) {
            $tree->store($base_path . $path, $props, { override_descendents => 0 });
        }
    }

    return $tree;
}

package Data::Hierarchy::Savable;
use base 'Data::Hierarchy::Relative';

sub new {
    my $class = shift;
    my $dh = shift;
    return $class->SUPER::new($dh, '');
}

sub load {
    my $self = shift;
    return $self->to_absolute('');
}

package Data::Hierarchy::Node;
use base 'Class::Accessor::Fast';

__PACKAGE__->mk_accessors(qw/inherited uninherited kids/);

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    $self->inherited({}) unless defined $self->inherited;
    $self->uninherited({}) unless defined $self->uninherited;
    $self->kids({}) unless defined $self->kids;
    return $self;
}

sub is_empty {
    my $self = shift;
    return (keys %{ $self->inherited } == 0 &&
            keys %{ $self->uninherited } == 0 &&
            keys %{ $self->kids } == 0);
}

sub _is_inherited { Data::Hierarchy::_is_inherited(@_) }

sub delete_key {
    my ($self, $k) = @_;
    if (_is_inherited($k)) {
        delete $self->inherited->{$k};
    } else {
        delete $self->uninherited->{$k};
    }
}

sub get_key {
    my ($self, $k) = @_;
    if (_is_inherited($k)) {
        return $self->inherited->{$k};
    } else {
        return $self->uninherited->{$k};
    }
}

sub set_key {
    my ($self, $k, $v) = @_;
    if (_is_inherited($k)) {
        $self->inherited->{$k} = $v;
    } else {
        $self->uninherited->{$k} = $v;
    }
}

1;

=back

=head1 COMPATIBILITY

A serialized (via YAML, FreezeThaw, etc) Data::Hierarchy::Savable or
Data::Hierarchy::Relative object should be readable by future versions
of this module; we do not make the same guarantee for Data::Hierarchy
objects themselves.  Versions of this module prior to 0.40 used an
internal representation had no Data::Hierarchy::Savable; in order to
not break applications which directly serialized Data::Hierarchy
objects, the module does attempt to convert pre-0.40 objects into
modern objects.

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>
David Glasser E<lt>glasser@mit.eduE<gt>

=head1 COPYRIGHT

Copyright 2003-2006 by Chia-liang Kao E<lt>clkao@clkao.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
