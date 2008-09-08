#!/usr/bin/perl
use ExtUtils::MakeMaker;

WriteMakefile(
    AUTHOR              => 'Chia-liang Kao (clkao@clkao.org)',
    ABSTRACT            => 'Handle inheritable attributes in a hierarchical environment.',
    NAME                => 'Data::Hierarchy',
    VERSION_FROM        => 'Hierarchy.pm',
    DISTNAME            => 'Data-Hierarchy',
    PREREQ_PM           => {
	'Clone'		=> '0',
    },
    dist                => {
        COMPRESS        => 'gzip -9',
        SUFFIX          => '.gz',
    },
);

