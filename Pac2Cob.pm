#!/usr/bin/perl -w
# migration du patrimoine Pacbase vers du cobol propre
use warnings;
use strict;

package Pac2Cob ;

my $prog="RSA011.pco";
print "analyse de $prog\n";

sub mef_ligne {
	$prog = shift(@_);
	print "mef_ligne de $prog\n";
}


sub mef_if {
	print "mef_if de $prog\n";
}

1;