#!/usr/bin/perl -w
# migration du patrimoine Pacbase vers du cobol propre
use warnings;
use strict;

my $pacDir="/app/Dia/rs/src/batch";
my $prog="./RSA010.pco";

#ouverture du programme ou arret avec affichage du msg erreur systeme
open my $pgm, "$prog" or die "ouverture de $prog impossible : $!";
open my $NEWP, '+>', "RSA010.tmp1" or die "ouverture de RSA010.tmp1 impossible : $!";

my $lg ;
my $n_lg ;
while($lg = <$pgm>){
	mef_ligne ();
}
print "$prog -Nb lignes : $n_lg\n";

close($pgm);
open $pgm, "RSA010.tmp1" or die "ouverture de RSA010.tmp1 impossible : $!";
open $NEWP, '+>', "RSA010.tmp2" or die "ouverture de RSA010.tmp2 impossible : $!";

my $proc = 0;
my $n_if = 0;
while($lg = <$pgm>){
	mef_verbe ();
}

close($pgm);
close($NEWP);

sub mef_verbe {
	$lg =~ /\s+PROCEDURE\s+DIVISION/ and $proc = 1 ;

    # report avant procedure
    if ($proc == 0) {
        print $NEWP ($lg) ;
    }else{
        my $colB = " " x 11;
        # recherche premier IF ou "."
        if ($lg =~ /(.*?)((\bIF\b|\.).*)/) {
            print "$.$lg \$3=$3\n";
            if ($3 eq "."){
				if ($n_if > 0) {
#					print $NEWP $colB."   " x --$n_if."END-IF\.\n$2\n" while $n_if;
					print $NEWP $colB."   " x --$n_if."END-IF\n" while $n_if;
					print $NEWP "$colB\." ;
				}else{
					print $NEWP ($lg) ;
				}
            }
            if ($3 eq "IF") {
#            print "$..">".$3\n";
                $n_if++;
                length $1 > 0 and print $NEWP "$1\n" ;
                print $NEWP "$colB$2\n";
            }
        }else{
            print $NEWP ($lg) ;
        }
	}
}

sub mef_ligne {
	my $colA = " " x 6;
	++$n_lg ;
	# effacer les 6 premiers caracteres
	# garder les colonnes 7 a 72
	$lg = $colA.substr  $lg, 6, 65;
	# suppression des espaces de fin de ligne
	$lg =~ s/(.*\S)\s*$/$1\n/;

	print $NEWP ($lg);
}
