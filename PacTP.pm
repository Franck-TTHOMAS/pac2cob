#!/usr/bin/perl -w
# migration du patrimoine Pacbase vers du cobol propre
package PacTP;
use strict;
use warnings;

sub transformer_data {
	my ($old , $new) = @_;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	# suppr commentaires Pac
	# <Dette> blocage du pgm si cette substitution ne s'opere pas
	print "00 Commentaires Pac:"
	.$source =~ s/
		(\sCONFIGURATION\s+SECTION\.\n
			#(.*?\n){12}		# a activer si on conserve les 12 lignes du cartouche
		)
		.*?						# on vire le reste
		(\sSPECIAL\-NAMES\.)
				/$1$2/smiox
	."\n";

	print "01 LE fichier erreur:"
	.$source =~ s/
		(\s01\s+LE00\.\n)
		.*?						# on vire le reste
		(\sWORKING\-STORAGE\sSECTION\.)
				/$1     COPY RSERR.\n*\n$2/smiox
	."\n";

	print "02 WSS-BEGIN:"
	.$source =~ s/
		\s01\s+WSS\-BEGIN\.\n
		.*?
		\s+05\s+7\-YCREX\s+PICTURE\sX\sVALUE\s\'X\'\.\n
				//smiox
	."+"
	.$source =~ s/
		\s+05\s+EN\-PRE\s+PICTURE\sX\.
				//smiox
	."\n";

	print "03 PACBASE-WORK:"
	.$source =~ s/
		\s01\s+PACBASE\-WORK\.\n
		.*?
		\s+05\s+TIMSEC\s+PICTURE\sXX\.\n
				/*\n     COPY RSWINITP.\n*\n/smiox
	."\n";

	print "03 CMES-COMMUNICATION:"
	.$source =~ s/
		\s01\s+CMES\-COMMUNICATION\.\n
		.*?
		(\s01\s+VALIDATION\-TABLE\-FIELDS\.\n)
				/*\n$1/smiox
	."\n";

	print "04 TT-DAT:"
	.$source =~ s/
		\s01\s+TT\-DAT\.\n
		.*?
		(\s01\s+PACBASE\-INDEXES\sCOMPUTATIONAL\sSYNC\.\n)
				/*\n$1/smiox
	."\n";

	print "05 NUMERIC-VALIDATION-FIELDS:"
	.$source =~ s/
		\s01\s+NUMERIC\-VALIDATION\-FIELDS\.\n
		.*?
		(\s01\s+TABLE\-OF\-ATTRIBUTES\.\n)
				/*\n$1/smiox
	."\n";

	print "06 zones Oracle:"
	.$source =~ s/
		\*\-+\n\*\-{3}\sATOQAS.*?\n\*\-+\n\s01\s+XO00\.\n
		.*?
		\s05\s+XO00\-XBUFFR\s+PIC\sX\(666\)\.\n
				/\n/smiox
	."\n";

	print "07 zones Tuxedo:"
	.$source =~ s/
		\*MESSAGE\sD'ERREUR\sTUXEDO\s\(LOG\sET\sSTDOUT\)\n
		.*?
		\s01\s+XO00-8TMODI\n\s+PICTURE\sX\.\n
				//smiox
	."\n";

	# SYNCHRONIZED a supprimer
	print "08 SYNC:"
	.$source =~ s/
		\sSYNC(?:CHRONIZED)?
				//smiox
	."\n";

	# COMPUTATIONAL par defaut a decrire
	print "09 COMP:"
	.$source =~ s/
		(\sCOMP)(?:UTATIONAL)?(?!-)
				/$1-2/smiox
	."\n";

	print { $new } $source ;

}

sub transformer_proc {
	# <Dette01> alleger commentaires (encadrements)
	my ($old , $newP, $appli) = @_;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	my $idt;
	$idt = substr ($appli,2) ;
	#print "idt:$idt\n";

=pod pas de SQL en copie
	print "F0B copy:"
	.$source =~ s/
		\n\sF0B\.
		.*
		\n\sF0B\-FN\.\s+EXIT\.
				/\n     PERFORM 01-INIT-SQL\n*/gsmiox
	."\n";
=cut

	print "F01 copy:"
	.$source =~ s/
		\n\sF01\.
		.*
		\n\sF0150\-FN\.\s+EXIT\.
				/\n*\n     PERFORM 01-INIT-STANDARD\n*/gsmiox
	."\n";

	print "F05 suppr:"
	.$source =~ s/
		\n\sF05\..*?$
				//smiox
	."\n";

	print "F0510 copy:"
	.$source =~ s/
		\n\sF0510\.
		.*
		\n\sF0510\-FN\.\s+EXIT\.
				/\n*\n     PERFORM 02-LIRE-ENTREE\n*/gsmiox
	."\n";

	print "GO TO suppr:"
	.$source =~ s/
		(MOVE\s'E'\sTO\sCATG)
		\s+GO\s+TO\s+(\S*)\.
		(\n\s\2\.)
				/$1\.\n$3/gsmiox
	."\n";

	print "F70 copy:"
	.$source =~ s/
		(\n\sF70\.\s+EXIT\.)
		.*
		\n\sF70\-FN\.\s+EXIT\.
				/$1\n*\n     PERFORM 08-TRAITER-ANOMALIE\n*/gsmiox
	."\n";

	print "F8Z copy:"
	.$source =~ s/
		\n\sF8Z\.
		.*
		\n\sF8Z\-FN\.\s+EXIT\.
				/\n*\n     PERFORM 09-AFFICHER-ECRAN\n*/gsmiox
	."\n";

=pod si non repetitive
	print "F8115 suppr:"
	.$source =~ s/
		\.?\n\s+PERFORM\sF8115\sTHRU\sF8115\-FN
				//gsmiox
	."+".$source =~ s/
		\n\sF8115\.\n?\s+EXIT\.\n\sF8115\-FN\.\n?\s+EXIT\.
				//smiox
	."\n";
=cut

	print "F81RE suppr:"
	.$source =~ s/
#		(\n\*.*$)*		essai de recup lg comm prec
		\n\sF81RE\.\n?.*F81RE\-FN\.\n?\s+EXIT\.
				//smiox
	."\n";

	print { $newP } $source ;

}

my $colB = " " x 4;
my $ind = " " x 3;
sub structurer_procedure {
	my ($old , $new) = @_;
	my $nb_lg=0 ;
	my $lec = 1 ;
	my $source = do { local $/ ; <$old> };
=pod
	print "F01:"
	.$source =~ s/
		\n\sF01\.
		.*
		\n\sF01\-D\-FN\.\s+EXIT\.
				//smiox
	."\n";
=cut
	print "F10:"
	.$source =~ s/
		(\n\sF10\.				# $1 trt reception
		.*)
		(\n\sF3999\-ITER\-FT\.	# $2 fin ext trt reception
		\n\s+EXIT\.)
		(\n\sF3999\-FN\.		# $3 fin int trt reception
		\n\s+EXIT\.)
		(.*)					# $4 trt emission et fin trt
		(\n\sF80\.)				# $5 procedures
				/\n*\n     PERFORM 1-TRAITER-ENTREE THRU 1-FIN\.\n*$2$4\n*\n 1-TRAITER-ENTREE.$1$3\n 1-FIN.\n ${colB}EXIT.\n*$5/smiox
	."\n";

	print "F60:"
	.$source =~ s/
		(\n\sF55				# $1 trt emission
		.*)
		(\n\sF6999\-ITER\-FT\.	# $2 fin ext trt emission
		\n\s+EXIT\.)
		(\n\sF6999\-FN\.		# $3 fin int trt emission
		\n\s+EXIT\.)
		(.*)					# $4 fin trx
		(\n\sF80\.
		\n\s+EXIT\.)
				/\n*\n     PERFORM 2-TRAITER-SORTIE THRU 2-FIN\.\n*$2$4\n*\n 2-TRAITER-SORTIE.$1$3\n 2-FIN.\n ${colB}EXIT.\n*$5/smiox
	."\n";

	print "F3999-ITER-FT:" ;
	1 while $source =~ s/
		(\n\s1\-TRAITER\-ENTREE\.
		.*\sGO\s+TO\s+F3999\-ITER\-)FT
		(.*\n\s1\-FIN)
				/$1FI$2/gsmiox ;
	print "\n";

=pod
F8Z20 appelle F81FI
F7020 copy ?
=cut
	print { $new } $source ;
}

=pod dico
#s/$old_var/$new-var/ICF/W0-ICF/Ciii/CECR/
ICF/W0-trt-input
ICF/W0-trt-output
OPER/W0-type-traitement/A=affichage:M=mise à jour:S=suite:P=meme ecran:O=autre ecran:E=fin conversation
CATM/W0-type-maj/A=annulation:C=creation:M=modification:X,Y,Z=crea ou modif
CATX/W0-categ/ =entete:R=repetitive:Z=bas d'ecran
ICATR/W0-ind-repetitive
IRR/W0-nb-lg
CATG/W0-erreur-categ/ =pas d'erreur:E=erreur
GR-EG/W0-erreur-ecran/1=pas d'erreur:4=erreur
PR-nn-RUBRIQ/W1-ano-RUBRIQ/0-5
F81ER/980-erreur
F81FK/09-fin-prog
F8155/chargement-zones-entree S->R,T
=cut
1;