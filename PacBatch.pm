#!/usr/bin/perl -w
# migration du patrimoine Pacbase vers du cobol propre
package PacBatch;
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
			(\*.*?\n){0,9}		# on conserve les eventuelles 9 lignes du cartouche
		)
		.*?						# on vire le reste
		(\sSOURCE\-COMPUTER\.)
				/$1$3/smiox
	."\n";

	# RS01-XROWID ... XO00-XORAC1 > RSWINIBD.cpy
	print "01 RSWINIPG:"
	.$source =~ s/
		(\n\s*\*\-+\sZONES\sDE\sMEMORISATION\sDES\sROWIDS\sDES\sCOURANTS)?
		\s+77\s+\S{4}\-XROWID\n
		\s+PICTURE\sX\(18\)\.
		.*
		XO00\-XORAC1\s+PIC\s+X\(001\)\s+OCCURS\s055\.
				//smiox
	."\n";

	#*CODE RETOUR DU PROGRAMME                                         
	# XA00-XRC ... XA81-STATUS2 > RSWINIBD.cpy
	# <Dette01> incorporer PAC-CONSTANTES
	print "02 RSWINIBD:"
	.$source =~ s/
		(\n\*CODE\sRETOUR\sDU\sPROGRAMME)?\n
		\s+77\s+XA00\-XRC\s+VALUE\sZERO\n
		\s+PICTURE\s9\(4\)\.
		.*
		\s+07\s+XA81\-STATUS2\s+PIC\s999\.
				/\n     COPY RSWINIPG.\n     COPY RSWINIBD./smiox
	."\n";

	# declaration CODE-ABORT > reporte dans RSWINIPG.cpy
	print "03 CODE-ABORT:"
	.$source =~ s/
		\n\s+01\s+CODE\-ABORT\s+PIC\sX\(2\)\.
		.*
		\s+05\s+IK\s+PICTURE\sX\.
				//smiox
	."\n";

	# DATCE .. DATSEP (DATSEW) > reporte dans RSWINIPG.cpy
	print "04 DATCE..DATSEP:"
	.$source =~ s/
		\n\s+01\s+DATCE\.
		.*
		\s+01\s+DATSEP\s+PICTURE\sX\sVALUE\s'\/'\.
		(\s+01\s+DATSEW\s+PICTURE\sX\.)?
				//smiox
	."\n";

=for suspens
	# suppression ZONES D'INITIALISATION DES CURRENCIES DE SETS
	print "05 Currencies:"
	.$source =~ s/
		\n\s*\*ZONES\sD\'INITIALISATION\sDES\sCURRENCIES\sDE\sSETS
		.*
		\s+05\s+D\-.*?COMPUTATIONAL\-3\.
				//smiox
	."\n";
=cut

	print { $new } $source ;
}

sub transformer_proc {
	my ($old , $newP) = @_;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	# F0BBA .. F0C-FN > reportes dans RSPINIPG.cpy et RSPINIBD.cpy
	# <Dette02> RTB420 perte lecture F10
	print "20 F0B-F0C:"
	.$source =~ s/
#		(\n\s*\*N0BBA\..*?\.)?
#		\n\s+F0BBA\.
		\n\s+F0B\.\s+EXIT\.
		.*
#		\s+F0C\-FN\.\s+EXIT\.
		\s+F01\.\s+EXIT\.
				/\n     PERFORM 01-INIT-PAC\n\*\n     PERFORM 02-INIT-ORA.\n\*/smiox
	."\n";

=for suspens
	# allegement NOTE
	print "21 Commentaire:"
	.$source =~ s/
		^\*(N.{9}NOTE \*)?
		\s*(.*?)
		(\s*\*(\.)$)?
				/\n\*    $1/gsmiox
	."\n";
=cut
	# allegement NOTE
	print "21 Commentaire:"
	.$source =~ s/
		^\*(?:[^\n]{15}\*)?
		(.*?)\s*\*?\.?$
				/\n\*    $1/gsmiox
	."\n";

	# F2080 .. F2085-FN > reporte dans RSPFINPG.cpy
	# F2090 .. F2095-FN > reporte dans RSPFINBD.cpy
	# <Dette01> RBU040 F2085 perte cloture curseurs
	print "22 F2080:"
	.$source =~ s/
		(\n\*\s+COMMIT\sORACLE\s+\*\.)?
		\n\sF2080\.
		.*
		\n\sF2095\-FN\.\s+EXIT\.
				/\n\*\n     PERFORM 08-TERM-ORA\n\*\n     PERFORM 09-TERM-PAC.\n\*\n 0-FIN.\n\*/smiox
	."\n";

	# RSPINIPG.cpy et RSPINIBD.cpy avant F20-FN
	print "23 COPY INIT:"
	.$source =~ s/
		(\n\s+F20\-FN\.\s+EXIT\.)
				/\n\*\n     COPY RSPINIPG.\n\*\n     COPY RSPINIBD.\n\*\n     COPY RSPFINBD.\n\*\n     COPY RSPFINPG.\n\*$1/smiox
	."\n";

	# F9590 .. F9599-FN > reporte dans RSPFINBD.cpy
	# <Dette01> F9590? - F9599?-FN le plus possible
	print "24 TERM ORA:"
	.$source =~ s/
		(\n\*\s+TEST\sCODE\sRETOUR\sORACLE)?
		\n\s+F959\d\.
		.*
		\s+F959\d\-FN\.\s+EXIT\.
				//smiox
	."\n";

	# PERFORM F0BBA -> PERFORM 011 de RSPINIPG.cpy
	# modificateur g : plusieurs occurences
	print "25 F0BBA:"
	.$source =~ s/
		\n\s+PERFORM\s+F0BBA\s+THRU\s+F0BBA\-FN\.
				/\n     PERFORM 011-DATE-HEURE./gsmiox
	."\n";

	# F98-D .. F98-D-FN > reporte dans RSPFINPG.cpy
	print "26 TERM PAC1:"
	.$source =~ s/
		\n\sF98\-D\.
		.*
		\s+F98\-D\-FN\.\s+EXIT\.
				//smiox
	."\n";

	# F99OR .. F99OR-FN > reporte dans RSPFINPG.cpy
	print "27 TERM PAC2:"
	.$source =~ s/
		(\n\*\s*TRAITEMENT\sDES\sERREURS\sORACLE\s+\*\.)?
		\n\s+F99OR\.
		.*
		\s+F99OR\-FN\.\s+EXIT\.
				//smiox
	."\n";

	# F99VE .. F9999-FN > reporte dans RSPFINPG.cpy
	print "28 TERM PAC3:"
	.$source =~ s/
		(\n\*\s+LECTURE\sVARIABLE\sD'ENVIRONNEMENT\s+\*\.)?
		\n\s+F99VE\.
		.*
		\s+F9999\-FN\.\s+EXIT\.
				//smiox
	."\n";

=for suspens
*N99OR.    NOTE *TRAITEMENT DES ERREURS ORACLE      *.
 F99OR.
	# suppression MOVE D-0204 TO C-0204
	print "28 MOVE D-xxxx TO C-xxxx:"
	.$source =~ s/
		\n\s+MOVE\s+D\-(.{4})\s+TO\s+C\-\1
				//gsmiox
	."\n";
=cut

	print { $newP } $source ;
}

sub transformer_proc_old {
	my ($old , $newP) = @_;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	# F0BBA .. F0C-FN > reportes dans RSPINIPG.cpy et RSPINIBD.cpy
	print "20 F0B-F0C:"
	.$source =~ s/
#		(\n\s*\*N0BBA\..*?\.)?
#		\n\s+F0BBA\.
		\n\s+F0B\.\s+EXIT\.
		.*
#		\s+F0C\-FN\.\s+EXIT\.
		\s+F01\.\s+EXIT\.
				/\n     PERFORM 01-INIT-PAC\n\*\n     PERFORM 02-INIT-ORA.\n\*/smiox
	."\n";

	# allegement NOTE
	print "21 Commentaire:"
	.$source =~ s/
		(\n\*[^\n]{15}\*)
				/\n\*    /gsmiox
	."\n";

	# F2080 .. F2085-FN > reporte dans RSPFINPG.cpy
	# F2090 .. F2095-FN > reporte dans RSPFINBD.cpy
	# <Dette01> RBU040 F2085 perte cloture curseurs
	print "22 F2080:"
	.$source =~ s/
		(\n\*\s+COMMIT\sORACLE\s+\*\.)?
		\n\sF2080\.
		.*
		\n\sF2095\-FN\.\s+EXIT\.
				/\n\*\n     PERFORM 08-TERM-ORA\n\*\n     PERFORM 09-TERM-PAC.\n\*/smiox
	."\n";

	# RSPINIPG.cpy et RSPINIBD.cpy avant F20-FN
	print "23 COPY INIT:"
	.$source =~ s/
		(\n\s+F20\-FN\.\s+EXIT\.)
				/\n\*\n     COPY RSPINIPG.\n\*\n     COPY RSPINIBD.\n\*$1/smiox
	."\n";

	# F9590 .. F9599-FN > reporte dans RSPFINBD.cpy
	# <Dette01> F9590? - F9599?-FN le plus possible
	print "24 TERM ORA:"
	.$source =~ s/
		(\n\*\s+TEST\sCODE\sRETOUR\sORACLE)?
		\n\s+F959\d\.
		.*
		\s+F959\d\-FN\.\s+EXIT\.
				/\n\*\n     COPY RSPFINBD./smiox
	."\n";

	# PERFORM F0BBA -> PERFORM 011 de RSPINIPG.cpy
	# modificateur g : plusieurs occurences
	print "25 F0BBA:"
	.$source =~ s/
		\n\s+PERFORM\s+F0BBA\s+THRU\s+F0BBA\-FN\.
				/\n     PERFORM 011-DATE-HEURE./gsmiox
	."\n";

	# F98-D .. F98-D-FN > reporte dans RSPFINPG.cpy
	print "26 TERM PAC1:"
	.$source =~ s/
		\n\sF98\-D\.
		.*
		\s+F98\-D\-FN\.\s+EXIT\.
				//smiox
	."\n";

	# F99OR .. F99OR-FN > reporte dans RSPFINPG.cpy
	print "27 TERM PAC2:"
	.$source =~ s/
		(\n\*\s*TRAITEMENT\sDES\sERREURS\sORACLE\s+\*\.)?
		\n\s+F99OR\.
		.*
		\s+F99OR\-FN\.\s+EXIT\.
				//smiox
	."\n";

	# F99VE .. F9999-FN > reporte dans RSPFINPG.cpy
	print "28 TERM PAC3:"
	.$source =~ s/
		(\n\*\s+LECTURE\sVARIABLE\sD'ENVIRONNEMENT\s+\*\.)?
		\n\s+F99VE\.
		.*
		\s+F9999\-FN\.\s+EXIT\.
				/\n\*\n     COPY RSPFINPG./smiox
	."\n";

=for suspens
*N99OR.    NOTE *TRAITEMENT DES ERREURS ORACLE      *.
 F99OR.
	# suppression MOVE D-0204 TO C-0204
	print "28 MOVE D-xxxx TO C-xxxx:"
	.$source =~ s/
		\n\s+MOVE\s+D\-(.{4})\s+TO\s+C\-\1
				//gsmiox
	."\n";
=cut

	print { $newP } $source ;
}

sub transformer_data_Oracle {
# obsolete car contient des EXEC SQL qu'on ne peut pas mettre en copie
# ni mettre des copies dans un EXEC SQL
	my ($old , $newD) = @_;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	# description BdD > COPY RSBDD.cpy
	# description SQL > reporte dans RSBDD.cpy
	# NON a cause des EXEC SQL
	$source =~ s/
		\n\s+EXEC\s+SQL\s+INCLUDE\s+SQLCA\s+END\-EXEC\.
				/\n     COPY RSBDD./smiox and print "01 SQLCA\n";

	# suppression *HOST-VARIABLES PARTICULIERES
	$source =~ s/
		\n\s*\*HOST\-VARIABLES\sPARTICULIERES
				//smiox and print "02 Host Variables particulieres\n";

	# suppression *POINT D'INSERTION DES HOST-VARIABLES SPECIFIQUES
	$source =~ s/
		\n\s*\*POINT\sD\'INSERTION\sDES\sHOST\-VARIABLES\sSPECIFIQUES
				//smiox and print "03 Host Variables specifiques\n";

	# suppression *ZONES DE MEMORISATION DES CURRENCIES DE SETS
	$source =~ s/
		\n\s*\*ZONES\sDE\sMEMORISATION\sDES\sCURRENCIES\sDE\sSETS
				//smiox and print "04 memo currencies\n";

	# suppression *ZONES D'INITIALISATION DES CURRENCIES DE SETS
	$source =~ s/
		\n\s*\*ZONES\sD\'INITIALISATION\sDES\sCURRENCIES\sDE\sSETS
				//smiox and print "05 init currencies\n";

	# declaration SQL > reporte dans RSWINIBD.cpy
	# modificateur g : plusieurs occurences
	# NON a cause des EXEC SQL
	$source =~ s/
		\n\s*EXEC\s+SQL\s+BEGIN\s+DECLARE\s+SECTION\s+END-EXEC\.
		(.*?)
		EXEC\s+SQL\s+END\s+DECLARE\s+SECTION\s+END-EXEC\.
				//gsmiox and print "06 EXEC SQL\n" ;

	print $newD $source ;
}

sub transformer_proc_Oracle {
# obsolete car contient des EXEC SQL qu'on ne peut pas mettre en copie
# ni mettre des copies dans un EXEC SQL
	my ($old , $newP) = @_;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	# F95-A .. F95-A-FN > reporte dans RSPINIBD.cpy
	# NON a cause des EXEC SQL
	$source =~ s/
		\n\s+F95\-A\.
		.*
		\s+F95\-A\-FN\.\s+EXIT\.
				//smiox and print "24 F95-A\n";

	print { $newP } $source ;
}

my $colB = " " x 4;
my $ind = " " x 3;
sub structurer_procedure {
=pod modeles
RBU040 : F05, F20 pas de lecture OK
RSB000 : F05, commentaires, F20 pas de lecture OK
RBG520 : F05 sans EXIT (avec des goto), F05TE, pas de commentaires, F020, F9099-ITER-FN(sans goto) : lecture d'avance OK
RBC050 : F05, F10, F19, pas de commentaires, F20, F22 synchro 4 niveaux OK
RSW301 : F05, commentaires, F10, F20 OK
=cut
# <Dette01> RSD200 KO
	my ($old , $new) = @_;
	my $nb_lg=0 ;
	my $lec = 1 ;
	my $source = do { local $/ ; <$old> };

	if ($source !~ m/
		\n\sF05\.		# parag F05
		.*READ.*
		\n\sF20\.		# parag F20
				/smiox){
		print "F05 vide:"
		.$source =~ s/
			\n\sF05\.\n\s+EXIT\.	# parag F05
			(?:\n\*.*$)*
			(\n\sF20\.				# parag F20
			.*						# traitement
			\n\sF9099\-ITER\-FN\.
			\n\s+GO\s+TO\s+F05\.)
					/\n ${colB}PERFORM 1-TRAITER-ENREG THRU 1-FN\n ${colB}$1\n\*/smiox
		."\n";
		$lec = 0 ;
	}

	$lec and print "F05:"
	.$source =~ s/
		\n\sF05\.(?:\n\s+EXIT\.)?	# parag F05
		(.*						# parag lecture
		\n\sF(?:05|10)\-FN\.\n\s+EXIT\.)
		(.*						# traitement
		\n\sF9099\-ITER\-FN\.
		\n\s+GO\s+TO\s+F05\.)
				/\n\*\n ${colB}PERFORM 901-LECTURE-ENREG THRU 901-FN\n\*\n ${colB}PERFORM 1-TRAITER-ENREG THRU 1-FN\n ${colB}  UNTIL FIN-FICHIER.\n\*$2\n*\n 901-LECTURE-ENREG.$1\n 901-FN.\n ${colB}EXIT.\n*/smiox
	."\n";

	print "F20:"
	.$source =~ s/
#		(?:\n\*.*)*
		\n\sF(?:19|20)\.
		\n\s+IF\s+FT\s+=\s+ALL\s+'1'
        \n\s+NEXT\s+SENTENCE\s+ELSE\s+GO\s*TO\s+F(?:19|20)\-FN\s+END\-IF.
				//gsmiox
	."\n";

	print "F22:"
	.$source =~ s/
		(\n\sF20\-FN\.
		\n\s+EXIT\.)
		(.*?
		\n\sF22\.
		.*
		\n\sF22\-FN\.\n\s+EXIT\.)
		(.*
#		\n\sF40\.
#		.*
		\n\s901\-FN\.
		\n\s+EXIT\.)
				/\n$1\n$3\n*\n 902-TRAITER-RUPTURE.$2\n 902-FN.\n ${colB}EXIT.\n\*/gsmiox
	."\n";

	print "F40:"
	.$source =~ s/
		\n\sF20\-FN\.
		\n\s+EXIT\.
		(.*
		\n\sF9099\-ITER\-FN\.)
		\n\s+GO\s+TO\s+F05\.
				/\n 1-TRAITER-ENREG.$1\n*\n F05.\n*/gsmiox
		."\n";

	if ($lec){
		print "F41:"
		.$source =~ s/
			(\n\sF05\.)
					/$1\n ${colB}PERFORM 901-LECTURE-ENREG THRU 901-FN.\n 1-FN.\n ${colB}EXIT./gsmiox
		."\n";
	}else{
		print "F41 ss lec:"
		.$source =~ s/
			(\n\sF05\.)
					/$1\n 1-FN.\n ${colB}EXIT./gsmiox
		."\n";
	}

	print "901:"
	.$source =~ s/
		(\n\s901.*?
		READ\s+..\-FICHIER)\s+(AT\sEND)
		\n\s+(MOVE\s+1\s+TO\s+..\-FI)\sEND-READ\.
		(\n\sF10..\-FN\.)
				/$1\n ${colB}$2\n ${colB}${ind}$3\n ${colB}NOT AT END\n ${colB}${ind}PERFORM 902-TRAITER-RUPTURE THRU 902-FN\n ${colB}END-READ.$4/gsmiox
	."\n";
	print { $new } $source ;
}


1;