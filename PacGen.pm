#!/usr/bin/perl -w
# migration du patrimoine Pacbase vers du cobol propre
package PacGen;
use PacProg;
use strict;
use warnings;

our $type;		# car de type de ligne " ", "D" ou "*"
				# (la ligne de continuite est supprimee
				#  dans PacLigne::extraire_ligne)
our $litteral=0;	# code/litteral
our $sql=0;		# lg pre-compil Oracle $sql => $litteral
our $suite;		# num de troncon dans la ligne
our $dern;		# dernier troncon de la ligne
our $sep="";		# delimiteur du litteral " ou '
my $colB = " " x 4;
my $ind = " " x 3;

our $nb_lg ;	# cpt transformations
sub parcourir_source{
	#parcours d'un code source pour y appliquer une transformation
	my ($prog , $out, $trans) = @_;
	$nb_lg = 0;
	$litteral=0;
	$sql=0;
	my $reste;
	while(defined(my $lg = <$prog>)){
		chomp $lg ;
		$reste = $lg;
		# report commentaires
		substr ($reste, 0, 1) eq "*" and $reste =~ s/\s*$// and print {$out} ("\n$reste") and next ;
		length $lg > 1 and extraire_troncon ($out, $reste, $trans);
    }
}

# ces donnees peuvent etre utilisees dans les procedures
# de traitement des troncons
my $debug;		# lg debug
my $parag;		# lg nom de paragraphe
sub extraire_troncon{
	# separation code/litteral
	# les litteraux sont reconduits sans transformation
	# on applique la transformation voulue sur le code
	# <Dette 02> dble separateur dans un litteral
	my ($out, $ligne, $traiter_troncon) = @_;
	#print "\nextraire_troncon A:$ligne" ;
	$suite = 0;
	$dern = 0 ;
	$type = substr ($ligne, 0, 1);
	my $reste = substr ($ligne, 1);
	# lg de debug
	$debug = 0 ;
	$type eq "D" and $debug = 1 ;
	# extraction paragraphe
	$parag = 0 ;
	$reste =~ /^[a-zA-Z0-9\-]{1,36}/ and $parag = 1 ;

	while (length ($reste) > 0){
		#print "\nextraire_troncon B:$reste:" ;
		# decouper lg en troncons code/litteral
		if ($reste =~ /(.*?)(["|'])(.*)/ and ! $sql){
			length ($3) == 0 and $dern = 1;
			if (! $litteral and ! $sql){
				&$traiter_troncon ($out, "$1");
				# memo new separateur de litteral
				$sep = $2 ;
				$litteral = $sep;
			}else{
				if ($2 eq $sep){
					$suite and $litteral .= "$1" ;
					$litteral .= "$2" ;
					&$traiter_troncon ($out, "$litteral");
					$suite and $litteral = 0;
				}else{
					$litteral .= "$1$2" ;
				}
			}
			$reste = $3 and $suite++ ;
		}elsif ($reste =~ /(.*?EXEC +SQL)(.*)/){
			$sql = $1;
			$reste = $2;
			if (length ($reste) == 0){
				$dern = 1;
				&$traiter_troncon ($out, "$sql");
			}else{
				$dern = 0 ;
				$sql .= $reste ;
				$reste =~ /END\-EXEC\.?$/ and $dern = 1 ;
				#print "\nextraire_troncon C exec-sql:l=$litteral:s=$suite:t=$type:d=$dern:r=$sql:" ;
				&$traiter_troncon ($out, "$sql");
				$reste = "" ;
				$dern and $sql = 0 ;
			}
		}elsif ($reste =~ /(.*?)(END\-EXEC)(.*)/){
			$sql = "$1$2" ;
			length ($3) == 0 and $dern = 1;
			if ($3 eq "."){
				$sql .= $3 ;
				$reste = "";
				$dern = 1;
			}else{
				$reste = $3 ;
			}
			#print "\nextraire_troncon D end-exec:l=$litteral:s=$suite:t=$type:d=$dern:r=$sql:" ;
			&$traiter_troncon ($out, "$sql");
			$sql = 0;
			$dern = 0 ;
		}else{
			if (! $litteral){
				$dern = 1 ;
				#print "\nextraire_troncon E:l=$litteral:s=$suite:t=$type:d=$dern:r=$reste:" ;
				&$traiter_troncon ($out, $reste);
			}else{
				$litteral .= $reste ;
			}
			$reste=""
		}
	}
}

my $Prog ;
sub charger_programme {
	my ($nom, $typo) = @_ ;
#	print "$nom:";
	# creer un objet programme contenant le code
	$Prog = PacProg->new($nom, $typo);
#	print "$Prog";
	$nb_lg=1 ;
}

sub decouper_point{
	my ($new, $reste) = @_;
	my $colB = " " x 4;
	$suite == 0 and print {$new} "\n$type" ;
	#print "\ns=$suite:d=$dern:litt=$litteral:sql=$sql:reste=$reste:";
	if (! $litteral and ! $sql){
		while (length ($reste) > 0){
			# decoupe sur "." non numerique
			if ($reste =~ /([^\.]*\.(?!\d|NEXTVAL))\s*(.*)$/) {
				#print "\n$reste:$1:$2";
				print {$new} ("$1");
				if (length ($2) > 0){
					$nb_lg++ ;
					print {$new} ("\n");
					if ($debug == 0){
						$reste=$type.$colB.$2;
					}else{
						$reste=$2
					}
				}else{
					$reste="";
				}
			}else{
				print {$new} ("$reste");
				$reste="";
			}
		}
	}else{
		print {$new} ("$reste");
	}
}

my $n_if=0 ;
sub inserer_ENDIF{
	# <Dette 01> un ELSE ferme tous les ELSE en cours
	# <Dette 02> RSC050 F0105 (END-IF derriere le EXIT)
	#	jusqu'au 1° IF sans ELSE
	my ($new, $reste) = @_;
	#print "inserer_ENDIF:$reste\n" ;
	my $newlg="" ;
	$parag and print {$new} ("\n", $type, $reste) and return;
	$litteral and print {$new} ("$reste") and return;
	if ($sql) {
		#print ("\n$dern:$reste") ;
		! $suite and print {$new} ("\n ") ;
		print {$new} ("$reste") ;
		return;
	}
	$suite == 0 and $newlg = "\n$type";
	while (length ($reste) > 0){
		# le IF ne doit pas etre precede ou suivi d'un "-"
		# le "." ne doit pas etre suivi d'un decimal
		if ($reste =~ /^(.*?)(\bEND\-IF\b|(?<!\-)\bIF(?!-)\b|\.(?!\d))(.*)$/i) {
			#print "inserer_END_IF_1:$reste\n";
			#print "$.:$n_if:$1:$2\n" ;
			if ($2 eq ".") {
				$newlg .= "$1";
				if ($n_if > 0) {
					while ($n_if > 0) {
						$n_if-- ;
						$newlg .= " END-IF";
						$nb_lg++ ;
					}
				}
				$newlg .= "$2" ;
				$reste=$3;
			}elsif ($2 eq "END\-IF"){
				#print "$.:$2" and length ($3) > 0 and print " - ($3)\n" ;
				$n_if-- ;
				$newlg .= "$1$2" ;
				$reste=$3;
			}else{	#IF
				$n_if++ ;
				$reste=$3;
				$newlg .= "$1$2" ;
				if ($reste =~ /^(.*)\.$/){
					$newlg .= "$1 END-IF.";
					$n_if--;
				}else{
					$newlg .= "$reste" ;
				}
				$reste="";
			}
		}else{
			#$suite == 0 and $newlg = "\n$type";
			$newlg .= $reste ;
			$reste="";
		}
		print {$new} ("$newlg");
		$newlg="";
	}
}

my $n_rd=0 ;
sub inserer_ENDREAD{
	my ($new, $reste) = @_;
	#print "inserer_ENDREAD:$reste\n" ;
	$parag and print {$new} ("\n", $type, $reste) and return;
	$litteral and print {$new} ("$reste") and return;
	my $newlg="" ;
	$suite == 0 and $newlg = "\n$type";
	while (length ($reste) > 0){
		# le READ ne doit pas etre precede d'un "-"
		# le "." ne doit pas etre suivi d'un decimal
		if ($reste =~ /^(.*?)(\bEND\-READ\b|(?<!\-)\bREAD\b|\.(?!\d))(.*)$/) {
			#print "inserer_END_READ_1:$reste\n";
			if ($2 eq ".") {
				#print "$.:$n_rd:$1:$2\n" ;
				$newlg .= $1;
#				print {$new} ("$1");
				if ($n_rd > 0) {
					while ($n_rd > 0) {
						$n_rd-- ;
						$newlg .= " END-READ";
#						print {$new} (" END-READ");
						$nb_lg++ ;
					}
				}
				$newlg .= "$2";
#				print {$new} ("$2") ;
			}elsif ($2 eq "END\-READ"){
				#print "$.:$2" and length ($3) > 0 and print " - ($3)\n" ;
				$n_rd-- ;
				$newlg .= "$1$2";
#				print {$new} ("$1$2") ;
			}else{	#READ
				$n_rd++ ;
				$newlg .= "$1$2";
#				print {$new} ("$1$2");
			}
			$reste=$3;
		}else{
			$newlg .= "$reste";
			$reste="";
		}
		print {$new} ("$newlg");
		$newlg="";
	}
}

my $att = 0 ;
my @t_att ;
our $nb_prg=0 ;
sub supprimer_point_parag {
	my ($old, $new) = @_;
	my $prec = "" ;
	my $att_exit = 0 ;
	my $decl = 0 ;
	while(defined(my $lg = <$old>)){
		chomp $lg ;
		# <Dette01> GO TO F90AA-FN / F90YU-FN
		# <Dette03> F2099. GOBACK
		# <Dette04> F99SX in RSPINIPG
		if ($lg =~ m/^ (END\s+)?DECLARATIVES\.$/){
			$decl = ! $decl ;
			$att_exit and print {$new} ("\n $colB"."EXIT.");
			$att_exit = 0;
			print {$new} "\n$lg" and next ;
		}
		$decl and print {$new} "\n$lg" and next ;

#		if ($lg =~ /^ ([-a-zA-Z0-9]{1,36})/){
		if ($lg =~ /^ ([^\. ]*)(\s+(SECT|DIVIS)ION)?\.$/) {
			# paragraphe
			#print "\n$att:P:$lg" ;
			my $p = $1 ;
			# pour chaque <nomParag> trouve
			#print "\n$p:$att:$att_exit:($prec):" ;
			if (defined($2)) {
				# on conserve les SECTION
				$att and print {$new} (".") ;
				destocker_attente ($new) ;
				print {$new} "\n$lg" ;
			} else {
				# puis une methode estUtilise qui retourne vrai
				# si le parag fourni est appele par PERFORM (THRU) ou GOTO
				if ($Prog->estUtilise("$p")) {
					#print ">garde" ;
					$att and print {$new} (".") ;
					$att and $nb_lg-- ;
					$att_exit = 0;
					destocker_attente ($new) ;
					print {$new} ("\n$lg");
					$prec = $p ;
				}else{
					if ($prec eq "exit"){
						# parag obligatoire derriere un EXIT
						$att and print {$new} (".") ;
						$att and $nb_lg-- ;
						destocker_attente ($new) ;
						print {$new} "\n$lg" ;
						$prec = $p ;
						next ;
					}else{
						# un parag non utilise est supprime
						#print ">suppr" ;
						$prec = "" ;
						$nb_prg++ ;
					}
				}
			}
		}else{
			if ($lg =~ /^\*/ or $lg =~ /^D/ or $lg =~ /^$/ or $lg =~ / COPY /) {
				# mise en attente commentaire ou ligne vide ou COPY
				#print "$att:*:$lg\n" ;
				$t_att [@t_att] = $lg ;
			}else{
				if ($lg =~ /EXIT\./) {
					if ($prec) {
						#print "(exit)" ;
						$att_exit = 1;
						$prec = "exit";
						print {$new} ("\n$lg") ;
					} else {
						# remplacement EXIT orphelin par "*"
						#$t_att [@t_att] = "*" ;
					}
#				} elsif ($lg =~ /GOBACK\./) {
#					print {$new} ("\n");
#					destocker_attente ($new) ;
#					print {$new} ("\n 0-FIN\n$lg");
				} else {
					# ligne
					destocker_attente ($new) ;
					# mise en attente d'un "."
					if ($lg =~ /(.*)\.$/) {
						$att = 1 ;
						print {$new} ("\n$1");
						$nb_lg++ ;
					}else{
						print {$new} ("\n$lg");
					}
					$prec = "" ;
				}
			}
		}
	}
	$att and print {$new} (".");
	destocker_attente ($new) ;
	return $nb_lg, $nb_prg ;

	sub destocker_attente {
		my ($new) = @_;
		#print "\ndestockageP\n" ;
		my $lg_att ;
		foreach $lg_att (@t_att) {print {$new} ("\n$lg_att")} ;
		@t_att = () ;
		$att = 0;
	}
}

sub supprimer_next_sentence {
	#<Dette01 : un "." dans un litteral ou un commentaire empeche la substitution
	my ($old , $new) = @_;
	$nb_lg=0 ;
	# $/ : separateur de ligne
	# local l'initialise a undef
	# donc lecture globale
	my $source = do { local $/ ; <$old> };

	$nb_lg = $source =~ s/
			\s+NEXT\sSENTENCE
			\s+ELSE
			\s+GO\sTO\s+(.*?)		# $1 : GO TO
			\s+(END\-IF)\n
			([^.]*)(\.)				# $3 : instructions a conserver
			(\n\s\1\.\n\s+EXIT\.)	# $4 : appele par le GO TO $1
				/\n$3\n     $2$4/gsmiox;
	print { $new } $source ;
}

=pod
sub supprimer_next_sentence {
	$nb_lg = $source =~ s/
			\s+NEXT\sSENTENCE
			\s+ELSE
			\s+GO\sTO\s+(.*?)		# $1 : GO TO
			\s+(END\-IF)\n
			([^.]*)(\.)				# $3 : instructions a conserver
			(\n\s\1\.\n\s+EXIT\.)	# $4 : appele par le GO TO $1
				/\n$3\n     $2$4/gsmiox;
}
=cut

sub formater_IF{
	my ($new, $reste) = @_;
	my $att = 0;
	my $deb = "";
	my $pivot = "";
	my $deblg = "$type$colB";
	my $newlg ;
	$parag and print {$new} ("\n", $type, $reste) and return;
	$litteral and print {$new} ("$reste") and return;
	if ($sql) {
		#print ("\nformater_IF SQL:$sql=$reste=") ;
		! $suite and print {$new} ("\n ") ;
		print {$new} ("$reste") ;
		return;
	}
	$suite == 0 and ! $att and !$sql and $newlg = "\n$deblg";
	while (length ($reste) > 0){
		#print "\n$. formatter_IF_1:$reste";
		# le IF ne doit pas etre precede ou suivi d'un "-", sauf END-IF
		if ($reste =~ /^\s*(.*?)\s*(\bEND\-IF\b|(?<!\-)\bIF\b(?!-)|\bELSE\b)\s*(.*)$/i) {
			# si le troncon precedent est un litteral
			# il n'apparait pas en $deb
			$deb = $1;
			$pivot = $2;
			$reste=$3;
			#print "\n match:$pivot:$reste";
			$deb =~ s/^\s*(.*?)/$1/;
			(length $deb > 0 or $suite) and $newlg .= $ind x $n_if."$deb\n$deblg";
			if ($pivot eq "END\-IF"){
				$n_if-- ;
				$newlg .= $ind x $n_if."$pivot" ;
				length $reste > 0 and $reste ne "." and $newlg .= "\n$deblg";
			}elsif ($pivot eq "ELSE"){
				#print "$.:$pivot(s=$suite:if=$n_if)" and length ($3) > 0 and print " - ($3)" ;
				$n_if-- ;
				$newlg .= $ind x $n_if."$pivot" ;
				length $reste > 0 and $newlg .= "\n$deblg";
				$n_if++ ;
			}else{	#IF
				$newlg .= $ind x $n_if."$pivot " ;
				# attente de la suite de la ligne
				$att = 1;
				$n_if++ ;
				$nb_lg++ ;
			}
		}else{
			#print "$. s:$suite:a:$att:$reste\n";
			$suite == 0 and $reste =~ s/^\s*(.*?)/$1/ ;
			$reste =~ /^(AND|OR)/ and $n_if--;
			! $att and $newlg .= $ind x $n_if ;
			$reste =~ /^(AND|OR)/ and $n_if++;
			$newlg .= ("$reste") and $att = 0;
			$reste="";
		}
		print {$new} ("$newlg");
		$newlg="";
	}
}

sub indenter_code{
	my ($new, $reste) = @_;
	my $att = 0;
	my $deb = "";
	my $pivot = "";
	my $deblg = "$type$colB";
	my $verbe = "READ|REWRITE|WRITE|MODIFY|DELETE";
	my $alt = "AT END|INVALID KEY" ;
	$parag and print {$new} ($type, $reste) and return;
	$suite == 0 and ! $att and !$sql and print {$new} "$deblg";
	$litteral and print {$new} ("$reste") and return;
	$sql and $dern and print {$new} ("$reste") and return;
	while (length ($reste) > 0){
		#print "$. formatter_IF_1:$reste\n";
		# le verbe ne doit pas etre precede d'un "-" (sauf END-verbe)
		if ($reste =~ /^\s*(.*?)\s*(\bEND\-$verbe\b|(?<!\-)\b$verbe\b|\b$alt\b)\s*(.*)$/) {
			# si le troncon precedent est un litteral
			# il n'apparait pas en $deb
			$deb = $1;
			$pivot = $2;
			$reste=$3;
			#print "$. match:$pivot\n";
			(length $deb > 0 or $suite) and print {$new} ($ind x $n_if."$deb\n$deblg");
			if ($pivot eq "END\-READ"){
				$n_if-- ;
				print {$new} ($ind x $n_if."$pivot") ;
				length $reste > 0 and $reste ne "." and print {$new} ("\n$deblg");
			}elsif ($pivot eq "$alt"){
				print "$.:$pivot(s=$suite:if=$n_if)" and length ($3) > 0 and print " - ($3)" ;
				$n_if-- ;
				print {$new} ($ind x $n_if."$pivot") ;
				length $reste > 0 and print {$new} ("\n$deblg");
				$n_if++ ;
			}else{	#READ
				print {$new} ($ind x $n_if."$pivot ") ;
				# attente de la suite de la ligne
				$att = 1;
				$n_if++ ;
				$nb_lg++ ;
			}
		}else{
			#print "$. s:$suite:a:$att:$reste\n";
			$suite == 0 and $reste =~ s/^\s*(.*?)/$1/ ;
			! $att and print {$new} ($ind x $n_if) ;
			print {$new} ("$reste") and $att = 0;
			$reste="";
		}
	}
}

1;