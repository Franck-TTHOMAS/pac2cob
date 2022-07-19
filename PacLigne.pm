#!/usr/bin/perl -w
# migration du patrimoine Pacbase vers du cobol propre
package PacLigne;
use PacGen;
use strict;
use warnings;

our $nb_lg ;	# cpt transformations

sub decoboliser_ligne {
	my ($prog , $newD , $newP) = @_;
   # la premiere partie du fichier est stockee en .tmpD0 (D comme Data)
	my $out = $newD ;

	my $typo="batch";
	my $appli ;
	my $reste;
	my $newlg ;
	my $type ;
	while(defined(my $lg = <$prog>)){
		chomp $lg ;
		$lg =~ /PROGRAM\-ID\.\s+(..).{1,8}\./ and $appli = $1 ;
		$lg =~ /01\s+WSS-BEGIN./x and $typo = "TP";
        if (length $lg > 6) {
			# report contenu de la ligne : col 7 a 72
			# raz 6 premiers caracteres
			$reste = substr $lg, 6, 66 ;
			$type = substr ($reste, 0, 1) ;
			if ($type eq "-"){
				# la ligne de continuation est renvoyee derriere la precedente
				$reste =~ s/^\-\s+["|'](.*)/$1/ ;
				$newlg .= $reste ;
				$nb_lg++ ;
			} else {
				$newlg and $newlg =~ s/\s*$// ;
				$newlg and print {$out} "\n$newlg" ;
				$newlg = $reste ;
			}
        }
		# la seconde partie du fichier commence en "PROCEDURE DIVISION"
		# elle est stockee en .tmpP0 (P comme Proc)
		$lg =~ /PROCEDURE\s+DIVISION/x and $out = $newP and print {$out} "\n$newlg" and $newlg = "" ;
    }
	$newlg and print {$out} "\n$newlg" ;
	return $., $appli,$typo, $nb_lg ;
}

sub recoboliser_ligne {
	my ($old, $new) = @_ ;
	while(defined(my $lg = <$old>)){
		chomp $lg ;
		print { $new } "      ";
		if (length ($lg) > 66){
			#print "$. :$lg:".length ($lg)."\n";
			PacGen::extraire_troncon ($new, $lg, \&compacter_troncon);
			$nb_lg++ ;
		}else{
			print { $new } "$lg\n";
		}
	}
}

my $ec=0;
my $worklg="";
my $newlg="";
my $xniv ;
my $nniv ;
my @niv_prec=0 ;
my $nivth=0 ;
#my %tniv ;
my $var ;
my $ytab ;
sub compacter_data{
	my ($new, $oldlg) = @_ ;
	#print "\nd=$PacGen::dern:$oldlg:";
	$. == 0 and $nb_lg = 0 ;
	if ($oldlg =~ /^\s*(\d\d)\s+([a-zA-Z0-9-]+)(.*)/){
		($xniv, $var, $oldlg) = ($1, $2, $3);
		calculer_niveau ();
		$ec=1;
		$worklg = "" ;
	}elsif ($PacGen::sql){
		#print "\nsuite=$PacGen::suite:$oldlg:";
		!$PacGen::suite and $worklg="";
		$ec=1 ;
	}
	if ($ec){
		$worklg .= " $oldlg" ;
		#print "\nsql=$PacGen::sql:$worklg";
		if ($PacGen::sql and $PacGen::dern){
			#print "\nsql=$worklg=";
			$ec=0;
			print { $new } "\n$worklg" ;
			$worklg = "";
			$newlg="";
		}else{
			if ($worklg =~ /\.$/){
				formater_data ();
				$ec=0;
				print { $new } "\n $newlg." ;
				$worklg = "";
				$newlg="";
			}
		}
	}else{
		$oldlg !~ /^ *$/ and print { $new } "\n $oldlg" ;
	}

	sub calculer_niveau{
#		($xniv, $var, $oldlg) = ($1, $2, $3);
		if ($xniv == "01" or $xniv == "77") {
			$nivth = 0 ;
			$nniv = 1 ;
#			$tniv {$nniv} = $nivth;
			@niv_prec=();
			push (@niv_prec, $nniv) ;
			# iso : $niv_prec [@niv_prec] = $nniv;
			#print "=$xniv:$nniv:$nivth:pr:".$niv_prec [- 1].":\n" ;
		}elsif ($xniv == "88"){
			$nivth = 88 ;
			$nniv = $xniv + 0;
		}else{
			$nniv = $xniv + 0;
			if ($nniv > $niv_prec [@niv_prec - 1] ) {
				++$nivth;
#				$tniv {$nniv} = ++$nivth;
				push (@niv_prec, $nniv) ;
				#print "+$xniv:$nniv:$nivth:pr:".$niv_prec [- 1].":\n" ;
			}else{
				#print "$nniv<pr:[".$#niv_prec."]=".$niv_prec [$#niv_prec].":\n" ;
				while ( $nniv < $niv_prec [$#niv_prec] ){
					pop @niv_prec ;
					--$nivth;
				}
			}
		}
		#print "$xniv:$nniv:$nivth:pr:".$niv_prec [- 1].":\n" ;
#		print "$xniv:$nniv:$nivth=th:".$tniv {$nniv}.":pr:".$niv_prec [- 1]."!" ;
	}

	sub formater_data{
		$nb_lg++ ;
		$worklg =~ s/^\s*(.*)\s*\.$/$1/ ;
		my $sync = ($worklg =~ s/ SYNC(HRONIZED)?// );
		#print "\n$xniv $var:$worklg:";
		my %clause;
		$worklg and %clause = ('DEB', split (/(PIC(?:TURE)?|OCCURS|VALUE|(?:USAGE\s+)?COMP(?:UTATIONAL)?(?![A-Z])|REDEFINES)/, $worklg));
		#print "==%clause 1 $var==\n";
		foreach my $k (keys %clause){
			$clause{$k} =~ s/^\s*(.*?)\s*$/$1/;
			#print "$k=$clause{$k}=\n";
		}
		# positionnement selon niv
		my $yniv ;
#		my $ytab ;
		if ($nivth){
			if ($nivth == 88){
				$yniv = "88" ;
				$newlg = "   $ytab$yniv $var" ;
			}else{
				$yniv = ($nivth) * 5;
				$ytab = "   " x ($nivth - 1);
				$newlg = "    $ytab" ;
				$nivth == 1 and $newlg .= "0";
				$newlg .= "$yniv $var" ;
			}
		}else{
			$yniv = "01" ;
			$ytab = " ";
			$newlg = "$yniv  $var" ;
		}
		# test DEB non renseigne
		if ($clause{'DEB'}) {
			print "\n*** $. $worklg\n\tclause inconnue :$clause{'DEB'}:";
		}
		delete $clause{'DEB'} ;
		exists($clause{'OCCURS'}) and $clause{'OCCURS'} =~ s/^\s+(.*?)(\s+TIMES)?\s*$/$1/;
		exists($clause{'REDEFINES'}) and $newlg .= " REDEFINES ".$clause{'REDEFINES'} and delete $clause{'REDEFINES'};
		exists($clause{'OCCURS'}) and $newlg .= " OCCURS ".$clause{'OCCURS'} and delete $clause{'OCCURS'};
		exists($clause{'PICTURE'}) and $clause{'PIC'} = $clause{'PICTURE'} and delete $clause{'PICTURE'};
		if (exists($clause{'PIC'})){
			$clause{'PIC'} =~ s/\s+/ /gsmiox;
			formater_clause ('PIC', $clause{'PIC'}, 33) ;
			delete $clause{'PIC'};
		}
		exists($clause{'COMPUTATIONAL'}) and $clause{'COMP'} = $clause{'COMPUTATIONAL'} and delete $clause{'COMPUTATIONAL'};
		exists($clause{'COMP'}) and $newlg .= " COMP".$clause{'COMP'} and delete $clause{'COMP'};
		$sync and $newlg .= " SYNC";
		if (exists($clause{'VALUE'})){
			formater_clause ('VALUE', $clause{'VALUE'}, 45) ;
			delete $clause{'VALUE'};
		}
#		print "==%clause 2==\n";
		foreach my $k (keys %clause){
			print "\n*** $. $worklg\n\tclause non traitee : $k=$clause{$k}=";
		}

		sub formater_clause {
			my ($mot, $val, $pos) = @_ ;
			$newlg .= " " ;
			$pos -= 8 ;
			length($newlg) < $pos and $newlg .= " " x ($pos - length($newlg)) ;
			$newlg .= $mot." ".$val ;
		}
	}

}

my $pref = "" ;
my $ligne = "";
sub compacter_troncon {
	# <Dette01> tasser code
	my ($nom, $troncon) = @_ ;
	#print "\n$. A lg=".length ($troncon).":t=$PacGen::type:s=$PacGen::suite:d=$PacGen::dern:l=$PacGen::litteral:$troncon:";
	$troncon =~ /^(\s*)([^ ].*[^ ]|\.)?\s*$/;
	# on conserve l'indentation du 1er troncon
	!$PacGen::suite and $pref = $1;
	#iso	my $trc = $2 // "";
	my $trc = "" ;
	defined ($2) and $trc = $2 or print "\n#undef:$troncon:";

	my $deblg = "$PacGen::type$pref";
	my $max = 66;
	! $PacGen::suite and $ligne = $deblg ;
	#print "\n$. C lg=".length ($ligne).":".length ($trc).":$ligne:$trc:";
	my $reste = "";
	my $nbcar = $max - length ($ligne) ;
	if ($PacGen::litteral){
		my $nbcar2 = $max - length ($deblg) ;
		#print "\n$. D m=$max:n=$nbcar:n2=$nbcar2:trc=".length ($trc).":$trc:";
		if (length ($trc) > $nbcar and length ($trc) < $nbcar2){
			print { $nom } "$ligne\n      " ;
			$ligne = "$deblg$trc" ;
			$reste = "" ;
		}else{
			$ligne .= " " ;
			$nbcar-- ;
			while ($trc){
				if (length ($trc) > $nbcar) {
					$reste = substr($trc, $nbcar) ;
					$trc = substr ($trc, 0 ,$nbcar) ;
				} else {
					$reste = "" ;
				}
				$ligne .= "$trc" ;
				if ($reste) {
					print { $nom } "$ligne\n      " ;
					$ligne = "-$pref$PacGen::sep" ;
					$nbcar = $nbcar2 - 1 ;	# a cause du sep
				}
				#print "\n$. E trc2=".length ($trc).":$trc:";
				#print "\n$. F reste=".length ($reste).":$reste:";
				$trc = $reste ;
			}
		}
		$PacGen::dern and print { $nom } "$ligne\n" ;
	}else{
		#print "\n$. G lg=${ligne}:".length($ligne).":".$nbcar ;
		my @mots = split (/ +/, $trc) ;
		my $cpt = 0 ;
		foreach my $mot (@mots) {
			$mot =~ s/^\s*(\S.*\S)\s*$/$1/ ;
			#print "\n?$. H $cpt ${mot}?".length($mot).">=?".$nbcar ;
			if (length($mot) >= $nbcar) {
				print { $nom } "$ligne\n=      " ;
				$ligne = "$deblg" ;
				$cpt = 0 ;
			}
			$ligne .= " " unless (! $PacGen::suite and ! $cpt or $mot eq ".") ;
			$ligne .= "$mot";
			$nbcar = $max - length ($ligne) ;
			$cpt += 1 ;
		}
		$PacGen::dern and print { $nom } "$ligne\n" ;#
		$trc="";
=pod suspens
=cut
	}
}

1;