#!/usr/bin/perl -w

# migration du patrimoine Pacbase vers du cobol propre
# pacbatch
# 1- formatter ligne
# 2- inserer copies pac std
# 3- determiner si base
# 4- inserer END-IF
# 7- structurer code niveau 0
# 5- supprimer points et noms parag inutile
# 6- supprimer NEXT SENTENCE ELSE GO TO
# ...
# 9- indenter lignes
#
# backlog
# DP : dictionnaire
# PacTP
# 

use warnings;
use strict;
use PacGen;
use PacBatch;
use PacTP;
use PacLigne;

my $fic = shift @ARGV;
my $pacDir="/app/Dia/rs/src/batch";
my $nom_prog="./RSA011.pco";
-e $fic and $nom_prog = $fic or die "source inconnu : $fic";#and print ">$nom_prog\n" ;

$nom_prog =~ s/^.*\///;
$nom_prog =~ /(.*)\..*$/;
my $nom = $1 ;
print "analyse de $nom\n" ;
my $d = 0;
my $p = 0;

my $old_pgm,
my $old_data;
my $old_proc;
my $new_data;
my $new_proc;
print "\n$d-extraction lignes de $nom : ";
open $old_pgm, "<", "$nom_prog" or die "ouverture de $nom_prog impossible : $!";
open $new_data, '>', "$nom.tmpD$d" or die "ouverture de $nom.tmpD$d impossible : $!";
open $new_proc, '>', "$nom.tmpP$d" or die "ouverture de $nom.tmpP$d impossible : $!";
my ($n_lg, $appli, $typo, $nb_lg) = PacLigne::decoboliser_ligne ($old_pgm, $new_data, $new_proc);
print "$n_lg lg ($appli,$typo) $nb_lg continuations\n";
close($old_pgm);
close($new_data);
close($new_proc);

open $old_data, "<", "$nom.tmpD$d" or die "ouverture de $nom.tmpD$d impossible : $!";
open $new_data, '>', "$nom.tmpD".++$d or die "ouverture de $nom.tmpD.$d impossible : $!";
print "\n$d-transformation data ($typo)\n";
if ($typo eq "batch"){
	PacBatch::transformer_data ($old_data, $new_data);
}else{
	PacTP::transformer_data ($old_data, $new_data);
}
close($old_data);
close($new_data);

open $old_data, "<", "$nom.tmpD$d" or die "ouverture de $nom.tmpD$d impossible : $!";
open $new_data, '>', "$nom.tmpD".++$d or die "ouverture de $nom.tmpD.$d impossible : $!";
print "\n$d-compactage data:";
PacGen::parcourir_source ($old_data, $new_data, \&PacLigne::compacter_data);
print $PacLigne::nb_lg."\n" ;
close($old_data);
close($new_data);

open $new_proc, "<", "$nom.tmpP$p" or die "ouverture de $nom.tmpP$p impossible : $!";
print "\ncharger programme:";
PacGen::charger_programme($new_proc, $typo);
print $PacGen::nb_lg."\n" ;

open $old_proc, "<", "$nom.tmpP$p" or die "ouverture de $nom.tmpP$p impossible : $!";
open $new_proc, '>', "$nom.tmpP".++$p or die "ouverture de $nom.tmpP.$p impossible : $!";
print "\n$p-transformation proc ($typo)\n";
if ($typo eq "batch"){
	PacBatch::transformer_proc ($old_proc, $new_proc);
}else{
	PacTP::transformer_proc ($old_proc, $new_proc, $nom);
}
close($old_proc);
close($new_proc);

decaler_fichier ();
print "\n$p-decouper point:";
PacGen::parcourir_source ($old_proc, $new_proc, \&PacGen::decouper_point);
print $PacGen::nb_lg."\n" ;

decaler_fichier ();
print "\n$p-inserer END-IF:";
PacGen::parcourir_source ($old_proc, $new_proc, \&PacGen::inserer_ENDIF);
print $PacGen::nb_lg."\n" ;

if ($typo eq "batch"){
	decaler_fichier ();
	print "\n$p-inserer END-READ:";
	PacGen::parcourir_source ($old_proc, $new_proc, \&PacGen::inserer_ENDREAD);
	print $PacGen::nb_lg."\n" ;
}

decaler_fichier ();
print "\n$p-structurer procedure ($typo)\n";
if ($typo eq "batch"){
	PacBatch::structurer_procedure ($old_proc, $new_proc) ;
}else{
	PacTP::structurer_procedure ($old_proc, $new_proc);
}

decaler_fichier ();
print "\n$p-supprimer point et parag:";
PacGen::supprimer_point_parag ($old_proc, $new_proc) ;
print $PacGen::nb_lg."+".$PacGen::nb_prg."\n" ;

decaler_fichier ();
print "\n$p-supprimer NEXT SENTENCE:";
PacGen::supprimer_next_sentence ($old_proc, $new_proc) ;
print $PacGen::nb_lg."\n" ;

=pod todo
decaler_fichier ();
print "\n$p-supprimer point et parag:";
PacGen::supprimer_point_parag ($old_proc, $new_proc) ;
print $PacGen::nb_lg."\n" ;

decaler_fichier ();
print "\n$p-supprimer NEXT SENTENCE:";
PacGen::supprimer_next_sentence ($old_proc, $new_proc) ;
print $PacGen::nb_lg."\n" ;
=cut

decaler_fichier ();
print "\n$p-formatter IF:";
PacGen::parcourir_source ($old_proc, $new_proc, \&PacGen::formater_IF);
print $PacGen::nb_lg."\n" ;

close($old_proc);
close($new_proc);

print "\nformatage donnees $nom.tmpD$d dans $nom.coob:";
open $old_data, "<", "$nom.tmpD$d" or die "ouverture de $nom.tmpD$d impossible : $!";
open my $new, '>', "$nom.coob" or die "ouverture de $nom.coob impossible : $!";
PacLigne::recoboliser_ligne ($old_data, $new);
print $PacLigne::nb_lg."\n" ;
close($old_data);
close($new);

close($old_proc);
print "\nformatage procedure $nom.tmpP$p dans $nom.coob:";
open $old_proc, "<", "$nom.tmpP$p" or die "ouverture de $nom.tmpP$p impossible : $!";
open $new, '>>', "$nom.coob" or die "ouverture de $nom.coob impossible : $!";
PacLigne::recoboliser_ligne ($old_proc, $new);
print $PacLigne::nb_lg."\n" ;
close($old_proc);
close($new);

sub decaler_fichier {
#	close($old_proc);
#	close($new_proc);

	open $old_proc, "<", "$nom.tmpP$p" or die "ouverture de $nom.tmpP$p impossible : $!";
	open $new_proc, '>', "$nom.tmpP".++$p or die "ouverture de $nom.tmpP.$p impossible : $!";
}