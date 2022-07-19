package PacProg ;
use strict;
use warnings;

sub new {
	my ($classe, $src, $typo) = @_ ;
	$classe = ref($classe) || $classe;
	my $self = {};
	$self->{_TYPO} = $typo ;
	#print "\nProg".$self->{_TYPO};
	bless ($self, $classe);

	charger_prog ($self, $src);
	return $self;
}

sub charger_prog{
	my ($self, $src) = @_ ;
	$self->{_SRC} = do { local $/ ; <$src> };
	return 0 ;
}

my @excbatch = qw ( 0-FIN
			1-TRAITER-ENREG 1-FN
			901-LECTURE-ENREG 901-FN
			902-TRAITER-RUPTURE 902-FN
			F95-WORK);
my @excTP = qw ( 0-FIN
			1-TRAITER-ENTREE 1-FIN
			2-TRAITER-SORTIE 2-FIN);
sub estUtilise{
	my ($self, $parag) = @_ ;
	#print "\nself:$self";
	#print "\nparag:$parag";
	# <Dette01> GO TO ... DEPENDING ON (RSB091)
	$self->{_TYPO} eq "TP" and $parag =~ m/^F8/ and return 1;
	$self->{_TYPO} eq "batch" and $parag =~ m/^F9[59]/ and return 1;
	if ($self->{_SRC} =~ m/(GO\s*TO|PERFORM|THRU)\s+$parag\b(?!\-)/){
		return 1 ;
	}
	if ($self->{_SRC} =~ m/GO\s*TO[^\.]*\s+$parag\b(?!\-)[^\.]*DEPENDING/){
		return 1 ;
	}

	if ($self->{_TYPO} eq "batch"){
		foreach my $exc (@excbatch){
			$parag eq "$exc" and return 1 ;
		}
	}
	if ($self->{_TYPO} eq "TP"){
		foreach my $exc (@excTP){
			$parag eq "$exc" and return 1 ;
		}
	}
	return 0 ;
}

1;
__END__