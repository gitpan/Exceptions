#!/sig/local/bin/perl5 -w

use ExtUtils::MakeMaker;

WriteMakefile(
# $Format: " VERSION => '$ExceptionsRelease$',"$
 VERSION => '0.002',
              NAME => 'Exceptions',
	      PREREQ_PM => {
			    mods => 0.004,
			    Data::Dumper => 2.07
			   },
#	      EXE_FILES => []
);
