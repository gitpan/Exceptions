package Exceptions;

#
# $Id: Exceptions.pm,v 1.2 1996/11/07 00:53:06 seibel Exp $
#

# Copyright (c) 1996 Organic Online. All rights reserved. This program is
# free software; you can redistribute it and/or modify it under the same
# terms as Perl itself.

use strict;
#use lib '/home/seibel/projects/perl/lib';
use Carp;

use Exporter;
use vars  qw ( @ISA @EXPORT );
@ISA    = qw ( Exporter );
@EXPORT = qw ( try finally except with otherwise $E ); # throw and catch
                                                       # are methods in
                                                       # the Exception
                                                       # class

use vars '$E'; # semi-global variable which will holds exceptions when
               # they're thrown.

sub try (&@) {
  my($try, @catchers) = @_;

  # Try the code. 
  eval {
    local($SIG{'__DIE__'});
    &$try()
  };

  # If there's a finally block it get's done before the exception
  # handler. This is useful for things like:
  #  try {
  #    get_a_lock();
  #    do_something or throw new Foo;
  #  } finally {
  #    release_the_lock();
  #  } catch 'Foo', {
  #    $E->do_whatever;
  #    # but don't worry about the lock, as it's already been released.
  #  }
  if (ref $catchers[0] eq 'FINALBLOCK' ) {
    my $finally = shift @catchers;
    &$finally();
  }

  # Check for exceptions and do the code block paired with the first type
  # that matches the type of the exception.
  if ($@) {
    my $code_ref; # where we'll store the code ref if we find a match

    # if we're coming from a plain old die there will be no exception
    # object so we'll make a nice low-overhead one.
    $E ||= new SimpleException $@;
    
    while (1) {
      my $type = shift @catchers or last;

      unless (ref $type) {
	# Get the following code ref and if the exception 'is' an
	# instance of the current type (including a subclass of it) we're
	# done.

	my $tmp_code_ref = shift @catchers;

	if ($E->isa($type)) {
	  $code_ref = $tmp_code_ref;
	  last;
	}

      } else {
      	# The type had better be a code ref. If so, it's the
      	# optional 'otherwise' case; if not, something's broken.
	ref $type eq 'CODE' or die "Syntax error in try block";
	$code_ref = $type;
	last;
      }
    }

    # Run the exception handling code if we have any. Otherwise, rethrow
    # the exception
    $code_ref ? &$code_ref() : throw $E;
  }

  # Now unset the semi-global exception. Note, if it was thrown above it
  # will remain set.
  $E = undef;
}

# These just pass their arguments up the chain so try ends up getting
# called with a big list of string => code ref, string => code ref pairs.
sub with      (&@) { @_; }
sub otherwise (&)  { @_; }

sub except    (&@) {
  # here you go, John Redford. except()'s first argument is a code block
  # which should return a hash of exception-type => handler-code-ref
  # pairs.
  my ($block, @rest) = @_;
  my @list = &$block(), @rest;
}

sub finally   (&@) {
  bless $_[0], 'FINALBLOCK';
  @_;
}

#sub keep_perl_mode_happy {};

package Exception;
$INC{'Exception.pm'}++;

use Class::MethodMaker
  new_with_init => 'new',
  new_hash_init => '_hash_init',
  fields => {
	     group => 'fields',
	     slots => [ qw / msg error_no pkg file line / ],
	    };

sub init {
  my ($self, @args) = @_;

  # XXX is this always right?
  # 0 is init()
  # 1 is new() (actually Class::MethodMaker::__ANON__)
  # 2 is throw()
  my ($pkg, $file, $line) = caller(3);

  $self->_hash_init(@args,
		    'pkg' => $pkg,
		    'file' => $file,
		    'line' => $line
		   );
}

sub catch { @_ }; # default catch method. Nobody should override this
                  # unless they really, really know what they are about.


sub throw () {
  my ($class_or_self, @args) = @_;
  my $class;

  # Store the exception in our semi-global var, constructing it if
  # neccessary.
  if ($class = ref $class_or_self) {
    $Exceptions::E = $class_or_self;
  } else {
    $class = $class_or_self;
    $Exceptions::E = $class->new(@args);
  }

  # And die. (Hopefully someone will catch us but if not the program will
  # die with some sort of useful error message.)
  my $dump = $Exceptions::E->dump;

  die "Uncaught exception ($class):\n$dump";
}

*raise = \&throw; # for Modula-3 style exception handling.

sub dump {
  my ($self) = @_;
  my $dump = "";
  foreach ($self->fields) {
    $self->$_() and 
      $dump .= sprintf("%9s: %s\n", $_, $self->$_());
  }
  $dump;
}

sub print {
  my ($self) = @_;
  print $self->dump;
}

package SimpleException;
$INC{'SimpleException.pm'}++;

@SimpleException::ISA = qw ( Exception );

sub new {
  my ($class, $string) = @_;
  bless \$string, $class;
}

sub dump {
  my ($self) = @_;
  $$self;
}
  
1;

__END__



