package Exceptions;

#
# Original Id: Exceptions.pm,v 1.2 1996/11/07 00:53:06 seibel Exp
#
# Copyright (c) 1996 Organic Online. All rights reserved. This program is
# free software; you can redistribute it and/or modify it under the same
# terms as Perl itself.

# Modified by Jesse Glick <jglick@sig.bsh.com>.

require 5.004;			# &UNIVERSAL::isa
use mods q{
  # $Format: "  {$\VERSION='$ExceptionsRelease$'}"$
  {$VERSION='0.002'}
  <try!, finally!, except!, with!, otherwise!, $E!>;
  # throw & catch are methods
  # $E is global containing current exception (if any)
};

sub try (&@) {
  my $try=shift;

  # Try the code. 
  eval {
    local($SIG{__DIE__});
    &$try();
  };
  my $err=$@;

  # If there's a finally block it get's done before the exception
  # handler. This is useful for things like:
  #  try {
  #    get_a_lock();
  #    do_something or throw new Foo;
  #  } catch Foo with {
  #    $E->do_whatever;
  #    # but don't worry about the lock, as it's already been released.
  #  } finally {
  #    release_the_lock();
  #  }
  my (@finals, @sequentials);
  my $finalize=sub {
    foreach (@finals) {
      &$_();
    }
  };
  foreach (@_) {
    if (ref($_) eq 'Exceptions::_::finally') {
      push @finals, $_;
    } else {
      push @sequentials, $_;
    }
  }

  # Check for exceptions and do the code block paired with the first type
  # that matches the type of the exception.
  if ($err) {
    my $code_ref; # where we'll store the code ref if we find a match

    # if we're coming from a plain old die there will be no exception
    # object so we'll make a nice low-overhead one.
    $E ||= SimpleException->new($err);
    
    while (@sequentials) {
      my $handler = shift @sequentials;
      my $type=ref $handler;

      unless ($type) {
	# Get the following code ref and if the exception 'is' an
	# instance of the current type (including a subclass of it) we're
	# done.

	my $tmp_code_ref = shift @sequentials
	  or do {&$finalize(); die "Syntax error in try block"};
	ref($tmp_code_ref) eq 'Exceptions::_::with'
	  or do {&$finalize(); die "Syntax error in try block"};

	if ($E->isa($handler)) {
	  $code_ref = $tmp_code_ref;
	  last;
	}

      } else {
	$type eq 'Exceptions::_::otherwise'
	  or do {&$finalize(); die "Syntax error in try block"};
	$code_ref = $handler;
	last;
      }
    }

    # Run the exception handling code if we have any. Otherwise, rethrow
    # the exception
    if ($code_ref) {
      eval {&$code_ref()};
      my $err=$@;
      undef $E;			# Mustn't propagate this now.
      &$finalize();
      die $err if $err;
    } else {
      &$finalize();
      throw $E;
    }

  } else {
    # No error.
    &$finalize();
  }
}

# These just pass their arguments up the chain so try ends up getting
# called with a big list of string => code ref, string => code ref pairs.

my $_OLDUNICATCH;		# File lexical for now.
sub with (&@) {
  # This allows us to catch errors whose classes are not even defined
  # until somewhere in the tried code; e.g. dynamic error classes from
  # HashException's SUBCLASS. Just be sure to use a with {} block:
  # catch NewPotentialClass with {}.
  $_OLDUNICATCH=(defined &UNIVERSAL::catch ? \&UNIVERSAL::catch : 0);
  *UNIVERSAL::catch=\&Exception::catch;
  bless $_[0], 'Exceptions::_::with';
  @_;
}
sub otherwise (&)  {
  bless $_[0], 'Exceptions::_::otherwise';
  @_;
}

# ???
sub except (&@) {
  # here you go, John Redford. except()'s first argument is a code block
  # which should return a hash of exception-type => handler-code-ref
  # pairs.
  my ($block, @rest) = @_;
  my %mapping=&$block();
  foreach (values %mapping) {
    die 'Syntax error in except-block'
      unless ref($_) eq 'CODE' or ref($_) eq 'Exceptions::_::with';
    bless $_, 'Exceptions::_::with';
  }
  %mapping, @rest;
}

sub finally (&@) {
  bless $_[0], 'Exceptions::_::finally';
  @_;
}

package Exception;
use mods q{
  @CALLER;
};
use Class::MethodMaker
  new_with_init => 'new',
  new_hash_init => '_hash_init',
  fields => {
	     group => 'fields',
	     slots => [qw(msg error_no pkg file line)],
	    };

sub init {
  my ($self, @args) = @_;
  $self->_hash_init(@args,
		    'pkg' => $CALLER[0][0],
		    'file' => $CALLER[0][1],
		    'line' => $CALLER[0][2]
		   );
}

# default catch method. Nobody should override this unless they
# really, really know what they are about.

sub catch {
  goto &catch_;
}

sub catch_ {
  if ($_OLDUNICATCH) {
    *UNIVERSAL::catch=$_OLDUNICATCH;
  } else {
    undef &UNIVERSAL::catch;
  }
  @_;
}

sub throw () {
  my ($class_or_self, @args) = @_;
  my $class;

  local(@CALLER);
  my ($i, $frame)=(0);
  push @CALLER, $frame while @{$frame=[caller $i++]};
  
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

*raise=*raise=\&throw; # for Modula-3 style exception handling.

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
use mods q{
  [Exception];
};

sub new {
  my ($class, $string) = @_;
  bless \$string, $class;
}

sub dump {
  my ($self) = @_;
  $$self;
}

package HashException;
use mods q{
  [Exception];
};

# Usage:
# throw HashException somekey => $someval, someotherkey => \%someotherval;
# try {
#   throw HashSysException SUBCLASS => Open, file => $file;
# } catch HashSysException::System {
#   print "Got me; $E->{ERRSTR}! $E->{file}\n";
# }

sub new {
  my ($class, %hash)=@_;
  @hash{qw(PKG FILE LINE)}=@{$Exception::CALLER[0]}[0..2];
  $hash{SUB}=$Exception::CALLER[1][3] if $Exception::CALLER[1][3]; # Sub of caller.
  # Skip hasargs & wantarray. The next two may not work as advertised...
  $hash{EVAL}=$Exception::CALLER[0][6] if $Exception::CALLER[0][6];
  $hash{REQUIRE}=$Exception::CALLER[0][7] if $Exception::CALLER[0][7];
  if (exists $hash{SUBCLASS}) {
    my $base=$class;
    $class .= '::' . delete $hash{SUBCLASS};
    no strict qw(refs);
    @{$class . '::ISA'}=($base);
  }
  bless \%hash, $class;
}

sub dump {
  my ($self)=@_;
  eval {require Data::Dumper};
  if ($@) {
    # Untested...
    my $msg="[Get Data::Dumper for full structure.]\n";
    $msg .= 'Class: ' . ref($self) . "\n";
    my ($key, $val); while (($key, $val)=each %$self) {
      $msg .= "$key: $val\n";	# Munges rich types.
    }
    $msg;
  } else {
    1;
    my $d=new Data::Dumper [$self], [qw(*EXCEPTION)];
    $d->Indent(2);
    $d->Useqq(1);
    $d->Dump;
  }
}

package HashSysException;
use mods q{
  [HashException];
};

sub new {
  my ($class, @args)=@_;
  my $self=$class->SUPER::new(@args);
  if ($!) {
    $self->{ERRNO}=0+$!;
    $self->{ERRSTR}="$!";
  }
  $self;
}

1;
__END__

=head1 NAME

B<Exceptions> - handle exceptions in a standard OO-ish way

=head1 SYNOPSIS

 use Exceptions;
 try {
   do_some_stuff();
   die "error!" if $condition;
   throw Exception msg => "Oops!" if $other_condition;
 } catch FileException with {
   print STDERR "File $E->{file} had a problem\n";
 } catch IO::Exception with {
   print STDERR $E->dump;	# Structure dump of error
 } except {
   my $general_handler=sub {send_message $E->{description}};
   UserException1 => $general_handler, UserException2 => $general_handler;
 } otherwise {
   print STDERR "Well I don't know what to say\n";
 } finally {
   close_the_garage_door_already(); # Should be reliable
 }

Throw or catch a "prototype-mode" exception:

 try {
   throw HashException SUBCLASS => 'FeltLikeIt', specific => $variable;
   opendir(...) or throw HashSysException SUBCLASS => 'OpenDir';
 } catch HashSysException with {
   print STDERR "Failed: $E->{ERRSTR}\n";
 } catch HashException::FeltLikeIt with {
   print STDERR "Well I didn't!\n";
 }

=head1 DESCRIPTION

I<This module is still in an experimental stage.> You know the basic routine: code in
the B<try>-block is executed, trapping errors; errors may be B<throw>n inside the block
(or any function it calls, of course), and they will be caught or not by
B<catch>-blocks according to the Perl inheritance hierarchy; B<otherwise>-blocks handle
all exceptions not otherwise caught; B<except>-blocks should return a hash from class
names to subroutine handlers, effectively creating part of the
B<catch>-I<class>-B<with>-I<block> chain dynamically; B<finally>-blocks are always
supposed to be run no matter what, and hopefully they in fact do (they run after
everything else, even if other blocks signal new errors).

=head1 EXCEPTION CLASSES

Exception objects can take any form. They need to be have a constructor which will be
called normally from B<throw>. They may implement B<dump> (create a string
representation) or B<print> (print out some representation, currently unused).

=head2 B<Exception>

The basic class. Takes optional B<msg> and B<error_no> initializers, filling in some
basic context information. Other exception classes should prefereably inherit from
this, even if every method is overridden.

=head2 B<SimpleException>

"Thrown" by the B<die> primitive. Just holds a message. Do not use yourself.

=head2 B<HashException>

Convenient superclass for prototyping purposes, before you have a rigidly-worked-out
exception hierarchy (if you ever do). Takes any key-value initializers, and can display
them. (B<Data::Dumper> is preferred for this.) Prepopulates some useful info from the
call stack for you. The special key B<SUBCLASS> causes the actual exception-class
thrown to be the named subclass (and subpackage) of the master throwing class (here,
B<HashException>, but also its children). B<catch> can handle catching such a
"prototype" class, even if it has never been defined or thrown before. Obviously this
is not great for long-term organization but it can tide you over.

=head2 B<HashSysException>

Just like B<HashException>, but includes info from C<$!>, so it is useful for throwing
exceptions related to system-level operations.

=head1 BUGS

There probably are some.

=head1 AUTHORS

Peter Seibel <peter@weblogic.com>, originally. Adopted by Jesse Glick
<jglick@sig.bsh.com>.

=head1 REVISION

X<$Format: "F<$Source$> last modified $Date$ release $ExceptionsRelease$. $Copyright$"$>
F<Exceptions/lib/Exceptions.pm> last modified Sat, 20 Sep 1997 19:46:47 -0400 release 0.002. Copyright (c) 1997 Strategic Interactive Group. All rights reserved. This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut
