package Inline::ASM;

use strict;
require Inline;
use Config;
use Data::Dumper;
use FindBin;
use Carp;
use Cwd qw(cwd abs_path);

$Inline::ASM::VERSION = '0.02';
@Inline::ASM::ISA = qw(Inline);

#==============================================================================
# Register this module as an Inline language support module
#==============================================================================
sub register {
    my $suffix = ($^O eq 'aix') ? 'so' : $Config{so};
    return {
	    language => 'ASM',
	    aliases => ['nasm', 'NASM', 'gasp', 'GASP', 'as', 'AS', 'asm'],
	    type => 'compiled',
	    suffix => $suffix,
	   };
}

#==============================================================================
# Validate the Assembler config options
#==============================================================================
sub usage_validate {
    my $key = shift;
    return <<END;
The value of config option '$key' must be a string or an array ref

END
}

sub validate {
    my $o = shift;

    $o->{ASM} = {};
    $o->{ASM}{XS} = {};
    $o->{ASM}{MAKEFILE} = {};

    # These are written at configuration time
    $o->{ASM}{AS} ||= '@ASSEMBLER';          # default assembler
    $o->{ASM}{ASFLAGS} ||= '@ASFLAGS';       # default asm flags
    $o->{ASM}{MAKEFILE}{CC} ||= '@COMPILER'; # default compiler
    $o->{ASM}{MAKEFILE}{LD} ||= '@LINKER';   # default linker

    $o->{ASM}{AUTO_INCLUDE} ||= <<END;
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
END

    while (@_) {
	my ($key, $value) = (shift, shift);
        if ($key eq 'MAKE') {
            $o->{ASM}{$key} = $value;
            next;
        }
	if ($key eq 'CC' or
	    $key eq 'LD') {
	    $o->{ASM}{MAKEFILE}{$key} = $value;
	    next;
	}
	if ($key eq 'LIBS') {
	    add_list($o->{ASM}{MAKEFILE}, $key, $value, []);
	    next;
	}
	if ($key eq 'AS' or
	    $key eq 'ASFLAGS') {
	    $o->{ASM}{$key} = $value;
	    next;
	}
	if ($key eq 'INC' or
            $key eq 'MYEXTLIB' or
	    $key eq 'LDFLAGS' or 
            $key eq 'LDDLFLAGS' or
            $key eq 'CCFLAGS') {
	    add_string($o->{ASM}{MAKEFILE}, $key, $value, '');
	    next;
	}
	if ($key eq 'TYPEMAPS') {
	    croak "TYPEMAPS file '$value' not found"
	      unless -f $value;
	    my ($path, $file) = ($value =~ m|^(.*)[/\\](.*)$|) ?
	      ($1, $2) : ('.', $value);
	    $value = abs_path($path) . '/' . $file;
	    add_list($o->{ASM}{MAKEFILE}, $key, $value, []);
	    next;
	}
	if ($key eq 'AUTO_INCLUDE') {
	    add_text($o->{ASM}, $key, $value, '');
	    next;
	}
	if ($key eq 'BOOT') {
	    add_text($o->{ASM}{XS}, $key, $value, '');
	    next;
	}
	if ($key eq 'PREFIX') {
	    croak "Invalid value for 'PREFIX' option"
	      unless ($value =~ /^\w*$/ and
		      $value !~ /\n/);
	    $o->{ASM}{XS}{PREFIX} = $value;
	    next;
	}
	if ($key eq 'PROTOTYPES' or
	    $key eq 'PROTO') {
	    croak "Invalid value for '$key' option"
	      unless ref $value eq 'HASH';
	    $o->{ASM}{PROTOTYPES} = $value;
	    next;
	}
	croak "'$key' is not a valid config option for Inline::C\n";
    }
}

sub add_list {
    my ($ref, $key, $value, $default) = @_;
    $value = [$value] unless ref $value;
    croak usage_validate($key) unless ref($value) eq 'ARRAY';
    for (@$value) {
	if (defined $_) {
	    push @{$ref->{$key}}, $_;
	}
	else {
	    $ref->{$key} = $default;
	}
    }
}

sub add_string {
    my ($ref, $key, $value, $default) = @_;
    $value = [$value] unless ref $value;
    croak usage_validate($key) unless ref($value) eq 'ARRAY';
    for (@$value) {
	if (defined $_) {
	    $ref->{$key} .= ' ' . $_;
	}
	else {
	    $ref->{$key} = $default;
	}
    }
}

sub add_text {
    my ($ref, $key, $value, $default) = @_;
    $value = [$value] unless ref $value;
    croak usage_validate($key) unless ref($value) eq 'ARRAY';
    for (@$value) {
	if (defined $_) {
	    chomp;
	    $ref->{$key} .= $_ . "\n";
	}
	else {
	    $ref->{$key} = $default;
	}
    }
}

#==============================================================================
# Parse and compile code
#==============================================================================
sub build {
    my $o = shift;
    $o->parse;
    $o->write_XS;
    $o->write_ASM;
    $o->write_Makefile_PL;
    $o->compile;
}

#==============================================================================
# Return a small report about the ASM code.
#==============================================================================
sub info {
    my $o = shift;
    my $text = '';

    $o->parse unless $o->{parser};

    my $sym;
    if (defined $o->{parser}) {
	my $num_bound = scalar keys %{$o->{parser}{bound}};
	my $num_unbound = scalar keys %{$o->{parser}{unbound}};
	my $num_missing = scalar keys %{$o->{parser}{missing}};
	if ($num_bound) {
	    $text .= "The following symbols have been bound to Perl:\n";
	    for $sym (keys %{$o->{parser}{bound}}) {
		my ($rtype, $args) = $o->{ASM}{PROTOTYPES}{$sym}
		  =~ m!([^\(]+)(\([^\)]*\))!g;
		$text .= "\t$rtype $sym $args\n";
	    }
	}
	if ($num_unbound) {
	    $text .= "The following unprototyped symbols were ignored:\n";
	    for $sym (keys %{$o->{parser}{unbound}}) { $text .= "\t$sym\n"; }
	}
	if ($num_missing) {
	    $text .= "The following prototyped symbols were missing:\n";
	    for $sym (keys %{$o->{parser}{missing}}) { $text .= "\t$sym\n"; }
	}
    }
    else {
	$text .= "No $o->{language} functions have been successfully bound to Perl.\n\n";
    }
    return $text;
}

sub config {
    my $o = shift;
}

#==============================================================================
# Parse the function definition information out of the ASM code
#==============================================================================
sub parse {
    my $o = shift;
    return if $o->{parser};
    $o->get_maps;
    $o->get_types;

    my $globals = $o->global_keys;

    # Extract the GLOBAL and COMMON symbols:
    my @symbols = ($o->{code} =~ m!^\s*(?:$globals)\s+(\w+)!mig);

    my %bound;
    my %unbound;
    my %missing;
    my $sym;

    for $sym (@symbols) {
	$bound{$sym}++ if $o->{ASM}{PROTOTYPES}{$sym};
	$unbound{$sym}++ unless $o->{ASM}{PROTOTYPES}{$sym};
    }
    for $sym (keys %{$o->{ASM}{PROTOTYPES}}) {
	$missing{$sym}++ unless $bound{$sym};
    }

    $o->{parser} = {bound => \%bound,
		    unbound => \%unbound,
		    missing => \%missing,
		   };

}

#==============================================================================
# Gather the path names of all applicable typemap files.
#==============================================================================
sub get_maps {
    my $o = shift;
    unshift @{$o->{ASM}{MAKEFILE}{TYPEMAPS}}, "$Config::Config{installprivlib}/ExtUtils/typemap";
    if (-f "$FindBin::Bin/typemap") {
	push @{$o->{ASM}{MAKEFILE}{TYPEMAPS}}, "$FindBin::Bin/typemap";
    }
}

#==============================================================================
# This routine parses XS typemap files to get a list of valid types to create
# bindings to. This code is mostly hacked out of Larry Wall's xsubpp program.
#==============================================================================
sub get_types {
    my (%type_kind, %proto_letter, %input_expr, %output_expr);
    my $o = shift;

    my $proto_re = "[" . quotemeta('\$%&*@;') . "]";
    foreach my $typemap (@{$o->{ASM}{MAKEFILE}{TYPEMAPS}}) {
	next unless -e $typemap;
	# skip directories, binary files etc.
	warn("Warning: ignoring non-text typemap file '$typemap'\n"), next 
	  unless -T $typemap;
	open(TYPEMAP, $typemap) 
	  or warn ("Warning: could not open typemap file '$typemap': $!\n"), next;
	my $mode = 'Typemap';
	my $junk = "";
	my $current = \$junk;
	while (<TYPEMAP>) {
	    next if /^\s*\#/;
	    my $line_no = $. + 1; 
	    if (/^INPUT\s*$/)   {$mode = 'Input';   $current = \$junk;  next}
	    if (/^OUTPUT\s*$/)  {$mode = 'Output';  $current = \$junk;  next}
	    if (/^TYPEMAP\s*$/) {$mode = 'Typemap'; $current = \$junk;  next}
	    if ($mode eq 'Typemap') {
		chomp;
		my $line = $_;
		TrimWhitespace($_);
		# skip blank lines and comment lines
		next if /^$/ or /^\#/;
		my ($type,$kind, $proto) = 
		  /^\s*(.*?\S)\s+(\S+)\s*($proto_re*)\s*$/ or
		    warn("Warning: File '$typemap' Line $. '$line' TYPEMAP entry needs 2 or 3 columns\n"), next;
		$type = TidyType($type);
		$type_kind{$type} = $kind;
		# prototype defaults to '$'
		$proto = "\$" unless $proto;
		warn("Warning: File '$typemap' Line $. '$line' Invalid prototype '$proto'\n") 
		  unless ValidProtoString($proto);
		$proto_letter{$type} = C_string($proto);
	    }
	    elsif (/^\s/) {
		$$current .= $_;
	    }
	    elsif ($mode eq 'Input') {
		s/\s+$//;
		$input_expr{$_} = '';
		$current = \$input_expr{$_};
	    }
	    else {
		s/\s+$//;
		$output_expr{$_} = '';
		$current = \$output_expr{$_};
	    }
	}
	close(TYPEMAP);
    }

    %Inline::ASM::valid_types = 
      map {($_, 1)}
    grep {defined $input_expr{$type_kind{$_}}}
    keys %type_kind;

    %Inline::ASM::valid_rtypes = 
      map {($_, 1)}
    grep {defined $output_expr{$type_kind{$_}}}
    keys %type_kind;
    $Inline::ASM::valid_rtypes{void} = 1;
}

sub ValidProtoString ($) {
    my $string = shift;
    my $proto_re = "[" . quotemeta('\$%&*@;') . "]";
    return ($string =~ /^$proto_re+$/) ? $string : 0;
}

sub TrimWhitespace {
    $_[0] =~ s/^\s+|\s+$//go;
}

sub TidyType {
    local $_ = shift;
    s|\s*(\*+)\s*|$1|g;
    s|(\*+)| $1 |g;
    s|\s+| |g;
    TrimWhitespace($_);
    $_;
}

sub C_string ($) {
    (my $string = shift) =~ s|\\|\\\\|g;
    $string;
}

#==============================================================================
# Write the ASM code
#==============================================================================
sub write_ASM {
    my $o = shift;
    open ASM, "> $o->{build_dir}/$o->{modfname}_asm.asm"
      or croak "Inline::ASM::write_ASM: $!";
    print ASM $o->{code};
    close ASM;
}

#==============================================================================
# Generate the XS glue code
#==============================================================================
sub write_XS {
    my $o = shift;
    my ($pkg, $module, $modfname) = @{$o}{qw(pkg module modfname)};
    my $prefix = (($o->{ASM}{XS}{PREFIX}) ?
		  "PREFIX = $o->{ASM}{XS}{PREFIX}" :
		  '');
		  
    $o->mkpath($o->{build_dir});
    open XS, "> $o->{build_dir}/$modfname.xs"
      or croak "Inline::ASM::write_XS: $!";

    print XS <<END;
$o->{ASM}{AUTO_INCLUDE}
END

    for my $sym (keys %{$o->{parser}{bound}}) {
	my ($rtype, $args) = $o->{ASM}{PROTOTYPES}{$sym}
	  =~ m!([^\(]+)(\([^\)]*\))!g;
	print XS "extern $rtype $sym $args;\n";
    }

    print XS <<END;

MODULE = $module	PACKAGE = $pkg	$prefix

PROTOTYPES: DISABLE
END

    warn("Warning. No Inline ASM functions bound to Perl\n" .
         "Check your PROTO option(s) for Inline compatibility\n\n")
      if ((not scalar keys %{$o->{parser}{bound}}) and ($^W));

    my $parm = "neil";
    for my $function (keys %{$o->{parser}{bound}}) {
	my ($rtype, $args) = $o->{ASM}{PROTOTYPES}{$function}
	  =~ m!([^\(]+)(\([^\)]*\))!g;

	$args =~ s/\(([^\)]*)\)/$1/;
	my @arg_types = split/\s*,\s*/, $args;
	my @arg_names = map { $parm++ } @arg_types;

	print XS ("\n$rtype\n$function (", 
		  join(', ', @arg_names), ")\n");

	for my $arg_name (@arg_names) {
	    my $arg_type = shift @arg_types;
	    last if $arg_type eq '...';
	    print XS "\t$arg_type\t$arg_name\n";
	}

	my $listargs = '';
	my $arg_name_list = join(', ', @arg_names);

	if ($rtype eq 'void') {
	    print XS <<END;
	PREINIT:
	I32* temp;
	PPCODE:
	temp = PL_markstack_ptr++;
	$function($arg_name_list);
	if (PL_markstack_ptr != temp) {
          /* truly void, because dXSARGS not invoked */
	  PL_markstack_ptr = temp;
	  XSRETURN_EMPTY; /* return empty stack */
        }
        /* must have used dXSARGS; list context implied */
	return; /* assume stack size is correct */
END
	}
    }
    print XS "\n";

    if (defined $o->{ASM}{XS}{BOOT} and
	$o->{ASM}{XS}{BOOT}) {
	print XS <<END;
BOOT:
$o->{ASM}{XS}{BOOT}
END
    }

    close XS;
}

#==============================================================================
# Generate the Makefile.PL
#==============================================================================
sub write_Makefile_PL {
    my $o = shift;

    $o->{xsubppargs} = '';
    for (@{$o->{ASM}{MAKEFILE}{TYPEMAPS}}) {
	$o->{xsubppargs} .= "-typemap $_ ";
    }

    my %options = (
		   VERSION => '0.00',
		   %{$o->{ASM}{MAKEFILE}},
		   NAME => $o->{module},
		   OBJECT => qq{\$(BASEEXT)\$(OBJ_EXT) $o->{modfname}_asm\$(OBJ_EXT)},
		  );

    open MF, "> $o->{build_dir}/Makefile.PL"
      or croak "Inline::ASM::write_Makefile_PL: $!\n";

    print MF <<END;
use ExtUtils::MakeMaker;
my %options = %\{
END

    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 1;
    print MF Data::Dumper::Dumper(\ %options);

    my $asmcmd;
    # This neato little hack notices that GASP is being used, and substitutes
    # 'gasp' for 'gasp <filename.asm> | as -o <filename.o>'
    if ($o->{ASM}{AS} =~ /^\s*gasp/) {
        $asmcmd = $o->{ASM}{AS};
        $asmcmd =~ s|gasp|gasp $o->{modfname}_asm.asm|;
        $asmcmd .= "| as $o->{ASM}{ASFLAGS} -o $o->{modfname}_asm\$(OBJ_EXT)";
    }
    else {
        $asmcmd = "$o->{ASM}{AS} $o->{ASM}{ASFLAGS} $o->{modfname}_asm.asm ";
        $asmcmd .= "-o $o->{modfname}_asm\$(OBJ_EXT)";
    }

    print MF <<END;
\};
WriteMakefile(\%options);

sub MY::postamble {
  <<'FOO';
$o->{modfname}_asm\$(OBJ_EXT) : $o->{modfname}_asm.asm
	$asmcmd
FOO
}

END
    close MF;
}

#==============================================================================
# Run the build process.
#==============================================================================
sub compile {
    my ($o, $perl, $make, $cmd, $cwd);
    $o = shift;
    my ($module, $modpname, $modfname, $build_dir, $install_lib) = 
      @{$o}{qw(module modpname modfname build_dir install_lib)};

    -f ($perl = $Config::Config{perlpath})
      or croak "Can't locate your perl binary";
    ($make = $Config::Config{make})
      or croak "Can't locate your make binary";
    $cwd = &cwd;
    for $cmd ("$perl Makefile.PL > out.Makefile_PL 2>&1",
	      \ &fix_make,   # Fix Makefile problems
	      "$make > out.make 2>&1",
	      "$make install > out.make_install 2>&1",
	     ) {
	if (ref $cmd) {
	    $o->$cmd();
	}
	else {
	    chdir $build_dir;
	    system($cmd) and do {
		$o->error_copy;
		croak <<END;

A problem was encountered while attempting to compile and install your Inline
$o->{language} code. The command that failed was:
  $cmd

The build directory was:
$build_dir

To debug the problem, cd to the build directory, and inspect the output files.

END
	    };
	    chdir $cwd;
	}
    }

    if ($o->{config}{CLEAN_AFTER_BUILD} and 
	not $o->{config}{REPORTBUG}
       ) {
	$o->rmpath($o->{config}{DIRECTORY} . 'build/', $modpname);
	unlink "$install_lib/auto/$modpname/.packlist";
	unlink "$install_lib/auto/$modpname/$modfname.bs";
	unlink "$install_lib/auto/$modpname/$modfname.exp"; #MSWin32 VC++
	unlink "$install_lib/auto/$modpname/$modfname.lib"; #MSWin32 VC++
    }
}

#==============================================================================
# This routine fixes problems with the MakeMaker Makefile.
#==============================================================================
my %fixes = (
	     INSTALLSITEARCH => 'install_lib',
	     INSTALLDIRS => 'installdirs',
	     XSUBPPARGS => 'xsubppargs',
	    );

sub fix_make {
    use strict;
    my (@lines, $fix);
    my $o = shift;

    $o->{installdirs} = 'site';

    open(MAKEFILE, "< $o->{build_dir}Makefile")
      or croak "Can't open Makefile for input: $!\n";
    @lines = <MAKEFILE>;
    close MAKEFILE;

    open(MAKEFILE, "> $o->{build_dir}Makefile")
      or croak "Can't open Makefile for output: $!\n";
    for (@lines) {
	if (/^(\w+)\s*=\s*\S+.*$/ and
	    $fix = $fixes{$1}
	   ) {
	    print MAKEFILE "$1 = $o->{$fix}\n"
	}
	else {
	    print MAKEFILE;
	}
    }
    close MAKEFILE;
}

#==============================================================================
# Returns a string which, when used in a regex, can extract global symbols.
# Depends on assembler being used.
#==============================================================================
sub global_keys {
    my $o = shift;
    my $asm = $o->{ASM}{AS};
    if ($asm =~ /nasm/i) {
	return 'GLOBAL|COMMON';
    }
    elsif ($asm =~ /gasp/i) {
        return '\.GLOBAL';
    }
    elsif ($asm =~ /as/i) {
	return '\.(?:globl|common)';
    }
}

1;

__END__
