use ExtUtils::MakeMaker;
use Config;

#=============================================================================
# Guess suitable defaults for assembler, compiler, linker, etc.
#=============================================================================
my $asm_guess;
my $cc_guess;
my $ld_guess;
if ($Config{osname} =~ /^MSWin/) {
    $asm_guess = 'masm';
    $cc_guess = 'cl';
    $ld_guess = 'link';
}
elsif ($Config{osname} eq 'linux') {
    $asm_guess = 'as';
    $cc_guess = 'gcc';
    $ld_guess = 'gcc';
}
else {
    $asm_guess = 'as';
    $cc_guess = 'gcc';
    $ld_guess = 'gcc';
}

my %asflags_guess = 
  (
   nasm => ($Config{osname} =~ /^MSWin/) ? '-f win32' : '-f elf',
  );

#=============================================================================
# Prompt for assembler to use
#=============================================================================
my $asm = prompt("What assembler would you like to use?",
                 $asm_guess);
my $asflags = prompt("What flags do you want to pass to $asm by default?",
                     $asflags_guess{$asm} || '');

#=============================================================================
# Default C compiler
#=============================================================================
print 
  "Inline::ASM needs a C compiler to generate the Assembler-Perl bindings.\n";
my $cc = prompt("What C compiler would you like to use?",
                $cc_guess);
my $ld = prompt("What linker would you like to use?", $ld_guess);

#=============================================================================
# Apply these defaults
#=============================================================================
open ASM, "ASM_pm";
my @lines = <ASM>;
close ASM;

for (@lines) {
    s|\@ASSEMBLER|$asm| if m|\# default assembler|;
    s|\@ASFLAGS|$asflags| if m|\# default asm flags|;
    s|\@COMPILER|$cc| if m|\# default compiler|;
    s|\@LINKER|$ld| if m|\# default linker|;
}

open ASM, ">ASM.pm"
  or die "Can't write to ASM.pm!";
print ASM @lines;
close ASM;

#=============================================================================
# Write the Makefile
#=============================================================================
WriteMakefile(
	      NAME => 'Inline::ASM',
	      VERSION_FROM => 'ASM.pm',
	      clean => {FILES => 'ASM.pm _Inline_test/'},
	     );
