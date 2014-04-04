package App::FatPacker::NonCore;

use 5.010001;
use strict;
use warnings;
use Log::Any '$log';

use Config;
use Cwd qw(abs_path);
use File::chdir;
use File::Copy;
use File::Path qw(make_path remove_tree);
use File::Slurp;
use File::Temp qw(tempdir);
use Log::Any::For::Builtins qw(system my_qx);
use Module::CoreList;
use Module::Path qw(module_path);
use SHARYANTO::Proc::ChildError qw(explain_child_error);
use String::ShellQuote;
use version;

sub _sq { shell_quote($_[0]) }

# VERSION

our %SPEC;

$SPEC{fatpack_noncore} = {
    v => 1.1,
    args => {
        input_file => {
            summary => 'Path to input file (script to be packed)',
            schema => ['str*'],
            req => 1,
            pos => 0,
            cmdline_aliases => { i=>{} },
        },
        output_file => {
            summary => 'Path to output file, defaults to INPUT.packed',
            schema => ['str*'],
            cmdline_aliases => { o=>{} },
        },
        perl_version => {
            summary => 'Perl version to target, defaults to current running version',
            schema => ['str*'],
            cmdline_aliases => { V=>{} },
        },
        #overwrite => {
        #    schema => [bool => default => 0],
        #    summary => 'Whether to overwrite output if previously exists',
        #},
    },
    deps => {
        exec => 'fatpack',
    },
};
sub fatpack_noncore {
    my %args = @_;

    my $tempdir = tempdir(CLEANUP => 0);
    $log->debugf("Created tempdir %s", $tempdir);

    # my understanding is that fatlib contains the stuffs beside the pure-perl
    # .pm files, and currently won't pack anyway.
    mkdir "$tempdir/fatlib";
    mkdir "$tempdir/lib";

    my $plver = version->parse($args{perl_version} // $^V);
    $log->debugf("Will be targetting perl %s", $plver);

    my $inputf = $args{input_file};
    my $abs_inputf = abs_path($inputf)
        or return [500, "Can't find path to input file $inputf"];

    # check output first, before the long process
    my $outputf = $args{output_file} || "$inputf.packed";
    my $abs_outputf = abs_path($outputf)
        or return [500, "Can't find path to output file $outputf"];
    write_file($outputf, "");

    my $tracef = "$tempdir/fatpacker.trace";
    system("fatpack", "trace", "--to", $tracef, $inputf);
    return [500, "Can't fatpack trace: ".explain_child_error()] if $?;

    $log->infof("Creating list of required non-core modules ...");
    my %mods;
    open my($fh), "<", $tracef or return [500, "Can't open $tracef: $!"];
    while (<$fh>) {
        chomp;
        my $mod = $_; $mod =~ s!/!::!g; $mod =~ s/\.pm\z//;
        if (Module::CoreList::is_core($mod, undef, $plver)) {
            $log->debugf("  Skipping %s (is core)", $mod);
            next;
        } else {
            $log->infof("  Including %s", $mod);
            $mods{$mod}++;
        }
    }

    # XXX this is slow because we're calling 'fatpack' multiple times for each
    # module, need to make it faster later
    my %packlists; # key = path
    my @inc = sort {length($b) <=> length($a)} grep {!ref($_)} @INC;

    $log->infof("Finding packlists ...");
    for my $mod (sort keys %mods) {
        my $modsl = $mod; $modsl =~ s!::!/!g; # back and forth :(
        my $path = my_qx("fatpack", "packlists-for", "$modsl.pm");
        chomp($path);
        if ($path) {
            $log->infof("  Found packlist for %s at %s", $mod, $path);
            next if $packlists{$path}++;
            my @files = read_file($path);
            for my $f (@files) {
                chomp($f);
                next unless $f =~ /\.pm\z/;
                for (@inc) {
                    if (index($f, $_) == 0) {
                        my $file = substr($f, length($_)+1);
                        $log->info("  Copying $f --> $tempdir/lib/$file");
                        my $dir = $file; $dir =~ m!(.+)/!;
                        make_path("$tempdir/lib/$dir");
                        copy($f, "$tempdir/lib/$file");
                        last;
                    }
                }
            }
        }
    }

    # copy ourselves, fapatck 'tree' currently doesn't do what i want (it won't
    # copy files to lib/, only to fatlib/?)

    $log->infof("Building lib at %s ...", "$tempdir/lib");
    {
        local $CWD = "$tempdir/lib";

    }

    $log->infof("Packing ...");
    {
        local $CWD = $tempdir;
        system "fatpack file "._sq($abs_inputf)." > "._sq($abs_outputf);
        return [500, "Can't fatpack file: ".explain_child_error()] if $?;
    }

    if ($log->is_debug) {
        $log->debug("Not cleaning up tempdir $tempdir for debugging");
    } else {
        $log->debug("Cleaning up tempdir $tempdir ...");
        remove_tree($tempdir);
    }

    [200];
}

1;
# ABSTRACT: Produce Perl script that only depends on core modules

=head1 SYNOPSIS

This distribution provides command-line utility called
L<fatpack-noncore>.

=cut
