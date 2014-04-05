package App::fatten;

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';
BEGIN { no warnings; $main::Log_Level = 'info' }

use Config;
use Cwd qw(abs_path);
use File::chdir;
use File::Copy;
use File::Path qw(make_path remove_tree);
use File::Slurp::Shortcuts qw(slurp slurp_c write_file);
use File::Temp qw(tempdir);
use List::Util qw(first);
use Log::Any::For::Builtins qw(system my_qx);
use Module::CoreList;
use Module::Path qw(module_path);
use SHARYANTO::Dist::Util qw(packlist_for);
use SHARYANTO::Proc::ChildError qw(explain_child_error);
use String::ShellQuote;
use version;

sub _sq { shell_quote($_[0]) }

# VERSION

our %SPEC;

$SPEC{fatten} = {
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
        include_modules => {
            summary => 'Modules to include',
            description => <<'_',

When the tracing process fails to include a required module, you can add it
here. Either specify in the form of `Package::SubPkg` or `Package/SubPkg.pm`.

_
            schema => ['array*' => of => 'str*'],
        },
        exclude_modules => {
            summary => 'Modules to exclude',
            description => <<'_',

When you don't want to include a module, specify it here. Either specify in the
form of `Package::SubPkg` or `Package/SubPkg.pm`.

_
            schema => ['array*' => of => 'str*'],
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
        strip => {
            summary => 'Whether to strip included modules using Perl::Stripper',
            schema => ['bool' => default=>0],
        },
        # XXX strip_opts
    },
    deps => {
        exec => 'fatpack',
    },
};
sub fatten {
    my %args = @_;

    my $incl = $args{include_modules} // [];
    my $excl = $args{exclude_modules} // [];

    for my $list ($excl, $incl) {
        for my $mod (@$list) {
            unless ($mod =~ /\.pm\z/) {
                $mod =~ s!::!/!g; $mod .= ".pm";
            }
        }
    }

    my $tempdir = tempdir(CLEANUP => 0);
    $log->debugf("Created tempdir %s", $tempdir);

    # my understanding is that fatlib contains the stuffs beside the pure-perl
    # .pm files, and currently won't pack anyway.
    #mkdir "$tempdir/fatlib";
    mkdir "$tempdir/lib";

    my $plver = version->parse($args{perl_version} // $^V);
    $log->debugf("Will be targetting perl %s", $plver);

    my $inputf = $args{input_file};
    (-f $inputf) or return [500, "Can't find input file $inputf"];
    my $abs_inputf = abs_path($inputf)
        or return [500, "Can't find path to input file $inputf"];

    my $outputf = $args{output_file} || "$inputf.packed";
    my $abs_outputf = abs_path($outputf)
        or return [500, "Can't find path to output file $outputf"];

    my %mods; # key = Package/SubPkg.pm, ...
    $log->infof("Tracing modules to be included ...");
    # XXX option to use Perl::PrereqScanner or other methods
    {
        for my $mod (@$incl) {
            next if $mod ~~ @$excl;
            $log->info("  Adding: $mod");
            require $mod;
            $mods{$mod}++;
        }

        my $tracef = "$tempdir/fatpacker.trace";
        system("fatpack", "trace", "--to", $tracef, $inputf);
        return [500, "Can't fatpack trace: ".explain_child_error()] if $?;

        # i'm not having much success using the 'fatpack packlists-for' and
        # 'fatpack tree' commands. so i'm adding the .pm files directly here for
        # now, and later finally using 'fatpack file'.

        open my($fh), "<", $tracef or return [500, "Can't open $tracef: $!"];
        while (my $mod = <$fh>) {
            chomp($mod);
            next if $mods{$mod};
            my $pm = $mod; $pm =~ s!/!::!g; $pm =~ s/\.pm\z//;
            my $is_core = Module::CoreList::is_core($pm, undef, $plver);
            do { $log->debug("  Excluding: $mod (excluded)"); next }
                if $mod ~~ @$excl;
            do { $log->debug("  Excluding: $mod (core)"); next }
                if !$mods{$mod} && $is_core;
            $log->info("  Adding: $mod");
            $mods{$mod}++;
        }
    }

    $log->infof("Checking modules to be included ...");
    {
        my %packlists; # key = path

        for my $mod (sort keys %mods) {
            my $path = packlist_for($mod);
            next unless $path;
            $log->debugf("  Found packlist for %s at %s", $mod, $path);
            next if $packlists{$path}++;
            my @files = slurp_c($path);
            if (first { /\.(so|bs)\z/ } @files) {
                $log->warnf("  Warning: $mod contains XS modules, won't pack properly");
            }
            #if (first { m!/(bin|script)/! } @files) {
            #    $log->warnf("  Note: $mod contains scripts");
            #}
            if (first { m!/LocaleData/! } @files) {
                $log->warnf("  Warning: $mod contains locale date (message catalogs, etc), not included");
            }
        }
    }

    $log->infof("Building lib/ ...");
    my $totsize = 0;
    my $totfiles = 0;
    {
        local $CWD = "$tempdir/lib";
        for my $mod (keys %mods) {
            my $mpath = module_path($mod)
                or return [500, "Can't find path for $mod"];
            my ($dir) = $mod =~ m!(.+)/(.+)!;
            if ($dir) {
                make_path($dir) unless -d $dir;
            }
            if ($args{strip}) {
                state $stripper = do {
                    require Perl::Stripper;
                    Perl::Stripper->new;
                };
                $log->debug("  Stripping $mpath --> $mod ...");
                my $src = slurp($mpath);
                my $stripped = $stripper->strip($src);
                write_file($mod, $stripped);
            } else {
                $log->debug("  Copying $mpath --> $mod ...");
                copy($mpath, $mod);
            }
            $totfiles++;
            $totsize += (-s $mod);
        }
        $log->infof("  Added %d files (%.1f KB)", $totfiles, $totsize/1024);
    }

    $log->infof("Packing ...");
    {
        local $CWD = $tempdir;
        system "fatpack file "._sq($abs_inputf)." > "._sq($abs_outputf);
        return [500, "Can't fatpack file: ".explain_child_error()] if $?;
        $log->infof("  Produced %s (%.1f KB)",
                    $abs_outputf, (-s $outputf)/1024);
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
# ABSTRACT: Pack your dependencies onto your script file

=head1 SYNOPSIS

This distribution provides command-line utility called L<fatten>.

=cut
