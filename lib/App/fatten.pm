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

sub _trace_with_fatpack {
    my $self = shift;

    my $tempdir = $self->{tempdir};

    for my $mod (@{ $self->{include_modules} }) {
        next if $mod ~~ @{ $self->{exclude_modules} };
        $log->info("  Adding: $mod");
        require $mod;
        $self->{mods}{$mod}++;
    }

    my $tracef = "$tempdir/fatpacker.trace";
    system("fatpack", "trace", "--to", $tracef, $self->{abs_input_file});
    die "Can't fatpack trace: ".explain_child_error() if $?;

    # i'm not having much success using the 'fatpack packlists-for' and 'fatpack
    # tree' commands. so i'm adding the .pm files directly here for now, and
    # later finally using 'fatpack file'.

    open my($fh), "<", $tracef or die "Can't open $tracef: $!";
    while (my $mod = <$fh>) {
        chomp($mod);
        next if $self->{mods}{$mod};
        my $pm = $mod; $pm =~ s!/!::!g; $pm =~ s/\.pm\z//;
        my $is_core = Module::CoreList::is_core(
            $pm, undef, $self->{perl_version});
        do { $log->debug("  Excluding: $mod (excluded)"); next }
            if $mod ~~ @{ $self->{exclude_modules} };
        do { $log->debug("  Excluding: $mod (core)"); next }
            if !$self->{mods}{$mod} && $is_core;
        $log->info("  Adding: $mod");
        $self->{mods}{$mod}++;
    }
}

sub _trace_with_prereq_scanner {
    my $self = shift;
}

# just emit some warnings
sub _check_modules {
    my $self = shift;

    my %packlists; # key = path

    for my $mod (sort keys %{ $self->{mods} }) {
        my $path = packlist_for($mod);
        next unless $path;
        $log->debugf("  Found packlist for %s at %s", $mod, $path);
        next if $packlists{$path}++;
        my @files = slurp_c($path);
        if (first { /\.(so|bs)\z/ } @files) {
            $log->warnf("  Warning: $mod is XS module, won't pack properly");
        }
        #if (first { m!/(bin|script)/! } @files) {
        #    $log->warnf("  Note: $mod contains scripts");
        #}
        if (first { m!/LocaleData/! } @files) {
            $log->warnf("  Warning: $mod contains locale date (message catalogs, etc), not included");
        }
    }
}

sub _build_lib {
    my $self = shift;

    my $tempdir = $self->{tempdir};

    my $totsize = 0;
    my $totfiles = 0;

    local $CWD = "$tempdir/lib";

    for my $mod (keys %{ $self->{mods} }) {
        my $mpath = module_path($mod) or die "Can't find path for $mod";
        my ($dir) = $mod =~ m!(.+)/(.+)!;
        if ($dir) {
            make_path($dir) unless -d $dir;
        }
        if ($self->{strip}) {
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

sub _pack {
    my $self = shift;

    my $tempdir = $self->{tempdir};

    local $CWD = $tempdir;
    system join(
        "",
        "fatpack file ",
        _sq($self->{abs_input_file}), " > ",
        _sq($self->{abs_output_file}),
    );
    die "Can't fatpack file: ".explain_child_error() if $?;
    $log->infof("  Produced %s (%.1f KB)",
                $self->{abs_output_file}, (-s $self->{abs_output_file})/1024);
}

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

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
    my $self = __PACKAGE__->new(%args);

    my $tempdir = tempdir(CLEANUP => 1);
    $log->debugf("Created tempdir %s", $tempdir);
    $self->{tempdir} = $tempdir;


    $self->{include_modules} //= [];
    $self->{exclude_modules} //= [];

    for my $list ($self->{include_modules}, $self->{exclude_modules}) {
        for my $mod (@$list) {
            unless ($mod =~ /\.pm\z/) {
                $mod =~ s!::!/!g; $mod .= ".pm";
            }
        }
    }

    # my understanding is that fatlib contains the stuffs beside the pure-perl
    # .pm files, and currently won't pack anyway.
    #mkdir "$tempdir/fatlib";
    mkdir "$tempdir/lib";

    $self->{perl_version} //= $^V;
    $self->{perl_version} = version->parse($self->{perl_version});
    $log->debugf("Will be targetting perl %s", $self->{perl_version});

    (-f $self->{input_file}) or die "No such input file: $self->{input_file}";
    $self->{abs_input_file} = abs_path($self->{input_file})
        or die "Can't find absolute path of input file $self->{input_file}";

    $self->{output_file} = "$self->{input_file}.packed";
    $self->{abs_output_file} = abs_path($self->{output_file})
        or die "Can't find absolute path of output file $self->{output_file}";

    $self->{mods} = {}; # key = Package/SubPkg.pm, ...

    $log->infof("Tracing modules to be included ...");
    $self->_trace_with_fatpack;
    $self->_trace_with_prereq_scanner;

    $log->infof("Checking modules to be included ...");
    $self->_check_modules;

    $log->infof("Building lib/ ...");
    $self->_build_lib;

    $log->infof("Packing ...");
    $self->_pack;

    [200];
}

1;
# ABSTRACT: Pack your dependencies onto your script file

=for Pod::Coverage ^(new)$

=head1 SYNOPSIS

This distribution provides command-line utility called L<fatten>.

=cut
