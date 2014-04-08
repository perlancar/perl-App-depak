package App::fatten;

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';
BEGIN { no warnings; $main::Log_Level = 'info' }

use Cwd qw(abs_path);
use File::chdir;
use File::Copy;
use File::Path qw(make_path remove_tree);
use File::Slurp::Shortcuts qw(slurp slurp_c write_file);
use File::Temp qw(tempfile tempdir);
use List::Util qw(first);
use Log::Any::For::Builtins qw(system my_qx);
use Module::CoreList;
use Module::Path qw(module_path);
#use SHARYANTO::Dist::Util qw(packlist_for);
use SHARYANTO::Module::Util qw(is_xs);
use SHARYANTO::Proc::ChildError qw(explain_child_error);
use String::ShellQuote;
use version;

sub _sq { shell_quote($_[0]) }

# VERSION

our %SPEC;

# add module to list to be included, return 1 if module is added the first time.
sub _consider_module {
    my ($self, $mod) = @_;

    my $sm = $self->{seen_mods};

    return 0 if exists $sm->{$mod};

    if ($self->{skip_not_found} && !module_path($mod)) {
        $sm->{$mod} = 0;
        $log->warn("  Skipped: $mod (not found)");
        return 0;
    }
    if ($mod !~ /\.pm\z/) {
        $sm->{$mod} = 0;
        $log->warn("  Skipped: $mod (not .pm file)");
        return 0;
    }
    if ($mod ~~ @{ $self->{exclude_modules} }) {
        $sm->{$mod} = 0;
        $log->warn("  Skipped: $mod (excluded)");
        return 0;
    }
    for (@{ $self->{exclude_module_patterns} }) {
        if ($mod ~~ $_) {
            $sm->{$mod} = 0;
            $log->warn("  Skipped: $mod (excluded $_)");
            return 0;
        }
    }
    my $pm = $mod; $pm =~ s!/!::!g; $pm =~ s/\.pm\z//;
    my $is_core = Module::CoreList::is_core($pm, undef, $self->{perl_version});
    if ($is_core && !($mod ~~ @{ $self->{include_modules} })) {
        $sm->{$mod} = 0;
        $log->warn("  Skipped: $mod (core)");
        return 0;
    }
    if (is_xs($mod)) {
        $sm->{$mod} = 0;
        $log->warn("  Skipped: $mod (XS)");
        return 0;
    }
    $log->info("  Added: $mod");
    $sm->{$mod} = 1;
    1;
}

sub _trace_with_fatpack_trace {
    my ($self, $path) = @_;

    $self->{_seen_paths} //= {};
    return if $self->{_seen_paths}{$path}++;

    $log->debug("  Analyzing $path with 'fatpack trace' ...");

    my $tempdir = $self->{tempdir};

    my ($tracefh, $tracef) = tempfile("fatpack.trace.XXXXXX", DIR=>$tempdir);
    system("fatpack", "trace", "--to", $tracef,
           (map {("--use", $_)} @{ $self->{use} // [] }), $path);
    die "Can't fatpack trace: ".explain_child_error() if $?;

    # i'm not having much success using the 'fatpack packlists-for' and 'fatpack
    # tree' commands. so i'm adding the .pm files directly here for now, and
    # later finally using 'fatpack file'.

    my @new_mods;

    open my($fh), "<", $tracef or die "Can't open $tracef: $!";
    while (my $mod = <$fh>) {
        chomp($mod);
        push @new_mods, $mod if $self->_consider_module($mod);
    }

    #my @paths = grep {$_} map { module_path($_) } sort @new_mods;
    #$self->_trace_with_fatpack_trace($_) for @paths;
}

sub _trace_with_prereq_scanner {
    my ($self, $path) = @_;

    $self->{_seen_paths} //= {};
    return if $self->{_seen_paths}{$path}++;

    $log->debug("  Analyzing $path with PrereqScanner ...");

    state $scanner = do {
        require Perl::PrereqScanner;
        Perl::PrereqScanner->new;
    };

    my @new_mods;
    my $res = $scanner->scan_file($path)->as_string_hash;
    for my $m0 (keys %$res) {
        next if $m0 =~ /\A(perl)\z/;

        my $m = $m0; $m =~ s!::!/!g; $m .= ".pm";
        push @new_mods, $m if $self->_consider_module($m);
    }

    my @paths = grep {$_} map { module_path($_) } @new_mods;
    $self->_trace_with_prereq_scanner($_) for @paths;
}

sub _build_lib {
    my $self = shift;

    my $tempdir = $self->{tempdir};

    my $totsize = 0;
    my $totfiles = 0;

    local $CWD = "$tempdir/lib";

    my $sm = $self->{seen_mods};
    for my $mod (sort(grep {$sm->{$_}} keys %$sm)) {
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
            cmdline_aliases => { I => {} },
        },
        exclude_modules => {
            summary => 'Modules to exclude',
            description => <<'_',

When you don't want to include a module, specify it here. Either specify in the
form of `Package::SubPkg` or `Package/SubPkg.pm`.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { E => {} },
        },
        exclude_module_patterns => {
            summary => 'Regex patterns of modules to exclude',
            description => <<'_',

When you don't want to include a pattern of modules, specify it here. The regex
will be matched against module name in the form `Package/SubPkg.pm`.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { p => {} },
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
        use_fatpack_trace => {
            summary => "Whether to use 'fatpack trace' to detect required modules",
            schema => ['bool', default=>1],
            description => <<'_',

`fatpack trace` uses `perl -c`.

_
        },
        use => {
            summary => 'Additional modules to "use"',
            schema => ['array*' => of => 'str*'],
            description => <<'_',

Will be passed to `fatpack trace`'s `--use` option.

_
        },
        use_prereq_scanner => {
            summary => 'Whether to use Perl::PrereqScanner to detect required modules',
            schema => ['bool', default=>0],
            description => <<'_',

Since fatpack uses `perl -c`, it might miss a few modules. You can add more
modules manually via `include_modules` (or `use`) or let Perl::PrereqScanner
does a static analysis of script/module source code. This on the other hand is
usually too aggressive, as it will include modules in cases like this:

    if ($some_feature_or_os) {
        require Win32::Console::ANSI;
    }

and you'll need to exclude some modules from being recursively searched using
`exclude_modules` or `exclude_module_patterns`.

_
            cmdline_aliases => { V=>{} },
        },
        skip_not_found => {
            summary => 'Instead of dying, skip modules that are not found',
            schema => 'bool',
            description => <<'_',

Unless you explicitly set it, this option is automatically turned on when you
use `use_prereq_scanner`.

_
        },
        strip => {
            summary => 'Whether to strip included modules using Perl::Stripper',
            schema => ['bool' => default=>0],
            cmdline_aliases => { s=>{} },
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

    $self->{skip_not_found} //= 1 if $self->{use_prereq_scanner};

    $self->{include_modules} //= [];
    $self->{exclude_modules} //= [];
    for my $list ($self->{include_modules}, $self->{exclude_modules}) {
        for my $mod (@$list) {
            unless ($mod =~ /\.pm\z/) {
                $mod =~ s!::!/!g; $mod .= ".pm";
            }
        }
    }

    $self->{exclude_module_patterns} //= [];
    for (@{ $self->{exclude_module_patterns} }) {
        $_ = qr/$_/;
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

    $self->{output_file} //= "$self->{input_file}.packed";
    $self->{abs_output_file} = abs_path($self->{output_file})
        or die "Can't find absolute path of output file $self->{output_file}";

    # list of modules that have been considered. key = Package/SubPkg.pm, val =
    # 1 if to be added, 0 if to be excluded
    $self->{seen_mods} = {};

    $log->infof("Tracing modules to be included ...");

    my @new;
    for my $mod (@{ $self->{include_modules} }) {
        push @new, $mod if $self->_consider_module($mod);
    }
    if ($self->{use_fatpack_trace}) {
        my @paths = (
            $self->{abs_input_file},
            #(grep {$_} map {module_path($_)} sort @new),
        );
        $self->_trace_with_fatpack_trace($_) for @paths;
    }
    if ($self->{use_prereq_scanner}) {
        my @paths = (
            $self->{abs_input_file},
            (grep {$_} map {module_path($_)} sort @new),
        );
        $self->_trace_with_prereq_scanner($_) for @paths;
    }

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
