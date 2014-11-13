package App::fatten;

# DATE
# VERSION

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
use File::Spec;
use File::Slurp::Tiny qw(write_file read_file);
use File::Temp qw(tempfile tempdir);
use List::MoreUtils qw(uniq);
use List::Util qw(first);
use Log::Any::For::Builtins qw(system my_qx);
use Module::Path qw(module_path);
use Proc::ChildError qw(explain_child_error);
use SHARYANTO::Dist::Util qw(list_dist_modules);
use SHARYANTO::File::Util qw(file_exists);
use String::ShellQuote;
use version;

sub _sq { shell_quote($_[0]) }

our %SPEC;

sub _trace {
    require App::tracepm;

    my $self = shift;

    $log->debugf("  Tracing with method '%s' ...", $self->{trace_method});
    my $res = App::tracepm::tracepm(
        method => $self->{trace_method},
        script => $self->{input_file},
        use => $self->{use},
        recurse_exclude_core => $self->{exclude_core} ? 1:0,
        recurse_exclude_xs   => 1,
        detail => 1,

        core => $self->{exclude_core} ? 0 : undef,
        xs   => 0,
    );
    die "Can't trace: $res->[0] - $res->[1]" unless $res->[0] == 200;
    $self->{deps} = $res->[2];
}

sub _build_lib {
    my $self = shift;

    my $tempdir = $self->{tempdir};

    my $totsize = 0;
    my $totfiles = 0;

    local $CWD = "$tempdir/lib";

    my @mods; # modules to add

    my $deps = $self->{deps};
    for (@$deps) {
        next if $_->{is_core} && $self->{exclude_core};
        next if $_->{is_xs};
        $log->debugf("  Adding module: %s (traced)", $_->{module});
        push @mods, $_->{module};
    }

    for (@{ $self->{include} // [] }) {
        $log->debugf("  Adding module: %s (included)", $_);
        push @mods, $_;
    }

    for (@{ $self->{include_dist} // [] }) {
        my @distmods = list_dist_modules($_);
        if (@distmods) {
            $log->debugf("  Adding modules: %s (included dist)", join(", ", @distmods));
            push @mods, @distmods;
        } else {
            $log->debugf("  Adding module: %s (included dist, but can't find other modules)", $_);
            push @mods, $_;
        }
    }

    @mods = uniq(@mods);

    # filter excluded
    my @fmods;
  MOD:
    for my $mod (@mods) {
        if ($self->{exclude} && $mod ~~ @{ $self->{exclude} }) {
            $log->infof("Excluding %s: skipped", $mod);
            next;
        }
        for (@{ $self->{exclude_pattern} // [] }) {
            if ($mod ~~ /$_/) {
                $log->infof("Excluding %s: skipped by pattern %s", $mod, $_);
                next MOD;
            }
        }
        push @fmods, $mod;
    }
    @mods = @fmods;

    for my $mod (@mods) {
        my $mpath = module_path($mod) or die "Can't find path for $mod";

        my $modp = $mod; $modp =~ s!::!/!g; $modp .= ".pm";
        my ($dir) = $modp =~ m!(.+)/(.+)!;
        if ($dir) {
            make_path($dir) unless -d $dir;
        }

        if ($self->{stripper}) {
            my $stripper = do {
                require Perl::Stripper;
                Perl::Stripper->new(
                    maintain_linum => $self->{stripper_maintain_linum},
                    strip_ws       => $self->{stripper_ws},
                    strip_comment  => $self->{stripper_comment},
                    strip_pod      => $self->{stripper_pod},
                    strip_log      => $self->{stripper_log},
                );
            };
            $log->debug("  Stripping $mpath --> $modp ...");
            my $src = read_file($mpath);
            my $stripped = $stripper->strip($src);
            write_file($modp, $stripped);
        } elsif ($self->{strip}) {
            require Perl::Strip;
            my $strip = Perl::Strip->new;
            $log->debug("  Stripping $mpath --> $modp ...");
            my $src = read_file($mpath);
            my $stripped = $strip->strip($src);
            write_file($modp, $stripped);
        } elsif ($self->{squish}) {
            $log->debug("  Squishing $mpath --> $modp ...");
            require Perl::Squish;
            my $squish = Perl::Squish->new;
            $squish->file($mpath, $modp);
        } else {
            $log->debug("  Copying $mpath --> $modp ...");
            copy($mpath, $modp);
        }

        $totfiles++;
        $totsize += (-s $mpath);
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

    chmod 0755, $self->{abs_output_file};

    # replace shebang line (which contains perl path used by fatpack) with a
    # default system perl. perhaps make this configurable in the future.
    {
        my $ct = read_file($self->{abs_output_file});
        $ct =~ s{\A#!(.+)}{#!/usr/bin/perl};
        write_file($self->{abs_output_file}, $ct);
    }

    $log->infof("  Produced %s (%.1f KB)",
                $self->{abs_output_file}, (-s $self->{abs_output_file})/1024);
}

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

$SPEC{fatten} = {
    v => 1.1,
    summary => 'Pack your dependencies onto your script file',
    args => {
        input_file => {
            summary => 'Path to input file (script to be fatpacked)',
            description => <<'_',

`-` (or if unspecified) means to take from standard input (internally, a
temporary file will be created to handle this).

_
            schema => ['str*'],
            default => '-',
            pos => 0,
            cmdline_aliases => { i=>{} },
        },
        output_file => {
            summary => 'Path to output file',
            description => <<'_',

If input is from stdin, then output defaults to stdout. You can also specify
stdout by using `-`.

Otherwise, defaults to `<script>.fatpack` in source directory. If source
directory happens to be unwritable by the script, will try `<script>.fatpack` in
current directory. If that fails too, will die.

_
            schema => ['str*'],
            cmdline_aliases => { o=>{} },
            pos => 1,
        },
        include => {
            summary => 'Include extra modules',
            description => <<'_',

When the tracing process fails to include a required module, you can add it
here.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { I => {} },
        },
        include_dist => {
            summary => 'Include extra modules',
            description => <<'_',

Just like the `include` option, but will include module as well as other modules
from the same distribution. Module name must be the main module of the
distribution. Will determine other modules from the `.packlist` file.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => {},
        },
        exclude => {
            summary => 'Modules to exclude',
            description => <<'_',

When you don't want to include a module, specify it here.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { E => {} },
        },
        exclude_pattern => {
            summary => 'Regex patterns of modules to exclude',
            description => <<'_',

When you don't want to include a pattern of modules, specify it here.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { p => {} },
        },
        exclude_core => {
            summary => 'Whether to exclude core modules',
            schema => ['bool' => default => 1],
        },
        perl_version => {
            summary => 'Perl version to target, defaults to current running version',
            description => <<'_',

This is for determining which modules are considered core and should be skipped
by default (when `exclude_core` option is enabled). Different perl versions have
different sets of core modules as well as different versions of the modules.

_
            schema => ['str*'],
            cmdline_aliases => { V=>{} },
        },
        overwrite => {
            schema => [bool => default => 0],
            summary => 'Whether to overwrite output if previously exists',
        },
        trace_method => {
            summary => "Which method to use to trace dependencies",
            schema => ['str*', default => 'fatpacker'],
            description => <<'_',

The default is `fatpacker`, which is the same as what `fatpack trace` does.
Different tracing methods have different pro's and con's, one method might
detect required modules that another method does not, and vice versa. There are
several methods available, please see `App::tracepm` for more details.

_
            cmdline_aliases => { t=>{} },
        },
        use => {
            summary => 'Additional modules to "use"',
            schema => ['array*' => of => 'str*'],
            description => <<'_',

Will be passed to the tracer. Will currently only affect the `fatpacker` and
`require` methods (because those methods actually run your script).

_
        },

        squish => {
            summary => 'Whether to squish included modules using Perl::Squish',
            schema => ['bool' => default=>0],
        },

        strip => {
            summary => 'Whether to strip included modules using Perl::Strip',
            schema => ['bool' => default=>0],
        },

        stripper => {
            summary => 'Whether to strip included modules using Perl::Stripper',
            schema => ['bool' => default=>0],
        },
        stripper_maintain_linum => {
            summary => "Will be passed to Perl::Stripper's maintain_linum",
            schema => ['bool'],
            default => 0,
        },
        stripper_ws => {
            summary => "Will be passed to Perl::Stripper's strip_ws",
            schema => ['bool'],
            default => 1,
        },
        stripper_comment => {
            summary => "Will be passed to Perl::Stripper's strip_comment",
            schema => ['bool'],
            default => 1,
        },
        stripper_pod => {
            summary => "Will be passed to Perl::Stripper's strip_pod",
            schema => ['bool'],
            default => 1,
        },
        stripper_log => {
            summary => "Will be passed to Perl::Stripper's strip_log",
            schema => ['bool'],
            default => 0,
        },
        # XXX strip_log_levels

        debug_keep_tempdir => {
            summary => 'Keep temporary directory for debugging',
            schema => ['bool' => default=>0],
        },
    },
    deps => {
        exec => 'fatpack',
    },
};
sub fatten {
    my %args = @_;
    my $self = __PACKAGE__->new(%args);

    my $tempdir = tempdir(CLEANUP => 0);
    $log->debugf("Created tempdir %s", $tempdir);
    $self->{tempdir} = $tempdir;

    # my understanding is that fatlib contains the stuffs beside the pure-perl
    # .pm files, and currently won't pack anyway.
    #mkdir "$tempdir/fatlib";
    mkdir "$tempdir/lib";

    $self->{perl_version} //= $^V;
    $self->{perl_version} = version->parse($self->{perl_version});
    $log->debugf("Will be targetting perl %s", $self->{perl_version});

    if ($self->{input_file} eq '-') {
        $self->{input_file_is_stdin} = 1;
        $self->{input_file} = $self->{abs_input_file} = (tempfile())[1];
        open my($fh), ">", $self->{abs_input_file}
            or return [500, "Can't write temporary input file '$self->{abs_input_file}': $!"];
        local $_; while (<STDIN>) { print $fh $_ }
        $self->{output_file} //= '-';
    } else {
        (-f $self->{input_file})
            or return [500, "No such input file: $self->{input_file}"];
        $self->{abs_input_file} = abs_path($self->{input_file}) or return
            [500, "Can't find absolute path of input file $self->{input_file}"];
    }

    my $output_file;
    {
        $output_file = $self->{output_file};
        if (defined $output_file) {
            if ($output_file eq '-') {
                $self->{output_file_is_stdout} = 1;
                $self->{output_file} = $self->{abs_output_file} = (tempfile())[1];
                last;
            } else {
                return [412, "Output file '$output_file' exists, won't overwrite (see --overwrite)"]
                    if file_exists($output_file) && !$self->{overwrite};
                last if open my($fh), ">", $output_file;
                return [500, "Can't write to output file '$output_file': $!"];
            }
        }

        my ($vol, $dir, $file) = File::Spec->splitpath($self->{input_file});
        my $fh;

        # try <input>.fatpack in the source directory
        $output_file = File::Spec->catpath($vol, $dir, "$file.fatpack");
        return [412, "Output file '$output_file' exists, won't overwrite (see --overwrite)"]
            if file_exists($output_file) && !$self->{overwrite};
        last if open $fh, ">", $output_file;

        # if failed, try <input>.fatpack in the current directory
        $output_file = "$CWD/$file.fatpack";
        return [412, "Output file '$output_file' exists, won't overwrite (see --overwrite)"]
            if file_exists($output_file) && !$self->{overwrite};
        last if open $fh, ">", $output_file;

        # failed too, bail
        return [500, "Can't write $file.fatpack in source- as well as ".
                    "current directory: $!"];
    }
    $self->{output_file} = $output_file;
    $self->{abs_output_file} //= abs_path($output_file) or return
        [500, "Can't find absolute path of output file '$self->{output_file}'"];

    $log->infof("Tracing dependencies ...");
    $self->_trace;

    $log->infof("Building lib/ ...");
    $self->_build_lib;

    $log->infof("Packing ...");
    $self->_pack;

    if ($self->{debug_keep_tempdir}) {
        $log->infof("Keeping tempdir %s for debugging", $tempdir);
    } else {
        $log->debugf("Deleting tempdir %s ...", $tempdir);
        remove_tree($tempdir);
    }

    if ($self->{input_file_is_stdin}) {
        unlink $self->{abs_input_file};
    }
    if ($self->{output_file_is_stdout}) {
        open my($fh), "<", $self->{abs_output_file}
            or return [500, "Can't open temporary output file '$self->{abs_output_file}': $!"];
        local $_; print while <$fh>; close $fh;
        unlink $self->{abs_output_file};
    }

    [200];
}

1;
# ABSTRACT: Pack your dependencies onto your script file

=for Pod::Coverage ^(new)$

=head1 SYNOPSIS

This distribution provides command-line utility called L<fatten>.


=head2 TODO

=over

=back

=cut
