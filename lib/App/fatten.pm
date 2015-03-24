package App::fatten;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use experimental 'smartmatch';
use Log::Any '$log';
use Log::Any::For::Builtins qw(system my_qx);
BEGIN { no warnings; $main::Log_Level = 'info' }

use App::tracepm ();

use File::chdir;
use File::Slurper qw(write_text read_text);
use version;

sub _sq {
    require String::ShellQuote;
    String::ShellQuote::shell_quote($_[0]);
}

our %SPEC;

sub _trace {
    my $self = shift;

    return if $self->{trace_method} eq 'none';

    $log->debugf("  Tracing with method '%s' ...", $self->{trace_method});
    my $res = App::tracepm::tracepm(
        method => $self->{trace_method},
        script => $self->{input_file},
        args => $self->{args},
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
    require Dist::Util;
    require File::Copy;
    require File::Find;
    require File::Path;
    require List::MoreUtils;
    require Module::Path::More;

    my $self = shift;

    my $tempdir = $self->{tempdir};

    my $totsize = 0;
    my $totfiles = 0;

    my %mods; # modules to add, key=name, val=path

    my $deps = $self->{deps};
    for (@$deps) {
        next if $_->{is_core} && $self->{exclude_core};
        next if $_->{is_xs};
        $log->debugf("  Adding module: %s (traced)", $_->{module});
        $mods{$_->{module}} = undef;
    }

    for (@{ $self->{include} // [] }) {
        $log->debugf("  Adding module: %s (included)", $_);
        $mods{$_} = undef;
    }

    for (@{ $self->{include_dist} // [] }) {
        my @distmods = Dist::Util::list_dist_modules($_);
        if (@distmods) {
            $log->debugf("  Adding modules: %s (included dist)", join(", ", @distmods));
            $mods{$_} = undef for @distmods;
        } else {
            $log->debugf("  Adding module: %s (included dist, but can't find other modules)", $_);
            $mods{$_} = undef;
        }
    }

    for (@{ $self->{include_from} // [] }) {
        $log->debugf("  Adding modules found in: %s", $_);
        local $CWD = $_;
        File::Find::find(
            sub {
                return unless -f;
                return unless /\.pm$/i;
                my $mod = $File::Find::dir eq '.' ? $_ : "$File::Find::dir/$_";
                $mod =~ s!^\.[/\\]!!;
                $mod =~ s![/\\]!::!g; $mod =~ s/\.pm$//i;
                $log->debugf("    Adding module: %s", $mod);
                $mods{$mod} = "$CWD/$_";
            }, ".",
        );
    }

    # filter excluded
    my $excluded_distmods;
    my %fmods; # filtered mods
  MOD:
    for my $mod (sort keys %mods) {
        if ($self->{exclude} && $mod ~~ @{ $self->{exclude} }) {
            $log->infof("Excluding %s: skipped", $mod);
            next MOD;
        }
        for (@{ $self->{exclude_pattern} // [] }) {
            if ($mod ~~ /$_/) {
                $log->infof("Excluding %s: skipped by pattern %s", $mod, $_);
                next MOD;
            }
        }
        if ($self->{exclude_dist}) {
            if (!$excluded_distmods) {
                $excluded_distmods = [];
                for (@{ $self->{exclude_dist} }) {
                    push @$excluded_distmods, Dist::Util::list_dist_modules($_);
                }
            }
            if ($mod ~~ @$excluded_distmods) {
                $log->infof("Excluding %s (by dist): skipped", $mod);
                next MOD;
            }
        }
        $fmods{$mod} = $mods{$mod};
    }
    %mods = %fmods;

    for my $mod (sort keys %mods) {
        my $mpath = $mods{$mod} // Module::Path::More::module_path(module=>$mod);
        defined $mpath or die "Can't find path for $mod";

        my $modp = $mod; $modp =~ s!::!/!g; $modp .= ".pm";
        my ($dir) = $modp =~ m!(.+)/(.+)!;
        if ($dir) {
            my $dir_to_make = "$tempdir/lib/$dir";
            unless (-d $dir_to_make) {
                File::Path::make_path($dir_to_make) or die "Can't make_path: $dir_to_make";
            }
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
            my $src = read_text($mpath);
            my $stripped = $stripper->strip($src);
            write_text("$tempdir/lib/$modp", $stripped);
        } elsif ($self->{strip}) {
            require Perl::Strip;
            my $strip = Perl::Strip->new;
            $log->debug("  Stripping $mpath --> $modp ...");
            my $src = read_text($mpath);
            my $stripped = $strip->strip($src);
            write_text("$tempdir/lib/$modp", $stripped);
        } elsif ($self->{squish}) {
            $log->debug("  Squishing $mpath --> $modp ...");
            require Perl::Squish;
            my $squish = Perl::Squish->new;
            $squish->file($mpath, "$tempdir/lib/$modp");
        } else {
            $log->debug("  Copying $mpath --> $tempdir/lib/$modp ...");
            File::Copy::copy($mpath, "$tempdir/lib/$modp");
        }

        $totfiles++;
        $totsize += (-s $mpath);
    }
    $log->infof("  Added %d files (%.1f KB)", $totfiles, $totsize/1024);
}

sub _pack {
    require Proc::ChildError;

    my $self = shift;

    my $tempdir = $self->{tempdir};

    local $CWD = $tempdir;
    system join(
        "",
        "fatpack file ",
        _sq($self->{abs_input_file}), " > ",
        _sq($self->{abs_output_file}),
    );
    die "Can't fatpack file: ".Proc::ChildError::explain_child_error() if $?;

    chmod 0755, $self->{abs_output_file};

    # replace shebang line (which contains perl path used by fatpack) with a
    # default system perl. perhaps make this configurable in the future.
    {
        my $ct = read_text($self->{abs_output_file});
        my $shebang = $self->{shebang} // '#!/usr/bin/perl';
        $shebang = "#!$shebang" unless $shebang =~ /^#!/;
        $shebang =~ s/\R+//g;
        $ct =~ s{\A#!(.+)}{$shebang};
        write_text($self->{abs_output_file}, $ct);
    }

    $log->infof("  Produced %s (%.1f KB)",
                $self->{abs_output_file}, (-s $self->{abs_output_file})/1024);
}

sub new {
    my $class = shift;
    bless { @_ }, $class;
}

my $trace_methods;
{
    my $sch = $App::tracepm::SPEC{tracepm}{args}{method}{schema};
    # XXX should've normalized schema
    if (ref($sch->[1]) eq 'HASH') {
        $trace_methods = $sch->[1]{in};
    } else {
        $trace_methods = $sch->[2];
    }
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
            'x.schema.entity' => 'filename',
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
            tags => ['category:output'],
            'x.schema.entity' => 'filename',
        },
        include => {
            summary => 'Include extra modules',
            'summary.alt.numnoun.singular' => 'Include an extra module',
            description => <<'_',

When the tracing process fails to include a required module, you can add it
here.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { I=>{} },
            tags => ['category:module-selection'],
            'x.schema.element_entity' => 'modulename',
        },
        include_from => {
            summary => 'Include extra modules under directories',
            'summary.alt.numnoun.singular' => 'Include extra modules under a directory',
            schema => ['array*' => of => 'str*'],
            tags => ['category:module-selection'],
            'x.schema.element_entity' => 'dirname',
        },
        include_dist => {
            summary => 'Include all modules of dist',
            description => <<'_',

Just like the `include` option, but will include module as well as other modules
from the same distribution. Module name must be the main module of the
distribution. Will determine other modules from the `.packlist` file.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => {},
            tags => ['category:module-selection'],
            'x.schema.element_entity' => 'distname',
        },
        exclude => {
            summary => 'Modules to exclude',
            'summary.alt.numnoun.singular' => 'Exclude a module',
            description => <<'_',

When you don't want to include a module, specify it here.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { E => {} },
            tags => ['category:module-selection'],
            'x.schema.element_entity' => 'modulename',
        },
        exclude_pattern => {
            summary => 'Regex patterns of modules to exclude',
            'summary.alt.numnoun.singular' => 'Regex pattern of modules to exclude',
            description => <<'_',

When you don't want to include a pattern of modules, specify it here.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => { p => {} },
            tags => ['category:module-selection'],
            #'x.schema.element_entity' => 'regex',
        },
        exclude_dist => {
            summary => 'Exclude all modules of dist',
            description => <<'_',

Just like the `exclude` option, but will exclude module as well as other modules
from the same distribution. Module name must be the main module of the
distribution. Will determine other modules from the `.packlist` file.

_
            schema => ['array*' => of => 'str*'],
            cmdline_aliases => {},
            tags => ['category:module-selection'],
            'x.schema.element_entity' => 'distname',
        },
        exclude_core => {
            summary => 'Exclude core modules',
            'summary.alt.bool.not' => 'Do not exclude core modules',
            schema => ['bool' => default => 1],
            tags => ['category:module-selection'],
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
            # XXX completion: list of known perl versions by Module::CoreList?
        },

        overwrite => {
            schema => [bool => default => 0],
            summary => 'Whether to overwrite output if previously exists',
            tags => ['category:output'],
        },
        trace_method => {
            summary => "Which method to use to trace dependencies",
            schema => ['str*', {
                default => 'fatpacker',
                in=>[@$trace_methods, 'none'],
            }],
            description => <<'_',

The default is `fatpacker`, which is the same as what `fatpack trace` does.
Different tracing methods have different pro's and con's, one method might
detect required modules that another method does not, and vice versa. There are
several methods available, please see `App::tracepm` for more details.

A special value of `none` is also provided. If this is selected, then fatten
will not perform any tracing. Usually used in conjunction with `--include-from`.

_
            cmdline_aliases => { t=>{} },
            tags => ['category:module-selection'],
        },
        use => {
            summary => 'Additional modules to "use"',
            'summary.alt.numnoun.singular' => 'Additional module to "use"',
            schema => ['array*' => of => 'str*'],
            description => <<'_',

Will be passed to the tracer. Will currently only affect the `fatpacker` and
`require` methods (because those methods actually run your script).

_
            tags => ['category:module-selection'],
            'x.schema.element_entity' => 'modulename',
        },
        args => {
            summary => 'Script arguments',
            'summary.alt.numnoun.singular' => 'Script argument',
            description => <<'_',

Will be used when running your script, e.g. when `trace_method` is `require`.
For example, if your script requires three arguments: `--foo`, `2`, `"bar baz"`
then you can either use:

    % fatten script output --args --foo --args 2 --args "bar baz"

or:

    % fatten script output --args-json '["--foo",2,"bar baz"]'

_
            schema => ['array*' => of => 'str*'],
        },

        shebang => {
            summary => 'Set shebang line/path',
            schema => 'str*',
            default => '/usr/bin/perl',
        },

        squish => {
            summary => 'Whether to squish included modules using Perl::Squish',
            schema => ['bool' => default=>0],
            tags => ['category:stripping'],
        },

        strip => {
            summary => 'Whether to strip included modules using Perl::Strip',
            schema => ['bool' => default=>0],
            tags => ['category:stripping'],
        },

        stripper => {
            summary => 'Whether to strip included modules using Perl::Stripper',
            schema => ['bool' => default=>0],
            tags => ['category:stripping'],
        },
        stripper_maintain_linum => {
            summary => "Set maintain_linum=1 in Perl::Stripper",
            schema => ['bool'],
            default => 0,
            tags => ['category:stripping'],
            description => <<'_',

Only relevant when stripping using Perl::Stripper.

_
        },
        stripper_ws => {
            summary => "Set strip_ws=1 (strip whitespace) in Perl::Stripper",
            'summary.alt.bool.not' => "Set strip_ws=0 (don't strip whitespace) in Perl::Stripper",
            schema => ['bool'],
            default => 1,
            tags => ['category:stripping'],
            description => <<'_',

Only relevant when stripping using Perl::Stripper.

_
        },
        stripper_comment => {
            summary => "Set strip_comment=1 (strip comments) in Perl::Stripper",
            'summary.alt.bool.not' => "Set strip_comment=0 (don't strip comments) in Perl::Stripper",
            schema => ['bool'],
            default => 1,
            description => <<'_',

Only relevant when stripping using Perl::Stripper.

_
            tags => ['category:stripping'],
        },
        stripper_pod => {
            summary => "Set strip_pod=1 (strip POD) in Perl::Stripper",
            'summary.alt.bool.not' => "Set strip_pod=0 (don't strip POD) in Perl::Stripper",
            schema => ['bool'],
            default => 1,
            tags => ['category:stripping'],
            description => <<'_',

Only relevant when stripping using Perl::Stripper.

_
        },
        stripper_log => {
            summary => "Set strip_log=1 (strip log statements) in Perl::Stripper",
            'summary.alt.bool.not' => "Set strip_log=0 (don't strip log statements) in Perl::Stripper",
            schema => ['bool'],
            default => 0,
            tags => ['category:stripping'],
            description => <<'_',

Only relevant when stripping using Perl::Stripper.

_
        },
        # XXX strip_log_levels

        debug_keep_tempdir => {
            summary => 'Keep temporary directory for debugging',
            schema => ['bool' => default=>0],
            tags => ['category:debugging'],
        },
    },
    deps => {
        exec => 'fatpack',
    },
};
sub fatten {
    require Cwd;
    require File::MoreUtil;
    require File::Spec;
    require File::Temp;

    my %args = @_;
    my $self = __PACKAGE__->new(%args);

    my $tempdir = File::Temp::tempdir(CLEANUP => 0);
    $log->debugf("Created tempdir %s", $tempdir);
    $self->{tempdir} = $tempdir;

    # for convenience of completion in bash, we allow / to separate namespace.
    # we convert it back to :: here.
    for (@{ $self->{exclude} // [] },
         @{ $self->{exclude_dist} // [] },
         @{ $self->{include} // [] },
         @{ $self->{include_dist} // [] },
         @{ $self->{use} // [] },
     ) {
        s!/!::!g;
        s/\.pm\z//;
    }

    # my understanding is that fatlib contains the stuffs beside the pure-perl
    # .pm files, and currently won't pack anyway.
    #mkdir "$tempdir/fatlib";
    mkdir "$tempdir/lib";

    $self->{perl_version} //= $^V;
    $self->{perl_version} = version->parse($self->{perl_version});
    $log->debugf("Will be targetting perl %s", $self->{perl_version});

    if ($self->{input_file} eq '-') {
        $self->{input_file_is_stdin} = 1;
        $self->{input_file} = $self->{abs_input_file} = (File::Temp::tempfile())[1];
        open my($fh), ">", $self->{abs_input_file}
            or return [500, "Can't write temporary input file '$self->{abs_input_file}': $!"];
        local $_; while (<STDIN>) { print $fh $_ }
        $self->{output_file} //= '-';
    } else {
        (-f $self->{input_file})
            or return [500, "No such input file: $self->{input_file}"];
        $self->{abs_input_file} = Cwd::abs_path($self->{input_file}) or return
            [500, "Can't find absolute path of input file $self->{input_file}"];
    }

    my $output_file;
    {
        $output_file = $self->{output_file};
        if (defined $output_file) {
            if ($output_file eq '-') {
                $self->{output_file_is_stdout} = 1;
                $self->{output_file} = $self->{abs_output_file} = (File::Temp::tempfile())[1];
                last;
            } else {
                return [412, "Output file '$output_file' exists, won't overwrite (see --overwrite)"]
                    if File::MoreUtil::file_exists($output_file) && !$self->{overwrite};
                last if open my($fh), ">", $output_file;
                return [500, "Can't write to output file '$output_file': $!"];
            }
        }

        my ($vol, $dir, $file) = File::Spec->splitpath($self->{input_file});
        my $fh;

        # try <input>.fatpack in the source directory
        $output_file = File::Spec->catpath($vol, $dir, "$file.fatpack");
        return [412, "Output file '$output_file' exists, won't overwrite (see --overwrite)"]
            if File::MoreUtil::file_exists($output_file) && !$self->{overwrite};
        last if open $fh, ">", $output_file;

        # if failed, try <input>.fatpack in the current directory
        $output_file = "$CWD/$file.fatpack";
        return [412, "Output file '$output_file' exists, won't overwrite (see --overwrite)"]
            if File::MoreUtil::file_exists($output_file) && !$self->{overwrite};
        last if open $fh, ">", $output_file;

        # failed too, bail
        return [500, "Can't write $file.fatpack in source- as well as ".
                    "current directory: $!"];
    }
    $self->{output_file} = $output_file;
    $self->{abs_output_file} //= Cwd::abs_path($output_file) or return
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
        File::Path::remove_tree($tempdir);
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
# ABSTRACT:

=for Pod::Coverage ^(new)$

=head1 SYNOPSIS

This distribution provides command-line utility called L<fatten>.


=head2 TODO

=over

=back

=cut
