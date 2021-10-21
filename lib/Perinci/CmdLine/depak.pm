package Perinci::CmdLine::depak;

use 5.010;
use strict;
use Log::ger;
use parent qw(Perinci::CmdLine::Lite);

# AUTHORITY
# DATE
# DIST
# VERSION

sub hook_before_read_config_file {
    my ($self, $r) = @_;

    if (defined $r->{config_profile}) {
        log_trace("[pericmd-depak] Using config profile '%s' (predefined)",
                     $r->{config_profile});
        return;
    }

    # this is a hack, not proper cmdline arg parsing like in parse_argv().

    my $input_file;
    my $in_args;
    for my $i (0..$#ARGV) {
        my $arg = $ARGV[$i];
        if ($arg eq '--') {
            $in_args++;
            next;
        }
        if ($arg =~ /^-/ && !$in_args) {
            if ($arg =~ /^(-i|--input-file)$/ && $i < $#ARGV) {
                $input_file = $ARGV[$i+1];
                last;
            }
        }
        if ($in_args) {
            $input_file = $arg;
            last;
        }
    }

    unless (defined $input_file) {
        log_trace("[pericmd-depak] Not selecting config profile (no input file defined)");
        return;
    }

    require File::Spec;
    my ($vol, $dir, $name) = File::Spec->splitpath($input_file);
    log_trace("[pericmd-depak] Selecting config profile '%s' (from input file)", $name);
    $r->{config_profile} = $name;
    $r->{ignore_missing_config_profile_section} = 1;
}

1;
# ABSTRACT: Subclass of Perinci::CmdLine::Lite to set config_profile default

=head1 DESCRIPTION

This subclass sets default config_profile to the name of input script, for
convenience. So for example:

 % depak -i ~/proj/Bar-Baz/bin/bar

will automatically set config_profile to C<bar>, as if you had written:

 % depak -i ~/proj/Bar-Baz/bin/bar --config-profile bar

Of course, you can explicitly set C<--config-profile> to something else to
override this.
