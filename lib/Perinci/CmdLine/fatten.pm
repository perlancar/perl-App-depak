package Perinci::CmdLine::fatten;

# DATE
# VERSION

use 5.010;
use parent qw(Perinci::CmdLine::Lite);

sub hook_before_read_config_file {
    my ($self, $r) = @_;

    return if defined $r->{config_profile};

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
        if ($arg !~ /^-/ || $in_args) {
            $input_file = $arg;
            last;
        }
    }

    return unless defined $input_file;

    require File::Spec;
    my ($vol, $dir, $name) = File::Spec->splitpath($input_file);
    $r->{config_profile} = $name;
    $r->{ignore_missing_config_profile_section} = 1;
}

1;
# ABSTRACT: Subclass of Perinci::CmdLine::Lite to set config_profile default

=head1 DESCRIPTION

This subclass sets default config_profile to the name of input script, for
convenience.
