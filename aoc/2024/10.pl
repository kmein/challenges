#!/usr/bin/env perl
use strict;
use warnings;

sub read_file {
  my @grid;

  my $filename = '10.txt';
  if (defined $ENV{'AOC_TEST'} && $ENV{'AOC_TEST'} ne '') {
    $filename = '10.txt.test';
  }
  open my $fh, '<', $filename or die "Could not open file '$filename': $!";

  while (my $line = <$fh>) {
    chomp $line;
    my @row = map { $_ eq '.' ? -1 : $_ } split //, $line;
    push @grid, \@row;
  }

  close $fh;
  return @grid;
}


sub find_paths {
  my ($grid_ref) = @_;
  my @paths;
  for my $i (0 .. $#$grid_ref) {
    for my $j (0 .. $#{$grid_ref->[$i]}) {
      if ($grid_ref->[$i][$j] == 0) {
        my @path;
        dfs($grid_ref, $i, $j, \@path, \@paths);
      }
    }
  }

  return @paths;
}

sub dfs {
  my ($grid_ref, $i, $j, $path_ref, $paths_ref) = @_;
  my $current_value = $grid_ref->[$i][$j];

  push @$path_ref, [$i, $j];

  if ($current_value == 9) {
    push @$paths_ref, [@$path_ref];
  } else {
    my @directions = (
      [-1, 0],  # Up
      [1, 0],   # Down
      [0, -1],  # Left
      [0, 1]    # Right
    );

    foreach my $dir (@directions) {
      my ($di, $dj) = @$dir;
      my $ni = $i + $di;
      my $nj = $j + $dj;

      # Check if the new indices are within bounds
      if ($ni >= 0 && $ni < @$grid_ref && $nj >= 0 && $nj < @{$grid_ref->[$ni]}) {
        my $next_value = $grid_ref->[$ni][$nj];
        # Only continue if the next value is exactly one greater
        if ($next_value == $current_value + 1) {
          dfs($grid_ref, $ni, $nj, $path_ref, $paths_ref);
        }
      }
    }
  }

  pop @$path_ref;
}

my @grid = read_file();
my @paths = find_paths(\@grid);

my %end_points;
my %path_count;

foreach my $path (@paths) {
  my $start = "$path->[0]->[0],$path->[0]->[1]";
  my $end = "$path->[-1]->[0],$path->[-1]->[1]";
  $end_points{$start}{$end} = 1;
  $path_count{$start}++;
}

my $score = 0;
my $rating = 0;

foreach my $start (keys %end_points) {
  my $unique_end_count = keys %{$end_points{$start}};
  my $count = $path_count{$start};
  $score += $unique_end_count;
  $rating += $count;
}

print "Score: $score\nRating: $rating\n";
