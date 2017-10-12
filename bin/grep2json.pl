#!/usr/bin/env perl

use feature ':5.14';
use strictures 2;
use warnings 'all';
$^W = 1;
use JSON ();

my $json = JSON->new->utf8->canonical->allow_nonref->allow_blessed->convert_blessed;

my $results = {};
while (<>) {
  my ($file,$line,$data) = split(':', $_, 3);
  chomp $data;
  $results->{$file} //= {};
  $results->{$file}{$line} = $data;
}

for my $file (keys %$results) {
  my $data = $results->{$file};
  $results->{$file} = [
    map {
      +{
        line => $_,
        data => $data->{$_},
      }
    } sort { $a <=> $b } keys %$data
  ];
}

$results = [
  sort {
    $a->{file} cmp $b->{file}
  } map {
    my $file = $_;
    my $data = $results->{$_};
    +{
      file => $file,
      results => $data,
    }
  } keys %$results
];

print $json->encode($_),"\n" for @$results;
