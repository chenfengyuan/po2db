#!/usr/bin/perl

use strict;
use warnings;
use Encode;
use PO2DB;
use DBI;
use Carp;

my $db_file="main.sqlite";
my $t1='translations';
my $t2='headerinfo';
my $i1="i_$t1";
my $i2="i_$t2";
if(! ($ARGV[0]=~ /\.po$/)){
    $db_file=shift;
}

if(-e $db_file){
    croak "\e[0;31m$db_file is existed\e[0m\n";
}
my $dbh;
$dbh= DBI->connect("dbi:SQLite:dbname=$db_file","","");
$dbh->do('BEGIN TRANSACTION');
$dbh->do("create table $t1 (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text)");
$dbh->do("create table $t2 (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text)");

my $id=0;

for my $fn (@ARGV){
    my $pof=$fn;
    $pof=~s/'/''/g;
    my($trans,$trans_e,$team,$team_e,$charset,$pf)=headerinfo($fn);
    $dbh->do("insert into $t2 values('$pof','$trans','$trans_e','$team','$team_e','$charset','$pf')");
    my($msgid,$msgstr,$msgctxt,$flag,$fuzzy);
    for(trans_array($fn)){
	($msgid,$msgstr,$msgctxt,$flag)=@$_;
	next if $msgid eq '';
	if($flag=~/fuzzy/){
	    $fuzzy=1;
	    $flag=join ",",grep !/fuzzy/,split ",",$flag;
	}
	else {
	    $fuzzy=0;
	}
	++$id;
	$dbh->do("insert into $t1 values($id,'$msgid','$msgstr','$msgctxt',$fuzzy,'$flag','$pof');");
    }
}
$dbh->do("create index $i1 on $t1 (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)");
$dbh->do("create index $i2 on $t2 (pof,lname,lmail,tname,tmail,charset,pforms)");
$dbh->do('commit');
$dbh->commit;
