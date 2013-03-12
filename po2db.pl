#!/usr/bin/perl

BEGIN{
    use File::Basename;
    # if(-l $0){
    # 	use lib dirname($0) .'/'. dirname(readlink $0);
    # }else {
    use lib dirname($0);
    # }
}
use strict;
use warnings;
use Encode;
use PO2DB;
use DBI;
use Carp;

my $db_file="main.sqlite";

#get the db name
if (! ($ARGV[0]=~ /\.po$/)) {
    $db_file=shift;
}
# if(-e $db_file){
#     croak "\e[0;31m$db_file is existed\e[0m\n";
# }


#get the table suffix
my $table_suffix='default';
unless( -f $ARGV[0]){
    $table_suffix=(shift);
}
$table_suffix=~s/^-++//;
# if($table_suffix=~/([\/\.-])/){
#     croak "\e[0;31millegal table suffix [$1]\e[0m\n";
# }

my @po_files=sort @ARGV;
#try to reduce the path length
my ($path_f,$path_l,$path_pre)=(@po_files[0,$#po_files],'');
if (defined $path_f) {
    while ($path_f=~/($path_pre[^\/]*\/)/) {
	my $t=$1;
	last unless($path_l=~/^$path_pre/);
	$path_pre=$t;
    }
}


$table_suffix= $path_pre if $table_suffix eq 'default';
$table_suffix=~s/\/$//;
$table_suffix=~s/.*\///;
$table_suffix=~s/'//g;

my $t1="t_$table_suffix";	#translations
my $t2="h_$table_suffix";	#headerinfo
my $i1="i_$t1";
my $i2="i_$t2";

my $id=0;


my $dbh;
$dbh= DBI->connect("dbi:SQLite:dbname=$db_file","","");
my %tables=map {$$_[0],undef} @{$dbh->selectall_arrayref("SELECT name FROM sqlite_master WHERE type='table';")};
$dbh->do('BEGIN TRANSACTION');
# $dbh->do("drop table if exists $t1");
# $dbh->do("drop table if exists $t2");
if (exists $tables{$t1}) {
    (${[reverse sort grep {/${t1}/} keys %tables]}[0])=~/(\d+)$/;
    my $i;
    if (defined $1) {
	$i=$1;
	$i++;
    } else {
	$i='0';
    }
    $dbh->do("alter table '$t1' rename to '${t1}_$i'");
    $dbh->do("alter table '$t2' rename to '${t2}_$i'");
    $dbh->do("drop index if exists '$i1'");
    $dbh->do("drop index if exists '$i2'");
    $dbh->do("create index '${i1}_$i' on '${t1}_$i' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)");
    $dbh->do("create index '${i2}_$i' on '${t2}_$i' (pof,lname,lmail,tname,tmail,charset,pforms)");
    if ($i=~/^10+$/) {
	for my $j (0..$i-1) {
	    my $j1=sprintf "%0".(log($i)/log(10))."d",$j;
	    my $j2=sprintf "%0".(log($i)/log(10)+1)."d",$j;
	    $dbh->do("alter table '${t1}_$j1' rename to '${t1}_$j2'");
	    $dbh->do("alter table '${t2}_$j1' rename to '${t2}_$j2'");
	    $dbh->do("drop index if exists '${i1}_$j1'");
	    $dbh->do("drop index if exists '${i2}_$j1'");
	    $dbh->do("create index '${i1}_$j2' on '${t1}_$j2' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)");
	    $dbh->do("create index '${i2}_$j2' on '${t2}_$j2' (pof,lname,lmail,tname,tmail,charset,pforms)");
	}
    }
    #    $dbh->do('commit');$dbh->commit;exit;
}
$dbh->do("create table '$t1' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text)");
$dbh->do("create table '$t2' (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text)");


for my $fn (@po_files) {
    my $pof=$fn;
    $pof=~s/'/''/g;
    my($trans,$trans_e,$team,$team_e,$charset,$pf)=headerinfo($fn);
    $dbh->do("insert into '$t2' values('$pof','$trans','$trans_e','$team','$team_e','$charset','$pf')");
    my($msgid,$msgstr,$msgctxt,$flag,$fuzzy);
    for (trans_array($fn)) {
	($msgid,$msgstr,$msgctxt,$flag)=@$_;
	next if $msgid eq '';
	if ($flag=~/fuzzy/) {
	    $fuzzy=1;
	    $flag=join ",",grep !/fuzzy/,split ",",$flag;
	} else {
	    $fuzzy=0;
	    $flag=~s/ +//g;
	}
	++$id;
	$dbh->do("insert into '$t1' values($id,'$msgid','$msgstr','$msgctxt',$fuzzy,'$flag','$pof');");
    }
}
$dbh->do("create index '$i1' on '$t1' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)");
$dbh->do("create index '$i2' on '$t2' (pof,lname,lmail,tname,tmail,charset,pforms)");
$dbh->do('commit');
$dbh->commit;
