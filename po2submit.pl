#!/usr/bin/perl

use strict;
use warnings;
use Encode;
use Carp;

my $quote_text=qr/[^"\\]*+(?:(?:\\.)++[^"\\]*+)*+/;
sub read_tailing{
    my $fh=shift;
    my $s='';
    while (<$fh>) {
	if (/^"($quote_text)"/) {
	    $s.=decode('utf-8',$1);
	} else {
	    last;
	}
    }
    if (length $_ == 0 && eof) {
	$_='EOF';
    }
    return $s,$_;
}
sub escape{
    for (@_) {
	if (defined $$_) {
	    $$_=~s/'/''/g;
	} else {
	    $$_='';
	}
    }
}

# return an array,which item is "msgid text,msgstr text,msgctxt text,flag text"
sub trans_array{
    my $fn=shift;
    open my $in,'<',$fn;
    my($msgid,$msgstr,$msgctxt,$flag,@a);
    while (<$in>) {
	if (/^msgid(?:\[0\])?\s+"($quote_text)"/) {
	    $msgid=decode('utf-8',$1);
	    $msgstr=undef;
	    @_=&read_tailing($in);
	    $msgid.=$_[0];
	    $_=$_[1];
	    confess "undefined $. in $fn\n" unless defined($_);
	    redo;
	}
	if (/^msgstr(?:\[0\])?\s+"($quote_text)"/) {
	    confess "repeated $. in $fn\n" if defined($msgstr);
	    $msgstr=decode('utf-8',$1);
	    @_=&read_tailing($in);
	    $msgstr.=$_[0];
	    $_=$_[1];
	    confess "undefined $. in $fn\n" unless defined($_);
	    redo;
	}
	if (/^msgctxt\s+"($quote_text)"/) {
	    confess "repeated $. in $fn\n" if defined($msgctxt);
	    $msgctxt=decode('utf-8',$1);
	    @_=&read_tailing($in);
	    $msgctxt.=$_[0];
	    $_=$_[1];
	    confess "undefined $. in $fn\n" unless defined($_);
	    redo;
	}
	if (/^#,(.+)/) {
	    confess "repeated $. in $fn\n" if defined($flag);
	    $flag=join ",",split /\s*,\s*/,$1;
	}
	if ((/^\s*$/ || /^EOF$/)) {
	    if ((defined($msgid) || defined($msgstr))) {
		&escape(\$msgid,\$msgstr,\$msgctxt,\$flag);
		push @a,[$msgid,$msgstr,$msgctxt,$flag];
		undef $_ for($msgid,$msgstr,$msgctxt,$flag);
		next;
	    } else {
		next;
	    }
	}
	if (/^#~/) {
	    if (defined $flag) {
		undef $flag;
	    }
	    next;
	}
	if (/^#/) {
	    next;
	}
	if (/^m/) {
	    @_=&read_tailing($in);
	    $_=$_[1];
	    confess "undefined $. in $fn\n" unless defined($_);
	    redo;
	}
	confess "$. in $fn:[$_]\n";
    }
    return @a;
}

my ($msg_id,$msg_str);

for my $i (&trans_array(shift)){
    next if($$i[0] =~ /^$/);
    $msg_id.="\n".$$i[0];
    $msg_str.="\n".$$i[1];
}
$msg_id=~s/\\n/\n/g;
$msg_str=~s/\\n/\n/g;
$msg_id=~s/\n+/\n/g;
$msg_str=~s/\n+/\n/g;
$msg_id=~s/\n+\z//g;
$msg_str=~s/\n+\z//g;
$msg_id=~s/\n//;
$msg_str=~s/\n//;

$msg_id=encode "utf-8",$msg_id;
$msg_str=encode "utf-8",$msg_str;

# print "[$msg_id]\n[$msg_str]\n";
print "Description: $msg_id\n";
print "Description-zh_CN: $msg_str\n";
