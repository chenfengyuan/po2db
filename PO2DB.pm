package PO2DB;
require Exporter;
use Encode;
use Carp;
our(@ISA, @EXPORT, $VERSION);
@ISA='Exporter';
@EXPORT=qw/trans_array headerinfo/;
$VERSION='0.01';
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
    if (eof) {
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
# return an array:lname text,lmail text,tname text,tmail text,charset text,pforms text
sub headerinfo{
    my $fn=shift;
    open my $in,'<',$fn;
    my($trans,$trans_e,$team,$team_e,$charset,$pf);
    while(<$in>){
	# "Last-Translator: Yinghua Wang <wantinghard@gmail.com>\n"
	if(/^"Last-Translator: *([^<]+[^ <]) *<([^>]+)>/){
	    $trans=$1;
	    $trans_e=$2;
	}
	# "Language-Team: Chinese (simplified) <i18n-zh@googlegroups.com>\n"
	if(/^"Language-Team: *([^<]+[^ <]) *<([^>]+)>/){
	    $team=$1;
	    $team_e=$2;
	}
	if(/^"Content-Type: text\/plain; charset=([^ ]+) *\\n"/){
	$charset=$1;
    }
	# "Plural-Forms: nplurals=1; plural=0;\n"
	if(/^"Plural-Forms: *(.+[^ ]) *\\n"/){
	    $pf=$1;
	}
    }
    &escape(\$trans,\$trans_e,\$team,\$team_e,\$charset,\$pf);
    return ($trans,$trans_e,$team,$team_e,$charset,$pf);
}
