##############################################
# $Id: 98_LCN.pm $
package main;

use strict;
use warnings;
use Time::HiRes qw(gettimeofday);
use HttpUtils;
use Blocking;
use IO::Socket;
use SetExtensions;

sub LCN_Write($$$);
sub LCN_Read($);
sub LCN_DoInit($$$);

my $msgstart = pack('H*', "81");# Every msg starts with this

# See also "FHZ1000 Protocol" http://fhz4linux.info/tiki-index.php?page=FHZ1000%20Protocol

# NOTE: for protocol analysis, especially the "serial" vs. "FHTcode" case
# is interestingly different yet similar:
# - code 0x84 (FHZ area) vs. 0x83 (FHT area),
# - register 0x57, _read_ vs. 0x9e, _write_ (hmm, or is this "house code" 0x9e01?)
# - _read_ 8 nibbles (4 bytes serial), _write_ 1 (1 byte FHTcode - align-corrected to two nibbles, right?)
# I did some few tests already (also scripted tests), no interesting findings so far,
# but despite that torture my 1300PC still works fine ;)

my %gets = (
  "temp"  => "c9 02011f64",
);
my %sets = (
  "reopen"   => "xx xx",
  "close"   => "xx xx",
  "open"   => "xx xx",

);
my %setnrparam = (
  "reopen"   => 0,
  "close"    => 0,
  "open"    => 0,

);

#####################################
# Note: we are a data provider _and_ a consumer at the same time
sub
LCN_Initialize($)
{
  my ($hash) = @_;

# Provider
  $hash->{ReadFn}  = "LCN_Read";
  $hash->{WriteFn} = "LCN_Write";
  $hash->{Clients} = ":LCN:";
  my %mc = (
  	"1:LCN" => ".M.*",
  );
  $hash->{MatchList} = \%mc;
  $hash->{ReadyFn} = "LCN_Ready";

# Consumer
  $hash->{Match}   = ".M.*";
  $hash->{ParseFn} = "LCN_Parse";

# Normal devices
  $hash->{DefFn}   = "LCN_Define";
  #$hash->{FingerprintFn} = "LCN_FingerprintFn";
  $hash->{UndefFn} = "LCN_Undef";
  $hash->{GetFn}   = "LCN_Get";
  $hash->{SetFn}   = "LCN_Set";
  $hash->{AttrList}= "IODev level do_not_notify:1,0 dummy:1,0 " .
                   "showtime:1,0 lcnmodel:linhk lcngroup subType model ".
               	   $readingFnAttributes;
}


#####################################
sub
LCN_Ready($)
{
  my ($hash) = @_;
  return DevIo_OpenDev($hash, 1, "Test_DoInit");
}

#####################################
sub
LCN_Set($@)
{
  my ($hash, @a) = @_;

  my $ret = undef;
  my $na = int(@a);
  my $name = $a[0];	

  return "Need one to three parameter" if(@a < 2);
  #return "Unknown argument $a[1], choose one of " . join(" ", sort keys %sets)
  #	if(!defined($sets{$a[1]}));
  #return "Need one to three parameter" if(@a > 4);
  #return "Wrong number of parameters for $a[1], need " . ($setnrparam{$a[1]}+2)
  #	if(@a != ($setnrparam{$a[1]} + 2));

  #my ($fn, $arg) = split(" ", $sets{$a[1]});

	my $fn = $a[1];
    my $group = AttrVal($name, "lcngroup", "");

	if (defined $fn and $fn ne "?") {
		my $value = $a[2];	
		my $segment = $hash->{segment};
		my $module = $hash->{module};
	    my $delay = 0;
		$delay = $a[3] if ($na > 2);
		$delay = $value if ($na < 4);	
	    my $command = $fn;
		
		if ($fn eq "add") {
			$command = "Free";
			$value = sprintf("GD+%03d",$value);
		} elsif ($fn eq "remove") {
			$command = "Free";
			$value = sprintf("GD-%03d",$value);
		} else {
		    if ($fn eq "toggle") {
			 my $s = ReadingsVal($name,"state","0");
		     $value = 100-$s;
		    } elsif ($fn eq "on") {
		     $value = 100;
		    } elsif ($fn eq "off") {
		     $value = 0;
		    } elsif (!($fn =~ /(Free|Key)/)) {
		      $value = $fn;
	            }
			if (!defined $delay) {
			  $delay = 0;
			}		
		}

  	 	Log3 $name, 3, "IOWrite "."$command $segment $module $value $delay $group";
	 	my $r1 = IOWrite($hash, "$command", "$command $segment $module ".$hash->{cmd}.$hash->{chnl}." $value $delay $group");			
	
		return undef;
	}

    my $list = "off:noArg on:noArg toggle:noArg statusRequest:noArg add remove";
    $list .= " state:slider,0,1,100 pct:slider,0,1,100 ";
    return SetExtensions($hash, $list, $name, @a);
  
}

#####################################

sub
LCN_Notify($$)
{
  my ($hash,$dev) = @_;
  my $name  = $hash->{NAME};
  my $type  = $hash->{TYPE};

  return if(!defined($attr{$name}{segment}));

  my $segment = $attr{$name}{segment};
  my $module = $attr{$name}{module};
  my $channel = $attr{$name}{chnl};
  

  Log3 $name, 2, "LCN notify $dev";
 
  return undef;
}

#####################################
sub
LCN_Get($@)
{
  my ($hash, @a) = @_;

  return "\"get LCN\" needs only one parameter" if(@a != 2);
  return "Unknown argument $a[1], choose one of " . join(",", sort keys %gets)
  	if(!defined($gets{$a[1]}));

  my ($fn, $arg) = split(" ", $gets{$a[1]});

  my $v = join(" ", @a);
  my $name = $hash->{NAME};
  Log3 $name, 2, "LCN get $v";

  $hash->{READINGS}{$a[1]}{VAL} = $v;
  $hash->{READINGS}{$a[1]}{TIME} = TimeNow();

  return "$a[0] $a[1] => $v";
}

#####################################
sub
LCN_DoInit($$$)
{
  my ($name,$type,$po) = @_;
  my @init;

  # Reset the counter
  my $hash = $defs{$name};
  $hash->{STATE} = "Initialized";
  return undef;
}

#####################################
sub
LCN_Define($$)
{
  my ($hash, $def) = @_;
  my @a = split("[ \t][ \t]*", $def);
  my $po;

  return "wrong syntax: define <name> LCN linhk IP" if(@a < 3 || @a > 7);

  delete $hash->{DeviceName};
  delete $hash->{FD};

  my $name = $a[0];
  my $type = $a[2];
  my $host 	= $a[3];
  
  $hash->{STATE} = "defined";
  if ($type eq "linhk") {
	$hash->{PARTIAL} = "";
    Log3 $name, 3, "LCN opening device $host:4114";
  	$host =~ s/http:\/\///;
	$hash->{DeviceName} = $host.":4114";
	my $ret = DevIo_OpenDev($hash, 0, "LCN_DoInit");
    Log3 $name, 3, "LCN opened device $host" if (defined $ret and $ret eq "");
  } else {
  	  my $segment = $a[3]; # unused
  	  my $module = $a[4];
  	  my $cmd = $a[5];
  	  my $chnl = $a[6];
  	 
	  $hash->{segment} = $segment;
	  $hash->{module} = $module;
	  $hash->{cmd} = $cmd;
	  $hash->{chnl} = $chnl;
	  $hash->{DEF_CODE} = "$segment$module$cmd$chnl";
	  
	  $attr{$name}{devStateIcon} = '{(LCN_devStateIcon($name),"toggle")}' if( !defined( $attr{$name}{devStateIcon} ) );
	 
  	  $hash->{NotifyFn}  = "LCN_Notify";
      $hash->{NOTIFYDEV} = "LCN";
	  	 
	  my $code = "$segment$module$cmd$chnl";
	  my $ncode = 1;
	  $modules{LCN}{defptr}{$code}{$name}   = $hash;
	  my $iodev = $type;
	  AssignIoPort($hash,$iodev) if( !$hash->{IODev} );
	  if(defined($hash->{IODev}->{NAME})) {
		Log3 $name, 3, "$name: I/O device is " . $hash->{IODev}->{NAME};
	  } else {
		Log3 $name, 1, "$name: no I/O device";
	  }	
	  
	  Log3 $name, 3, "LCN $type defptr $code";
  }

  return undef;
}

#####################################
sub
LCN_Undef($$)
{
  my ($hash, $arg) = @_;
  my $name = $hash->{NAME};

  if (defined $hash->{DeviceName}) {
	  DevIo_Disconnected($hash);
	  DevIo_CloseDev($hash);
  };

  my $code = $hash->{DEF_CODE};

  foreach my $d (sort keys %defs) {
    if(defined($defs{$d}) &&
       defined($defs{$d}{IODev}) &&
       $defs{$d}{IODev} == $hash)
      {
        Log3 $name, 2, "deleting port for $d";
        delete $defs{$d}{IODev};
      }
  }
  delete $modules{LCN}{defptr}{$code};
  
  return undef;
}

#####################################
sub
LCN_devStateIcon($)
{
  my($hash) = @_;
  $hash = $defs{$hash} if( ref($hash) ne 'HASH' );

  return undef if( !$hash );
  return undef if( $hash->{helper}->{group} );

  my $name = $hash->{NAME};

  return ".*:off:toggle"
         if( ReadingsVal($name,"state","off") eq "off" || ReadingsVal($name,"state","0") eq 0 );

  my $s = ReadingsVal($name,"state","100");
  $s="on" if( $s eq "100" );

  return ".*:$s:toggle";
 
}
sub
LCN_summaryFn($$$$)
{
  my ($FW_wname, $d, $room, $pageHash) = @_; # pageHash is set for summaryFn.
  my $hash   = $defs{$d};
  my $name = $hash->{NAME};

  return LCN_devStateIcon($hash);
}

#####################################
sub
LCN_Parse($$)
{
  my ($hash,$msg) = @_;
  my $name = $hash->{NAME};
  my $omsg = $msg;
  $msg = substr($msg, 2);	# The first 2 bytes are not really interesting

  Log3 $hash, 4, $name.": Parsing: $omsg";

  if ($omsg =~ /\%M(\d\d\d)(\d\d\d).(\d*)/) { #%M000039.01205
  
	my $segment = $1; # unused
	my $module = $2;
	my $value = $3;
  	
	my $def = $modules{LCN}{defptr}{"$segment$module"."A1"};
	if ($def) {
		my @list;
        foreach my $n (keys %{ $def }) {
  		  Log3 $hash, 4, $name."...."."$segment$module $value ".$n;
          my $lh = $def->{$n};
          $n = $lh->{NAME};        
		  return "" if(IsIgnored($n));   # Little strange.
		  
  		  readingsSingleUpdate($lh, "temp", ($value/10)-100, 1);
		  push(@list, $n);
	    }
		return @list;
	}
  
  } elsif ($omsg =~ /=M(\d\d\d)(\d\d\d).N(\d)(.*)/) {
  	my $segment = $1; # unused
  	my $module = $2;
	my $part = $3;
	my $value = $4;
	
	Log3 $hash, 4, $name.": Naming $module $part  = $value";
	
	if ($part == 1) {
		readingsSingleUpdate($hash, "Name_$segment$module",$value, 1);		
	} else {
		my $name1 = ReadingsVal($name, "Name_$segment$module", "");
		readingsSingleUpdate($hash, "Name_$segment$module",$name1.$value, 1);				
	}
	
  } elsif ($omsg =~ /.M(\d\d\d)(\d\d\d)(A|Bx|Rx)(\d)(\d*)/) {
	my $segment = $1; # unused
	my $module = $2;
	my $cmd = $3;
	my $chnl = $4;
	my $value = $5;
	
	if ($cmd ne "A" and defined $value) {
	  $value = $chnl.$value;
	  $chnl = "0";
	}

	Log3 $hash, 4, $name.": Parsing $msg $cmd $chnl  = $value";
	  
  	my $def = $modules{LCN}{defptr}{"$segment$module$cmd$chnl"};
    if($def) {
      my @list;
      foreach my $n (keys %{ $def }) {
		  Log3 $hash, 4, $name."...."."$segment$module$cmd$chnl ".$n;
        my $lh = $def->{$n};
        $n = $lh->{NAME};        # It may be renamed
		
		Log3 $n, 2, "Strange..." if(IsIgnored($n));
        return "" if(IsIgnored($n));   # Little strange.

		if ($cmd eq "Rx") {
	        readingsSingleUpdate($lh, "state", sprintf("%08b",$value), 1);
		} elsif ($cmd eq "Bx") {
	        readingsSingleUpdate($lh, "state", sprintf("%08b",$value), 1);
		} else {
	        readingsSingleUpdate($lh, "state", "$value", 1);			
		}

        Log3 $n, 4, "LCN $n $omsg";
		push(@list, $n);
	}
	return @list;
   } else {
	  Log3 $hash, 3, "LCN Unknown device $segment$module$cmd$chnl, $value";
	  return "UNDEFINED LCN_$segment$module$cmd$chnl LCN $name $segment $module $cmd $chnl";
   }	
  }

  Log3 $name, 4, "LCN $name: Parse $msg";
  $hash->{CHANGED}[0] = "LCN: $msg";
  return $hash->{NAME};
}

#####################################
sub
LCN_Write($$$)
{
    my ($hash,$fn,$msg) = @_;
	my $name = $hash->{NAME};

#LCN Write 50 000 023 A1 50
#2014.02.06 19:44:55 2: send >M000000!DI000000
	
    Log3 $name, 2, "LCN Write ".$msg;
    # ex: Test_Write A1, A1 000 024 0,

    $msg =~ /(\S) (\d{1,3}) (\d{1,3}) (..) (.*)/;

    my $output = $4;
    my $segment = $2;
    my $module = $3;
    my ($v2, $duration, $group) = split(" ", $5);

  	my $sendString;
  
    if (defined $group and $group eq "1") {
		$sendString = ">G";
	} else {
		$sendString = ">M";
	}
  
	if($fn eq "Temp") {
	} elsif ($fn eq "Key") {
		$sendString .= sprintf("%03i%03i!TS%s",$segment, $module, $v2) ;
	} elsif ($fn eq "Free") {
		$sendString .= sprintf("%03i%03i!%s",$segment, $module, $v2) ;			
	} else {
		if ($output eq "AY") {
		    $sendString .= sprintf("%03i%03i!%s%03d%03d",$segment, $module, $output, $v2, $duration) ;		
		} else {
		    $sendString .= sprintf("%03i%03i!%sDI%03d%03d",$segment, $module, $output, $v2, $duration) ;					
		}
	}	
  
  	if (length($sendString)>0) {
  	   Log3 $hash, 2, "send $sendString";
  	   DevIo_SimpleWrite($hash, "$sendString\n", 0);
  	}

    if(!$hash || !defined($hash->{DeviceName})) {
      Log3 $hash, 3, "LCN device $hash->{NAME} is not active, cannot send";
      return;
    }  
  
}

#####################################
sub
LCN_Read($)
{
  my ($hash) = @_;

  my $buffer = DevIo_SimpleRead($hash);
  my $iohash = $modules{$hash->{TYPE}}; # Our (FHZ) module pointer
  my $name = $hash->{NAME};


  my $culdata = $hash->{PARTIAL};
  $culdata .= $buffer;
	
  while($culdata =~ m/\n/) {
	my $rmsg;
		
	($rmsg,$culdata) = split("\n", $culdata, 2);
	$rmsg =~ s/\r//;

    Log3 $name, 3, "LCN: Callback called: Hash: ".$iohash.", Name: $name, buffer: $rmsg";
  
		
	if ($rmsg =~ /Username/) {
	    DevIo_SimpleWrite($hash, "guest\n", 0);
	}
	if ($rmsg =~ /Password/) {
	  DevIo_SimpleWrite($hash, "+3uf3l\n", 0);
	}	
	
	if ($rmsg =~ /:M(\d\d\d)(\d\d\d)(A|Bx|Rx)(\d)(\d*)/) {
		Log3 $name, 4, "LCN: Dispatch $rmsg";
		
		$hash->{"${name}_MSGCNT"}++;
		$hash->{"${name}_TIME"} = TimeNow();
		$hash->{RAWMSG} = $rmsg;
		my %addvals = (RAWMSG => $rmsg);
		my $foundp = Dispatch($hash, $rmsg, \%addvals);
		#Log3 $name, 5, "found:$foundp" if (defined($foundp));
	}	
		
	if ($rmsg =~ /=M(\d\d\d)(\d\d\d).N\d/) {
		Log3 $name, 4, "LCN: Dispatch $rmsg";
		
		$hash->{"${name}_MSGCNT"}++;
		$hash->{"${name}_TIME"} = TimeNow();
		$hash->{RAWMSG} = $rmsg;
		my %addvals = (RAWMSG => $rmsg);
		my $foundp = Dispatch($hash, $rmsg, \%addvals);
	}
		
	if ($rmsg =~ /\%M(\d\d\d)(\d\d\d).(\d*)/) { #%M000039.01205
		Log3 $name, 4, "LCN: Dispatch $rmsg";
		
		$hash->{"${name}_MSGCNT"}++;
		$hash->{"${name}_TIME"} = TimeNow();
		$hash->{RAWMSG} = $rmsg;
		my %addvals = (RAWMSG => $rmsg);
		my $foundp = Dispatch($hash, $rmsg, \%addvals);
	}
		
  }

  $hash->{PARTIAL} = $culdata;

}

1;

=pod
=begin html

<a name="FHZ"></a>
<h3>FHZ</h3>
<ul>
  Note: this module requires the Device::SerialPort or Win32::SerialPort module
  if the devices is connected via USB or a serial port.
  <br><br>

  <a name="FHZdefine"></a>
  <b>Define</b>
  <ul>
    <code>define &lt;name&gt; FHZ &lt;serial-device&gt;</code> <br>
    <br>
    Specifies the serial port to communicate with the FHZ1000PC or FHZ1300PC.
    The name(s) of the serial-device(s) depends on your distribution. <br>

    If the serial-device is called none, then no device will be opened, so you
    can experiment without hardware attached.<br>

    The program can service multiple devices, FS20 and FHT device commands will
    be sent out through the last FHZ device defined before the definition of
    the FS20/FHT device. To change the association, use the IODev attribute.<br>
    <br>

    For GNU/Linux you may want to read our <a href="linux.html">hints for
    GNU/Linux</a> about <a href="linux.html#multipledevices">multiple USB
    devices</a>.<br>

    <b>Note:</b>The firmware of the FHZ1x00 will drop commands if the airtime
    for the last hour would exceed 1% (which corresponds roughly to 163
    commands). For this purpose there is a command counter for the last hour
    (see list FHZDEVICE), which triggers with "TRANSMIT LIMIT EXCEEDED" if
    there were more than 163 commands in the last hour.<br><br>

    If you experience problems (for verbose 4 you get a lot of "Bad CRC
    message" in the log), then try to define your device as <br> <code>define
    &lt;name&gt; FHZ &lt;serial-device&gt; strangetty</code><br>
  </ul>
  <br>

  <a name="FHZset"></a>
  <b>Set </b>
  <ul>
    <code>set FHZ &lt;variable&gt; [&lt;value&gt;]</code>
    <br><br>
    where <code>value</code> is one of:<br>
    <ul>
      FHTcode<br>
      initFS20<br>
      initHMS<br>
      stopHMS<br>
      initfull<br>
      raw<br>
      open<br>
      reopen<br>
      close<br>
      time<br>
    </ul>
    Notes:
    <ul>
      <li>raw is used to send out "raw" FS20/FHT messages (&quot;setters&quot; only - no query messages!).
          See message byte streams in FHEM/00_FHZ.pm and the doc directory for some examples.</li>
      <li>In order to set the time of your FHT's, schedule this command every
      minute:<br>
      <code>define fhz_timer at +*00:01:00 set FHZ time</code><br>
      See the <a href="#verbose">verbose</a> to prevent logging of
          this command.
      </li>
      <li>FHTcode is a two digit hex number (from 00 to 63?) and sets the
          central FHT code, which is used by the FHT devices. After changing
          it, you <b>must</b> reprogram each FHT80b with: PROG (until Sond
          appears), then select CEnt, Prog, Select nA.</li>
      <li>If the FHT ceases to work for FHT devices whereas other devices
          (e.g. HMS, KS300) continue to work, a<ul>
          <code>set FHZ initfull</code></ul> command could help. Try<ul>
          <code>set FHZ reopen</code></ul> if the FHZ
          ceases to work completely. If all else fails, shutdown fhem, unplug
          and replug the FHZ device. Problems with FHZ may also be related to
          long USB cables or insufficient power on the USB - use a powered hub
          to improve this particular part of such issues.
          See <a href="http://www.fhem.de/USB.html">our USB page</a>
          for detailed USB / electromag. interference troubleshooting.</li>
      <li><code>initfull</code> issues the initialization sequence for the FHZ
          device:<br>
          <ul><code>
            get FHZ init2<br>
            get FHZ serial<br>
            set FHZ initHMS<br>
            set FHZ initFS20<br>
            set FHZ time<br>
            set FHZ raw 04 01010100010000<br>
          </code></ul></li>
      <li><code>reopen</code> closes and reopens the serial device port. This
          implicitly initializes the FHZ and issues the
          <code>initfull</code> command sequence.</li>
      <li><code>stopHMS</code> probably is the inverse of <code>initHMS</code>
          (I don't have authoritative info on what exactly it does).</li>
      <li><code>close</code> closes and frees the serial device port until you open
          it again with <code>open</code>, e.g. useful if you need to temporarily
          unload the ftdi_sio kernel module to use the <a href="http://www.ftdichip.com/Support/Documents/AppNotes/AN232B-01_BitBang.pdf" target="_blank">bit-bang mode</a>.</li>

    </ul>
  </ul>
  <br>

  <a name="FHZget"></a>
  <b>Get</b>
  <ul>
    <code>get FHZ &lt;value&gt;</code>
    <br><br>
    where <code>value</code> is one of:<br>
    <ul>
      init1<br>
      init2<br>
      init3<br>
      serial<br>
      fhtbuf<br>
    </ul>
    Notes:
    <ul>
      <li>The mentioned codes are needed for initializing the FHZ1X00</li>
      <li>The answer for a command is also displayed by <code>list FHZ</code>
      </li>
      <li>
          The FHZ1x00PC has a message buffer for the FHT (see the FHT entry in
          the <a href="#set">set</a> section). If the buffer is full, then newly
          issued commands will be dropped, if the attribute <a
          href="#fhtsoftbuffer">fhtsoftbuffer</a> is not set.
          <code>fhtbuf</code> returns the free memory in this buffer (in hex),
          an empty buffer in the FHZ1000 is 2c (42 bytes), in the FHZ1300 is 4a
          (74 bytes). A message occupies 3 + 2x(number of FHT commands) bytes,
          this is the second reason why sending multiple FHT commands with one
          <a href="#set"> set</a> is a good idea. The first reason is, that
          these FHT commands are sent at once to the FHT.
          </li>
    </ul>
  </ul>
  <br>

  <a name="FHZattr"></a>
  <b>Attributes</b>
  <ul>
    <a name="do_not_notify"></a>
    <li>do_not_notify<br>
    Disable FileLog/notify/inform notification for a device. This affects
    the received signal, the set and trigger commands.</li><br>

    <li><a href="#attrdummy">dummy</a></li><br>

    <li><a href="#showtime">showtime</a></li><br>

    <a name="loglevel"></a>
    <li>loglevel<br>
    <b>Note:</b>Deprecated! The module maintainer is encouraged to replace it
    with verbose.<br><br>

    Set the device loglevel to e.g. 6 if you do not wish messages from a
    given device to appear in the global logfile (FHZ/FS20/FHT).  E.g. to
    set the FHT time, you should schedule "set FHZ time" every minute, but
    this in turn makes your logfile unreadable.  These messages will not be
    generated if the FHZ attribute loglevel is set to 6.<br>
    On the other hand, if you have to debug a given device, setting its
    loglevel to a smaller value than the value of the global verbose attribute,
    it will output its messages normally seen only with higher global verbose
    levels.
    </li> <br>

    <li><a href="#model">model</a> (fhz1000,fhz1300)</li><br>

    <a name="fhtsoftbuffer"></a>
    <li>fhtsoftbuffer<br>
        As the FHZ command buffer for FHT devices is limited (see fhtbuf),
        and commands are only sent to the FHT device every 120 seconds,
        the hardware buffer may overflow and FHT commands get lost.
        Setting this attribute implements an "unlimited" software buffer.<br>
        Default is disabled (i.e. not set or set to 0).</li><br>
  </ul>
  <br>
</ul>



=end html
=cut
