#!/usr/bin/env perl

use Mojo::Weixin;

my ($host,$port,$post_api);

$host = "0.0.0.0";
$port = 3000;
#$post_api = 'http://xxxx';

my $client = Mojo::Weixin->new(log_encoding=>"utf8",elog_level=>"info",http_debug=>0);
$client->load("ShowMsg");
$client->load("IRCShell", data=>{load_friend=>0});
$client->load("ShowQRCodeInTerm");
$client->load("Openwx",data=>{listen=>[{host=>$host,port=>$port}], post_api=>$post_api});
$client->run();
