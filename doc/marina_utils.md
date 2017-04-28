

# Module marina_utils #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#ip_to_bin-1">ip_to_bin/1</a></td><td></td></tr><tr><td valign="top"><a href="#pack-1">pack/1</a></td><td></td></tr><tr><td valign="top"><a href="#sync_msg-2">sync_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td></td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr><tr><td valign="top"><a href="#uuid_to_string-1">uuid_to_string/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-2"></a>

### connect/2 ###

<pre><code>
connect(Ip::<a href="inet.md#type-socket_address">inet:socket_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; {ok, <a href="inet.md#type-socket">inet:socket()</a>} | {error, atom()}
</code></pre>
<br />

<a name="ip_to_bin-1"></a>

### ip_to_bin/1 ###

<pre><code>
ip_to_bin(Ip::string()) -&gt; binary()
</code></pre>
<br />

<a name="pack-1"></a>

### pack/1 ###

<pre><code>
pack(Iolist::binary() | iolist()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

<a name="sync_msg-2"></a>

### sync_msg/2 ###

<pre><code>
sync_msg(Socket::<a href="inet.md#type-socket">inet:socket()</a>, Msg::iodata()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="timeout-2"></a>

### timeout/2 ###

<pre><code>
timeout(Timeout::pos_integer(), Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; integer()
</code></pre>
<br />

<a name="unpack-1"></a>

### unpack/1 ###

<pre><code>
unpack(X1::binary()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

<a name="uuid_to_string-1"></a>

### uuid_to_string/1 ###

<pre><code>
uuid_to_string(X1::&lt;&lt;_:128&gt;&gt;) -&gt; list()
</code></pre>
<br />

