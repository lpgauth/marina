

# Module marina_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-consistency_level">consistency_level()</a> ###


<pre><code>
consistency_level() = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
</code></pre>




### <a name="type-query_opts">query_opts()</a> ###


<pre><code>
query_opts() = #{consistency_level =&gt; <a href="#type-consistency_level">consistency_level()</a>, page_size =&gt; pos_integer(), paging_state =&gt; binary(), pid =&gt; pid(), routing_key =&gt; binary(), skip_metadata =&gt; boolean(), timeout =&gt; pos_integer(), values =&gt; <a href="#type-values">values()</a>}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = binary()
</code></pre>




### <a name="type-values">values()</a> ###


<pre><code>
values() = [<a href="#type-value">value()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#pack-1">pack/1</a></td><td></td></tr><tr><td valign="top"><a href="#query-2">query/2</a></td><td></td></tr><tr><td valign="top"><a href="#query_opts-2">query_opts/2</a></td><td></td></tr><tr><td valign="top"><a href="#sync_msg-2">sync_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td></td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="connect-2"></a>

### connect/2 ###

<pre><code>
connect(Ip::<a href="inet.md#type-socket_address">inet:socket_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; {ok, <a href="inet.md#type-socket">inet:socket()</a>} | {error, atom()}
</code></pre>
<br />

<a name="pack-1"></a>

### pack/1 ###

<pre><code>
pack(Iolist::binary() | iolist()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

<a name="query-2"></a>

### query/2 ###

<pre><code>
query(Socket::<a href="inet.md#type-socket">inet:socket()</a>, Query::iodata()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="query_opts-2"></a>

### query_opts/2 ###

<pre><code>
query_opts(X1::atom(), QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; term()
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

