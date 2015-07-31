

# Module marina_client #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-frame_flag">frame_flag()</a> ###


<pre><code>
frame_flag() = {compression, boolean()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#after_connect-2">after_connect/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_data-2">handle_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-1">terminate/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="after_connect-2"></a>

### after_connect/2 ###

<pre><code>
after_connect(Socket::<a href="inet.md#type-socket">inet:socket()</a>, State::#state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}) -&gt; {ok, #state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}} | {error, atom(), #state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}}
</code></pre>
<br />

<a name="handle_cast-2"></a>

### handle_cast/2 ###

<pre><code>
handle_cast(Request::term(), State::#state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}) -&gt; {ok, pos_integer(), iodata(), #state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}}
</code></pre>
<br />

<a name="handle_data-2"></a>

### handle_data/2 ###

<pre><code>
handle_data(Data::binary(), State::#state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}) -&gt; {ok, [{pos_integer(), term()}], #state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}}
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; {ok, [{ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {reconnect, boolean()} | {state, #state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}}]}
</code></pre>
<br />

<a name="terminate-1"></a>

### terminate/1 ###

<pre><code>
terminate(State::#state{buffer = any(), frame_flags = [<a href="#type-frame_flag">frame_flag()</a>], keyspace = any(), requests = any()}) -&gt; ok
</code></pre>
<br />

