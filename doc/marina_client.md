

# Module marina_client #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-buffer">buffer()</a> ###


<pre><code>
buffer() = #buffer{buffered = iolist(), current = non_neg_integer(), pending = non_neg_integer() | undefined}
</code></pre>




### <a name="type-frame_flag">frame_flag()</a> ###


<pre><code>
frame_flag() = 0..1
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{buffer = <a href="#type-buffer">buffer()</a>, frame_flags = <a href="#type-frame_flag">frame_flag()</a>, keyspace = binary() | undefined, requests = non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle_data-2">handle_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_request-2">handle_request/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#setup-2">setup/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-1">terminate/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle_data-2"></a>

### handle_data/2 ###

<pre><code>
handle_data(Data::binary(), State::<a href="#type-state">state()</a>) -&gt; {ok, [{pos_integer(), term()}], <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="handle_request-2"></a>

### handle_request/2 ###

<pre><code>
handle_request(X1::term(), State::<a href="#type-state">state()</a>) -&gt; {ok, pos_integer(), iodata(), <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="setup-2"></a>

### setup/2 ###

<pre><code>
setup(Socket::<a href="inet.md#type-socket">inet:socket()</a>, State::<a href="#type-state">state()</a>) -&gt; {ok, <a href="#type-state">state()</a>} | {error, atom(), <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="terminate-1"></a>

### terminate/1 ###

<pre><code>
terminate(State::<a href="#type-state">state()</a>) -&gt; ok
</code></pre>
<br />

