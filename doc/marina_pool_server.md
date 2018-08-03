

# Module marina_pool_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{bootstrap_ips = list(), datacenter = undefined | binary(), node_count = undefined | pos_integer(), port = pos_integer(), strategy = random | token_aware, subscribers = [pid()], timer_ref = undefined | reference()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle_msg-2">handle_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#subscribe-1">subscribe/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle_msg-2"></a>

### handle_msg/2 ###

<pre><code>
handle_msg(X1::term(), State::<a href="#type-state">state()</a>) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(Name::atom(), Parent::pid(), X3::undefined) -&gt; no_return()
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

<a name="subscribe-1"></a>

### subscribe/1 ###

<pre><code>
subscribe(Pid::pid()) -&gt; ok | {error, marina_not_started}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::term(), State::<a href="#type-state">state()</a>) -&gt; ok
</code></pre>
<br />

