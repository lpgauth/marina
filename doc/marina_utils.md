

# Module marina_utils #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_name-1">child_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#child_specs-0">child_specs/0</a></td><td></td></tr><tr><td valign="top"><a href="#info_msg-2">info_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#pack-1">pack/1</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td></td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr><tr><td valign="top"><a href="#warning_msg-2">warning_msg/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_name-1"></a>

### child_name/1 ###

<pre><code>
child_name(N::integer()) -&gt; atom()
</code></pre>
<br />

<a name="child_specs-0"></a>

### child_specs/0 ###

<pre><code>
child_specs() -&gt; [<a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>]
</code></pre>
<br />

<a name="info_msg-2"></a>

### info_msg/2 ###

<pre><code>
info_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::term(), Values::[{term(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="pack-1"></a>

### pack/1 ###

<pre><code>
pack(Iolist::binary() | iolist()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

<a name="timeout-2"></a>

### timeout/2 ###

<pre><code>
timeout(Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>, Timeout::non_neg_integer()) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="unpack-1"></a>

### unpack/1 ###

<pre><code>
unpack(X1::binary()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

<a name="warning_msg-2"></a>

### warning_msg/2 ###

<pre><code>
warning_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

