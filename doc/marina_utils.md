

# Module marina_utils #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#pack-1">pack/1</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td></td></tr><tr><td valign="top"><a href="#unpack-1">unpack/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="pack-1"></a>

### pack/1 ###

<pre><code>
pack(Iolist::binary() | iolist()) -&gt; {ok, binary()} | {error, term()}
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

