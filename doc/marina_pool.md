

# Module marina_pool #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#node-0">node/0</a></td><td></td></tr><tr><td valign="top"><a href="#node-1">node/1</a></td><td></td></tr><tr><td valign="top"><a href="#node_id-1">node_id/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

<a name="node-0"></a>

### node/0 ###

<pre><code>
node() -&gt; {ok, atom()} | {error, marina_pool_not_started}
</code></pre>
<br />

<a name="node-1"></a>

### node/1 ###

<pre><code>
node(RoutingKey::binary() | undefined) -&gt; {ok, atom()} | {error, marina_pool_not_started}
</code></pre>
<br />

<a name="node_id-1"></a>

### node_id/1 ###

<pre><code>
node_id(X1::binary()) -&gt; atom()
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(X1::random | token_aware, Nodes::[{binary(), binary()}]) -&gt; ok
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(N::non_neg_integer()) -&gt; ok
</code></pre>
<br />

