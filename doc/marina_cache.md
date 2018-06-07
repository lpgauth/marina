

# Module marina_cache #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#erase-2">erase/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="erase-2"></a>

### erase/2 ###

<pre><code>
erase(Pool::atom(), Key::binary()) -&gt; ok | {error, not_found}
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Pool::atom(), Key::binary()) -&gt; {ok, term()} | {error, not_found}
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; marina_cache
</code></pre>
<br />

<a name="put-3"></a>

### put/3 ###

<pre><code>
put(Pool::atom(), Key::binary(), Value::term()) -&gt; ok
</code></pre>
<br />

