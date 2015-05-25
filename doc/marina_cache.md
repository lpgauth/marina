

# Module marina_cache #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove-1">remove/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Key::binary()) -&gt; {ok, term()} | {error, not_found}
</code></pre>
<br />


<a name="init-0"></a>

### init/0 ###


<pre><code>
init() -&gt; marina_cache
</code></pre>
<br />


<a name="put-2"></a>

### put/2 ###


<pre><code>
put(Key::binary(), Value::term()) -&gt; true
</code></pre>
<br />


<a name="remove-1"></a>

### remove/1 ###


<pre><code>
remove(Key::binary()) -&gt; true | {error, not_found}
</code></pre>
<br />


