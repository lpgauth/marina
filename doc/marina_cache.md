

# Module marina_cache #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-buffer">buffer()</a> ###



<pre><code>
buffer() = #buffer{buffered = undefined | iolist(), current = undefined | non_neg_integer(), pending = non_neg_integer() | undefined}
</code></pre>





### <a name="type-frame">frame()</a> ###



<pre><code>
frame() = #frame{flags = any(), stream = undefined | integer(), opcode = undefined | non_neg_integer(), body = undefined | binary()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td></td></tr></table>


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


