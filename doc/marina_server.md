

# Module marina_server #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-2"></a>

### init/2 ###


<pre><code>
init(Parent::pid(), Name::atom()) -&gt; no_return()
</code></pre>
<br />


<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Name::atom()) -&gt; {ok, pid()}
</code></pre>
<br />


