

# Module marina #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#query-0">query/0</a></td><td></td></tr><tr><td valign="top"><a href="#query-1">query/1</a></td><td></td></tr><tr><td valign="top"><a href="#query-2">query/2</a></td><td></td></tr><tr><td valign="top"><a href="#query-3">query/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="query-0"></a>

### query/0 ###

`query() -> any()`


<a name="query-1"></a>

### query/1 ###

`query(Query) -> any()`


<a name="query-2"></a>

### query/2 ###

`query(Query, ConsistencyLevel) -> any()`


<a name="query-3"></a>

### query/3 ###

`query(Query, ConsistencyLevel, Timeout) -> any()`


