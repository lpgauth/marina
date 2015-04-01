

# Module marina_request #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute-5">execute/5</a></td><td></td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td></td></tr><tr><td valign="top"><a href="#query-4">query/4</a></td><td></td></tr><tr><td valign="top"><a href="#startup-0">startup/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute-5"></a>

### execute/5 ###

`execute(Stream, StatementId, Values, ConsistencyLevel, Flags) -> any()`


<a name="prepare-2"></a>

### prepare/2 ###

`prepare(Stream, Query) -> any()`


<a name="query-4"></a>

### query/4 ###

`query(Stream, Query, ConsistencyLevel, Flags) -> any()`


<a name="startup-0"></a>

### startup/0 ###

`startup() -> any()`


