

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_execute-5">async_execute/5</a></td><td></td></tr><tr><td valign="top"><a href="#async_prepare-2">async_prepare/2</a></td><td></td></tr><tr><td valign="top"><a href="#async_query-4">async_query/4</a></td><td></td></tr><tr><td valign="top"><a href="#async_reusable_query-6">async_reusable_query/6</a></td><td></td></tr><tr><td valign="top"><a href="#execute-5">execute/5</a></td><td></td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td></td></tr><tr><td valign="top"><a href="#query-4">query/4</a></td><td></td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td></td></tr><tr><td valign="top"><a href="#reusable_query-5">reusable_query/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_execute-5"></a>

### async_execute/5 ###

`async_execute(StatementId, Values, Pid, ConsistencyLevel, Flags) -> any()`


<a name="async_prepare-2"></a>

### async_prepare/2 ###

`async_prepare(Query, Pid) -> any()`


<a name="async_query-4"></a>

### async_query/4 ###

`async_query(Query, Pid, ConsistencyLevel, Flags) -> any()`


<a name="async_reusable_query-6"></a>

### async_reusable_query/6 ###

`async_reusable_query(Query, Values, Pid, ConsistencyLevel, Flags, Timeout) -> any()`


<a name="execute-5"></a>

### execute/5 ###

`execute(StatementId, Values, ConsistencyLevel, Flags, Timeout) -> any()`


<a name="prepare-2"></a>

### prepare/2 ###

`prepare(Query, Timeout) -> any()`


<a name="query-4"></a>

### query/4 ###

`query(Query, ConsistencyLevel, Flags, Timeout) -> any()`


<a name="response-1"></a>

### response/1 ###

`response(X1) -> any()`


<a name="reusable_query-5"></a>

### reusable_query/5 ###

`reusable_query(Query, Values, ConsistencyLevel, Flags, Timeout) -> any()`


