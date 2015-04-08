

# Module marina #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-consistency">consistency()</a> ###



<pre><code>
consistency() = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 16
</code></pre>





### <a name="type-flag">flag()</a> ###



<pre><code>
flag() = {page_size, pos_integer()} | {paging_state, binary()} | {skip_metadata, boolean()} | {values, boolean()}
</code></pre>





### <a name="type-query">query()</a> ###



<pre><code>
query() = binary()
</code></pre>





### <a name="type-statement_id">statement_id()</a> ###



<pre><code>
statement_id() = binary()
</code></pre>





### <a name="type-value">value()</a> ###



<pre><code>
value() = binary()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_execute-4">async_execute/4</a></td><td></td></tr><tr><td valign="top"><a href="#async_execute-5">async_execute/5</a></td><td></td></tr><tr><td valign="top"><a href="#async_prepare-2">async_prepare/2</a></td><td></td></tr><tr><td valign="top"><a href="#async_query-4">async_query/4</a></td><td></td></tr><tr><td valign="top"><a href="#async_reusable_query-5">async_reusable_query/5</a></td><td></td></tr><tr><td valign="top"><a href="#async_reusable_query-6">async_reusable_query/6</a></td><td></td></tr><tr><td valign="top"><a href="#execute-4">execute/4</a></td><td></td></tr><tr><td valign="top"><a href="#execute-5">execute/5</a></td><td></td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td></td></tr><tr><td valign="top"><a href="#query-4">query/4</a></td><td></td></tr><tr><td valign="top"><a href="#query-5">query/5</a></td><td></td></tr><tr><td valign="top"><a href="#receive_response-2">receive_response/2</a></td><td></td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td></td></tr><tr><td valign="top"><a href="#reusable_query-4">reusable_query/4</a></td><td></td></tr><tr><td valign="top"><a href="#reusable_query-5">reusable_query/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_execute-4"></a>

### async_execute/4 ###


<pre><code>
async_execute(StatementId::<a href="#type-statement_id">statement_id()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Pid::pid()) -&gt; {ok, reference()} | {error, backlog_full}
</code></pre>
<br />


<a name="async_execute-5"></a>

### async_execute/5 ###


<pre><code>
async_execute(StatementId::<a href="#type-statement_id">statement_id()</a>, Values::[<a href="#type-value">value()</a>], ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Pid::pid()) -&gt; {ok, reference()} | {error, backlog_full}
</code></pre>
<br />


<a name="async_prepare-2"></a>

### async_prepare/2 ###


<pre><code>
async_prepare(Query::<a href="#type-query">query()</a>, Pid::pid()) -&gt; {ok, reference()} | {error, backlog_full}
</code></pre>
<br />


<a name="async_query-4"></a>

### async_query/4 ###


<pre><code>
async_query(Query::<a href="#type-query">query()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Pid::pid()) -&gt; {ok, reference()} | {error, backlog_full}
</code></pre>
<br />


<a name="async_reusable_query-5"></a>

### async_reusable_query/5 ###


<pre><code>
async_reusable_query(Query::<a href="#type-query">query()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Pid::pid(), Timeout::timeout()) -&gt; {ok, reference()} | {error, term()}
</code></pre>
<br />


<a name="async_reusable_query-6"></a>

### async_reusable_query/6 ###


<pre><code>
async_reusable_query(Query::<a href="#type-query">query()</a>, Values::[<a href="#type-value">value()</a>], ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Pid::pid(), Timeout::timeout()) -&gt; {ok, reference()} | {error, term()}
</code></pre>
<br />


<a name="execute-4"></a>

### execute/4 ###


<pre><code>
execute(StatementId::<a href="#type-statement_id">statement_id()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="execute-5"></a>

### execute/5 ###


<pre><code>
execute(StatementId::<a href="#type-statement_id">statement_id()</a>, Values::[<a href="#type-value">value()</a>], ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="prepare-2"></a>

### prepare/2 ###


<pre><code>
prepare(Query::<a href="#type-query">query()</a>, Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="query-4"></a>

### query/4 ###


<pre><code>
query(Query::<a href="#type-query">query()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="query-5"></a>

### query/5 ###


<pre><code>
query(Query::<a href="#type-query">query()</a>, Values::[<a href="#type-value">value()</a>], ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="receive_response-2"></a>

### receive_response/2 ###


<pre><code>
receive_response(Ref::reference(), Timeout::non_neg_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="response-1"></a>

### response/1 ###


<pre><code>
response(X1::{ok, term()} | {error, term()}) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="reusable_query-4"></a>

### reusable_query/4 ###


<pre><code>
reusable_query(Query::<a href="#type-query">query()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


<a name="reusable_query-5"></a>

### reusable_query/5 ###


<pre><code>
reusable_query(Query::<a href="#type-query">query()</a>, Values::[<a href="#type-value">value()</a>], ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::[<a href="#type-flag">flag()</a>], Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />


