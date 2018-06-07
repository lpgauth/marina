

# Module marina #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-consistency_level">consistency_level()</a> ###


<pre><code>
consistency_level() = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, term()}
</code></pre>




### <a name="type-query">query()</a> ###


<pre><code>
query() = binary()
</code></pre>




### <a name="type-query_opts">query_opts()</a> ###


<pre><code>
query_opts() = #{consistency_level =&gt; <a href="#type-consistency_level">consistency_level()</a>, page_size =&gt; pos_integer(), paging_state =&gt; binary(), pid =&gt; pid(), routing_key =&gt; binary(), skip_metadata =&gt; boolean(), timeout =&gt; pos_integer(), values =&gt; <a href="#type-values">values()</a>}
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = binary()
</code></pre>




### <a name="type-values">values()</a> ###


<pre><code>
values() = [<a href="#type-value">value()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_query-2">async_query/2</a></td><td></td></tr><tr><td valign="top"><a href="#async_reusable_query-2">async_reusable_query/2</a></td><td></td></tr><tr><td valign="top"><a href="#query-2">query/2</a></td><td></td></tr><tr><td valign="top"><a href="#receive_response-1">receive_response/1</a></td><td></td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td></td></tr><tr><td valign="top"><a href="#reusable_query-2">reusable_query/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_query-2"></a>

### async_query/2 ###

<pre><code>
async_query(Query::<a href="#type-query">query()</a>, QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; {ok, reference()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="async_reusable_query-2"></a>

### async_reusable_query/2 ###

<pre><code>
async_reusable_query(Query::<a href="#type-query">query()</a>, QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; {ok, reference()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="query-2"></a>

### query/2 ###

<pre><code>
query(Query::<a href="#type-query">query()</a>, QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; {ok, term()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="receive_response-1"></a>

### receive_response/1 ###

<pre><code>
receive_response(RequestId::term()) -&gt; {ok, term()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="response-1"></a>

### response/1 ###

<pre><code>
response(X1::{ok, term()} | <a href="#type-error">error()</a>) -&gt; {ok, term()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="reusable_query-2"></a>

### reusable_query/2 ###

<pre><code>
reusable_query(Query::<a href="#type-query">query()</a>, QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; {ok, term()} | <a href="#type-error">error()</a>
</code></pre>
<br />

