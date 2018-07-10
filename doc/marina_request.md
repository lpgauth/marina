

# Module marina_request #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-consistency_level">consistency_level()</a> ###


<pre><code>
consistency_level() = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
</code></pre>




### <a name="type-frame_flag">frame_flag()</a> ###


<pre><code>
frame_flag() = 0..1
</code></pre>




### <a name="type-query">query()</a> ###


<pre><code>
query() = binary()
</code></pre>




### <a name="type-query_opts">query_opts()</a> ###


<pre><code>
query_opts() = #{consistency_level =&gt; <a href="#type-consistency_level">consistency_level()</a>, page_size =&gt; pos_integer(), paging_state =&gt; binary(), pid =&gt; pid(), routing_key =&gt; binary(), skip_metadata =&gt; boolean(), timeout =&gt; pos_integer(), values =&gt; <a href="#type-values">values()</a>}
</code></pre>




### <a name="type-statement_id">statement_id()</a> ###


<pre><code>
statement_id() = binary()
</code></pre>




### <a name="type-stream">stream()</a> ###


<pre><code>
stream() = 0..32768
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#auth_response-3">auth_response/3</a></td><td></td></tr><tr><td valign="top"><a href="#execute-4">execute/4</a></td><td></td></tr><tr><td valign="top"><a href="#prepare-3">prepare/3</a></td><td></td></tr><tr><td valign="top"><a href="#query-4">query/4</a></td><td></td></tr><tr><td valign="top"><a href="#startup-1">startup/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="auth_response-3"></a>

### auth_response/3 ###

<pre><code>
auth_response(FrameFlags::<a href="#type-frame_flag">frame_flag()</a>, Username::binary(), Password::binary()) -&gt; iolist()
</code></pre>
<br />

<a name="execute-4"></a>

### execute/4 ###

<pre><code>
execute(Stream::<a href="#type-stream">stream()</a>, FrameFlags::<a href="#type-frame_flag">frame_flag()</a>, StatementId::<a href="#type-statement_id">statement_id()</a>, QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="prepare-3"></a>

### prepare/3 ###

<pre><code>
prepare(Stream::<a href="#type-stream">stream()</a>, FrameFlags::<a href="#type-frame_flag">frame_flag()</a>, Query::<a href="#type-query">query()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="query-4"></a>

### query/4 ###

<pre><code>
query(Stream::<a href="#type-stream">stream()</a>, FrameFlags::<a href="#type-frame_flag">frame_flag()</a>, Query::<a href="#type-query">query()</a>, QueryOpts::<a href="#type-query_opts">query_opts()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="startup-1"></a>

### startup/1 ###

<pre><code>
startup(FrameFlags::<a href="#type-frame_flag">frame_flag()</a>) -&gt; iolist()
</code></pre>
<br />

