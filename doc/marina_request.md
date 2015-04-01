

# Module marina_request #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-consistency">consistency()</a> ###



<pre><code>
consistency() = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 16
</code></pre>





### <a name="type-flags">flags()</a> ###



<pre><code>
flags() = 0..254
</code></pre>





### <a name="type-query">query()</a> ###



<pre><code>
query() = binary()
</code></pre>





### <a name="type-statement_id">statement_id()</a> ###



<pre><code>
statement_id() = binary()
</code></pre>





### <a name="type-stream">stream()</a> ###



<pre><code>
stream() = 0..32768
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute-5">execute/5</a></td><td></td></tr><tr><td valign="top"><a href="#prepare-2">prepare/2</a></td><td></td></tr><tr><td valign="top"><a href="#query-4">query/4</a></td><td></td></tr><tr><td valign="top"><a href="#startup-0">startup/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute-5"></a>

### execute/5 ###


<pre><code>
execute(Stream::<a href="#type-stream">stream()</a>, StatementId::<a href="#type-statement_id">statement_id()</a>, Values::[binary()], ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::<a href="#type-flags">flags()</a>) -&gt; binary()
</code></pre>
<br />


<a name="prepare-2"></a>

### prepare/2 ###


<pre><code>
prepare(Stream::<a href="#type-stream">stream()</a>, Query::<a href="#type-query">query()</a>) -&gt; binary()
</code></pre>
<br />


<a name="query-4"></a>

### query/4 ###


<pre><code>
query(Stream::<a href="#type-stream">stream()</a>, Query::<a href="#type-query">query()</a>, ConsistencyLevel::<a href="#type-consistency">consistency()</a>, Flags::<a href="#type-flags">flags()</a>) -&gt; binary()
</code></pre>
<br />


<a name="startup-0"></a>

### startup/0 ###


<pre><code>
startup() -&gt; binary()
</code></pre>
<br />


