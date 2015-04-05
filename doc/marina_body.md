

# Module marina_body #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-frame">frame()</a> ###



<pre><code>
frame() = #frame{flags = undefined | <a href="#type-frame_flag">frame_flag()</a>, stream = undefined | integer(), opcode = undefined | non_neg_integer(), body = undefined | binary()}
</code></pre>





### <a name="type-frame_flag">frame_flag()</a> ###



<pre><code>
frame_flag() = 0 | 1
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Frame::<a href="#type-frame">frame()</a>) -&gt; {ok, term()} | {error, atom()}
</code></pre>
<br />


