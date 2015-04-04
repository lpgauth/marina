

# Module marina_frame #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-frame">frame()</a> ###



<pre><code>
frame() = #frame{flags = any(), stream = undefined | integer(), opcode = undefined | non_neg_integer(), body = undefined | binary()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#flags-1">flags/1</a></td><td></td></tr><tr><td valign="top"><a href="#pending_size-1">pending_size/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Bin::binary()) -&gt; {binary(), [<a href="#type-frame">frame()</a>]}
</code></pre>
<br />


<a name="encode-1"></a>

### encode/1 ###


<pre><code>
encode(Frame::<a href="#type-frame">frame()</a>) -&gt; binary()
</code></pre>
<br />


<a name="flags-1"></a>

### flags/1 ###


<pre><code>
flags(X1::boolean()) -&gt; 0 | 1
</code></pre>
<br />


<a name="pending_size-1"></a>

### pending_size/1 ###


<pre><code>
pending_size(X1::binary()) -&gt; pos_integer() | undefined
</code></pre>
<br />


