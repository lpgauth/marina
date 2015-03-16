

# Module marina_app #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md).

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###


<pre><code>
start() -&gt; ok
</code></pre>
<br />


<a name="start-2"></a>

### start/2 ###

`start(StartType, StartArgs) -> any()`


<a name="stop-1"></a>

### stop/1 ###

`stop(State) -> any()`


