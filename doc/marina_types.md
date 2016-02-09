

# Module marina_types #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode_bytes-1">decode_bytes/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_int-1">decode_int/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_long-1">decode_long/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_long_string-1">decode_long_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_short-1">decode_short/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_short_bytes-1">decode_short_bytes/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_string-1">decode_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_string_list-1">decode_string_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_string_map-1">decode_string_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_string_multimap-1">decode_string_multimap/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_uuid-1">decode_uuid/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_boolean-1">encode_boolean/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_bytes-1">encode_bytes/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_int-1">encode_int/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_long-1">encode_long/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_long_string-1">encode_long_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_short-1">encode_short/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_short_bytes-1">encode_short_bytes/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_string-1">encode_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_string_list-1">encode_string_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_string_map-1">encode_string_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_string_multimap-1">encode_string_multimap/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode_bytes-1"></a>

### decode_bytes/1 ###

<pre><code>
decode_bytes(Bin::binary()) -&gt; {null, binary()} | {binary(), binary()}
</code></pre>
<br />

<a name="decode_int-1"></a>

### decode_int/1 ###

<pre><code>
decode_int(X1::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="decode_long-1"></a>

### decode_long/1 ###

<pre><code>
decode_long(X1::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="decode_long_string-1"></a>

### decode_long_string/1 ###

<pre><code>
decode_long_string(Bin::binary()) -&gt; {binary(), binary()}
</code></pre>
<br />

<a name="decode_short-1"></a>

### decode_short/1 ###

<pre><code>
decode_short(X1::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="decode_short_bytes-1"></a>

### decode_short_bytes/1 ###

<pre><code>
decode_short_bytes(Bin::binary()) -&gt; {binary(), binary()}
</code></pre>
<br />

<a name="decode_string-1"></a>

### decode_string/1 ###

<pre><code>
decode_string(Bin::binary()) -&gt; {binary(), binary()}
</code></pre>
<br />

<a name="decode_string_list-1"></a>

### decode_string_list/1 ###

<pre><code>
decode_string_list(Bin::binary()) -&gt; {[binary()], binary()}
</code></pre>
<br />

<a name="decode_string_map-1"></a>

### decode_string_map/1 ###

<pre><code>
decode_string_map(Bin::binary()) -&gt; {[{binary(), binary()}], binary()}
</code></pre>
<br />

<a name="decode_string_multimap-1"></a>

### decode_string_multimap/1 ###

<pre><code>
decode_string_multimap(Bin::binary()) -&gt; {[{binary(), [binary()]}], binary()}
</code></pre>
<br />

<a name="decode_uuid-1"></a>

### decode_uuid/1 ###

<pre><code>
decode_uuid(X1::binary()) -&gt; {binary(), binary()}
</code></pre>
<br />

<a name="encode_boolean-1"></a>

### encode_boolean/1 ###

<pre><code>
encode_boolean(X1::boolean()) -&gt; binary()
</code></pre>
<br />

<a name="encode_bytes-1"></a>

### encode_bytes/1 ###

<pre><code>
encode_bytes(Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="encode_int-1"></a>

### encode_int/1 ###

<pre><code>
encode_int(Value::integer()) -&gt; binary()
</code></pre>
<br />

<a name="encode_long-1"></a>

### encode_long/1 ###

<pre><code>
encode_long(Value::integer()) -&gt; binary()
</code></pre>
<br />

<a name="encode_long_string-1"></a>

### encode_long_string/1 ###

<pre><code>
encode_long_string(Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="encode_short-1"></a>

### encode_short/1 ###

<pre><code>
encode_short(Value::integer()) -&gt; binary()
</code></pre>
<br />

<a name="encode_short_bytes-1"></a>

### encode_short_bytes/1 ###

<pre><code>
encode_short_bytes(Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="encode_string-1"></a>

### encode_string/1 ###

<pre><code>
encode_string(Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="encode_string_list-1"></a>

### encode_string_list/1 ###

<pre><code>
encode_string_list(Values::[binary()]) -&gt; binary()
</code></pre>
<br />

<a name="encode_string_map-1"></a>

### encode_string_map/1 ###

<pre><code>
encode_string_map(KeyValues::[{binary(), binary()}]) -&gt; binary()
</code></pre>
<br />

<a name="encode_string_multimap-1"></a>

### encode_string_multimap/1 ###

<pre><code>
encode_string_multimap(KeyValues::[{binary(), [binary()]}]) -&gt; binary()
</code></pre>
<br />

