# marina

__Author:__ Louis-Philippe Gauthier.

Non-blocking Erlang Cassandra CQL3 client

[![Build Status](https://travis-ci.org/lpgauth/marina.svg?branch=master)](https://travis-ci.org/lpgauth/marina)

### Requirements

* Cassandra 2.1.* +
* Erlang 16.0 +

### Features

* Backpressure via backlog (OOM protection)
* Compression support (lz4)
* CQL spec 3.2.0
* Fast pool implementation (random | round_robin)
* Performance optimized
* Prepared statement cache

### Environment variables

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>ip</td>
    <td>list()</td>
    <td>"127.0.0.1"</td>
    <td>server ip</td>
  </tr>
  <tr>
    <td>port</td>
    <td>pos_integer()</td>
    <td>9042</td>
    <td>server port</td>
  </tr>
  <tr>
    <td>keyspace</td>
    <td>undefined | binary()</td>
    <td>undefined</td>
    <td>default keyspace</td>
  </tr>
  <tr>
    <td>pool_size</td>
    <td>pos_integer()</td>
    <td>16</td>
    <td>number of connections</td>
  </tr>
  <tr>
    <td>pool_strategy</td>
    <td>random | round_robin</td>
    <td>random</td>
    <td>connection selection strategy</td>
  </tr>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer()</td>
    <td>1024</td>
    <td>maximum number of concurrent requests per connection</td>
  </tr>
  <tr>
    <td>compression</td>
    <td>boolean()</td>
    <td>false</td>
    <td>enable lz4 compression</td>
  </tr>
  <tr>
    <td>reconnect</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect closed connections</td>
  </tr>
  <tr>
    <td>reconnect_time_max</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect maximum time</td>
  </tr>
  <tr>
    <td>reconnect_time_min</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect minimum time</td>
  </tr>
</table>

## API
<a href="http://github.com/lpgauth/marina/blob/master/doc/marina.md#index" class="module">Function Index</a>

## Examples

```erlang
1> marina_app:start().
ok

2> marina:query(<<"SELECT * FROM test.users LIMIT 1;">>, ?CONSISTENCY_ONE, [], 1000).
{ok,{result,{result_metadata,4,
    [{column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
     {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
     {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
     {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}]},
    1,
    [[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>,
      <<"test">>,
      <<"test2">>,
      <<0,0,0,0>>]]
}}

3> marina:query(<<"SELECT * FROM test.users WHERE key = ?;">>,
[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>], ?CONSISTENCY_ONE,
[{skip_metadata, true}], 1000).

{ok,{result,{result_metadata,4,[]},
    1,
    [[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>,
      <<"test">>,
      <<"test2">>,
      <<0,0,0,0>>]]
}}

4> marina:async_reusable_query(<<"SELECT * FROM test.users WHERE key = ?;">>,
[<<207,85,107,110,157,137,17,226,167,153,120,43,203,102,219,173>>],
?CONSISTENCY_ONE, [],self(),500).

{ok,#Ref<0.0.0.124>}

5> flush().
Shell got {marina,#Ref<0.0.0.124>,
    {ok,{frame,0,0,8,
        <<0,0,0,2,0,0,0,1,0,0,0,4,0,3,82,84,66,0,5,117,
        115,101,114,115,0,3,107,101,121,0,12,0,7,99,
        111,108,117,109,110,49,0,13,0,7,99,111,108,
        117,109,110,50,0,13,0,5,118,97,108,117,101,0,
        3,0,0,0,0>>
    }}}
```

## TODO

* batched queries
* token aware load balancing

## Tests

```makefile
make dialyzer
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2015 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
