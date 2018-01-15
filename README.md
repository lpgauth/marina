# marina

High-Performance Erlang Cassandra CQL Client

[![Build Status](https://travis-ci.org/lpgauth/marina.svg?branch=master)](https://travis-ci.org/lpgauth/marina)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/marina/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/marina?branch=master)

#### Requirements

* Cassandra 2.1+
* Erlang 16.0+

#### Features

* Compression support (LZ4)
* CQL spec 3.2.0
* Fast pool implementation (random | round_robin)
* Performance optimized
* Prepared statement cache

## API
<a href="http://github.com/lpgauth/marina/blob/master/doc/marina.md#index" class="module">Function Index</a>

#### Environment variables

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer()</td>
    <td>1024</td>
    <td>maximum number of concurrent requests per connection</td>
  </tr>
  <tr>
    <td>bootstrap_ips</td>
    <td>[list()]</td>
    <td>["5.5.5.5", "5.5.5.6"]</td>
    <td>ips used to bootstrap the pool</td>
  </tr>
  <tr>
    <td>compression</td>
    <td>boolean()</td>
    <td>false</td>
    <td>enable lz4 compression</td>
  </tr>
  <tr>
    <td>datacenter</td>
    <td>binary()</td>
    <td><<"MYDC">></td>
    <td>filter peers to a specific data center</td>
  </tr>
  <tr>
    <td>ip (deprecated)</td>
    <td>list()</td>
    <td>"127.0.0.1"</td>
    <td>server ip</td>
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
    <td>port</td>
    <td>pos_integer()</td>
    <td>9042</td>
    <td>server port</td>
  </tr>
  <tr>
    <td>reconnect</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect closed connections</td>
  </tr>
  <tr>
    <td>reconnect_time_max</td>
    <td>pos_integer() | infinity</td>
    <td>120000</td>
    <td>reconnect maximum time</td>
  </tr>
  <tr>
    <td>reconnect_time_min</td>
    <td>pos_integer()</td>
    <td>1500</td>
    <td>reconnect minimum time</td>
  </tr>
  <tr>
    <td>socket_options</td>
    <td>[gen_tcp:connect_option()]</td>
    <td>
        [binary,
        {buffer, 65535},
        {nodelay, true},
        {packet, raw},
        {send_timeout, 50},
        {send_timeout_close, true}]
    </td>
    <td>options passed to the socket</td>
  </tr>
</table>

## Pool

The pool is bootstraped by querying the `system.peers` table. If `bootstrap_ips` is provided, `marina` will try each ip until it can connect and retrieve the peers. For backward compatibility, `ip` will be used if `bootstrap_ips` is not provided. You can filter the lists of connected peers with `filter_dc`.

## Examples

```erlang
1> marina_app:start().

{ok,[granderl,metal,foil,shackle,marina]}

2> marina:query(<<"SELECT * FROM test.users LIMIT 1;">>, [], ?CONSISTENCY_ONE, [], 1000).

{ok,{result,{result_metadata,4,
                             [{column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                              {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                              {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                              {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}],
                             undefined},
            1,
            [[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,
                125>>,
              <<"test">>,<<"test2">>,
              <<0,0,0,0>>]]}}

3> marina:query(<<"SELECT * FROM test.users WHERE key = ?;">>,
[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,125>>], ?CONSISTENCY_ONE,
[{skip_metadata, true}], 1000).

{ok,{result,{result_metadata,4,[],undefined},
            1,
            [[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,
                125>>,
              <<"test">>,<<"test2">>,
              <<0,0,0,0>>]]}}

4> {ok, Ref} = marina:async_reusable_query(<<"SELECT * FROM test.users WHERE key = ?;">>,
[<<207,85,107,110,157,137,17,226,167,153,120,43,203,102,219,173>>],
?CONSISTENCY_ONE, [],self(),500).

{ok,{'marina_00000000-0000-0000-0000-000000000000_2',#Ref<0.584071807.1659371522.17616>}}

5> marina:receive_response(Ref).

{ok,{result,{result_metadata,4,
                             [{column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                              {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                              {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                              {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}],
                             undefined},
            0,[]}}
```

## TODO

* Batch queries
* Token-aware load balancing

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2015-2017 Louis-Philippe Gauthier

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
