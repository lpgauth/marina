

# marina #

Non-blocking Erlang Cassandra CQL3 client

## Requirements

* Cassandra 2.1 +
* Erlang 17.0 +

### Features

* Binary protocol version 3
* CQL protocol 3.2.0
* Performance optimized
* Request pipelining
* Backpressure via backlog (OOM protection)
* Prepared statement cache
* Compression support (lz4)

## Examples

```erlang
marina:query(<<"SELECT * FROM \"test\".users LIMIT 1;">>, ?CONSISTENCY_ONE, [], 1000).
{ok,{result,{result_metadata,4,
                             [{column_spec,<<"test">>,<<"users">>,<<"key">>,uid},
                              {column_spec,<<"test">>,<<"users">>,<<"column1">>,varchar},
                              {column_spec,<<"test">>,<<"users">>,<<"column2">>,varchar},
                              {column_spec,<<"test">>,<<"users">>,<<"value">>,blob}]},
            1,
            [[<<153,73,45,254,217,74,17,228,175,57,88,244,65,16,117,
                125>>,
              <<"test">>,<<"test2">>,
              <<0,0,0,0>>]]}}

```

## Environment variables

<table width="100%" border="0"><theader><th>Name</th><th>Type</th><th>Default</th><th>Description</th></tr><tr><td>ip</td><td>list()</td><td>"127.0.0.1"</td><td>server ip</td></tr><tr><td>port</td><td>pos_integer()</td><td>9042</td><td>server port</td></tr><tr><td>keyspace</td><td>undefined | binary()</td><td>undefined</td><td>default keyspace</td></tr><tr><td>pool_size</td><td>pos_integer()</td><td>16</td><td>number of connections</td></tr><tr><td>backlog_size</td><td>pos_integer()</td><td>1024</td><td>maximum number of concurrent requests per connection</td></tr><tr><td>compression</td><td>boolean()</td><td>false</td><td>enable lz4 compression</td></tr>
</table>

## Modules

<table width="100%" border="0">
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina.md" class="module">marina</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_app.md" class="module">marina_app</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_backlog.md" class="module">marina_backlog</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_body.md" class="module">marina_body</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_buffer.md" class="module">marina_buffer</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_cache.md" class="module">marina_cache</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_frame.md" class="module">marina_frame</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_queue.md" class="module">marina_queue</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_request.md" class="module">marina_request</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_server.md" class="module">marina_server</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_sup.md" class="module">marina_sup</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_types.md" class="module">marina_types</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/dev/doc/marina_utils.md" class="module">marina_utils</a></td></tr></table>

## TODO

* batched queries
* documentation
* token aware load balancing
