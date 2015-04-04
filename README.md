

# marina #

__Authors:__ Louis-Philippe Gauthier.

Non-blocking Erlang Cassandra CQL3 client


## Requirements ##

* Cassandra 2.1 +



## Features ##

* Backpressure via backlog (OOM protection)
* Binary protocol version 3
* CQL protocol 3.2.0
* Performance optimized
* Prepared statement cache
* Request pipelining



## Environment variables ##

* ip : server ip (default: "127.0.0.1")
* port : server port (default: 9042)
* keyspace : default keyspace (default: undefined)
* pool_size : number of connections (default: 16)
* max_backlog_size: maximum number of concurrent requests per connection (default: 1024)
* compression: enable lz4 compression (default: false)



## TODO ##

* batched queries
* documentation




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina.md" class="module">marina</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_app.md" class="module">marina_app</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_backlog.md" class="module">marina_backlog</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_body.md" class="module">marina_body</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_buffer.md" class="module">marina_buffer</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_cache.md" class="module">marina_cache</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_frame.md" class="module">marina_frame</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_queue.md" class="module">marina_queue</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_request.md" class="module">marina_request</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_server.md" class="module">marina_server</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_sup.md" class="module">marina_sup</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_types.md" class="module">marina_types</a></td></tr>
<tr><td><a href="http://github.com/lpgauth/marina/blob/master/doc/marina_utils.md" class="module">marina_utils</a></td></tr></table>

