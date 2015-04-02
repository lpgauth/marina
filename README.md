

# marina #

__Authors:__ Louis-Philippe Gauthier.

Non-blocking Erlang Cassandra CQL3 client


## Features ##

* Async mode
* Backpressure via backlog (OOM protection)
* Binary protocol V3
* CQL protocol 3.2.0
* Performance optimized
* Prepared statement cache
* Request pipelining



## Environment variables ##

* ip : server ip
* port : server port
* keyspace : default keyspace
* pool_size : number of connections
* max_backlog_size: maximum number of concurrent requests per connection



## Requirements ##

* Cassandra 2.1 +



## TODO ##
* batched queries
* compression
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

