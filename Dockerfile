FROM scylladb/scylla:6.2.3

RUN apt-get update && apt-get -y --no-install-recommends install \
  ca-certificates \
  dpkg-dev \
  g++ \
  gcc \
  git \
  libncurses6 \
  libsctp1 \
  libssl-dev \
  make \
  && rm -rf /var/lib/apt/lists/*

ENV PATH="/usr/local/bin:${PATH}"

COPY --from=erlang:28.3.1 /usr/local/lib/erlang /usr/local/lib/erlang
COPY --from=erlang:28.3.1 /usr/local/bin/ct_run /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/dialyzer /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/epmd /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/erl /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/erlc /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/escript /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/rebar3 /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/run_erl /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/to_erl /usr/local/bin/
COPY --from=erlang:28.3.1 /usr/local/bin/typer /usr/local/bin/
