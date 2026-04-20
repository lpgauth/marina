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
ENV LD_LIBRARY_PATH="/opt/erlang-libs"

# Erlang 28.3.1 is built on Debian trixie (OpenSSL 3.4); Scylla 6.2.3 is
# Ubuntu 24.04 noble (OpenSSL 3.0). Shipping the trixie libssl alongside
# the Erlang install lets crypto.so resolve its OPENSSL_3.4.0 symbol version
# without overwriting the Ubuntu system libs that Scylla itself links
# against.
COPY --from=erlang:28.3.1 /usr/lib/x86_64-linux-gnu/libssl.so.3 /opt/erlang-libs/
COPY --from=erlang:28.3.1 /usr/lib/x86_64-linux-gnu/libcrypto.so.3 /opt/erlang-libs/

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
